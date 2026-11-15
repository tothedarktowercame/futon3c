(ns futon3c.giraffe.reread-service-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.giraffe.preview-refresh]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.excursions :as excursions]
            [futon3c.giraffe.reread-cli :as cli]
            [futon3c.giraffe.reread-mint-batch :as batch]
            [futon3c.giraffe.reread-queue :as queue]
            [futon3c.giraffe.reread-service :as service]
            [futon3c.giraffe.reread-worker :as worker]
            [futon3c.giraffe.run :as run]
            [futon3c.xiang.turn-service :as turn-service]
            [futon3c.xiang.turn-store :as ts]
            [next.jdbc :as jdbc])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit]))

(defn- temp-dir [] (.toFile (Files/createTempDirectory "reread-event-test-" (make-array FileAttribute 0))))
(defn- service-state [] @(var-get (ns-resolve 'futon3c.giraffe.reread-service 'state)))
(defn- join! []
  (when-let [executor (:executor (service-state))]
    (is (.awaitTermination ^ScheduledThreadPoolExecutor executor 2 TimeUnit/SECONDS))))

(use-fixtures :each
  (fn [f]
    (service/stop!) (join!)
    (with-redefs [worker/agency-contact (fn [& _] (throw (ex-info "Real reader forbidden" {})))
                  worker/kimi-contact (fn [& _] (throw (ex-info "Real reader forbidden" {})))
                  cli/health (fn [{:keys [store]}] {:requests (queue/requests store)})]
      (try (f) (finally (service/stop!) (join!))))))

(defn- fixture []
  (let [root (temp-dir) store (queue/open-store (io/file root "queue.sqlite"))]
    {:store store :env-file false :environment {} :reader-seats ["fixture-reader"]
     :new-item-cap 1 :mint-window-ms 100 :debounce-ms 30
     :mint-state-path (io/file root "mint.edn")
     :snapshot-fn (fn [_] {:open-items [] :coverage {:members []}})
     :mint-fn (fn [] {:status :minted :run-id "fixture-run"})}))

(def path "/invented/holes/missions/M-event-fixture.md")
(defn- snapshot [path _]
  {:id "M-event-fixture" :source {:path path :sha256 "new"}
   :item {:id :M-event-fixture :source-pin {:path path :sha256 "new"}}
   :coverage {:members [{:id "M-event-fixture" :status :read
                         :source {:path path :sha256 "old"}}]}})

(deftest event-reconciles-one-item-and-debounces-on-real-executor
  (let [config (fixture) calls (atom 0) reconciles (atom 0) boot (promise) event (promise)]
    (with-redefs [service/item-snapshot (fn [p c] (swap! reconciles inc) (snapshot p c))
                  worker/run-once! (fn [_]
                                     (if (= 1 (swap! calls inc)) (deliver boot true) (deliver event true))
                                     {:status :idle})]
      (is (= :starting (:status (service/start! config))))
      (is (= true (deref boot 2000 :timeout)))
      (is (= :already-started (:status (service/start! config))))
      (dotimes [_ 8] (service/notify-doc-changed! path))
      (is (= true (deref event 2000 :timeout)))
      ;; claude-13 review: let any further (wrongly undebounced) runs land
      ;; before counting, or the count races the executor (30 ms debounce).
      (Thread/sleep 300)
      (is (= 1 @reconciles))
      (is (= 2 @calls) "boot drain + one event drain, not one per notify")
      (is (= [["M-event-fixture" "new" :pending]]
             (mapv (juxt :target_id :source_sha :state) (queue/requests (:store config)))))
      (is (empty? (:errors (service/health)))))))

(deftest startup-catches-up-once-and-landed-event-mints
  (let [config (fixture) boot (promise) minted (promise) snapshots (atom 0)]
    (with-redefs [service/item-snapshot snapshot
                  worker/run-once!
                  (fn [{:keys [store now-ms new-item-cap reader-id]}]
                    (if-let [claim (queue/claim! store now-ms 1000 new-item-cap reader-id)]
                      {:status :landed :request (queue/settle! store claim {:status :landed :digest "fixture"} now-ms 3)}
                      (do (deliver boot true) {:status :idle})))]
      (service/start! (assoc config :snapshot-fn (fn [_] (swap! snapshots inc)
                                                 {:open-items [] :coverage {:members []}})
                             :mint-fn #(do (deliver minted true) {:status :minted :run-id "fixture"})))
      (is (= true (deref boot 2000 :timeout)))
      (service/notify-doc-changed! path)
      (is (= true (deref minted 2000 :timeout)))
      (is (= 1 @snapshots))
      (is (= [:landed] (mapv :state (queue/requests (:store config))))))))

(deftest unfinished-mint-has-one-deadline-even-with-blocked-queue
  (let [config (fixture) done (promise)]
    (batch/step! {:path (:mint-state-path config) :window-ms 100 :now-ms (System/currentTimeMillis)
                  :landed-count 1 :queue-idle? true :mint-fn (:mint-fn config)})
    (with-redefs [worker/run-once! (fn [_] {:status :idle})
                  queue/requests (fn [& _] [{:state :claimed}])]
      ;; A previously accumulated landing with a current mint window.
      (batch/step! {:path (:mint-state-path config) :window-ms 100 :now-ms (System/currentTimeMillis)
                    :landed-count 1 :queue-idle? false :mint-fn (:mint-fn config)})
      (service/start! (assoc config :mint-fn #(do (deliver done true) {:status :minted :run-id "fixture-next"})))
      (is (= true (deref done 2000 :timeout))))))

(deftest stop-cancels-debounce-and-does-not-start-from-notify
  (let [boot (promise) calls (atom 0)]
    (is (= :stopped (:status (service/notify-doc-changed! path))))
    (with-redefs [worker/run-once! (fn [_] (deliver boot true) {:status :idle})
                  service/item-snapshot (fn [& _] (swap! calls inc))]
      (service/start! (assoc (fixture) :debounce-ms 500))
      (is (= true (deref boot 2000 :timeout)))
      (service/notify-doc-changed! path)
      (service/stop!) (join!)
      (is (= 0 @calls)))))

(deftest failed-document-remains-visible-without-a-poll-loop
  (let [boot (promise) attempted (promise)]
    (with-redefs [worker/run-once! (fn [_] (deliver boot true) {:status :idle})
                  service/item-snapshot (fn [& _] (deliver attempted true) (throw (ex-info "fixture failure" {})))]
      (service/start! (fixture))
      (is (= true (deref boot 2000 :timeout)))
      (service/notify-doc-changed! path)
      (is (= true (deref attempted 2000 :timeout)))
      (service/stop!) (join!)
      (is (= "fixture failure" (get-in (service/health) [:errors "M-event-fixture" :message]))))))

(deftest environment-file-and-process-precedence
  (let [f (io/file (temp-dir) "service.env")]
    (spit f (str "# invented config\nFUTON3C_GIRAFFE_REREAD_READERS='fixture-a,fixture-b'\n"
                 "FUTON3C_GIRAFFE_REREAD_NEW_ITEM_CAP=1\nFUTON3C_GIRAFFE_REREAD_MINT_WINDOW_MS=123\n"))
    (let [c (service/load-config {:env-file f :environment {"FUTON3C_GIRAFFE_REREAD_NEW_ITEM_CAP" "2"}})]
      (is (= ["fixture-a" "fixture-b"] (:reader-seats c)))
      (is (= 2 (:new-item-cap c)))
      (is (= 123 (:mint-window-ms c))))))

(deftest per-item-checker-does-not-check-unrelated-records-or-admit-duplicates
  (let [root (temp-dir) checker (io/file root "check_edges.py") members (io/file root "members.json")
        records (io/file root "batch-fixture.jsonl") calls (atom [])
        a {:id "M-fixture-a" :source {:path "/invented/a" :sha256 "a"} :edges []}
        b {:id "M-fixture-b" :source {:path "/invented/b" :sha256 "b"} :edges []}
        options {:checker checker :members-file members :batches [records] :repos [] :target-id "M-fixture-a"}]
    (spit checker "# invented checker")
    (spit members (json/generate-string [{:id "M-fixture-a" :path "/invented/a" :sha256 "a"}
                                         {:id "M-fixture-b" :path "/invented/b" :sha256 "b"}]))
    (spit records (str (json/generate-string a) "\n" (json/generate-string b) "\n"))
    (with-redefs [shell/sh (fn [cmd isolated check-path record-path]
                            (is (= ["python3" "-I" (str checker)] [cmd isolated check-path]))
                            (swap! calls conj (:id (json/parse-string (slurp record-path) true)))
                            {:exit 0 :out "" :err ""})]
      (let [c (:coverage (readings/consume options))]
        (is (= ["M-fixture-a"] @calls))
        (is (= [:read] (mapv :status (:members c))))
        (is (= :partial (:status c))))
      (spit records (str (json/generate-string a) "\n") :append true)
      (is (= [:ambiguous] (mapv :status (get-in (readings/consume options) [:coverage :members])))))))

(deftest service-has-no-process-launch-or-periodic-scheduling
  ;; Supplement the runtime tests with a guard against introducing a timer or
  ;; delegating the service back to a CLI JVM.
  (let [text (slurp "src/futon3c/giraffe/reread_service.clj")]
    (is (not (re-find #"scheduleAtFixedRate|scheduleWithFixedDelay|Timer\.|ProcessBuilder|shell/sh|supported-mint!|/-main" text)))))

(deftest in-process-mint-uses-pass-writer-and-retained-focus
  (let [written (atom nil)]
    (with-redefs [run/build-adapter-result (fn [_] {:input {}})
                  excursions/combined-pass (fn [_] {:order []})
                  run/retained-focus-input (fn [_] {:fixture :focus})
                  run/write-run! (fn [args] (reset! written args) {:run-id "fixture-run"})
                  shell/sh (fn [& args] (is (= ["git" "rev-parse" "HEAD"] (take 3 args)))
                             {:exit 0 :out "fixture-sha\n"})]
      (is (= {:status :minted :run-id "fixture-run"}
             (run/mint! {:root "/invented" :edges "/invented/edges" :readings-root "/invented/corpus"})))
      (is (= {:fixture :focus} (:focus-relation-input @written)))
      (is (true? (get-in @written [:invocation :with-retained-focus?]))))))

(deftest startup-resumes-an-orphaned-claim-at-its-one-shot-lease-deadline
  (let [config (fixture) store (:store config) now (System/currentTimeMillis)
        snap (snapshot path nil) minted (promise)]
    (queue/reconcile-item! store "M-event-fixture" (:item snap) (:coverage snap) now)
    (queue/claim! store now 150 1 "previous-process")
    (with-redefs [worker/run-once!
                  (fn [{:keys [store now-ms new-item-cap reader-id]}]
                    (if-let [claim (queue/claim! store now-ms 1000 new-item-cap reader-id)]
                      {:status :landed :request (queue/settle! store claim {:status :landed :digest "fixture"} now-ms 3)}
                      {:status :idle}))]
      (service/start! (assoc config :snapshot-fn (fn [_] {:open-items [(:item snap)] :coverage (:coverage snap)})
                             :mint-fn #(do (deliver minted true) {:status :minted :run-id "fixture"})))
      (is (= true (deref minted 2000 :timeout)))
      (is (= [:landed] (mapv :state (queue/requests store)))))))

(deftest changed-real-document-enqueues-its-exact-digest
  (let [config (fixture) root (temp-dir) repo (io/file root "fixture-repo")
        doc (io/file repo "holes/excursions/E-event-fixture.md")
        corpus (io/file root "corpus") boot (promise) enqueued (promise)]
    (.mkdirs (io/file repo ".git"))
    (.mkdirs (.getParentFile doc))
    (.mkdirs corpus)
    (spit doc "# Invented excursion\nStatus: OPEN\n")
    (spit (io/file corpus "members.json") "[]")
    (spit (io/file corpus "check_edges.py") "# No admitted records in this fixture")
    (with-redefs [worker/run-once! (fn [{:keys [store]}]
                                     (if (seq (queue/requests store))
                                       (deliver enqueued true) (deliver boot true))
                                     {:status :idle})
                  shell/sh (fn [& _] (throw (ex-info "No process needed for unread item" {})))]
      (service/start! (assoc config :pointer (str corpus)))
      (is (= true (deref boot 2000 :timeout)))
      (service/notify-doc-changed! (str doc))
      (is (= true (deref enqueued 2000 :timeout)))
      (let [rows (queue/requests (:store config))]
        (is (= 1 (count rows)))
        (is (= "E-event-fixture" (:target_id (first rows))))
        (is (= (readings/sha256 (slurp doc)) (:source_sha (first rows))))))))

(deftest health-remains-callable-when-corpus-authority-is-unavailable
  (let [boot (promise)]
    (with-redefs [worker/run-once! (fn [_] (deliver boot true) {:status :idle})]
      (service/start! (fixture))
      (is (= true (deref boot 2000 :timeout)))
      (with-redefs [cli/health (fn [_] (throw (ex-info "fixture corpus unavailable" {})))]
        (let [h (service/health)]
          (is (= :attention (:status h)))
          (is (= "fixture corpus unavailable" (:health-error h)))
          (is (= [] (:requests h))))))))

(defn- turn-fixture []
  (let [config (fixture) root (temp-dir) sidecar (io/file root "entities.sqlite")
        db (jdbc/get-datasource {:dbtype "sqlite" :dbname (str sidecar)})
        store (ts/store (io/file root "turns"))]
    (jdbc/execute! db ["CREATE TABLE ent_node(id TEXT, external_id TEXT, type TEXT)"])
    (doseq [[id external kind] [["fixture/mission/x" "M-x" ":mission/doc"]
                                ["fixture/ticket/y" "T-y" ":ticket/doc"]
                                ["fixture/excursion/z" "E-z" "excursion/doc"]
                                ["legacy-x" "M-x" ":mission"]]]
      (jdbc/execute! db ["INSERT INTO ent_node VALUES(?,?,?)" id external kind]))
    (assoc config :entity-sidecar (str sidecar) :turn-store store :fixture-db db)))

(defn- flush-events! []
  (.get (.submit ^ScheduledThreadPoolExecutor (:executor (service-state))
                 ^java.util.concurrent.Callable (fn [] true)) 2 TimeUnit/SECONDS))

(deftest settled-turn-marks-two-items-and-mints-once-without-rereading
  (let [config (turn-fixture) listener (atom nil) boot (promise) minted (promise)
        workers (atom 0) mints (atom 0) id "turn-fixtureDiscuss"]
    (ts/write-record-id! (:turn-store config) id
                         {:source_text "Discuss M-x and M-x; M-unknown is not indexed."
                          :reply_text "This also concerns T-y."})
    (ts/publish-analysis! (:turn-store config) id {:acts [{:referent "M-x"}]})
    (with-redefs [turn-service/add-turn-listener! (fn [key f]
                                                   (is (= :futon3c.giraffe.reread-service/giraffe key))
                                                   (reset! listener f))
                  worker/run-once! (fn [_] (swap! workers inc) (deliver boot true) {:status :idle})
                  service/item-snapshot (fn [& _] (throw (ex-info "Turn must not reread a document" {})))
                  queue/reconcile-item! (fn [& _] (throw (ex-info "Turn must not enqueue" {})))]
      (service/start! (assoc config :mint-fn #(do (swap! mints inc) (deliver minted true)
                                                 {:status :minted :run-id "fixture-turn-priority"})))
      (is (= true (deref boot 2000 :timeout)))
      (@listener {:event :recorded :id id})
      (@listener {:event :settled :id id})
      (@listener {:event :settled :id id})
      (flush-events!)
      (let [h (service/health)]
        (is (= #{"M-x" "T-y"} (set (keys (:discussed h)))))
        (is (= #{"fixture/mission/x" "fixture/ticket/y"}
               (set (map :entity-id (vals (:discussed h))))))
        (is (every? #(and (= id (:turn-id %)) (pos-int? (:at-ms %))) (vals (:discussed h))))
        (is (= 1 (:unknown-turn-item-count h)))
        (is (= [] (:requests h))))
      (is (= true (deref minted 2000 :timeout)))
      (flush-events!)
      (is (= 1 @mints))
      (is (= 1 @workers))
      (is (= 0 (:pending-landings (batch/read-state (:mint-state-path config)))))
      (is (empty? (:errors (service/health)))))))

(deftest turns-with-no-items-or-only-unknown-items-do-not-mint
  (let [config (turn-fixture) listener (atom nil) boot (promise) mints (atom 0)]
    (ts/write-record-id! (:turn-store config) "turn-fixtureNone" {:source_text "No task identifiers here."})
    (ts/write-record-id! (:turn-store config) "turn-fixtureUnknown" {:reply_text "M-unknown M-unknown"})
    (with-redefs [turn-service/add-turn-listener! (fn [_ f] (reset! listener f))
                  worker/run-once! (fn [_] (deliver boot true) {:status :idle})]
      (service/start! (assoc config :mint-fn #(do (swap! mints inc) {:status :minted})))
      (is (= true (deref boot 2000 :timeout)))
      (@listener {:event :settled :id "turn-fixtureNone"})
      (flush-events!)
      (is (= {} (:discussed (service/health))))
      (is (= 0 (:unknown-turn-item-count (service/health))))
      (@listener {:event :settled :id "turn-fixtureUnknown"})
      (flush-events!)
      (is (= 1 (:unknown-turn-item-count (service/health))))
      (is (nil? (:turn-mint-task (service-state))))
      (is (= 0 @mints)))))

(deftest turn-resolution-is-read-only-and-rejects-ambiguous-document-identities
  (let [config (turn-fixture)]
    (is (= ["E-z"] (mapv :external-id (:items (service/items-touched-by-turn {:象 {:referent "E-z"}} config)))))
    (jdbc/execute! (:fixture-db config) ["INSERT INTO ent_node VALUES('other/x','M-x',':mission/doc')"])
    (let [result (service/items-touched-by-turn {:source_text "M-x T-y M-absent"} config)]
      (is (= ["T-y"] (mapv :external-id (:items result))))
      (is (= ["M-x"] (:ambiguous-ids result)))
      (is (= ["M-absent"] (:unknown-ids result))))
    (let [missing (io/file (temp-dir) "missing.sqlite")]
      (is (thrown? java.sql.SQLException
                   (service/items-touched-by-turn {:source_text "M-x"} {:entity-sidecar (str missing)})))
      (is (not (.exists missing))))))

(deftest stop-removes-the-circus-listener
  (let [removed (atom [])]
    (with-redefs [turn-service/remove-turn-listener! #(swap! removed conj %)]
      (service/stop!)
      (is (= [:futon3c.giraffe.reread-service/giraffe] @removed)))))

;; ------------------------------------------------- preview refresh ------

(defn- wait-for
  "Poll PRED until true or TIMEOUT-MS passes; returns the last value."
  [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (let [v (pred)]
        (if (or v (>= (System/currentTimeMillis) deadline))
          v
          (do (Thread/sleep 20) (recur)))))))

(defn- preview-config
  "Injected process layer: records every argv, and the fake giraffe_party
   writes a party.json with one top item so the top-list join is exercised."
  [root calls & {:keys [fail-party? gate]}]
  (let [out (io/file root "gcc-preview")]
    {:repo-dir (str root) :out-dir (str out)
     :exec-fn (fn [args]
                (when gate (deref gate 2000 nil))
                (swap! calls conj (vec args))
                (if (and fail-party? (some #(str/ends-with? % "giraffe_party.py") args))
                  {:exit 1 :out "" :err "party fixture failure"}
                  (do (when (some #(str/ends-with? % "giraffe_party.py") args)
                        (.mkdirs out)
                        (spit (io/file out "party.json")
                              (json/generate-string
                               {:party {:top [{:rank 1 :item "M-party-top"}]}})))
                      {:exit 0 :out "" :err ""})))}))

(defn- party-calls [calls]
  (filter #(some (fn [a] (str/ends-with? a "giraffe_party.py")) %) @calls))

(defn- preview-calls [calls]
  (filter #(some (fn [a] (str/ends-with? a "gcc_preview.py")) %) @calls))

(deftest landed-mint-triggers-exactly-one-preview-refresh-with-changed-items
  (let [config (fixture) recorded (atom []) boot (promise)]
    (with-redefs [service/item-snapshot (fn [p c] (snapshot p c))
                  worker/run-once!
                  (fn [{:keys [store now-ms new-item-cap reader-id]}]
                    (if-let [claim (queue/claim! store now-ms 1000 new-item-cap reader-id)]
                      {:status :landed :request (queue/settle! store claim {:status :landed :digest "fixture"} now-ms 3)}
                      (do (deliver boot true) {:status :idle})))]
      (service/start! (assoc config :preview (preview-config (temp-dir) recorded)))
      (is (= true (deref boot 2000 :timeout)))
      (service/notify-doc-changed! path)
      (is (true? (wait-for #(some? (:last-refresh (service/health))) 2000)))
      (Thread/sleep 200)
      (is (= 1 (count (party-calls recorded))) "exactly one refresh, no spares")
      (is (= 1 (count (preview-calls recorded))))
      (let [run-dir (str (io/file (get-in (service-state) [:config :giraffe-root]) "fixture-run"))]
        (is (contains? (set (first (party-calls recorded))) run-dir)))
      (is (= "M-event-fixture,M-party-top"
             (get-in (vec (preview-calls recorded)) [0 5]))
          "preview covers the reconciled item plus the party top list")
      (let [refresh (:last-refresh (service/health))]
        (is (true? (:ok? refresh)))
        (is (= ["M-event-fixture" "M-party-top"] (:items refresh)))
        (is (pos-int? (:at-ms refresh))))
      (is (empty? (:errors (service/health)))))))

(deftest two-mints-during-one-refresh-yield-one-follow-up
  (let [config (fixture) gate (promise) recorded (atom []) boot (promise)
        first-mint (promise) mints (atom 0)]
    (with-redefs [service/item-snapshot (let [n (atom 0)]
                                      (fn [p c] (let [sha (str "new-" (swap! n inc))]
                                                  (-> (snapshot p c)
                                                      (assoc-in [:source :sha256] sha)
                                                      (assoc-in [:item :source-pin :sha256] sha)))))
                  worker/run-once!
                  (fn [{:keys [store now-ms new-item-cap reader-id]}]
                    (if-let [claim (queue/claim! store now-ms 1000 new-item-cap reader-id)]
                      {:status :landed :request (queue/settle! store claim {:status :landed :digest "fixture"} now-ms 3)}
                      (do (deliver boot true) {:status :idle})))]
      (service/start! (assoc config :preview (preview-config (temp-dir) recorded :gate gate)
                             :mint-fn #(do (deliver first-mint true) (swap! mints inc)
                                           {:status :minted :run-id (str "fixture-run-" @mints)})))
      (is (= true (deref boot 2000 :timeout)))      (service/notify-doc-changed! path)
      (is (= true (deref first-mint 2000 :timeout)))
      ;; the first refresh is now blocked inside its first party call
      (is (true? (wait-for #(= 1 (count (party-calls recorded))) 2000)))
      (service/notify-doc-changed! path)
      (is (= true (wait-for #(= 2 @mints) 2000))
          "second mint queued while the first refresh was blocked")
      (deliver gate true)
      (is (true? (wait-for #(= 2 (count (preview-calls recorded))) 2000)))
      (Thread/sleep 200)
      (is (= 2 (count (party-calls recorded))) "one blocked + one follow-up, not more")
      (is (= 2 (count (preview-calls recorded))))
      (is (empty? (:errors (service/health)))))))

(deftest failing-script-is-recorded-and-the-service-keeps-running
  (let [config (fixture) root (temp-dir) recorded (atom []) boot (promise) mint-count (atom 0)]
    (with-redefs [service/item-snapshot (let [n (atom 0)]
                                      (fn [p c] (let [sha (str "new-" (swap! n inc))]
                                                  (-> (snapshot p c)
                                                      (assoc-in [:source :sha256] sha)
                                                      (assoc-in [:item :source-pin :sha256] sha)))))
                  worker/run-once!
                  (fn [{:keys [store now-ms new-item-cap reader-id]}]
                    (if-let [claim (queue/claim! store now-ms 1000 new-item-cap reader-id)]
                      {:status :landed :request (queue/settle! store claim {:status :landed :digest "fixture"} now-ms 3)}
                      (do (deliver boot true) {:status :idle})))]
      ;; fail the first party call only; later calls succeed
      (let [first? (atom true)
            preview (preview-config root recorded)]
        (service/start! (assoc config
                               :preview (assoc preview
                                               :exec-fn (fn [args]
                                                          (if (and @first?
                                                                   (some #(str/ends-with? % "giraffe_party.py") args))
                                                            (do (reset! first? false)
                                                                (swap! recorded conj (vec args))
                                                                {:exit 1 :out "" :err "party fixture failure"})
                                                            ((:exec-fn preview) args))))
                               :mint-fn #(do (swap! mint-count inc)
                                             {:status :minted :run-id (str "fixture-run-" @mint-count)})))
        (is (= true (deref boot 2000 :timeout)))
        (service/notify-doc-changed! path)
        (is (true? (wait-for #(= 1 @mint-count) 2000)))
        (is (true? (wait-for #(false? (:ok? (:last-refresh (service/health)))) 2000)))
        (let [refresh (:last-refresh (service/health))]
          (is (= [] (:items refresh)))
          (is (re-find #"party fixture failure" (:error refresh))))
        (is (not= :failed (:status (service/health))))
        (is (empty? (:errors (service/health))))
        ;; a second mint refreshes successfully: the service survived
        (service/notify-doc-changed! path)
        (is (true? (wait-for #(= 2 @mint-count) 2000)))
        (is (true? (wait-for #(true? (:ok? (:last-refresh (service/health)))) 2000)))))))

(deftest no-preview-refresh-without-a-mint
  (let [config (fixture) recorded (atom []) boot (promise) event (promise) calls (atom 0)]
    (with-redefs [service/item-snapshot (fn [p c] (snapshot p c))
                  worker/run-once! (fn [_]
                                     (if (= 1 (swap! calls inc)) (deliver boot true) (deliver event true))
                                     {:status :idle})]
      (service/start! (assoc config :preview (preview-config (temp-dir) recorded)))
      (is (= true (deref boot 2000 :timeout)))
      (service/notify-doc-changed! path)
      ;; reading lands but the mint batch window defers the mint
      (service/start! {}) ;; already started; no-op
      (is (= true (deref event 2000 :timeout)))
      (Thread/sleep 300)
      (is (= [] @recorded) "no helper process without a minted run")
      (is (nil? (:last-refresh (service/health)))))))


(deftest default-exec-runs-a-real-command
  ;; claude-13: the stand-in exec-fn hid that the real launcher could not run
  ;; a command at all. This calls the real one, the way `run` does.
  (let [r (futon3c.giraffe.preview-refresh/default-exec ["echo" "ok"])]
    (is (zero? (:exit r)))
    (is (= "ok" (clojure.string/trim (:out r))))))

(deftest restart-during-an-in-flight-reading-starts-at-once-and-stays-serial
  ;; claude-13, 2026-11-15: start! after stop! returned :already-started for
  ;; as long as the old executor's reading ran (~5 min live). start! now
  ;; returns at once; the new executor's first task waits for the old one.
  (let [config (fixture) gate (promise) first-in (promise) second-in (promise)
        calls (atom 0) overlap (atom false) running (atom 0)]
    (with-redefs [worker/run-once!
                  (fn [_]
                    (when (> (swap! running inc) 1) (reset! overlap true))
                    (try
                      (case (swap! calls inc)
                        1 (do (deliver first-in true) @gate)
                        2 (deliver second-in true)
                        nil)
                      {:status :idle}
                      (finally (swap! running dec))))]
      (is (= :starting (:status (service/start! config))))
      (is (= true (deref first-in 2000 :timeout)))
      (let [old-executor (:executor (service-state))]
        (service/stop!)
        (is (= :starting (:status (service/start! config)))
            "restart does not wait for the in-flight reading")
        (is (= :already-started (:status (service/start! config)))
            "a running executor is still refused")
        (is (true? (:draining-previous? (service-state))))
        (is (= :timeout (deref second-in 300 :timeout))
            "the new boot round waits for the old reading")
        (deliver gate true)
        (is (.awaitTermination ^ScheduledThreadPoolExecutor old-executor 2 TimeUnit/SECONDS))
        (is (= true (deref second-in 2000 :timeout)))
        (is (false? @overlap) "two rounds never ran at once")
        (is (false? (:draining-previous? (service-state))))))))
