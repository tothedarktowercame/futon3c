(ns futon3c.giraffe.reread-service-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.giraffe.preview-refresh]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.missions :as missions]
            [futon2.aif.mission-registry :as registry]
            [futon3c.giraffe.excursions :as excursions]
            [futon3c.giraffe.reread-cli :as cli]
            [futon3c.giraffe.reread-mint-batch :as batch]
            [futon3c.giraffe.reread-queue :as queue]
            [futon3c.giraffe.reread-service :as service]
            [futon3c.giraffe.reread-worker :as worker]
            [futon3c.giraffe.run :as run]
            [futon3c.arxana.turn-mention-writer :as mention-writer]
            [futon3c.circus.hou-store :as hou-store]
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
  (let [root (temp-dir) store (queue/open-store (io/file root "queue.sqlite"))
        corpus (doto (io/file root "corpus") .mkdirs)]
    (spit (io/file corpus "members.json") "[]")
    {:pointer corpus :store store :env-file false :environment {} :reader-seats ["fixture-reader"]
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
    ;; consume now checks its lines in ONE batch-driver process (43d3542d7);
    ;; the stub answers per line and records which ids were sent.
    (with-redefs [shell/sh (fn [cmd isolated driver check-path batch-path]
                            (is (= ["python3" "-I" (str checker)] [cmd isolated check-path]))
                            (is (str/ends-with? driver "giraffe_check_batch.py"))
                            (let [lines (remove str/blank? (str/split-lines (slurp batch-path)))]
                              (swap! calls into (map #(:id (json/parse-string % true)) lines))
                              {:exit 0 :err ""
                               :out (str/join "\n" (map-indexed
                                                    (fn [i _] (json/generate-string {:line (inc i) :exit 0 :out "" :err ""}))
                                                    lines))}))]
      (let [c (:coverage (readings/consume options))]
        (is (= ["M-fixture-a"] @calls))
        (is (= [:read] (mapv :status (:members c))))
        (is (= :partial (:status c))))
      (spit records (str (json/generate-string a) "\n") :append true)
      (is (= [:ambiguous] (mapv :status (get-in (readings/consume options) [:coverage :members])))))))

(deftest service-has-no-process-launch-or-fixed-rate-backlog
  ;; Backstop uses coalesced one-shot scheduling, not a fixed-rate backlog or
  ;; a separate timer thread. Never delegate this service to a CLI JVM.
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
                  hou-store/headers-for-turn (fn [& _] {:headers [{:id "header"}]})
                  mention-writer/read-turn-mentions (fn [& _] {:item-ids ["M-x" "T-y" "M-unknown"]})
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
                  worker/run-once! (fn [_] (deliver boot true) {:status :idle})
                  hou-store/headers-for-turn (fn [& _] {:headers [{:id "header"}]})
                  mention-writer/read-turn-mentions
                  (fn [_ record] {:item-ids (if (= "turn-fixtureUnknown" (:id record))
                                              ["M-unknown"] [])})]
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

(deftest typed-turn-resolution-ignores-prose-and-rejects-ambiguous-identities
  (let [config (turn-fixture)]
    (with-redefs [hou-store/headers-for-turn (fn [& _] {:headers [{:id "header"}]})
                  mention-writer/read-turn-mentions (fn [& _] {:item-ids ["M-x" "T-y" "M-absent"]})]
      (jdbc/execute! (:fixture-db config) ["INSERT INTO ent_node VALUES('other/x','M-x',':mission/doc')"])
      (let [result (service/items-touched-by-turn
                    {:id "turn" :analysis "象 says E-z but it has no typed mention"} config)]
        (is (= ["T-y"] (mapv :external-id (:items result))))
        (is (= ["M-x"] (:ambiguous-ids result)))
        (is (= ["M-absent"] (:unknown-ids result)))
        (is (not-any? #{"E-z"} (map :external-id (:items result))))
        (with-redefs [clojure.core/re-seq
                      (fn [& _] (throw (AssertionError. "old regex path called")))]
          (is (= ["T-y"]
                 (mapv :external-id
                       (:items (service/items-touched-by-turn
                                {:id "turn" :analysis "M-x E-z T-y"} config)))))))))
  (let [config (turn-fixture)]
    (with-redefs [hou-store/headers-for-turn (fn [& _] {:pending :header-absent})
                  mention-writer/read-turn-mentions
                  (fn [& _] (throw (AssertionError. "mention read before completion")))]
      (is (= :header-absent (:pending (service/items-touched-by-turn {:id "turn"} config)))))))

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
  [root calls & {:keys [fail-party? gate entered]}]
  (let [out (io/file root "gcc-preview")]
    {:repo-dir (str root) :out-dir (str out)
     :exec-fn (fn [args]
                ;; Signal entry BEFORE blocking. A timeout here would release the
                ;; refresh behind the test's back and race its second mint.
                (when gate (deliver entered true) @gate)
                (swap! calls conj (vec args))
                (if (and fail-party? (some #(str/ends-with? % "giraffe_party.py") args))
                  {:exit 1 :out "" :err "party fixture failure"}
                  (do (when (some #(str/ends-with? % "giraffe_party.py") args)
                        (.mkdirs out)
                        (spit (io/file out "party.json")
                              (json/generate-string
                               {:party {:top [{:rank 1 :item "M-party-top"}]}})))
                      (when (some #(str/ends-with? % "giraffe_party_conformance.py") args)
                        (spit (io/file out "conformance.json")
                              (json/generate-string {:summary {:PASS 1 :FAIL 0 :ABSENT 0} :failing []})))
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
  (let [config (fixture) gate (promise) entered (promise)
        recorded (atom []) boot (promise) mints (atom 0)]
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
      (try
        (service/start! (assoc config
                               :preview (preview-config (temp-dir) recorded
                                                        :gate gate :entered entered)
                               :mint-fn #(hash-map :status :minted
                                                   :run-id (str "fixture-run-" (swap! mints inc)))))
        (is (= true (deref boot 2000 :timeout)))
        (service/notify-doc-changed! path)
        (is (= true (deref entered 2000 :timeout))
            "first refresh entered its process call and cannot finish until released")
        (service/notify-doc-changed! path)
        ;; Counting mints alone races schedule-preview-refresh!, which is
        ;; called only after mint-fn returns. Wait for its queued request too.
        (is (true? (wait-for #(let [s (service-state)]
                               (and (= 2 @mints)
                                    (:refresh-running? s)
                                    (:refresh-requested? s)
                                    (= "fixture-run-2" (:refresh-run-id s))))
                            2000))
            "second mint queued while the first refresh is still blocked")
        (is (empty? @recorded) "the gate has not released either process call")
        (deliver gate true)
        (is (true? (wait-for #(let [s (service-state)]
                               (and (:last-refresh s) (not (:refresh-running? s))))
                            2000))
            "the refresh and its follow-up finished")
        (finally
          ;; Release even on failure, then join BOTH executors before the
          ;; redefs unwind or counts are checked; no fixed settling sleep.
          (deliver gate true)
          (service/stop!)
          (join!)
          (when-let [executor (:preview-executor (service-state))]
            (is (.awaitTermination ^ScheduledThreadPoolExecutor executor 2 TimeUnit/SECONDS)))))
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

;; FRESHNESS P1: real SQLite and file-backed snapshots, no watcher notification.
(deftest backstop-catches-missed-edits-after-terminal-outcomes
  (doseq [outcome [{:status :landed :digest "invented-generation"}
                   {:status :superseded :reason :giraffe-generation/source-changed}
                   {:status :failed :reason :giraffe-generation/checker-failed}]]
    (let [config (fixture) store (:store config) doc (io/file (temp-dir) "M-backstop.md")
          boot (promise) snapshots (atom 0)]
      (spit doc "invented version one\n")
      (let [old (readings/sha256 (slurp doc))
            item (fn [] {:id :M-backstop :source-pin {:path (str doc) :sha256 (readings/sha256 (slurp doc))}})
            coverage {:members [{:id "M-backstop" :status :read
                                 :source {:path (str doc) :sha256 (if (= :failed (:status outcome))
                                                                  "previous-reading" old)}}]}
            snap (fn [_] (swap! snapshots inc) {:open-items [(item)] :coverage coverage})]
        (queue/reconcile! store [(item)]
                          (assoc-in coverage [:members 0 :source :sha256] "previous-reading") 10)
        (queue/settle! store (queue/claim! store 11 1000 1 "fixture-reader") outcome 12 1)
        (with-redefs [worker/run-once! (fn [_] (deliver boot true) {:status :idle})]
          (try
            (service/start! (assoc config :snapshot-fn snap :reconcile-interval-ms 60
                                    :clock (constantly 100)))
            (is (= true (deref boot 2000 :timeout)))
            (flush-events!)
            (is (= (:status outcome) (:state (first (queue/requests store))))
                "startup has not revived the terminal request before the missed edit")
            ;; For failed, leave the source unchanged: terminal failure must
            ;; survive reconciliations during the cool-off with its attempt/error.
            (when-not (= :failed (:status outcome)) (spit doc "invented version two\n"))
            (is (true? (wait-for #(some? (:last-backstop-at-ms (service-state))) 2000)))
            (let [rows (queue/requests store)
                  current-sha (:sha256 (:source-pin (item)))
                  current (first (filter #(= current-sha (:source_sha %)) rows))]
              (is (>= @snapshots 2))
              (if (= :failed (:status outcome))
                (do (is (= :failed (:state current)))
                    (is (= 1 (:attempts current)))
                    (is (= :giraffe-generation/checker-failed (:last_error current))))
                (do (is (= :pending (:state current)))
                    (is (not= old current-sha)))))
            (finally (service/stop!) (join!))))))))

(deftest backstop-delayed-by-a-round-still-runs-and-stops
  (let [config (fixture) boot (promise) entered (promise) release (promise)
        rounds (atom 0) snapshots (atom 0)]
    (with-redefs [worker/run-once! (fn [_]
                                   (if (= 1 (swap! rounds inc))
                                     (deliver boot true)
                                     (do (deliver entered true) @release))
                                   {:status :idle})]
      (try
        (service/start! (assoc config :reconcile-interval-ms 60
                               :snapshot-fn (fn [_] (swap! snapshots inc)
                                              {:open-items [] :coverage {:members []}})))
        (is (= true (deref boot 2000 :timeout)))
        (flush-events!)
        (let [{:keys [token executor]} (service-state)]
          (.execute ^ScheduledThreadPoolExecutor executor
                    ^Runnable #((ns-resolve 'futon3c.giraffe.reread-service 'drain!) token)))
        (is (= true (deref entered 2000 :timeout)))
        (Thread/sleep 100)
        (is (= 1 @snapshots) "the timer cannot run a concurrent full round")
        (deliver release true)
        (is (true? (wait-for #(>= @snapshots 2) 2000))
            "a tick delayed by a reader round still runs one full round afterwards")
        (service/stop!) (join!)
        (is (= 2 @snapshots) "only one delayed tick runs after the round")
        (let [n @snapshots {:keys [token backstop-task]} (service-state)]
          (is (.isCancelled ^java.util.concurrent.ScheduledFuture backstop-task))
          ((ns-resolve 'futon3c.giraffe.reread-service 'backstop-tick!) token)
          (is (= n @snapshots) "even a previously dispatched tick cannot run after stop"))
        (finally (deliver release true))))))

(deftest backstop-interval-configuration
  (is (= 600000 (:reconcile-interval-ms (service/load-config (fixture)))))
  (is (= 120000 (:reconcile-interval-ms
                  (service/load-config (assoc (fixture) :environment
                                              {"FUTON3C_GIRAFFE_REREAD_RECONCILE_INTERVAL_MS" "120000"})))))
  (is (thrown? clojure.lang.ExceptionInfo
               (service/load-config (assoc (fixture) :reconcile-interval-ms 0)))))

(deftest full-authority-fixture-cost-and-reconciliation
  ;; Measure the actual census + generation hash + consume + SQLite reconcile.
  ;; Stub ONLY the Python process port: unit tests must not spawn checkers.
  (let [root (temp-dir) repo (io/file root "fixture-repo")
        doc (io/file repo "holes/excursions/E-backstop-cost.md")
        corpus (io/file root "corpus") edges (io/file root "edges.json")
        store (queue/open-store (io/file root "queue.sqlite")) calls (atom 0)]
    (.mkdirs (io/file repo ".git"))
    (.mkdirs (.getParentFile doc))
    (.mkdirs corpus)
    (spit doc "# Invented excursion\nStatus: OPEN\n")
    (spit edges "[]")
    (spit (io/file corpus "check_edges.py") "# invented checker port")
    (spit (io/file corpus "members.json")
          (json/generate-string [{:id "E-backstop-cost" :path (str doc) :sha256 "old"}]))
    (spit (io/file corpus "batch-cost.jsonl")
          (str (json/generate-string {:id "E-backstop-cost" :source {:path (str doc) :sha256 "old"}
                                     :edges [] :unresolved []}) "\n"))
    (with-redefs [registry/default-code-root (str root)
                  missions/default-repos [(str repo)]
                  ;; one batch-driver process answers one line per record
                  shell/sh (fn [& _] (swap! calls inc)
                             {:exit 0 :err ""
                              :out (json/generate-string {:line 1 :exit 0 :out "" :err ""})})]
      (let [start (System/nanoTime)
            snapshot (cli/authority-snapshot {:pointer (str corpus) :edges (str edges)})
            result (queue/reconcile! store (:open-items snapshot) (:coverage snapshot) 100)
            elapsed (/ (- (System/nanoTime) start) 1000000.0)]
        (is (= 1 @calls))
        (is (= [:E-backstop-cost] (mapv :id (:open-items snapshot))))
        (is (= [:pending] (mapv :state (:requests result))))
        (println "BACKSTOP fixture snapshot+reconcile ms:" elapsed
                 "documents: 1; records: 1; checker process port stubbed")))))

(deftest automatic-retirement-batches-caps-excludes-and-retries
  (let [config (fixture)
        base (:pointer config)
        root (.getParentFile base)
        pointer (io/file root "current")
        generations (doto (io/file root "generations") .mkdirs)
        ids ["M-a" "M-b" "M-c" "M-moved" "M-pending" "M-claimed"]
        members (mapv (fn [id] {:id id :path (str (io/file root (str id ".md")))
                                :sha256 "invented"}) ids)
        executor (ScheduledThreadPoolExecutor. 1)
        token (Object.)
        scheduled (atom 0)
        mints (atom [])
        state-atom (var-get (ns-resolve 'futon3c.giraffe.reread-service 'state))
        tick-var (ns-resolve 'futon3c.giraffe.reread-service 'backstop-tick!)
        tick (fn [] (.get (.submit executor ^Runnable #(tick-var token))))]
    (spit (io/file base "members.json") (json/generate-string members))
    (spit (io/file base "check_edges.py") "# invented checker port stubbed")
    (spit (io/file base "batch-00.jsonl")
          (str/join "\n" (map #(json/generate-string {:id (:id %) :source (dissoc % :id)}) members)))
    (Files/createSymbolicLink (.toPath pointer) (.toPath base) (make-array FileAttribute 0))
    (queue/reconcile! (:store config)
                      (mapv #(hash-map :id (:id %) :source-pin (dissoc % :id)) (take-last 2 members))
                      {:members []} 1)
    (queue/claim! (:store config) 2 100000 1 "invented-seat")
    (reset! state-atom {:running? true :status :running :token token :executor executor
                        :config (assoc config :pointer pointer :generations-dir generations :retire-cap 2)
                        :identities {"M-moved" "/invented/new-location"}
                        :errors {} :pending {} :refresh-items #{}})
    (with-redefs-fn
      {(ns-resolve 'futon3c.giraffe.reread-service 'full-round!) (fn [& _] nil)
       (ns-resolve 'futon3c.giraffe.reread-service 'schedule-backstop!)
       (fn [_] (swap! scheduled inc))
       #'readings/check-records (fn [_ entries & _]
                                 (mapv (fn [{:keys [line]}] {:passed? true :record (json/parse-string line true)})
                                       entries))
       (ns-resolve 'futon3c.giraffe.reread-service 'mint-step!)
       (fn [_ landed idle?] (swap! mints conj [landed idle?]) {:status :deferred :pending-landings landed})
       (ns-resolve 'futon3c.giraffe.reread-service 'after-mint!) (fn [& _] nil)
       (ns-resolve 'futon3c.giraffe.reread-service 'schedule-mint!) (fn [& _] nil)}
      (fn []
        (tick)
        (is (= 2 (get-in (service/health) [:last-retirement :retired])))
        (is (= [[1 false]] @mints) "a retirement is one landing; requests are queued, so the window applies")
        (let [current (:path (generation/resolve-generation pointer))
              rows (json/parse-string (slurp (io/file current "members.json")) true)]
          (is (= ["M-a" "M-b"] (mapv :id (filter :retired rows))))
          (is (= 1 (count (filter #(re-matches #"[0-9a-f]{64}" (.getName %)) (.listFiles generations))))))
        (tick)
        (is (= 1 (get-in (service/health) [:last-retirement :retired])))
        (let [rows (json/parse-string
                    (slurp (io/file (:path (generation/resolve-generation pointer)) "members.json")) true)]
          (is (= ["M-a" "M-b" "M-c"] (mapv :id (filter :retired rows)))))
        (is (= [[1 false] [1 false]] @mints))
        (is (empty? (:refresh-items (service-state))))
        ;; A new missing member becomes eligible; refusal stays visible and
        ;; does not prevent scheduling the next tick, which retries it.
        (swap! state-atom assoc :identities {})
        (with-redefs [generation/retire-readings! (fn [_] (throw (ex-info "invented refusal" {:kind :fixture/refused})))]
          (tick))
        (is (= {:kind :fixture/refused} (get-in (service/health) [:last-retirement :refused])))
        (is (contains? (:errors (service-state)) :retirement))
        (is (= 3 @scheduled))
        (tick)
        (is (= 1 (get-in (service/health) [:last-retirement :retired])))
        (is (not (contains? (:errors (service-state)) :retirement)))
        (let [before (generation/resolve-generation pointer)]
          (swap! state-atom assoc :running? false)
          (tick)
          (is (= 4 @scheduled))
          (is (= before (generation/resolve-generation pointer))))))
    (.shutdown executor)))

(deftest retirement-cap-config
  (is (= 200 (:retire-cap (service/load-config (fixture)))))
  (is (= 2 (:retire-cap (service/load-config (assoc (fixture) :retire-cap 2)))))
  (is (thrown? clojure.lang.ExceptionInfo
               (service/load-config (assoc (fixture) :retire-cap 0)))))

(deftest failed-retries-use-current-doc-age-identities-and-budget
  (let [config (service/load-config (assoc (fixture) :failed-retry-after-ms 100
                                          :failed-retry-limit 3 :clock (constantly 1000)))
        store (:store config)
        root (temp-dir)
        ids ["M-old" "M-young" "M-changed" "M-exhausted" "M-gone" "M-withdrawn"]
        paths (into {} (for [id ids :let [file (io/file root (str id ".md"))]]
                         (do (spit file "invented source") [id (str file)])))
        items (mapv (fn [[id path]] {:id id :source-pin {:path path :sha256 (generation/file-sha path)}}) paths)
        state-atom (var-get (ns-resolve 'futon3c.giraffe.reread-service 'state))
        token (Object.)
        retry! (ns-resolve 'futon3c.giraffe.reread-service 'retry-failed!)]
    (queue/reconcile! store items {:members []} 1)
    (doseq [i (range 6)]
      (let [claim (queue/claim! store (+ 2 i) 100 1 "toy")]
        (queue/settle! store claim {:status :failed :reason :toy-failure} 10 1)))
    (jdbc/execute! (:datasource store) ["UPDATE reread_requests SET updated_at_ms=950 WHERE target_id='M-young'"])
    (jdbc/execute! (:datasource store) ["UPDATE reread_requests SET retries=3 WHERE target_id='M-exhausted'"])
    (jdbc/execute! (:datasource store) ["UPDATE reread_requests SET last_error='operator-withdrawn: invented triage' WHERE target_id='M-withdrawn'"])
    (spit (get paths "M-changed") "changed document")
    (reset! state-atom {:running? true :status :running :token token :config config
                        :identities (dissoc paths "M-gone") :errors {} :pending {}})
    (retry! token)
    (let [rows (into {} (map (juxt :target_id identity)) (queue/requests store))]
      (is (= :pending (get-in rows ["M-old" :state])))
      (is (= 1 (get-in rows ["M-old" :retries])))
      (is (= 0 (get-in rows ["M-old" :attempts])))
      (is (= :toy-failure (get-in rows ["M-old" :last_error])))
      (doseq [id (rest ids)] (is (= :failed (get-in rows [id :state])))))
    (is (= {:retried 1 :exhausted 1} (:last-failed-retry (service/health))))
    (is (= ["M-exhausted"] (:failed-exhausted (service/health))))
    (swap! state-atom assoc :running? false)
    (let [before (queue/requests store)]
      (retry! token)
      (is (= before (queue/requests store))))))

(deftest failed-retry-config-defaults
  (let [config (service/load-config (fixture))]
    (is (= 86400000 (:failed-retry-after-ms config)))
    (is (= 3 (:failed-retry-limit config))))
  (is (= 500 (:failed-retry-after-ms
              (service/load-config (assoc (fixture) :environment
                                          {"FUTON3C_GIRAFFE_REREAD_FAILED_RETRY_AFTER_MS" "500"}))))))

(deftest retirement-on-an-idle-queue-mints-at-once
  ;; claude-13 2026-11-16 (Joe: event driven, no fixed cadence): with nothing
  ;; queued, a retirement asks for the mint now, as a final landing does.
  (let [config (fixture)
        base (:pointer config)
        root (.getParentFile base)
        pointer (io/file root "current")
        generations (doto (io/file root "generations") .mkdirs)
        member {:id "M-gone" :path (str (io/file root "M-gone.md")) :sha256 "invented"}
        executor (ScheduledThreadPoolExecutor. 1)
        token (Object.)
        mints (atom [])
        state-atom (var-get (ns-resolve 'futon3c.giraffe.reread-service 'state))
        tick-var (ns-resolve 'futon3c.giraffe.reread-service 'backstop-tick!)]
    (spit (io/file base "members.json") (json/generate-string [member]))
    (spit (io/file base "check_edges.py") "# invented checker port stubbed")
    (spit (io/file base "batch-00.jsonl") (json/generate-string {:id "M-gone" :source (dissoc member :id)}))
    (Files/createSymbolicLink (.toPath pointer) (.toPath base) (make-array FileAttribute 0))
    (reset! state-atom {:running? true :status :running :token token :executor executor
                        :config (assoc config :pointer pointer :generations-dir generations :retire-cap 200)
                        :identities {} :errors {} :pending {} :refresh-items #{}})
    (with-redefs-fn
      {(ns-resolve 'futon3c.giraffe.reread-service 'full-round!) (fn [& _] nil)
       (ns-resolve 'futon3c.giraffe.reread-service 'schedule-backstop!) (fn [_] nil)
       #'readings/check-records (fn [_ entries & _]
                                 (mapv (fn [{:keys [line]}] {:passed? true :record (json/parse-string line true)})
                                       entries))
       (ns-resolve 'futon3c.giraffe.reread-service 'mint-step!)
       (fn [_ landed idle?] (swap! mints conj [landed idle?]) {:status :deferred :pending-landings landed})
       (ns-resolve 'futon3c.giraffe.reread-service 'after-mint!) (fn [& _] nil)
       (ns-resolve 'futon3c.giraffe.reread-service 'schedule-mint!) (fn [& _] nil)}
      (fn []
        (.get (.submit executor ^Runnable #(tick-var token)))
        (is (= 1 (get-in (service/health) [:last-retirement :retired])))
        (is (= [[1 true]] @mints))))
    (swap! state-atom assoc :running? false)
    (.shutdown executor)))

(deftest pages-behind-the-last-mint-are-refreshed
  ;; claude-13 2026-11-16: a stop!/start! dropped the 00:46 mint's queued
  ;; refresh; the next pass must notice party.json is on an older run.
  (let [config (fixture)
        out (temp-dir)
        mint-state (io/file (temp-dir) "mint-batch.edn")
        token (Object.)
        refreshed (atom [])
        state-atom (var-get (ns-resolve 'futon3c.giraffe.reread-service 'state))
        catch-up (ns-resolve 'futon3c.giraffe.reread-service 'refresh-if-behind!)]
    (spit mint-state (pr-str {:pending-landings 0 :last-result {:status :minted :run-id "run-new"}}))
    (reset! state-atom {:running? true :status :running :token token :errors {} :pending {}
                        :config (assoc config :mint-state-path (str mint-state)
                                       :preview {:out-dir (str out)})})
    (with-redefs-fn
      {(ns-resolve 'futon3c.giraffe.reread-service 'schedule-preview-refresh!)
       (fn [_ run-id] (swap! refreshed conj run-id))}
      (fn []
        (spit (io/file out "party.json") (json/generate-string {:sources {:giraffe_run "run-old"}}))
        (catch-up token)
        (is (= ["run-new"] @refreshed) "behind -> refresh the minted run")
        (spit (io/file out "party.json") (json/generate-string {:sources {:giraffe_run "run-new"}}))
        (catch-up token)
        (is (= ["run-new"] @refreshed) "up to date -> nothing")
        (swap! state-atom assoc :running? false)
        (spit (io/file out "party.json") (json/generate-string {:sources {:giraffe_run "run-old"}}))
        (catch-up token)
        (is (= ["run-new"] @refreshed) "stopped -> nothing")))))

(deftest old-token-round-cannot-touch-the-new-service
  ;; codex-7 review 2026-11-16: an old executor's round completing after a
  ;; restart overwrote :last-round and cancelled the new mint/lease tasks.
  (let [executor (ScheduledThreadPoolExecutor. 1)
        new-token (Object.) old-token (Object.)
        mint-task (.schedule executor ^Runnable (fn []) 1 TimeUnit/HOURS)
        lease-task (.schedule executor ^Runnable (fn []) 1 TimeUnit/HOURS)
        state-atom (var-get (ns-resolve 'futon3c.giraffe.reread-service 'state))
        record! (ns-resolve 'futon3c.giraffe.reread-service 'record-round!)]
    (reset! state-atom {:running? true :status :running :token new-token :executor executor
                        :config (fixture) :errors {} :pending {}
                        :last-round {:marker :new} :mint-task mint-task :lease-task lease-task})
    (record! old-token {:mint {:status :deferred :pending-landings 1} :workers [{:status :landed}]})
    (is (= {:marker :new} (:last-round (service-state))))
    (is (not (.isCancelled mint-task)))
    (is (not (.isCancelled lease-task)))
    (swap! state-atom assoc :running? false)
    (.shutdownNow executor)))

(deftest failed-document-event-retries-and-clears-error
  (let [config (assoc (fixture) :event-retry-ms 50 :event-retry-limit 3)
        calls (atom 0)]
    (with-redefs [worker/run-once! (constantly {:status :idle})
                  service/item-snapshot (fn [p c]
                                          (if (= 1 (swap! calls inc))
                                            (throw (ex-info "invented parse failure" {}))
                                            (snapshot p c)))]
      (service/start! config) (flush-events!)
      (service/notify-doc-changed! path)
      (is (true? (wait-for #(boolean (seq (queue/requests (:store config)))) 2000)))
      (is (= 2 @calls))
      (is (nil? (get-in (service/health) [:errors "M-event-fixture"])))
      (is (empty? (:pending (service-state)))))))

(deftest persistent-document-failure-exhausts-event-budget
  (let [config (assoc (fixture) :event-retry-ms 20 :event-retry-limit 3)
        calls (atom 0)]
    (with-redefs [worker/run-once! (constantly {:status :idle})
                  service/item-snapshot (fn [& _] (swap! calls inc)
                                          (throw (ex-info "invented persistent parse failure" {})))]
      (service/start! config) (flush-events!)
      (service/notify-doc-changed! path)
      (is (true? (wait-for #(and (= 3 @calls) (empty? (:pending (service-state)))) 2000)))
      (Thread/sleep 80)
      (is (= 3 @calls))
      (is (some? (get-in (service/health) [:errors "M-event-fixture"])))
      (is (empty? (queue/requests (:store config)))))))

(deftest newer-notification-or-stop-cancels-event-retry
  (doseq [action [:newer :stop]]
    (let [config (assoc (fixture) :event-retry-ms 200 :event-retry-limit 3)
          calls (atom 0)]
      (with-redefs [worker/run-once! (constantly {:status :idle})
                    service/item-snapshot (fn [p c]
                                            (if (= 1 (swap! calls inc))
                                              (throw (ex-info "invented transient failure" {}))
                                              (snapshot p c)))]
        (service/start! config) (flush-events!)
        (service/notify-doc-changed! path)
        (is (true? (wait-for #(= 2 (get-in (service-state) [:pending "M-event-fixture" :attempt])) 2000)))
        (let [task (get-in (service-state) [:pending "M-event-fixture" :task])]
          (if (= :stop action)
            (do (service/stop!) (join!)
                (is (= :stopped (:status (service/notify-doc-changed! path)))))
            (do (service/notify-doc-changed! path)
                (is (true? (wait-for #(boolean (seq (queue/requests (:store config)))) 2000)))))
          (is (.isCancelled ^java.util.concurrent.ScheduledFuture task)))
        (Thread/sleep 250)
        (is (= (if (= :stop action) 1 2) @calls))
        (service/stop!) (join!)))))
