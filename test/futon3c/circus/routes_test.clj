(ns futon3c.circus.routes-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.circus.chain-certificate :as certificate]
            [futon3c.circus.chain-record :as chain]
            [futon3c.circus.routes :as routes]
            [futon3c.giraffe.run :as giraffe]
            [futon3c.xiang.turn-store :as turn-store]))

(defn- temp-dir [tag]
  (.toFile (java.nio.file.Files/createTempDirectory
            (str "circus-routes-" tag)
            (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- valid-run! [root run-id]
  (let [dir (io/file root run-id)
        files ["input.edn" "rejected.edn" "pass.edn"
               "work-clusters.json" "report.md"]
        git-sha (str/trim
                 (:out (shell/sh "git" "rev-parse" "HEAD"
                                 :dir "/home/joe/code/futon3c")))]
    (.mkdirs dir)
    (doseq [file files] (spit (io/file dir file) (str file "\n")))
    (spit (io/file dir "manifest.edn")
          (pr-str {:run-id run-id :git-sha git-sha :sources {}
                   :generated-at "2026-11-17T02:00:00Z"
                   :files (mapv (fn [file] {:file file
                                            :sha256 (giraffe/sha256 (io/file dir file))})
                                files)}))
    dir))

(defn- top-result []
  {:status :ok :items [{:item "M-one" :priority-band :focus
                        :cutoff-status :within-cutoff
                        :focus-relations [{:turn {:id "turn-one"}}]}]})

(deftest latest-selects-newest-verified-run-and-normalizes-json
  (let [root (temp-dir "latest")
        valid (valid-run! root "20261117T020000.000Z-valid")
        invalid (valid-run! root "20261117T030000.000Z-invalid")
        _ (spit (io/file invalid "input.edn") "tampered")
        response (routes/top-items-response
                  {:root (str root) :qa-top-fn (fn [_] (top-result))}
                  {"run" "latest" "n" "10"})]
    (is (= 200 (:status response)))
    (is (= (.getName valid) (get-in response [:body "run"])))
    (is (= "2026-11-17T02:00:00Z" (get-in response [:body "generated_at"])))
    (is (= "focus" (get-in response [:body "items" 0 "priority-band"])))
    (is (= "within-cutoff" (get-in response [:body "items" 0 "cutoff-status"])))
    (is (= {"path" nil "check" nil}
           (get-in response [:body "items" 0 "focus-relations" 0 "certificate"])))))

(deftest run-and-parameter-errors-are-typed
  (let [root (temp-dir "errors")]
    (is (= {"status" "error" "reason" "run-not-found" "run" "missing"}
           (:body (routes/top-items-response {:root (str root)} {"run" "missing"}))))
    (is (= 404 (:status (routes/top-items-response {:root (str root)}
                                                     {"run" "missing"}))))
    (is (= {:status 400 :body {"status" "error" "reason" "item-required"}}
           (routes/why-not-response {:root (str root)} {"run" "latest"})))))

(deftest qa-query-refusal-is-a-typed-response
  (let [root (temp-dir "refused")
        run-id "20261117T020000.000Z-run"
        _ (valid-run! root run-id)
        response (routes/top-items-response
                  {:root (str root)
                   :qa-top-fn (fn [_]
                                (throw (ex-info "truncated" {:reason :truncated})))}
                  {"run" run-id})]
    (is (= 503 (:status response)))
    (is (= "error" (get-in response [:body "status"])))
    (is (= "truncated" (get-in response [:body "failure"])))))

(deftest certificate-block-reads-real-sidecar-shape
  (let [root (temp-dir "cert-run")
        run-id "20261117T020000.000Z-run"
        _ (valid-run! root run-id)
        cert-root (temp-dir "certs")
        cert (io/file cert-root (str "turn-one-" run-id ".lean"))
        _ (spit cert "-- fixture")
        check {"exit" 0 "ok" true "at" "2026-11-17T02:01:00Z"
               "record_sha256" "sha"}
        _ (spit (str cert ".check.json") (json/generate-string check))
        response (routes/top-items-response
                  {:root (str root) :certificate-root (str cert-root)
                   :qa-top-fn (fn [_] (top-result))}
                  {"run" run-id})]
    (is (= (.getAbsolutePath cert)
           (get-in response [:body "items" 0 "focus-relations" 0 "certificate" "path"])))
    (is (= check
           (get-in response [:body "items" 0 "focus-relations" 0 "certificate" "check"])))))

(deftest why-not-response-is-normalized
  (let [root (temp-dir "why")
        run-id "20261117T020000.000Z-run"
        _ (valid-run! root run-id)
        response (routes/why-not-response
                  {:root (str root)
                   :qa-why-fn (fn [_] {:status :ok :stage :priority
                                       :reason :operator-gated})}
                  {"run" run-id "item" "M-one"})]
    (is (= "operator-gated" (get-in response [:body "reason"])))
    (is (= "priority" (get-in response [:body "stage"])))))

(deftest write-and-check-writes-check-sidecar
  (let [dir (temp-dir "sidecar")
        output (io/file dir "certificate.lean")
        record {:turn {:id "turn-one"} :run {:run-id "run-one"}
                :gcc {} :items [] :focus-link [] :priority {:items []}
                :record/sha256 "record-sha"}]
    (with-redefs [chain/chain-record (constantly record)
                  certificate/pinned-ids (constantly [])
                  certificate/lean-check (constantly {:exit 0})]
      (is (:ok? (certificate/write-and-check!
                 {:turn-id "turn-one" :run-dir (str dir)
                  :output-path (str output)}))))
    (let [sidecar (json/parse-string (slurp (str output ".check.json")))]
      (is (= 0 (get sidecar "exit")))
      (is (true? (get sidecar "ok")))
      (is (= "record-sha" (get sidecar "record_sha256")))
      (is (string? (get sidecar "at"))))))

(deftest qa-answer-is-cached-per-run-and-certificates-stay-fresh
  (let [root (temp-dir "cache")
        run (valid-run! root "20261117T020000.000Z-cache")
        certs (temp-dir "certs")
        calls (atom 0)
        top-fn (fn [_] (swap! calls inc) (top-result))
        cfg {:root (str root) :certificate-root (str certs) :qa-top-fn top-fn}
        params {"run" (.getName run) "n" "10"}
        first-response (routes/top-items-response cfg params)
        cert (io/file certs (str "turn-one-" (.getName run) ".lean"))]
    (spit cert "-- fixture")
    (spit (io/file (str cert ".check.json")) (json/generate-string {:exit 0 :ok true}))
    (let [second-response (routes/top-items-response cfg params)]
      (is (= 1 @calls))
      (is (nil? (get-in first-response [:body "items" 0 "focus-relations" 0 "certificate" "path"])))
      (is (= true (get-in second-response [:body "items" 0 "focus-relations" 0 "certificate" "check" "ok"]))))
    (routes/top-items-response cfg (assoc params "n" "5"))
    (is (= 2 @calls))))

(deftest refused-qa-answer-is-not-cached
  (let [root (temp-dir "nocache")
        run (valid-run! root "20261117T020000.000Z-nocache")
        calls (atom 0)
        cfg {:root (str root) :qa-top-fn (fn [_] (swap! calls inc) {:status :refused})}]
    (routes/top-items-response cfg {"run" (.getName run)})
    (routes/top-items-response cfg {"run" (.getName run)})
    (is (= 2 @calls))))

(defn- strand-fixture []
  (let [prev {"turn-four" "turn-three" "turn-three" "turn-two"
              "turn-two" "turn-one" "turn-one" nil}
        calls (atom {})
        gcc-fn (fn [{:keys [turn-id]}]
                 (swap! calls update turn-id (fnil inc 0))
                 {:turn {:id turn-id :created_at (str "at-" turn-id)
                         :prev_operator_turn_id (get prev turn-id)
                         :next_operator_turn_id nil}
                  :ma-connection (if (= turn-id "turn-one")
                                   {:status :session-start}
                                   {:status :linked :turn/id (get prev turn-id)
                                    :sha256 (str "sha-" (get prev turn-id))})
                  :pattern-verdicts
                  {:window ["turn-one" turn-id]
                   :patterns
                   (if (= turn-id "turn-four")
                     [{:pattern "pattern-void" :status :void
                       :turn {:I1 {:why "model/budget"}}
                       :chain {:I1 {:why "model/budget"}}}
                      {:pattern "pattern-partial" :status :partial
                       :turn {:I1 {:element "intent" :turn turn-id}}
                       :chain {:G {:element "gap" :turn "turn-one"}}}
                      {:pattern "pattern-occurrence" :status :occurrence
                       :turn {:B {:element "bridge" :turn "turn-three"}}
                       :chain {}}]
                     [])}
                  :still-open (when (= turn-id "turn-four")
                                [{:act "act-one" :author "operator"
                                  :kind "ask-action" :text "not exposed"
                                  :since "turn-one"}])
                  :gcc {:驴 {:status :present :clusion-ids [(str "proposal-" turn-id)]}
                        :馬 {:status :absent :reason :no-clusions}
                        :狗 {:status :absent :reason :no-clusions}}
                  :items [(str "M-" turn-id)]
                  :item-links [{:item-id (str "M-" turn-id) :kind :mention}]
                  :item-absence nil})]
    {:cfg {:turn-store ::store :session-head-fn (fn [_ session]
                                                  (when (= session "session-one")
                                                    "turn-four"))
           :turn-gcc-fn gcc-fn :strand-cache (routes/strand-cache)}
     :calls calls}))

(deftest strand-walks-backpointers-honors-n-and-since-and-preserves-absence
  (let [{:keys [cfg]} (strand-fixture)
        limited (routes/strand-response cfg {"session" "session-one" "n" "2"})
        incremental (routes/strand-response cfg {"session" "session-one"
                                                 "since" "turn-two" "n" "20"})]
    (is (= 200 (:status limited)))
    (is (= ["turn-four" "turn-three"]
           (mapv #(get % "id") (get-in limited [:body "turns"]))))
    (is (= ["turn-four" "turn-three"]
           (mapv #(get % "id") (get-in incremental [:body "turns"]))))
    (is (= "turn-four" (get-in incremental [:body "cursor"])))
    (is (= {"status" "absent" "reason" "no-clusions"}
           (get-in limited [:body "turns" 0 "gcc" "馬"])))
    (is (some #(= "strand" (get % "kind")) (get-in limited [:body "edges"])))
    (is (some #(= "proposed" (get % "kind")) (get-in limited [:body "edges"])))))

(deftest strand-projects-ma-threads-quotes-and-open-acts
  (let [{:keys [cfg]} (strand-fixture)
        response (routes/strand-response cfg {"session" "session-one" "n" "1"})
        turn (get-in response [:body "turns" 0])
        edges (get-in response [:body "edges"])]
    (is (= "read" (get-in turn ["ma" "status"])))
    (is (= 1 (get-in turn ["ma" "void-count"])))
    (is (= #{"pattern-partial" "pattern-occurrence"}
           (set (map #(get % "pattern") (get-in turn ["ma" "threads"])))))
    (is (= {"window" "chain" "from" "turn-one"}
           (get-in turn ["ma" "threads" 0 "parts" "G"])))
    (is (= [{"act" "act-one" "author" "operator" "kind" "ask-action"}]
           (get turn "open_acts")))
    (is (some #(= {"from" "turn-four" "to" "turn-one" "kind" "quotes"
                   "pattern" "pattern-partial" "part" "G"}
                 %)
              edges))
    (is (some #(= {"from" "turn-four" "to" "turn-three" "kind" "quotes"
                   "pattern" "pattern-occurrence" "part" "B"}
                 %)
              edges))
    (is (not-any? #(and (= "quotes" (get % "kind"))
                        (= "turn-four" (get % "to")))
                  edges))
    (is (some #(= "partial" (get % "kind")) edges))
    (is (some #(= "occurrence" (get % "kind")) edges))
    (is (not-any? #(= "pattern-void" (get % "to")) edges))))

(deftest stable-cached-turn-is-requeried-only-after-event
  (let [{:keys [cfg calls]} (strand-fixture)
        params {"session" "session-one" "n" "4"}]
    (routes/strand-response cfg params)
    (routes/strand-response cfg params)
    ;; Newest two remain live; older turns are reused.
    (is (= 2 (get @calls "turn-four")))
    (is (= 2 (get @calls "turn-three")))
    (is (= 1 (get @calls "turn-two")))
    (routes/mark-strand-dirty! (:strand-cache cfg) "turn-two")
    (routes/strand-response cfg params)
    (is (= 2 (get @calls "turn-two")))))

(deftest cursor-poll-returns-only-turns-newer-than-cursor
  (let [{:keys [cfg]} (strand-fixture)
        response (routes/strand-response cfg {"session" "session-one"
                                              "since" "turn-three"})]
    (is (= ["turn-four"]
           (mapv #(get % "id") (get-in response [:body "turns"]))))))

(deftest unknown-strand-session-is-typed-404
  (let [{:keys [cfg]} (strand-fixture)
        response (routes/strand-response cfg {"session" "missing"})]
    (is (= 404 (:status response)))
    (is (= {"status" "error" "reason" "session-not-found"
            "session" "missing"}
           (:body response)))))

(deftest a-lagging-graph-turn-is-a-typed-uncached-row-not-a-failed-strand
  ;; 2026-11-18: one :stale-index answer made /circus/herd return 503.
  (let [{:keys [cfg calls]} (strand-fixture)
        lagging (atom true)
        real-gcc (:turn-gcc-fn cfg)
        cfg (assoc cfg :turn-gcc-fn
                   (fn [{:keys [turn-id] :as opts}]
                     (if (and @lagging (= turn-id "turn-two"))
                       (throw (ex-info "Chain graph query refused"
                                       {:answer {:ok {:status :stale-index}}}))
                       (real-gcc opts))))
        params {"session" "session-one" "n" "4"}]
    (with-redefs [turn-store/read-record
                  (fn [_ id] {:created_at (str "at-" id)
                              :prev_operator_turn_id ({"turn-two" "turn-one"} id)})]
      (let [r (routes/strand-response cfg params)
            two (some #(when (= "turn-two" (get % "id")) %) (get-in r [:body "turns"]))]
        (is (= 200 (:status r)))
        (is (= ["turn-four" "turn-three" "turn-two" "turn-one"]
               (mapv #(get % "id") (get-in r [:body "turns"]))) "the walk continues past it")
        (is (= "stale-index" (get two "unavailable"))))
      (reset! lagging false)
      (routes/strand-response cfg params)
      (is (= 1 (get @calls "turn-two")) "not cached while unavailable; read once it answers"))))
