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

(defn- related-view [scopes]
  (fn [_mission _base-url _binders] {:scopes scopes}))

(deftest related-missions-ranks-by-shared-pattern-rarity
  (let [self "M-self"
        scopes [{:type "pattern" :pattern_id "p/common"}
                {:type "pattern" :pattern_id "p/rare"}
                {:type "psr" :pattern_id "p/psr"}
                {:type "pur" :pattern_id "p/pur"}]
        citations {"p/common" #{self "M-a" "M-b" "M-c"}
                   "p/rare" #{self "M-a"}
                   "p/psr" #{self "M-b"}
                   "p/pur" #{self "M-b"}}
        fetch (fn [pattern _binder _base-url] (get citations pattern))
        response (routes/mission-related-missions-response
                  {"mission" self "n" "50"} (related-view scopes) fetch)
        body (:body response)
        related (get body "related")
        by-mission (into {} (map (juxt #(get % "mission") identity)) related)
        common-weight (/ 1.0 (/ (Math/log 5.0) (Math/log 2.0)))
        rare-weight (/ 1.0 (/ (Math/log 3.0) (Math/log 2.0)))]
    (is (= 200 (:status response)))
    (is (= ["p/common" "p/psr" "p/pur" "p/rare"] (get body "patterns")))
    (is (= ["M-b" "M-a" "M-c"] (mapv #(get % "mission") related)))
    (is (< (Math/abs (- (+ common-weight rare-weight)
                        (get-in by-mission ["M-a" "score"]))) 1.0e-12))
    (is (= ["p/common" "p/psr" "p/pur"]
           (get-in by-mission ["M-b" "shared"])))
    (is (nil? (get by-mission self)))))

(deftest related-missions-without-patterns-is-empty
  (let [called? (atom false)
        response (routes/mission-related-missions-response
                  {"mission" "M-empty"}
                  (related-view [])
                  (fn [& _] (reset! called? true) #{}))]
    (is (= 200 (:status response)))
    (is (= [] (get-in response [:body "patterns"])))
    (is (= [] (get-in response [:body "related"])))
    (is (false? @called?))))

(deftest related-missions-reports-futon1b-failure
  (let [response (routes/mission-related-missions-response
                  {"mission" "M-one"}
                  (related-view [{:type "pattern" :pattern_id "p/one"}])
                  (fn [& _] (throw (ex-info "futon1b refused" {}))))]
    (is (= 503 (:status response)))
    (is (= "mission-related-missions-unavailable"
           (get-in response [:body "reason"])))
    (is (= "futon1b refused" (get-in response [:body "message"])))))

(def related-now (java.time.Instant/parse "2026-11-21T12:00:00Z"))

(defn- related-session-cfg [heads records rows]
  {:turn-store ::store
   :now-fn (constantly related-now)
   :active-session-heads-fn (fn [_ _] heads)
   :read-turn-fn (fn [_ id] (get records id))
   :related-turn-fn (fn [id] (get rows id))})

(deftest related-sessions-ranks-by-rarity-and-excludes-self
  (let [heads [{:session "self" :head "self-2"}
               {:session "a" :head "a-1"}
               {:session "b" :head "b-1"}
               {:session "c" :head "c-1"}]
        records {"self-2" {:created_at "2026-11-21T11:50:00Z"
                            :prev_operator_turn_id "self-1" :agent_id "agent-self"}
                 "self-1" {:created_at "2026-11-21T11:40:00Z"
                            :prev_operator_turn_id nil :agent_id "agent-self"}
                 "a-1" {:created_at "2026-11-21T11:45:00Z" :agent_id "agent-a"}
                 "b-1" {:created_at "2026-11-21T11:35:00Z" :agent_id "agent-b"}
                 "c-1" {:created_at "2026-11-21T11:25:00Z" :agent_id "agent-c"}}
        rows {"self-2" {:mission_id "M-self"
                         :patterns {:proposed ["p/common"] :occurs []}}
              "self-1" {:mission_id "M-self"
                         :patterns {:proposed ["p/common"] :occurs ["p/rare"]}}
              "a-1" {:mission_id "M-a"
                       :patterns {:proposed ["p/common" "p/rare"] :occurs []}}
              "b-1" {:mission_id "M-b"
                       :patterns {:proposed ["p/common"] :occurs []}}
              "c-1" {:mission_id "M-c"
                       :patterns {:proposed [] :occurs ["p/common"]}}}
        response (routes/session-related-sessions-response
                  (related-session-cfg heads records rows)
                  {"session" "self" "since-hours" "24" "n" "11"})
        related (get-in response [:body "related"])
        common-weight (/ 1.0 (/ (Math/log 5.0) (Math/log 2.0)))
        rare-weight (/ 1.0 (/ (Math/log 3.0) (Math/log 2.0)))]
    (is (= 200 (:status response)))
    (is (= ["p/common" "p/rare"] (get-in response [:body "patterns"])))
    (is (= ["a" "b" "c"] (mapv #(get % "session") related)))
    (is (< (Math/abs (- (+ common-weight rare-weight)
                        (get-in related [0 "score"]))) 1.0e-12))
    (is (= "agent-a" (get-in related [0 "agent"])))
    (is (= "M-a" (get-in related [0 "mission"])))
    (is (not-any? #(= "self" (get % "session")) related))))

(deftest related-sessions-window-excludes-old-heads
  (let [cutoff (atom nil)
        active (fn [_ since-ms]
                 (reset! cutoff since-ms)
                 [{:session "self" :head "self-1"}])
        cfg (assoc (related-session-cfg
                    []
                    {"self-1" {:created_at "2026-11-21T11:00:00Z"}}
                    {"self-1" {:patterns {:proposed ["p/one"] :occurs []}}})
                   :active-session-heads-fn active)
        response (routes/session-related-sessions-response
                  cfg {"session" "self" "since-hours" "2"})]
    (is (= (.toEpochMilli (java.time.Instant/parse "2026-11-21T10:00:00Z"))
           @cutoff))
    (is (= 1 (get-in response [:body "window" "sessions_seen"])))
    (is (= [] (get-in response [:body "related"])))))

(deftest related-sessions-caps-each-predecessor-walk-at-thirty
  (let [ids (mapv #(str "turn-cap-" %) (range 35))
        records (into {}
                      (map-indexed
                       (fn [i id]
                         [id {:created_at (str "2026-11-21T11:"
                                               (format "%02d" (- 59 i)) ":00Z")
                              :prev_operator_turn_id (get ids (inc i))}])
                       ids))
        reads (atom 0)
        cfg (assoc (related-session-cfg
                    [{:session "self" :head (first ids)}] records
                    (into {} (map (fn [id] [id {:patterns {:proposed ["p"]
                                                            :occurs []}}]) ids)))
                   :read-turn-fn (fn [_ id] (swap! reads inc) (get records id)))
        response (routes/session-related-sessions-response cfg {"session" "self"})]
    (is (= 30 @reads))
    (is (= 30 (get-in response [:body "window" "turns_read"])))))

(deftest related-sessions-unknown-or-patternless-is-empty
  (let [cfg (related-session-cfg
             [{:session "self" :head "self-1"}]
             {"self-1" {:created_at "2026-11-21T11:00:00Z"}}
             {"self-1" {:patterns {:proposed [] :occurs []}}})
        empty (routes/session-related-sessions-response cfg {"session" "self"})
        unknown (routes/session-related-sessions-response cfg {"session" "missing"})]
    (is (= [] (get-in empty [:body "patterns"])))
    (is (= [] (get-in empty [:body "related"])))
    (is (= [] (get-in unknown [:body "patterns"])))
    (is (= [] (get-in unknown [:body "related"])))))

(deftest related-sessions-retains-requested-head-at-sixty-session-bound
  (let [peers (mapv #(str "a-" (format "%02d" %)) (range 60))
        sessions (conj peers "z-target")
        heads (mapv (fn [session] {:session session :head (str "turn-" session)}) sessions)
        records (into {} (map (fn [session]
                                [(str "turn-" session)
                                 {:created_at "2026-11-21T11:00:00Z"}]) sessions))
        rows (into {} (map (fn [session]
                             [(str "turn-" session)
                              {:patterns {:proposed [(str "p/" session)] :occurs []}}]) sessions))
        response (routes/session-related-sessions-response
                  (related-session-cfg heads records rows) {"session" "z-target"})]
    (is (= ["p/z-target"] (get-in response [:body "patterns"])))
    (is (= 60 (get-in response [:body "window" "sessions_seen"])))))

(deftest related-turns-ranks-by-rarity-excludes-self-and-other-session
  (let [session "s-one"
        records {"turn-c" {:session_id session :created_at "2026-11-21T11:50:00Z"
                            :prev_operator_turn_id "turn-b" :origin "operator"
                            :source_text "newest common turn"}
                 "turn-b" {:session_id session :created_at "2026-11-21T11:40:00Z"
                            :prev_operator_turn_id "turn-x" :origin "operator"
                            :source_text "second common turn"}
                 "turn-x" {:session_id "s-other" :created_at "2026-11-21T11:35:00Z"
                            :prev_operator_turn_id "turn-a" :origin "operator"
                            :source_text "must not appear"}
                 "turn-a" {:session_id session :created_at "2026-11-21T11:30:00Z"
                            :prev_operator_turn_id "turn-target" :origin "agent"
                            :caller "claude-a" :source_text (apply str (repeat 130 "a"))}
                 "turn-target" {:session_id session :created_at "2026-11-21T11:20:00Z"
                                 :prev_operator_turn_id nil :origin "operator"
                                 :source_text "target"}}
        rows {"turn-c" {:patterns {:proposed ["p/common"] :occurs []}}
              "turn-b" {:patterns {:proposed [] :occurs ["p/common"]}}
              "turn-x" {:patterns {:proposed ["p/common"] :occurs ["p/rare"]}}
              "turn-a" {:patterns {:proposed ["p/common" "p/rare"] :occurs []}}
              "turn-target" {:patterns {:proposed ["p/common"] :occurs ["p/rare"]}}}
        cfg {:turn-store ::store
             :read-turn-fn (fn [_ id] (get records id))
             :related-turn-fn (fn [id] (get rows id))
             :session-head-fn (fn [_ _] "turn-c")}
        response (routes/turn-related-turns-response cfg {"turn" "turn-target" "n" "11"})
        related (get-in response [:body "related"])
        common-weight (/ 1.0 (/ (Math/log 5.0) (Math/log 2.0)))
        rare-weight (/ 1.0 (/ (Math/log 3.0) (Math/log 2.0)))]
    (is (= 200 (:status response)))
    (is (= session (get-in response [:body "session"])))
    (is (= ["p/common" "p/rare"] (get-in response [:body "patterns"])))
    (is (= ["turn-a" "turn-c" "turn-b"] (mapv #(get % "turn") related)))
    (is (< (Math/abs (- (+ common-weight rare-weight)
                        (get-in related [0 "score"]))) 1.0e-12))
    (is (= "claude-a" (get-in related [0 "speaker"])))
    (is (= 120 (count (get-in related [0 "excerpt"]))))
    (is (not-any? #(contains? #{"turn-target" "turn-x"} (get % "turn")) related))))

(deftest related-turns-caps-session-walk-at-two-hundred
  (let [ids (mapv #(str "turn-long-" %) (range 205))
        records (into {}
                      (map-indexed (fn [i id]
                                     [id {:session_id "s-long"
                                          :created_at "2026-11-21T11:00:00Z"
                                          :prev_operator_turn_id (get ids (inc i))
                                          :origin "operator"}]) ids))
        rows (into {} (map (fn [id] [id {:patterns {:proposed ["p"] :occurs []}}]) ids))
        reads (atom 0)
        cfg {:turn-store ::store
             :read-turn-fn (fn [_ id] (swap! reads inc) (get records id))
             :related-turn-fn (fn [id] (get rows id))
             :session-head-fn (fn [_ _] (first ids))}
        response (routes/turn-related-turns-response cfg {"turn" (first ids)})]
    ;; One direct read establishes the requested turn, then the bounded walk.
    (is (= 201 @reads))
    (is (= 200 (get-in response [:body "window" "turns_read"])))
    (is (= 11 (count (get-in response [:body "related"]))))))

(deftest related-turns-unknown-or-patternless-is-empty
  (let [records {"turn-empty" {:session_id "s-empty"
                                :created_at "2026-11-21T11:00:00Z"
                                :origin "operator"}}
        cfg {:turn-store ::store
             :read-turn-fn (fn [_ id] (get records id))
             :related-turn-fn (fn [_] {:patterns {:proposed [] :occurs []}})
             :session-head-fn (fn [_ session]
                                (when (= session "s-empty") "turn-empty"))}
        empty (routes/turn-related-turns-response cfg {"turn" "turn-empty"})
        unknown (routes/turn-related-turns-response cfg {"turn" "turn-missing"})]
    (is (= [] (get-in empty [:body "patterns"])))
    (is (= [] (get-in empty [:body "related"])))
    (is (= [] (get-in unknown [:body "patterns"])))
    (is (= [] (get-in unknown [:body "related"])))))

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
                  :closed-this-turn (when (= turn-id "turn-four")
                                      [{:act "act-zero" :kind "propose" :text "not exposed"}])
                  :acts (when (= turn-id "turn-four")
                          [{:id "act-two" :kind "propose" :about "an invented proposal"
                            :author "operator" :port? true :text "not exposed" :start 0 :end 9}
                           {:id "act-three" :kind "commit" :author "agent-invented"
                            :target "act-zero" :port? false :text "not exposed"}
                           {:id "act-four" :kind "report" :author "agent-invented"
                            :port? false :text "not exposed"}])
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
    ;; the intent graph's inputs: what this turn closed, and the acts that
    ;; open an intent or close one (a plain report is left out), no text
    (is (= [{"act" "act-zero" "kind" "propose"}] (get turn "closed_acts")))
    (is (= ["act-two" "act-three"] (mapv #(get % "id") (get turn "acts"))))
    (is (= "act-zero" (get-in turn ["acts" 1 "target"])))
    (is (= "an invented proposal" (get-in turn ["acts" 0 "about"])))
    (is (not-any? #(contains? % "text") (concat (get turn "acts") (get turn "closed_acts"))))
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

(deftest common-ground-refreshes-old-cache-shape-once
  (let [{:keys [cfg calls]} (strand-fixture)
        params {"session" "session-one" "n" "4"}]
    (routes/strand-response cfg params)
    (swap! (:entries (:strand-cache cfg)) update "turn-two" dissoc :mission_id :patterns)
    (routes/strand-response cfg params)
    (is (= 2 (get @calls "turn-two")))
    (routes/strand-response cfg params)
    (is (= 2 (get @calls "turn-two")))
    (is (= {:proposed [] :occurs []}
           (get-in @(:entries (:strand-cache cfg)) ["turn-two" :patterns])))))

(deftest unavailable-turn-keeps-recorded-common-ground-without-caching
  (let [cfg {:turn-store ::store :strand-cache (routes/strand-cache)
             :session-head-fn (fn [_ _] "turn-offline")
             :turn-gcc-fn (fn [_] (throw (ex-info "index lagging" {:reason :stale-index})))}]
    (with-redefs [turn-store/read-record
                  (fn [_ _] {:created_at "2026-01-01T02:00:00Z" :mission_id "M-invented"
                             :lu_ranked {:ranked [{:id "p-invented" :rank 1}]}
                             :pattern_verdicts {:patterns [{:pattern "p-invented" :status :partial}]}})]
      (let [response (routes/strand-response cfg {"session" "s-invented"})]
        (is (= 200 (:status response)))
        (is (= "M-invented" (get-in response [:body "turns" 0 "mission_id"])))
        (is (= {"proposed" ["p-invented"] "occurs" ["p-invented"]}
               (get-in response [:body "turns" 0 "patterns"])))
        (is (= {} @(:entries (:strand-cache cfg))))))))
