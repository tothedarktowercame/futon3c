(ns futon3c.circus.herd-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [futon3c.xiang.turn-store :as turns]
            [futon3c.circus.routes :as routes]))

(defn- fixture []
  (let [store (turns/store (str (java.nio.file.Files/createTempDirectory
                                "circus-herd-" (make-array java.nio.file.attribute.FileAttribute 0))))
        add (fn [session agent at]
              (:id (turns/write-record! store {:origin "operator" :session_id session
                                               :agent_id agent :created_at at})))
        old (add "s-a" "agent-a" "2026-01-01T00:00:00Z")
        boundary (add "s-a" "agent-a" "2026-01-01T01:00:00Z")
        a (add "s-a" "agent-a" "2026-01-01T02:00:00Z")
        b (add "s-b" "agent-b" "2026-01-01T03:00:00Z")
        inactive (add "inactive" "agent-c" "2026-01-01T00:00:00Z")
        _ (.setLastModified (io/file (:dir store) "session-heads" "inactive.json") 0)
        bell (:id (turns/write-record! store {:origin "agent" :caller "agent-a"
                                               :caller_session "s-a" :agent_id "agent-b"
                                               :session_id "pending" :surface "bell"
                                               :created_at "2026-01-01T04:00:00Z"}))
        calls (atom [])
        gcc (fn [{:keys [turn-id]}]
              (swap! calls conj turn-id)
              (let [record (turns/read-record store turn-id)]
                {:turn (assoc record :id turn-id)
                 :lu-ranked (:lu_ranked record)
                 :pattern-verdicts (or (:pattern_verdicts record) {:state :read
                                  :patterns (if (= turn-id a)
                                              [{:pattern "invented-pattern" :status :partial
                                                :chain {:G {:element "gap" :turn old}}}]
                                              [])}) :gcc {}}))]
    {:store store :a a :b b :old old :inactive inactive :boundary boundary :bell bell :calls calls
     :cfg {:turn-store store :strand-cache (routes/strand-cache) :turn-gcc-fn gcc}}))

(def window {"since" "2026-01-01T01:00:00Z" "until" "2026-01-01T05:00:00Z"})

(deftest herd-walks-heads-stops-at-since-and-never-scans
  (let [{:keys [store cfg a b old inactive bell boundary calls]} (fixture)
        reads (atom []) original turns/read-record]
    (with-redefs [turns/list-records (fn [& _] (throw (ex-info "no scans" {})))
                  turns/read-record (fn [s id]
                                      (swap! reads conj id)
                                      (when (= old id) (throw (ex-info "walk passed since" {})))
                                      (original s id))]
      (let [response (routes/herd-response cfg window) body (:body response)]
        (is (= 200 (:status response)))
        (is (= ["s-a" "s-b"] (mapv #(get % "session") (get body "lanes"))))
        (is (= [a b] (mapv #(get-in % ["turns" 0 "id"]) (get body "lanes"))))
        (is (= "2026-01-01T02:00:00Z" (get-in body ["lanes" 0 "first"])))
        (is (= {"id" a "created_at" "2026-01-01T02:00:00Z" "ma_status" "read"
                "threads_n" 1 "quotes_n" 1 "mission_id" nil
                "patterns" {"proposed" [] "occurs" ["invented-pattern"]}} (get-in body ["lanes" 0 "turns" 0])))
        (is (= [{"at" "2026-01-01T04:00:00Z" "from_agent" "agent-a" "from_session" "s-a"
                 "to_agent" "agent-b" "turn_id" bell "kind" "bell"}] (get body "bells")))
        (is (= "2026-01-01T04:00:00Z" (get body "cursor")))
        (is (some #{boundary} @reads))
        (is (not (some #{old} @reads)))
        (is (not (some #{inactive} @reads)) "mtime eliminates the inactive head")
        (is (= #{a b} (set @calls)))
        (is (= [] (get-in (routes/herd-response cfg (assoc window "since" (get body "cursor")))
                          [:body "bells"])))
      ;; Updating the record resolves the session; no reindex needed.
      (turns/update-record! store bell #(assoc % :session_id "s-b"))
      (is (= "s-b" (get-in (routes/herd-response cfg window) [:body "bells" 0 "to_session"])))))))

(deftest herd-reuses-strand-projection-and-dirty-events
  (let [{:keys [cfg a calls]} (fixture)]
    (routes/strand-response cfg {"session" "s-a" "n" "1"})
    (is (= [a] @calls))
    (routes/herd-response cfg (assoc window "agents" "agent-a"))
    (is (= [a] @calls) "herd does not compute the strand projection twice")
    (routes/mark-strand-dirty! (:strand-cache cfg) a)
    (routes/herd-response cfg (assoc window "agents" "agent-a"))
    (is (= [a a] @calls))))

(deftest herd-default-window-validation-and-until
  (let [{:keys [cfg]} (fixture)
        cfg (assoc cfg :now-fn #(java.time.Instant/parse "2026-01-01T08:00:00Z"))]
    (is (= ["s-b"] (mapv #(get % "session")
                            (get-in (routes/herd-response cfg {}) [:body "lanes"]))))
    (is (= ["s-a"] (mapv #(get % "session")
                             (get-in (routes/herd-response cfg (assoc window "until" "2026-01-01T02:30:00Z"))
                                     [:body "lanes"]))))
    (is (= [] (get-in (routes/herd-response cfg (assoc window "until" "2026-01-01T03:30:00Z"))
                      [:body "bells"])))
    (is (= 400 (:status (routes/herd-response cfg {"since" "nonsense"}))))
    (is (= 400 (:status (routes/herd-response cfg {"since" "2027-01-01T00:00:00Z"}))))))

(deftest bell-backfill-is-explicit-and-idempotent
  (let [{:keys [store bell]} (fixture)
        legacy "turn-legacy-bell"
        rec {:origin "agent" :agent_id "agent-b" :caller "agent-c"
             :created_at "2026-01-01T03:00:00Z"}]
    ;; Simulate a pre-index record, not a live write.
    (spit (turns/record-path store legacy) (json/generate-string rec))
    (is (= 2 (:bells (turns/backfill-bells! store))))
    (is (= [bell] (mapv :id (mapcat :turns (turns/bell-indexes store)))))
    (turns/backfill-bells! store :write? true)
    (turns/backfill-bells! store :write? true)
    (is (= #{legacy bell} (set (map :id (mapcat :turns (turns/bell-indexes store))))))
    (is (= 2 (count (mapcat :turns (turns/bell-indexes store)))))
    (turns/write-record-id! store "turn-deterministic-bell" rec)
    (turns/write-record-id! store "turn-deterministic-bell" rec)
    (is (= 3 (count (mapcat :turns (turns/bell-indexes store)))))))

(deftest common-ground-uses-windowed-cached-rows-without-scanning
  (let [{:keys [store cfg a b boundary calls]} (fixture)
        _ (turns/update-record! store a
                               #(assoc % :mission_id "M-shared"
                                       :lu_ranked {:ranked [{:id "p-proposed" :rank 1}
                                                            {:id "p-mixed" :rank 2}
                                                            {:id "p-only-a" :rank 3}]}
                                       :pattern_verdicts {:patterns [{:pattern "p-void" :status "void"}]}))
        _ (turns/update-record! store b
                               #(assoc % :mission_id "M-shared"
                                       :lu_ranked {:ranked [{:id "p-proposed" :rank 1}
                                                            {:id "p-mixed" :rank 2}]}
                                       :pattern_verdicts {:patterns [{:pattern "p-mixed" :status "partial"}
                                                                    {:pattern "p-void" :status "void"}]}))
        _ (turns/update-record! store boundary #(assoc % :mission_id "M-outside"))
        a2 (:id (turns/write-record! store
                                     {:origin "operator" :session_id "s-a" :agent_id "agent-a"
                                      :created_at "2026-01-01T04:00:00Z" :mission_id "M-next"
                                      :lu_ranked {:ranked [{:id "p-only-a" :rank 1}]}
                                      :pattern_verdicts {:patterns [{:pattern "p-mixed" :status :occurrence}]}}))]
    (with-redefs [turns/list-records (fn [& _] (throw (ex-info "no scans" {})))]
      (let [strand (routes/strand-response cfg {"session" "s-a" "n" "2"})
            _ (is (= "M-next" (get-in strand [:body "turns" 0 "mission_id"])))
            _ (is (= {"proposed" ["p-only-a"] "occurs" ["p-mixed"]}
                     (get-in strand [:body "turns" 0 "patterns"])))
            response (routes/herd-response cfg window)
            body (:body response)
            ground (get body "common_ground")
            by-key (into {} (map (juxt #(get % "key") identity)) ground)]
        (is (= 200 (:status response)))
        (is (= ["M-shared" "M-next"] (get-in body ["lanes" 0 "missions"])))
        (is (= #{"M-shared" "p-proposed" "p-mixed"} (set (keys by-key))))
        (is (= {"kind" "mission" "key" "M-shared" "via" []
                "lanes" [{"session" "s-a" "agent" "agent-a" "turns" [a]
                          "first_at" "2026-01-01T02:00:00Z"}
                         {"session" "s-b" "agent" "agent-b" "turns" [b]
                          "first_at" "2026-01-01T03:00:00Z"}]}
               (get by-key "M-shared")))
        (is (= ["proposed"] (get-in by-key ["p-proposed" "via"])))
        (is (= ["proposed" "occurs"] (get-in by-key ["p-mixed" "via"])))
        (is (= [a a2] (get-in by-key ["p-mixed" "lanes" 0 "turns"])))
        (is (= [b] (get-in by-key ["p-mixed" "lanes" 1 "turns"]))
            "a turn that both proposes and occurs is listed once")
        (is (= "p-mixed" (get-in ground [0 "key"])) "recency breaks the lane-count tie")
        (is (= [a2 a b] @calls) "the herd reused both strand cache entries")
        (is (= body (:body (routes/herd-response cfg window))))
        (is (= [a2 a b] @calls) "a repeated herd poll does not compute again")
        (is (= [] (get-in (routes/herd-response cfg (assoc window "agents" "agent-a"))
                         [:body "common_ground"])))
        (is (= [] (get-in (routes/herd-response cfg (assoc window "until" "2026-01-01T02:30:00Z"))
                         [:body "common_ground"])))))))

(deftest common-ground-orders-by-lane-count-then-recency-and-caps-at-fifty
  (let [ground (ns-resolve 'futon3c.circus.routes 'common-ground)
        keys (mapv #(str "p-" %) (range 60))
        lane (fn [session patterns at]
               {:session session :agent session
                :turns [{:id (str "turn-" session) :created_at at
                         :patterns {:proposed patterns :occurs []}}]})
        result (ground [(lane "a" (conj keys "widest") "2026-01-01T02:00:00Z")
                        (lane "b" keys "2026-01-01T03:00:00Z")
                        (lane "c" ["widest"] "2026-01-01T01:00:00Z")
                        (lane "d" ["widest"] "2026-01-01T01:00:00Z")])]
    (is (= 50 (count result)))
    (is (= "widest" (:key (first result))) "three lanes beat a more recent two-lane key")
    (is (= 3 (count (:lanes (first result)))))
    (is (every? #(>= (count (:lanes %)) 2) result))))
