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
              {:turn (assoc (turns/read-record store turn-id) :id turn-id)
               :pattern-verdicts {:state :read
                                  :patterns (if (= turn-id a)
                                              [{:pattern "invented-pattern" :status :partial
                                                :chain {:G {:element "gap" :turn old}}}]
                                              [])} :gcc {}})]
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
                "threads_n" 1 "quotes_n" 1} (get-in body ["lanes" 0 "turns" 0])))
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
