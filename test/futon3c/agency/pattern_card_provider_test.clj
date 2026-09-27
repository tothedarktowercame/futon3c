(ns futon3c.agency.pattern-card-provider-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.pattern-card-provider :as pattern]))

(defn entry [id agent session at pattern-id]
  {:evidence/id id :evidence/author agent :evidence/session-id session
   :evidence/at at
   :evidence/body {"event" "context-retrieval"
                   "results" [{"id" pattern-id "score" 0.7 "rank" 1}
                               {"id" "other/two" "score" 0.6 "rank" 2}]}})

(use-fixtures :each (fn [f] (pattern/reset-cache!) (f)))

(deftest refresh-and-provider-use-exact-session
  (let [records [(entry "e-wrong" "claude-17" "other"
                       "2026-09-27T19:59:30Z" "wrong/pattern")
                 (entry "e-right" "claude-17" "target"
                        "2026-09-27T19:59:20Z" "right/pattern")]]
    (pattern/refresh! "claude-17" "target" (constantly records))
    (let [segment (pattern/provider {:agent-id "claude-17" :session-id "target"
                                     :render-at "2026-09-27T20:00:00Z"})]
      (is (= "~right/pattern" (:segment/value segment)))
      (is (= "e-right" (get-in segment [:segment/basis :evidence-ref])))
      (is (= "retrieved right/pattern 0.7; also other/two"
             (:segment/header segment))))))

(deftest stale-retrieval-is-omitted
  (pattern/observe-entry! (entry "e-stale" "claude-17" "target"
                                 "2026-09-27T18:00:00Z" "old/pattern"))
  (is (nil? (pattern/provider {:agent-id "claude-17" :session-id "target"
                               :render-at "2026-09-27T20:00:00Z"}))))

(deftest persisted-edn-string-body-is-readable
  (pattern/observe-entry!
   (assoc (entry "e-wire" "claude-17" "target"
                 "2026-09-27T19:59:50Z" "wire/pattern")
          :evidence/body
          (pr-str {"event" "context-retrieval"
                   "results" [{:id "wire/pattern" :score 0.8 :rank 1}]})))
  (is (= "~wire/pattern"
         (:segment/value
          (pattern/provider {:agent-id "claude-17" :session-id "target"
                             :render-at "2026-09-27T20:00:00Z"})))))
