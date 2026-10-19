(ns futon3c.agency.refusal-reset-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.refusal-reset :as rr]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.boundary :as boundary]))

(def refusal-text "{\"message\":\"HTTP 400\",\"body\":{\"error\":{\"code\":\"1261\",\"message\":\"Prompt exceeds max length\"}}}")

(deftest only-the-providers-error-counts
  (is (rr/refusal? refusal-text))
  (testing "the bad case: a reply that talks about the error is not the error (zai-7's T3 report, 2026-10-19)"
    (is (not (rr/refusal? "Done. On a 1261 / \"Prompt exceeds max length\" error: compact once ...")))
    (is (not (rr/refusal? "{\"message\":\"HTTP 500\",\"body\":{\"error\":{\"code\":\"1300\"}}}")))))

(deftest a-refused-turn-resets-its-seat-once
  (let [resets (atom [])]
    (reset! rr/!reset #{})
    (with-redefs [reg/reset-session! (fn [a] (swap! resets conj a) {:ok true :old-session-id "s-old"})
                  boundary/append! (fn [& _] {:ok true})]
      (let [ev {:kind :harness-turn :author "zai-7" :session-id "s-old" :evidence-id "e1"
                :entry {:evidence/body {:event "turn-round" :final true :text refusal-text}}}]
        (rr/on-turn-final ev)
        (rr/on-turn-final ev)
        (is (= ["zai-7"] @resets) "once per session, not on every refused turn")
        (rr/on-turn-final (assoc-in ev [:entry :evidence/body :text] "all done"))
        (rr/on-turn-final (assoc ev :kind :reply))
        (is (= ["zai-7"] @resets) "ordinary ends and Claude replies reset nothing")))))
