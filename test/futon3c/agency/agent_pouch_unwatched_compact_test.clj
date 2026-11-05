(ns futon3c.agency.agent-pouch-unwatched-compact-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.agent-pouch :as pouch]))

(defn- prompt [origin body]
  (str "--- CURRENT TURN ---\n"
       "Surface: bell\n"
       "Origin: " origin "\n"
       "---\n"
       body))

(defn- note! [agent session n prompt-str]
  (dotimes [_ n]
    (pouch/note-turn-origin! agent session prompt-str)))

(deftest unwatched-runs-enable-auto-compaction
  (with-redefs [pouch/joey-max-tokens (constantly 200000)
                pouch/auto-compact-seat? (constantly false)]
    (testing "six non-operator turns compact an oversized session once"
      (let [tokens (atom 200001)
            calls (atom 0)]
        (with-redefs [pouch/session-context-tokens #(when % @tokens)]
          (note! "unwatched-six" "s-six" 6 (prompt "agent" "work"))
          (pouch/joey-eligible-or-compact?
           "unwatched-six" "s-six" #(do (swap! calls inc) (reset! tokens nil)))
          (is (= 1 @calls)))))

    (testing "five non-operator turns do not enable compaction"
      (let [calls (atom 0)]
        (with-redefs [pouch/session-context-tokens (constantly 200001)]
          (note! "unwatched-five" "s-five" 5 (prompt "agent" "work"))
          (pouch/joey-eligible-or-compact?
           "unwatched-five" "s-five" #(swap! calls inc))
          (is (zero? @calls)))))

    (testing "an operator turn resets a six-turn run"
      (let [calls (atom 0)]
        (with-redefs [pouch/session-context-tokens (constantly 200001)]
          (note! "unwatched-reset" "s-reset" 6 (prompt "agent" "work"))
          (is (zero? (pouch/note-turn-origin!
                      "unwatched-reset" "s-reset" (prompt "operator" "review"))))
          (pouch/joey-eligible-or-compact?
           "unwatched-reset" "s-reset" #(swap! calls inc))
          (is (zero? @calls)))))

    (testing "quoted operator origin after the first header does not reset"
      (let [calls (atom 0)
            tokens (atom 200001)
            p (prompt "harness" "quoted below:\nOrigin: operator")]
        (with-redefs [pouch/session-context-tokens #(when % @tokens)]
          (note! "unwatched-quoted" "s-quoted" 6 p)
          (pouch/joey-eligible-or-compact?
           "unwatched-quoted" "s-quoted" #(do (swap! calls inc) (reset! tokens nil)))
          (is (= 1 @calls)))))

    (testing "a run on one session does not compact another session"
      (let [calls (atom 0)]
        (with-redefs [pouch/session-context-tokens (constantly 200001)]
          (note! "unwatched-split" "s-a" 6 (prompt "agent" "work"))
          (note! "unwatched-split" "s-b" 1 (prompt "agent" "work"))
          (pouch/joey-eligible-or-compact?
           "unwatched-split" "s-b" #(swap! calls inc))
          (is (zero? @calls)))))

    (testing "a resume that quotes a whole operator header block still counts as non-operator"
      (let [p (prompt "harness" (str "resumed:\n" (prompt "operator" "old turn")))]
        (note! "unwatched-nested" "s-nested" 5 (prompt "agent" "work"))
        (is (= 6 (pouch/note-turn-origin! "unwatched-nested" "s-nested" p)))))

    (testing "headerless provider continuations do not count as turns"
      (note! "unwatched-continuation" "s-continuation" 1
             (prompt "agent" "semantic dispatch"))
      (dotimes [_ 11]
        (is (= 1 (pouch/note-turn-origin!
                  "unwatched-continuation" "s-continuation"
                  "tool_result continuation without a turn envelope"))))
      (is (false? (pouch/unwatched-run?
                   "unwatched-continuation" "s-continuation"))))

    (testing "headerless continuations do not undo an operator reset"
      (note! "operator-continuation" "s-operator-continuation" 6
             (prompt "agent" "work"))
      (is (zero? (pouch/note-turn-origin!
                  "operator-continuation" "s-operator-continuation"
                  (prompt "operator" "review"))))
      (dotimes [_ 4]
        (is (zero? (pouch/note-turn-origin!
                    "operator-continuation" "s-operator-continuation"
                    "provider continuation")))))

    (testing "ten non-operator turns stay below the token threshold"
      (let [calls (atom 0)]
        (with-redefs [pouch/session-context-tokens (constantly 199999)]
          (note! "unwatched-small" "s-small" 10 (prompt "agent" "work"))
          (pouch/joey-eligible-or-compact?
           "unwatched-small" "s-small" #(swap! calls inc))
          (is (zero? @calls)))))))
