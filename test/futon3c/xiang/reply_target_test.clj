(ns futon3c.xiang.reply-target-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.xiang.reply-target :as rt]))

;; Joe's turn of 2026-10-04 02:2x, verbatim in shape: two pointers and one
;; own-intent paragraph, answering claude-17's turn that replied to a
;; work-orders nudge (a bellback), not its previous turn to Joe.
(def joe-turn
  (str "🈖: So, to me <mark>: means that I'm replying to your point above.\n\n"
       "🈸: Subject to my clarification above, yes, let's work on this directly.\n\n"
       "㊩ On this line I am not using the colon form b/c I am marking my own intent."))

(def to-joe
  {:turn-id "t-to-joe" :origin "operator"
   :text (str "㊥ (which frame) Yes, the REPL can know that.\n\n"
              "🈖 (how 大象 would resolve it) Your ㊩: answers ...\n\n"
              "🈸 (next) Shall I start with the recording side?")})

(def to-nudge
  {:turn-id "t-to-nudge" :origin "work-orders"
   :text (str "㊥ (nudged myself) The machine just nudged me.\n\n"
              "🈖 (the wider risk) Without this fix ...\n\n"
              "🈸 (still the open decision) Shall I start on recording your leading mark?")})

(deftest pointers-resolve-to-the-newest-turn-carrying-the-mark
  (let [{:keys [replies declared-intents]} (rt/resolve-targets joe-turn [to-nudge to-joe])]
    (is (= [["🈖" :mark-match "t-to-nudge"] ["🈸" :mark-match "t-to-nudge"]]
           (map (juxt :mark :rule :turn-id) replies))
        "the bellback-answering turn is newest and carries both marks")
    (is (re-find #"still the open decision" (:paragraph (second replies))))
    (is (= [{:index 2 :mark "㊩"}] declared-intents)
        "a mark without a colon is Joe's own intent, not a pointer")))

(deftest a-mark-only-in-an-older-turn-skips-the-newer-one
  (let [older {:turn-id "t-old" :origin "operator" :text "㊭ (order) 1. ... 2. ..."}
        {:keys [replies]} (rt/resolve-targets "㊭: yes, that order" [to-nudge older])]
    (is (= [:mark-match "t-old"] ((juxt :rule :turn-id) (first replies))))))

(deftest no-match-falls-back-to-the-newest-and-says-so
  (testing "a mark no candidate carries"
    (is (= [:newest "t-to-nudge"]
           ((juxt :rule :turn-id) (first (:replies (rt/resolve-targets "🈚: no" [to-nudge to-joe])))))))
  (testing "no candidates"
    (is (= :none (:rule (first (:replies (rt/resolve-targets "🈚: no" []))))))))

(deftest unmarked-turns-point-at-nothing
  (is (= {:replies [] :declared-intents []}
         (rt/resolve-targets "ok please continue" [to-joe]))))

(deftest a-colon-pointer-inside-a-sentence-is-read
  (testing "turn 392, verbatim: the pointer is not at the start of the paragraph"
    (is (= [["🈸" :mark-match "t-to-nudge"]]
           (map (juxt :mark :rule :turn-id)
                (:replies (rt/resolve-targets "OK, I've tried the hydra, and 🈸:yes" [to-nudge to-joe]))))))
  (testing "an opening pointer and an inline one in the same paragraph"
    (is (= ["㊭" "🈸"]
           (map :mark (filter :pointer? (rt/operator-marks "㊭: fine, and 🈸: yes")))))))

(deftest a-mark-written-about-is-not-a-pointer
  (testing "a bare mark inside a sentence"
    (is (= {:replies [] :declared-intents []}
           (rt/resolve-targets "I liked the 🈸 in your last turn" [to-joe]))))
  (testing "the colon form quoted in backticks or quotation marks"
    (is (= [] (rt/operator-marks "the form `🈸:` means a reply")))
    (is (= [] (rt/operator-marks "writing \"🈸: yes\" answers it")))
    (is (= [] (rt/operator-marks "writing “🈸: yes” answers it")))))
