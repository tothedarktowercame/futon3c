(ns futon3c.xiang.reply-target-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
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

;; Joe's turn of 2026-10-04 05:56Z (record turn-WdLaYK), verbatim, and
;; claude-17's reply to his previous turn (turn-uJNPcf), which it answers:
;; evidence emacs-0b3b74bca37997e6f56b4b4357a62c5d. The bracket text "no
;; requisition line" occurs in exactly one paragraph of that reply, the ㊟.
(def wdlayk
  (str "🈯 (no requisition line): I meant in an informal sense, the same general way that Kimi agents are requisitioned but not the same exact \"Kimi requisition line\".  🈸:Let's keep the experimental design \"design only\" and not bother claude-4 right now.  We could pick any similar task.  The Lean port can be kept as an e.g."))

(def turn-42-reply
  {:turn-id "repl-claude-17-turn-42" :origin "operator"
   :text (slurp "test/futon3c/xiang/turn_acts_fixtures/turn-uJNPcf.reply.txt")})

(deftest a-bracketed-target-with-a-colon-is-a-pointer
  (let [{:keys [replies declared-intents]} (rt/resolve-targets wdlayk [turn-42-reply])
        [bracketed inline] replies]
    (is (= [:bracket-match "🈯" "no requisition line"]
           ((juxt :rule :mark :bracket) bracketed)))
    (is (str/starts-with? (:paragraph bracketed) "(your \"send it to the War Machine"))
    (is (= "㊟" (:paragraph-mark bracketed)) "the agent paragraph's own mark, for the session join")
    (is (= [:mark-match "🈸"] ((juxt :rule :mark) inline))
        "the inline 🈸: in the same paragraph still answers the 🈸 paragraph")
    (is (= [{:index 0 :mark "🈯"}] declared-intents)
        "the mark in the bracket form is the operator's own intent")))

(deftest bracket-forms-that-are-not-pointers
  (testing "no colon after the bracket: a declared intent"
    (is (= {:replies [] :declared-intents [{:index 0 :mark "㊟"}]}
           (rt/resolve-targets "㊟ (the order) fine as it stands" [turn-42-reply]))))
  (testing "a colon later in the sentence"
    (is (= [] (:replies (rt/resolve-targets "㊟ (x) note: y" [turn-42-reply])))))
  (testing "bracket text found nowhere, or too short to match: the newest fallback"
    (is (= :newest (:rule (first (:replies (rt/resolve-targets "🈯 (zebra crossings): no" [turn-42-reply]))))))
    (is (= :newest (:rule (first (:replies (rt/resolve-targets "🈯 (job): no" [turn-42-reply])))))))
  (testing "bracket text in two paragraphs of the newest turn: not a match"
    (is (= :newest (:rule (first (:replies (rt/resolve-targets "🈯 (codex-10): yes" [turn-42-reply]))))))))
