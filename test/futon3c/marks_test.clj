(ns futon3c.marks-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.marks :as marks]))

(defn- entry
  [author text]
  {:evidence/id (str "e-test-" (hash [author text]))
   :evidence/subject {:ref/type :agent :ref/id "joe"}
   :evidence/type :coordination
   :evidence/claim-type :observation
   :evidence/author author
   :evidence/at "2026-07-12T00:00:00Z"
   :evidence/body {:event "chat-turn" :role "user" :text text}
   :evidence/tags [:chat-turn]})

(deftest real-corpus-mention-correction
  (let [text "I could mark corrections with ✘ for example"
        marks (marks/recognize-marks text)
        decorated (marks/decorate-turn (entry "joe" text))]
    (is (= [{:glyph "✘"
             :verdict :mention
             :type :correction
             :ref nil
             :payload nil
             :offset 30}]
           marks))
    (is (not (contains? (set (:evidence/tags decorated)) :correction)))
    (is (= :mention (get-in decorated [:evidence/body :marks 0 :verdict])))))

(deftest real-corpus-approval-event-plus-mention
  (let [text "OK, that's good ✓ (maybe we can add a ✓ for approval)"
        decorated (marks/decorate-turn (entry "joe" text))
        marks (get-in decorated [:evidence/body :marks])]
    (is (= 2 (count marks)))
    (is (= [:event :mention] (mapv :verdict marks)))
    (is (= [:approval] (filterv #{:approval} (:evidence/tags decorated))))))

(deftest real-corpus-idea-event
  (let [text "💡 It will use the tags that I enter here, because D1 strips glyphs."
        decorated (marks/decorate-turn (entry "joe" text))]
    (is (= [{:glyph "💡"
             :verdict :event
             :type :idea
             :ref nil
             :payload nil
             :offset 0}]
           (get-in decorated [:evidence/body :marks])))
    (is (contains? (set (:evidence/tags decorated)) :idea))))

(deftest edn-long-form-event
  (let [text "(✘ :ref E-foo \"wrong seam\")"
        mark (first (marks/recognize-marks text))]
    (is (= {:glyph "✘"
            :verdict :event
            :type :correction
            :ref "E-foo"
            :payload "wrong seam"
            :offset 0}
           mark))))

(deftest multi-mark-turn-tags-each-event-type
  (let [text "✓ shipped. 💡 follow-up: index the tag. (✘ :ref E-old \"bad parse\")"
        decorated (marks/decorate-turn (entry "joe" text))]
    (is (= [:approval :idea :correction]
           (filterv #{:approval :idea :correction} (:evidence/tags decorated))))
    (is (= [:approval :idea :correction]
           (mapv :type (get-in decorated [:evidence/body :marks]))))))

(deftest decorate-turn-idempotent
  (let [once (marks/decorate-turn (entry "joe" "✓ good"))
        twice (marks/decorate-turn once)]
    (is (= once twice))
    (is (= 1 (count (filter #{:approval} (:evidence/tags twice)))))))

(deftest non-operator-turn-untouched
  (let [e (entry "claude-5" "✓ looks good")]
    (is (= e (marks/decorate-turn e)))))

(deftest boundary-append-decorates-chat-turn
  (let [store (atom {:entries {} :order []})
        result (boundary/append! store (entry "joe" "✓ good"))]
    (is (:ok result))
    (is (contains? (set (get-in result [:entry :evidence/tags])) :approval))
    (is (= :event (get-in result [:entry :evidence/body :marks 0 :verdict])))))

(deftest event-not-suppressed-by-distant-context-words
  ;; Regression: the v1 mention heuristic matched broad words (type, write,
  ;; marked, ...) anywhere in the preceding 80 chars, silently dropping real
  ;; approval events. The rule is now immediate-token only.
  (doseq [text ["I fixed the type error ✓"
                "Marked it done, will write the docs later ✓"
                "we should add tests for the parser ✓"]]
    (is (= :event (:verdict (first (marks/recognize-marks text)))) text)))

(deftest mention-still-recognized-without-parens
  (is (= :mention (:verdict (first (marks/recognize-marks "you can add a ✓ for approval")))))
  (is (= :mention (:verdict (first (marks/recognize-marks "mark corrections with ✘"))))))

(deftest second-string-glyphs-recognized
  ;; The C-c , shadow tier (operator-named cluster types, 2026-07-12).
  (let [decorated (marks/decorate-turn (entry "joe" "⚖ this is the decision point for the migration"))]
    (is (= :event (get-in decorated [:evidence/body :marks 0 :verdict])))
    (is (contains? (set (:evidence/tags decorated)) :hinge)))
  (let [mark (first (marks/recognize-marks "(⚠ :ref M-marks-to-labels \"grain drift\")"))]
    (is (= {:glyph "⚠" :verdict :event :type :concern :ref "M-marks-to-labels"
            :payload "grain drift" :offset 0} mark)))
  ;; mention rule applies uniformly: glyph as noun-phrase object
  (is (= :mention (:verdict (first (marks/recognize-marks "you could tag facts with a 📌 here"))))))

(deftest second-string-mints-no-core-tags
  (let [decorated (marks/decorate-turn (entry "joe" "🧭 aim for the smallest diff"))]
    (is (contains? (set (:evidence/tags decorated)) :guidance))
    (is (not-any? #{:correction :approval :idea} (:evidence/tags decorated)))))

;; --- agent self-mark channel (zai/zaif turn-rounds, M-points-de-fuite) -----

(defn- round-entry
  [author text]
  {:evidence/id (str "e-test-" (hash [author text :round]))
   :evidence/subject {:ref/type :agent :ref/id author}
   :evidence/type :coordination
   :evidence/claim-type :step
   :evidence/author author
   :evidence/session-id "zai-test"
   :evidence/at "2026-07-13T00:00:00Z"
   :evidence/body {:event :turn-round :round 2 :final false :text text :calls []}
   :evidence/tags [:transcript :turn-round]})

(deftest agent-round-self-correction-event
  (let [text "✘ read_file on the wrong path — the spec lives in holes/, retrying there."
        decorated (marks/decorate-agent-round (round-entry "zai-1" text))]
    (is (= :event (get-in decorated [:evidence/body :marks 0 :verdict])))
    (is (contains? (set (:evidence/tags decorated)) :self-correction))
    (is (not-any? #{:correction} (:evidence/tags decorated)))))

(deftest agent-round-long-form-and-idea
  (let [text "(✘ :ref M-custom-harness \"assumed 8-round budget, it is 24\") 💡 the budget could be adaptive"
        decorated (marks/decorate-agent-round (round-entry "zai-1" text))
        marks (get-in decorated [:evidence/body :marks])]
    (is (= [:correction :idea] (mapv :type marks)))
    (is (= "M-custom-harness" (:ref (first marks))))
    (is (= [:self-correction :self-idea]
           (filterv #{:self-correction :self-idea} (:evidence/tags decorated))))))

(deftest agent-round-decoration-idempotent
  (let [once (marks/decorate-agent-round (round-entry "zai-1" "✘ wrong branch"))
        twice (marks/decorate-agent-round once)]
    (is (= once twice))
    (is (= 1 (count (filter #{:self-correction} (:evidence/tags twice)))))))

(deftest agent-chat-turn-still-untouched
  ;; Self-marks apply to :turn-round transcripts only; agent chat-turns stay
  ;; out of both channels (the existing non-operator-turn-untouched contract).
  (let [e (entry "zai-1" "✘ scratch that")]
    (is (= e (marks/maybe-decorate-turn e)))))

(deftest operator-turn-round-not-self-marked
  ;; Defensive: a joe-authored turn-round (should not exist) must not mint
  ;; self-tags.
  (let [e (round-entry "joe" "✘ wrong")]
    (is (not-any? #{:self-correction} (:evidence/tags (marks/maybe-decorate-turn e))))))

(deftest boundary-append-decorates-agent-round
  (let [store (atom {:entries {} :order []})
        result (boundary/append! store (round-entry "zai-1" "✘ the edit hit the wrong seam, reverting"))]
    (is (:ok result))
    (is (contains? (set (get-in result [:entry :evidence/tags])) :self-correction))
    (is (= :event (get-in result [:entry :evidence/body :marks 0 :verdict])))))
