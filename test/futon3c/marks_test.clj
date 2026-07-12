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
