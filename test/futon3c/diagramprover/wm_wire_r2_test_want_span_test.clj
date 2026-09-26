(ns futon3c.diagramprover.wm-wire-r2-test-want-span-test
  "Wire [:r2-served-by-reading :r2-test :want-span]: the read step's placed
  want span reaching the component's own test.

  The writer is served-by-reading/proposal, which turns a seat's want quote
  into a code-point span (zero-based, end-exclusive, the unit every :via
  span uses). The reader is futon2/test/futon2/aif/served_by_reading_test.clj
  (the :box/kind :test box), whose reads of this field are the assertions

    (is (= roles-sentence ((f 'cp-subs) (:text ctx) (first (:want-span p))
                           (second (:want-span p)))))
    (is (= (:want-span p) (get-in r [:via :want-span])))

  — it resolves the span back to the quoted sentence and equates it with
  the verifier row's [:via :want-span]. A test box has no runtime var to
  drive through, so the hermetic witness performs exactly those reads:
  proposal is called (writer), the proposal is verified (so the second read
  has its r), and the reader's expressions are evaluated and observed.

  No live record carries the writer's end: every flight record under
  holes/labs/M-wm-wiring/spike/ records :proposals {:absent :no-quotes}
  (see live-records-read, each pinned and read). So the wire is
  WITNESSED-HERMETICALLY over the live M-futon-seams text (content sha
  pinned below, the same pin the reader itself carries)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon2.aif.served-by-reading :as sbr]
            [futon2.wm.extract-outcomes]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- f [v] @(ns-resolve 'futon2.wm.extract-outcomes v))

(def mission-path "holes/missions/M-futon-seams.md")
;; content sha of M-futon-seams.md since futon3c 3f5f44dd (the pin the
;; reader, served_by_reading_test.clj, itself carries)
(def mission-sha-pinned "d13c5cfe9e9b19b445bd5bb73507f286a9e5ff3b478a1c5bc6a2250d70c6f6fd")

(def roles-sentence
  "Roles resolve to seats; seats declare provider and availability; code asks for a role instead of pattern-matching an id.")

(def reading-route
  ;; 4/:caller-converted -> :o-2, the reader's own fixture proposal
  {:instance 4 :outcome :o-2
   :artefact {:outcome-phrase "later implementation" :want-phrase "code asks for a role"}
   :want-quote roles-sentence :outcome-quote "later implementation"})

(defn- context [text]
  (let [isecs ((f 'instance-sections) ((f 'headings) text) (count (str/split-lines text)))
        section-of (fn [l] (:instance (first (filter #(<= (first (:lines %)) l (second (:lines %))) isecs))))
        cs (mapv #(assoc %1 :id (keyword (str "o-" (inc %2))))
                 ((f 'consolidate) ((f 'extract) text section-of {})) (range))]
    {:text text :isecs isecs :outcomes (:outcomes ((f 'filter-outcomes) text cs))}))

(defn observe
  "Proposal (writer), then the reader's two reads of :want-span as
  served_by_reading_test.clj performs them; TAMPER edits the proposal
  before the reads (the bad cases). {:writer the proposal's :want-span,
  :reader the value read off the proposal (a typed absence when the key is
  not carried), :reader-resolves? the reader's cp-subs round-trip,
  :reader-matches-via? the reader's equality with the verified row's
  [:via :want-span]}."
  ([] (observe identity))
  ([tamper]
   (let [{:keys [text isecs outcomes]} (context (slurp mission-path))
         w0 (sbr/proposal text isecs outcomes reading-route)
         p (tamper w0)
         r (when (contains? p :want-span)
             ((f 'verify-proposed-link) text isecs outcomes p))
         span (when (contains? p :want-span) (:want-span p))]
     {:writer (:want-span w0)
      :reader (if (contains? p :want-span) span {:absent :field-not-carried})
      :reader-resolves? (and span (= roles-sentence ((f 'cp-subs) text (first span) (second span))))
      :reader-matches-via? (and r (= :proposed-verified (get-in r [:via :basis]))
                                (= span (get-in r [:via :want-span])))})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why ":readings[0] :proposals {:absent :no-quotes} — no proposal ran, so no writer end"}
     {:path (p "flight-6cda5ee8.edn")
      :sha256 "b26d4c3cc98009b1f7a828cd2355f6b01bb03feaeda8f03bf43a8a94f0292e34"
      :why ":readings[0] :proposals {:absent :no-quotes}"}
     {:path (p "flight-7f89646a.edn")
      :sha256 "d782b3830a040dca8cfe080440869ab4a08ede22fbfd9650eb488d5f7449cc26"
      :why ":readings[0] :proposals {:absent :no-quotes}"}
     {:path (p "flight-d00574c8.edn")
      :sha256 "68e531b462d535983ea5114a8da370daec943a3b4283b6b87831e63ecf9eadd8"
      :why ":readings[0] :proposals {:absent :no-quotes}"}
     {:path (p "flight-e70b4baf.edn")
      :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c"
      :why ":readings[0] :proposals {:absent :no-quotes}"}
     {:path (p "flight-ffcd772b.edn")
      :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575"
      :why ":readings[0] :proposals {:absent :no-quotes}"}
     {:path (p "flight-ada87008/flight-ada87008.edn")
      :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
      :why ":readings[0] :proposals {:absent :no-quotes}"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records (no :want-span, no proposals; grep of the exemplar directory finds neither key)"}]))

(def wire
  {:wire [:r2-served-by-reading :r2-test :want-span]
   :kind :witnessed-hermetically
   :test `the-want-span-reaches-the-components-test
   :check check
   :live-records-read live-records-read})

(deftest the-want-span-reaches-the-components-test
  (is (= mission-sha-pinned (sbr/sha256 (slurp mission-path)))
      "the fixture text is the pinned M-futon-seams")
  (let [o (check)]
    (is (:reader-resolves? o) "the reader's first read: the span resolves to the quote")
    (is (:reader-matches-via? o) "the reader's second read: the span is the verified row's")
    (is (w/received? o))))

(deftest a-missing-span-is-a-typed-absence-and-fails-the-wire
  (let [o (observe #(dissoc % :want-span))]
    (is (= {:absent :field-not-carried} (:reader o)))
    (is (not (:reader-resolves? o)))
    (is (not (w/received? o)))))

(deftest a-different-span-fails-the-wire
  ;; shift the span one code point right: it still resolves to text, but the
  ;; reader's cp-subs round-trip no longer yields the quote and the value on
  ;; the wire is not the writer's span
  (let [o (observe #(update % :want-span (fn [[a b]] [(inc a) (inc b)])))]
    (is (some? (:reader o)))
    (is (not (:reader-resolves? o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))

(deftest the-live-records-carry-no-proposal
  (doseq [{:keys [path sha256]} (filter :path live-records-read)]
    (is (= sha256 (w/sha256-file path)) path)
    (is (= {:absent :no-quotes}
           (get-in (w/read-record path) [:flight :readings 0 :served-by :proposals])) path)))
