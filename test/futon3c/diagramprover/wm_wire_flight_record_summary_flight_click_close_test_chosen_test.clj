(ns futon3c.diagramprover.wm-wire-flight-record-summary-flight-click-close-test-chosen-test
  "Wire [:flight-record-summary :flight-click-close-test :chosen]: the click's
  selection as record-summary reads it from the run record ([:decision
  :chosen], select-keys [:id :candidate :precedence], only when the
  chosen's target is the flight's) reaching the component's own test
  (futon2/test/futon2/aif/flight_click_close_test.clj, the :box/kind :test
  box), whose read of this field is

    (is (= :C1 (get-in e [:chosen :candidate])))
    (is (= (get-in seventh-run [:decision :chosen :precedence])
           (get-in e [:chosen :precedence])))

  in the-seventh-flights-selection-and-close-are-kept — e being the click
  entry record-click wrote over record-summary of the run record. A test
  box has no runtime var to drive through, so the hermetic witness
  performs exactly that read over a real call of the writer's var
  (record-summary of a run record run-opportunity! wrote from a real
  select-action-cascades decision, the r9 wire's seam), and observes the
  entry's :chosen beside the run record's [:decision :chosen] — the box's
  own comparisons.

  No live record carries the reader's end (the reader is a test, its read
  on no record); the live records read say what the field's live presence
  is (the seventh flight's run record chose :C1; its click entry, written
  before WM-CAST-I 2, kept no :chosen — the defect the reader box pins).
  So the wire is WITNESSED-HERMETICALLY."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.hermetic-repair-fixture :as hermetic]
            [futon2.aif.learning-trial-ledger :as learning-ledger]
            [futon2.aif.policy :as policy]
            [futon2.aif.trace :as trace]
            [futon3c.diagramprover.wm-wire :as w]))

(defn- step [id] {:id id :target "M-t" :guard {:clauses [{:present #{} :absent #{}}]} :produces #{}})

(defn- ranked [cascade-id pattern g rank]
  {:action {:kind :cascade-candidate :id cascade-id :cascade-id cascade-id :target "M-t"
            :precedence [(step pattern)]
            :construction-receipt {:kind :fixture} :interpretation-receipts {cascade-id {:kind :fixture}}}
   :cascade true :cascade-id cascade-id :controller-score g :rank rank})

(def roster [(ranked :cas/b :p/b 1.0 1) (ranked :cas/a :p/a 3.0 2)])
(def roster-other [(ranked :cas/a :p/a 1.0 1) (ranked :cas/b :p/b 3.0 2)])

(defn- runner-opts
  "The r9 wire's isolated options (futon2 full-loop-runner-test's fixture
  cannot load here: it reads futon2-relative fixture paths), up to
  selection."
  [decision]
  (merge (hermetic/runner-repair-options)
         {:cohort? false :author "zai-5" :reviewer "codex-7" :repair-reviewer "codex-1"
          :phase-log-fn (fn [_])
          :roster-fn (fn [_] {:zai-5 {:status "idle" :invoke-ready? true}
                              :codex-7 {:status "idle" :invoke-ready? true}
                              :codex-1 {:status "idle" :invoke-ready? true}})
          :refresh-fn (fn [])
          :substrate-preflight-fn (fn [_] {:route :test})
          :code-state-fn (fn [] {:repo "/futon2" :git-sha "head" :git-dirty? false :repo-heads {}})
          :mode-flags-fn (fn [] {}) :version-stamp-fn identity :mission-fn (fn [t] {:id t})
          :repair-open-fn (constantly [])
          :repair-system-record-fn (fn [m] {:repair/id "repair-wire-1" :repair/class (:repair-class m)})
          :r16-park-fn (fn [_ _] {:ok true :id "park-wire" :status :parked})
          :delivery-qa-fn (fn [_ _] {:morning-brief/addendum-id "qa-wire"})
          :queue-fn identity
          :judge-fn (fn [_] {:judgement {:decision decision :belief {} :belief-pre {} :observation {}
                                         :free-energy {} :prediction-errors {} :precision-state {}
                                         :micro-step-trace [] :mode :maintain}})
          :construct-fn (fn [& _] (throw (ex-info "stop after selection" {:outcome :incomplete})))}))

(defn- run-record
  "The run record run-opportunity! writes for ROSTER's decision, in temp
  stores."
  [roster]
  (let [decision (policy/select-action-cascades roster {:beta 1 :novelty-inputs {}})]
    (with-redefs-fn {#'trace/default-trace-dir (w/tmp-dir "wire-trace")
                     #'runner/default-run-record-dir (w/tmp-dir "wire-run-records")
                     #'learning-ledger/default-root (w/tmp-dir "wire-learning")}
      #(binding [runner/*wm-status-reporting?* false]
         (edn/read-string (slurp (:run-record (runner/run-opportunity! (runner-opts decision)))))))))

(defn observe
  "The run record for ROSTER read by record-summary for TARGET, kept by
  record-click: {:writer the summary's :chosen, :reader the click entry's
  :chosen (a typed absence when the key is not carried), :record-chosen
  the run record's [:decision :chosen], :reader-agrees? the box's own
  comparisons}."
  ([roster] (observe roster "M-t"))
  ([roster target]
   (let [record (run-record roster)
         summary (fr/record-summary target "click-1" record)
         e (first (:clicks (flight/record-click
                            (flight/start {:target target :chosen-because {:kind :requested}}
                                          {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                          {:id "flight-wire"})
                            (merge summary {:wants [:t/b] :before {} :after {}}))))]
     {:writer (:chosen summary)
      :reader (if (contains? e :chosen) (:chosen e) {:absent :field-not-carried})
      :record-chosen (get-in record [:decision :chosen])
      :reader-agrees? (and (= (get-in record [:decision :chosen :candidate])
                              (get-in e [:chosen :candidate]))
                           (= (get-in record [:decision :chosen :precedence])
                              (get-in e [:chosen :precedence])))})))

(defn check [] (observe roster))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "the writer's source only: [:decision :chosen] is present with :candidate :C1; a run record is not the flight record, and the matching flight record's click entry predates WM-CAST-I 2"}
     {:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "the reader's end absent: the seventh flight's click entry has no :chosen (written before WM-CAST-I 2 kept it)"}
     {:path (p "flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn")
      :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970"
      :why "the eighth flight chose nothing: [:decision :chosen] {:status :absent :reason :no-chosen-action}"}
     {:path (p "flight-ada87008/flight-ada87008.edn")
      :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
      :why "and its click entry accordingly carries no :chosen"}]))

(def wire
  {:wire [:flight-record-summary :flight-click-close-test :chosen]
   :kind :witnessed-hermetically
   :test `the-clicks-selection-reaches-the-components-own-test
   :check check
   :live-records-read live-records-read})

(deftest the-clicks-selection-reaches-the-components-own-test
  (let [o (check)]
    (is (= :cas/b (get-in o [:record-chosen :candidate])) "the run record's chosen")
    (is (= (select-keys (:record-chosen o) [:id :candidate :precedence]) (:writer o))
        "record-summary's read of the run record")
    (is (:reader-agrees? o) "the box's own comparisons: candidate and precedence")
    (is (w/received? o))))

(deftest another-targets-selection-is-not-kept-and-fails-the-wire
  ;; record-summary's guard: :chosen only when its :target is the flight's
  (let [o (observe roster "M-some-other-target")]
    (is (some? (:record-chosen o)))
    (is (nil? (:writer o)))
    (is (= {:absent :field-not-carried} (:reader o)))
    (is (not (w/received? o))))
  (is (not (w/received? (assoc (check) :reader {:absent :no-selection}))))
  (is (not (w/received? (assoc (check) :reader {:status :absent :reason :no-selection})))))

(deftest a-different-selection-fails-the-wire
  (let [o (check)
        other (observe roster-other)]
    (is (= :cas/a (get-in other [:writer :candidate])))
    (is (not= (:writer o) (:reader other)))
    (is (not (w/received? (assoc o :reader (:reader other)))))))

(deftest the-live-records-carry-one-end-at-most
  (let [[run flight eighth-run eighth-flight] live-records-read]
    (doseq [{:keys [path sha256]} live-records-read]
      (is (= sha256 (w/sha256-file path)) path))
    (is (= :C1 (get-in (w/read-record (:path run)) [:decision :chosen :candidate])))
    (is (not-any? :chosen (:clicks (:flight (w/read-record (:path flight))))))
    (is (= {:status :absent :reason :no-chosen-action}
           (get-in (w/read-record (:path eighth-run)) [:decision :chosen])))
    (is (not-any? :chosen (:clicks (:flight (w/read-record (:path eighth-flight))))))))
