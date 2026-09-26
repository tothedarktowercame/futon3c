(ns futon3c.diagramprover.wm-wire-r9-candidate-enact-test
  "Wire [:r9-selection-law :r0-enact-step :candidate]: the selected
  candidate reaching the enactment step, the wire PROOF-2a's Clause C joins
  an enactment on.

  No flight has enacted, so no live record carries the reader's end
  (live-records-read): every flight record's enactment is a typed absence,
  and only the seventh flight's run record carries the writer's end
  (:candidate :C1). So the wire is WITNESSED-HERMETICALLY, through the
  flight seam: a real select-action-cascades decision handed to
  full-loop-runner/run-opportunity! by :judge-fn, stopped after selection,
  its run record read by flight-runner/record-summary, kept by
  flight/record-click, and enacted by flight-runner/enact-fn with a fixture
  seat. The writer's value is the selection law's :candidate (the chosen
  entry's :cascade-id); the reader's is the enactment record's
  :decision-candidate.

  The carrier between them is full-loop-runner/chosen-summary, which puts
  the chosen ACTION's :id on the run record, not the selection law's
  :candidate; the two agree when an entry's action :id is its :cascade-id
  (the seventh flight: :C1 and :C1). The bad case a-cascade-id-unlike-the-
  action-id shows the wire failing when they differ."
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

(defn- ranked [cascade-id action-id pattern g rank]
  {:action {:kind :cascade-candidate :id action-id :cascade-id cascade-id :target "M-t"
            :precedence [(step pattern)]
            :construction-receipt {:kind :fixture} :interpretation-receipts {cascade-id {:kind :fixture}}}
   :cascade true :cascade-id cascade-id :controller-score g :rank rank})

(def roster [(ranked :cas/b :cas/b :p/b 1.0 1) (ranked :cas/a :cas/a :p/a 3.0 2)])

(defn- runner-opts
  "The runner test's isolated options (futon2 full-loop-runner-test, which
  cannot load here: it reads futon2-relative fixtures), up to selection."
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
  "The run record run-opportunity! writes for DECISION, in temp stores."
  [decision]
  (with-redefs-fn {#'trace/default-trace-dir (w/tmp-dir "wire-trace")
                   #'runner/default-run-record-dir (w/tmp-dir "wire-run-records")
                   #'learning-ledger/default-root (w/tmp-dir "wire-learning")}
    #(binding [runner/*wm-status-reporting?* false]
       (edn/read-string (slurp (:run-record (runner/run-opportunity! (runner-opts decision))))))))

(defn- enact [record click]
  (let [f (flight/record-click (flight/start {:target "M-t" :chosen-because {:kind :requested}}
                                             {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                             {:id "flight-wire"})
                               (merge click {:wants [:t/b] :before {} :after {}}))
        out ((fr/enact-fn {:dispatch-step! (fn [s] {:commit "c1" :produced (first (get-in s [:interpretation :produces]))
                                                    :check {:class :fixture}})
                           :check-fn (fn [_] {:observed true})
                           :interpretations (constantly {:p/b {:produces #{:t/b}} :p/a {:produces #{:t/a}}})
                           :fetch-run-record (constantly record)
                           :record-dir (w/tmp-dir "wire-enactments")})
             f (last (:clicks f)))]
    (if (:enactment out) (:enactment out) out)))

(defn observe
  "ROSTER through selection, the runner, the click and the enactment step:
  {:writer the selection law's :candidate, :reader the enactment's
  :decision-candidate (the enactment itself when it is a typed absence)}.
  CLICK-FN edits the click before it is recorded (the bad cases)."
  ([roster] (observe roster identity))
  ([roster click-fn]
   (let [decision (policy/select-action-cascades roster {:beta 1 :novelty-inputs {}})
         record (run-record decision)
         e (enact record (click-fn (fr/record-summary "M-t" "click-1" record)))]
     {:writer (get-in decision [:selection-law :candidate])
      :run-record-chosen (get-in record [:decision :chosen :candidate])
      :reader (if (w/typed-absence? e) e (:decision-candidate e))})))

(defn check [] (observe roster))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
      :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
      :why "the writer's end only: [:decision :selection-law :candidate] :C1 and [:decision :chosen :candidate] :C1; a run record carries no enactment"}
     {:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "the same click's flight record: its click entry has no :chosen (written before WM-CAST-I) and its enactment is {:absent :no-dispatch-configured}"}
     {:paths (mapv p ["flight-ffcd772b.edn" "flight-d00574c8.edn" "flight-e70b4baf.edn" "flight-7f89646a.edn"
                       "flight-6cda5ee8.edn" "flight-ada87008/flight-ada87008.edn"])
      :why "every other flight record: its one enactment is {:absent :no-decision}"}
     {:path "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
      :why "carries :candidate :cas/a-registry-first, hand-authored (claude-10, 2026-09-24), as is click-001.edn (claude-1): neither the selection law nor enact-fn wrote it"}]))

(def wire
  {:wire [:r9-selection-law :r0-enact-step :candidate]
   :kind :witnessed-hermetically
   :test `the-selected-candidate-reaches-the-enactment
   :check check
   :live-records-read live-records-read})

(deftest the-selected-candidate-reaches-the-enactment
  (let [o (check)]
    (is (= :cas/b (:writer o)))
    (is (= :cas/b (:run-record-chosen o)) "the carrier: chosen-summary on the run record")
    (is (w/received? o))))

(deftest no-chosen-candidate-is-a-typed-absence-and-fails-the-wire
  (let [o (observe roster #(dissoc % :chosen))]
    (is (= :no-decision (:absent (:reader o))))
    (is (not (w/received? o)))))

(deftest a-cascade-id-unlike-the-action-id-still-crosses-the-wire
  ;; Until futon2 f69f103d (WM-CHOSEN-CANDIDATE-I) the reader received the
  ;; action's :id, not the law's :candidate, and this deftest asserted that
  ;; defect (the two ids coincide on every live record, :C1/:C1, which is
  ;; why no flight saw it). The fix carries the law's :candidate beside the
  ;; action's :id; the wire now holds when they differ, and the action id no
  ;; longer stands in for the candidate.
  (let [o (observe [(ranked :cas/b :act/other :p/b 1.0 1) (ranked :cas/a :cas/a :p/a 3.0 2)])]
    (is (= :cas/b (:writer o)))
    (is (= :cas/b (:reader o)))
    (is (not= :act/other (:reader o)))
    (is (w/received? o))))

(deftest the-live-records-carry-no-enactment
  (let [[run fl] live-records-read]
    (is (= (:sha256 run) (w/sha256-file (:path run))))
    (is (= (:sha256 fl) (w/sha256-file (:path fl))))
    (let [r (w/read-record (:path run))
          x (:flight (w/read-record (:path fl)))]
      (is (= :C1 (get-in r [:decision :selection-law :candidate]) (get-in r [:decision :chosen :candidate])))
      (is (not-any? :chosen (:clicks x)))
      (is (= [{:absent :no-dispatch-configured}] (mapv :enactment (:enactments x))))))
  (doseq [path (:paths (nth live-records-read 2))]
    (is (= [{:absent :no-decision}] (mapv :enactment (:enactments (:flight (w/read-record path))))) path)))
