(ns futon3c.diagramprover.wm-wire-r7-fold-call-enactment-fold-test
  "Wire [:r7-fold-call :habit-fold-call-test :enactment-fold]: the fold the
  tick's judge hands selection reaching the fold call's own test.

  The writer is war-machine/judge (WM-HABIT-FOLD-CALL-I): at each selection
  it folds the flights' increment receipts (enactment-fold-source/
  fold-from-flights over <machine-interpretations-dir>/flights/*.edn) and
  passes the fold to select-and-record-cascade! as :enactment-fold. The
  reader is futon2/test/futon2/report/habit_fold_call_test.clj (the
  :box/kind :test box), whose read is the joint selection's habit-read
  receipt: the :state it records as consumed (cascade-habit-store/
  attach-state), and the selection law's :e-source.

  The hermetic witness is the reader's own run: judge over a temp store
  with one flight record carrying one increment receipt, the options judge
  hands select-and-record-cascade! captured, then the real
  select-and-record-cascade! over the cascade-decision fixture's tick-1
  family with the captured :enactment-fold. {:writer the fold judge
  handed, :reader the habit-read receipt's consumed :state}.

  No live record carries both ends (live-records-read, pinned and read):
  the live tick run records predate WM-HABIT-FOLD-CALL-I, so every habit
  read on them is {:status :absent :reason :no-enactment-fold}. So the
  wire is WITNESSED-HERMETICALLY."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.cascade-problems :as problems]
            [futon2.aif.enactment-habit :as eh]
            [futon2.aif.locator-fixtures :as locators]
            [futon2.aif.mission-registry :as mr]
            [futon2.aif.scoring-input-receipts :as receipts]
            [futon2.aif.ticket-queue :as ticket-queue]
            [futon2.report.cascade-decision-test :as fixture]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire :as w]))

(def pkey [:pattern-cascade "M-t" [:p/a] {}])

(defn- receipt [click]
  (eh/increment {:click click :candidate :cas/a :attempts [{:pattern :p/a}]}
                pkey []))

(defn- write-flight! [store id receipts]
  (let [f (io/file store "flights" (str id ".edn"))]
    (io/make-parents f)
    (spit f (pr-str {:plan {} :flight {:flight/id id
                                       :enactments (mapv (fn [r] {:click-id (first (:record-id r))
                                                                  :wc {:verdict []} :increment r})
                                                         receipts)}}))
    (str f)))

(defn- judge-opts
  "The options judge hands select-and-record-cascade!, over STORE."
  [store]
  (let [captured (atom nil)
        tmp (w/tmp-dir "wire-fold-judge")]
    (with-redefs-fn {#'mr/load-missions (fn [& _] {:missions []})
                     #'mr/load-tickets (fn [& _] {:tickets []})
                     #'wm/select-and-record-cascade!
                     (fn [_ opts] (reset! captured opts) (throw (ex-info "stop" {::stop true})))}
      #(try (wm/judge {} {:cascade-sources-dir tmp :cascade-proposals-dir tmp
                          :repair-obligations-root tmp
                          :machine-interpretations-dir store
                          :ticket-queue ticket-queue/empty-declaration})
            (catch clojure.lang.ExceptionInfo e
              (when-not (::stop (ex-data e)) (throw e)))))
    @captured))

(defn- habit-read
  "The joint selection's habit-read receipt when the real
  select-and-record-cascade! runs over the tick-1 family with FOLD as
  :enactment-fold (:none: no key, the pre-fix production state)."
  [fold]
  (let [reads (atom [])
        assembled (problems/assemble {:targets [fixture/tick-1-target]
                                      :sources (locators/locate-all fixture/tick-1-sources)})
        opts (cond-> (merge fixture/live-c-opts
                            {:cascade-habit-path (str (io/file (w/tmp-dir "wire-fold-habit") "absent.edn"))})
               (not= :none fold) (assoc :enactment-fold fold))]
    (binding [receipts/*habit-reads* reads]
      (wm/select-and-record-cascade! assembled opts))
    (:receipt (first (filter #(= :joint-selection (:purpose %)) @reads)))))

(defn observe
  "judge over a store with one flight record; the writer's fold and the
  reader's consumed :state. SUPPLIED :judge passes judge's fold on (the
  wire), :none passes no fold (the typed absence), or a fold value passes
  that instead (a different value than the writer's)."
  [supplied]
  (let [store (w/tmp-dir "wire-fold-store")
        _ (write-flight! store "flight-a" [(receipt "click-1")])
        opts (judge-opts store)
        written (:enactment-fold opts)
        fold (case supplied :judge written :none :none supplied)
        read (habit-read fold)]
    {:writer written
     :reader (if (w/typed-absence? read) read (:state read))}))

(defn check [] (observe :judge))

(def live-records-read
  [{:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn")
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "predates WM-HABIT-FOLD-CALL-I: both habit reads {:status :absent :reason :no-enactment-fold}; judge passed no fold live"}])

(def wire
  {:wire [:r7-fold-call :habit-fold-call-test :enactment-fold]
   :kind :witnessed-hermetically
   :test `the-fold-judge-hands-reaches-the-selection
   :check check
   :live-records-read live-records-read})

(deftest the-fold-judge-hands-reaches-the-selection
  (let [o (check)]
    (is (= 1 (count (:enactment-records (:writer o)))))
    (is (= 1 (get-in (:reader o) [:samples])))
    (is (some? (get-in (:reader o) [:folded-from])) "the consumed state names what was read")
    (is (w/received? o))))

(deftest no-fold-passed-is-a-typed-absence-and-fails-the-wire
  ;; the production state before WM-HABIT-FOLD-CALL-I
  (let [o (observe :none)]
    (is (= {:status :absent :reason :no-enactment-fold}
           (select-keys (:reader o) [:status :reason])))
    (is (not (w/received? o)))))

(deftest a-different-fold-than-the-writers-fails-the-wire
  (let [other (eh/fold nil [(receipt "click-9") (receipt "click-8")])
        o (observe other)]
    (is (= 2 (count (:enactment-records (:reader o)))))
    (is (not (w/received? o)) "present, not absent, but not the value the writer wrote")))

(deftest the-live-records-carry-no-fold
  (doseq [{:keys [path sha256]} live-records-read]
    (is (= sha256 (w/sha256-file path)) path)
    (let [occ (get-in (w/read-record path) [:habit-reads :occurrences])]
      (is (seq occ))
      (is (every? #(= :no-enactment-fold (get-in % [:receipt :reason])) occ) path))))
