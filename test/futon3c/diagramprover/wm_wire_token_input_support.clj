(ns futon3c.diagramprover.wm-wire-token-input-support
  "P5 receipt handoffs. Wrappers call the real writer/reader, changing only
   the field at the reader boundary. No flight, click or authority service."
  (:require [futon2.aif.efe :as efe]
            [futon2.aif.token-belief-carry :as carry]
            [futon2.aif.token-belief-carry-test :as fixture]
            [futon2.aif.token-belief-predecessor :as predecessor]
            [futon2.aif.token-initialization-policy :as policy]
            [futon2.report.cascade-decision-test :as decision-fixture]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire :as w]))

(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "V2 token-belief-input and node-evaluation readback exist, but helper intermediate receipts and the rank-state literal are not separately recorded; no witness for the newly scoped writer endpoints."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn"
    :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
    :why "Tick record has no token-belief-input-to-kernel readback pair."}])

(defn live-reader-absent? []
  (every? (fn [{:keys [path sha256]}]
            (let [r (w/read-record path)]
              (and (= sha256 (w/sha256-file path))
                   (not-any? #(and (map? %) (some (set (keys %))
                                                [:legacy-token-belief-input :initialized-token-belief-input :rank-state]))
                             (tree-seq coll? seq r)))))
          live-records-read))

(defn mutate [mode q]
  (case mode :none q :absent {:absent :writer-unavailable} :different {#{} 1}))

(def admission {:status :refused :kind :carry-no-predecessor})
(def inspection (predecessor/inspect-trace nil))

(defn stage [v2?]
  (carry/stage {:value {#{[:A :seed]} 1}}
               [{:target :A :declaration {:facts {:seed true :done false}
                                         :want [:done] :interpretations {}}}]
               nil (cond-> {:occurrence-id "p5-wire"}
                     v2? (assoc :observation-initialization {:A {:policy policy/disabled}}))))

(defn helper-observe [hop mode]
  (let [written (atom nil) consumed (atom nil)
        legacy @#'predecessor/legacy-input-receipt
        initialize @#'predecessor/initialization-input-receipt
        temporal @#'predecessor/consume-temporal
        writer (fn [f args]
                 (let [r (apply f args)]
                   (reset! written (:continuation-belief r))
                   (update r :continuation-belief #(mutate mode %))))
        result
        (if (= hop :legacy-initialization)
          (with-redefs-fn {#'predecessor/legacy-input-receipt
                          (fn [& args] (writer legacy args))}
            #(let [r (initialize (stage false) inspection admission nil)]
               (reset! consumed (:continuation-belief r)) r))
          (with-redefs-fn {#'predecessor/initialization-input-receipt
                          (fn [& args] (writer initialize args))
                          #'predecessor/consume-temporal
                          (fn [& args]
                            (let [r (apply temporal args)]
                              (reset! consumed (:continuation-belief r)) r))}
            #(predecessor/input-receipt (stage true) inspection admission nil)))]
    {:writer @written :reader @consumed :product result}))

(defn incoming [ranked]
  ;; This is produced by the real kernel, not an echo of the wrapper's arg.
  (some #(get-in % [:certificate :node-evaluations 0 :incoming-belief])
        (when (sequential? ranked) ranked)))

(defn decision-observe [hop mode]
  (let [written (atom nil) consumed (atom nil) read-calls (atom 0)
        temporal @#'predecessor/consume-temporal
        dispatch efe/rank-actions kernel efe/rank-cascade-actions
        assembled (update (fixture/assembled) :problems
                          (fn [ps] (mapv #(assoc-in % [:cascade-problem :token-initialization]
                                                   {:policy policy/disabled}) ps)))
        result
        (with-redefs-fn
          {#'predecessor/production-authority (fn [& _] admission)
           #'predecessor/consume-temporal
           (fn [& args]
             (let [r (apply temporal args)]
               (if (= hop :temporal-decision)
                 (do (reset! written (:continuation-belief r))
                     (update r :continuation-belief #(mutate mode %))) r)))
           #'efe/rank-actions
           (fn [state actions opts]
             (if-not (:prediction-context opts) (dispatch state actions opts)
               (let [_ (when (#{:decision-dispatch :decision-kernel} hop)
                         (reset! written (:cascade-belief state)))
                     state (if (= hop :decision-dispatch)
                             (update state :cascade-belief #(mutate mode %)) state)
                     r (dispatch state actions opts)]
                 (when (= hop :decision-dispatch)
                   (swap! read-calls inc) (reset! consumed (incoming r))) r)))
           #'efe/rank-cascade-actions
           (fn [state actions opts]
             (if-not (:prediction-context opts) (kernel state actions opts)
               (let [state (if (= hop :decision-kernel)
                             (update state :cascade-belief #(mutate mode %)) state)
                     r (kernel state actions opts)]
                 (when (= hop :decision-kernel)
                   (swap! read-calls inc) (reset! consumed (incoming r))) r)))}
          #(try
             (let [r (wm/cascade-decision
                      assembled (assoc decision-fixture/live-c-opts
                                       :cascade-habit-path fixture/absent-habit-path
                                       :token-belief-context {:occurrence-id "p5-wire"}))]
               (when (= hop :temporal-decision)
                 (swap! read-calls inc)
                 (reset! consumed
                         (some (fn [trace] (get-in trace [:evaluations 0 :incoming-belief]))
                               (get-in r [:decision :selection-certificate :node-evaluation-traces]))))
               r)
             (catch Exception e
               {:exception (.getName (class e)) :detail (ex-data e)})))]
    {:writer @written :reader @consumed :reader-calls @read-calls :product result}))

(defn observe [hop mode]
  (if (#{:legacy-initialization :initialization-temporal} hop)
    (helper-observe hop mode)
    (decision-observe hop mode)))
