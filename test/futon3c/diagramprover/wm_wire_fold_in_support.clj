(ns futon3c.diagramprover.wm-wire-fold-in-support
  "Real judge and producers, with external input readers isolated. The older
  r9/run-tick fixture throws instead of invoking judge, so it cannot witness
  these reads. All trace persistence is confined to a temporary directory."
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [futon2.report.war-machine :as wm]
            [futon2.report.cascade-decision-test :as fixture]
            [futon2.aif.locator-fixtures :as loc]
            [futon2.aif.belief :as belief]
            [futon2.aif.mission-registry :as mr]
            [futon2.aif.morning-brief :as brief]
            [futon2.aif.anticipation :as anticipation]
            [futon2.aif.policy-depth :as depth]
            [futon2.aif.sorry-registry :as sorry]
            [futon2.aif.ticket-queue :as tq]
            [futon2.aif.trace :as trace]
            [futon3c.diagramprover.wm-wire :as w]))

(def events [{:event-id "qa-1" :entity-id "known" :type :strengthened :weight 1.0}])
(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "Contains enactment-fold, but no mu-post, driver, horizon-steps or morning-brief: no fold-input/produced-judge pair."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn"
    :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
    :why "No fold-input/produced-judge pair: no mu-post, driver, horizon-steps or morning-brief."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn"
    :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
    :why "Refused click; no mu-post, driver, horizon-steps or morning-brief."}])

(defn assert-live-pins []
  (doseq [{:keys [path sha256]} live-records-read]
    (assert (= sha256 (w/sha256-file path)))
    (let [r (w/read-record path)]
      (assert (not-any? #(and (map? %) (some (partial contains? %) [:mu-post :driver :horizon-steps :morning-brief]))
                        (tree-seq coll? seq r))))))

(defn observe [kind mutation]
  (let [root (w/tmp-dir "fold-input-")
        fresh (belief/initial-belief-state ["known"])
        carried (:belief (wm/apply-morning-brief-events fresh #{} events))
        alternative (:belief (wm/apply-morning-brief-events fresh #{}
                                                          [(assoc (first events) :type :foreclosed)]))
        written (atom []) fold wm/apply-morning-brief-events anticipate depth/anticipation
        aggregate belief/r3d-aggregate-driver
        opts (merge fixture/live-c-opts
                    {:trace-dir root :step-portfolio? false :eval-invariant-fallback? false
                     :cascade-sources (loc/locate-all fixture/tick-1-sources)
                     :cascade-proposals-dir root :repair-obligations-root root
                     :machine-interpretations-dir root :ticket-queue tq/empty-declaration
                     :policy-depth {:anticipation 3 :cascade-rollout 3}})]
    (try
      (let [persisted (when (= kind :carry)
                        (trace/write-trace! {:run/id "previous-offline" :belief carried}
                                            :dir root :date-str "2026-09-26" :return-record? true))
            _ (when persisted
                (swap! written conj (get-in persisted [:record :mu-post]))
                (let [record (edn/read-string (slurp (:path persisted)))
                      changed (case mutation :none record
                                :absent (dissoc record :mu-post)
                                :different (assoc record :mu-post alternative))]
                  (spit (:path persisted) (str (pr-str changed) "\n"))))
            run (fn []
                  (binding [belief/*carry-belief?* true]
                    (wm/judge (if (= kind :driver) {:annotation-graph {:health 0.9}} {}) opts)))
            result
            (try
              (with-redefs [mr/load-missions (constantly {:missions []})
                            mr/load-tickets (constantly {:tickets []})
                            sorry/open-sorrys (constantly [])
                            belief/section-ids-from-stack-annotations (constantly ["known"])
                            brief/unseen-belief-events (constantly (if (= kind :brief) events []))
                            anticipation/anticipation-snapshot
                            (constantly {:events-loaded? true :events [{:event/id "offline"}]})
                            wm/apply-morning-brief-events
                            (fn [& args]
                              (let [r (apply fold args)]
                                (if (= kind :brief)
                                  (do (swap! written conj (:belief r))
                                      (assoc r :belief (case mutation :none (:belief r)
                                                        :absent {:absent :not-carried}
                                                        :different alternative))) r)))
                            depth/anticipation
                            (fn [& args]
                              (let [r (apply anticipate args)]
                                (if (= kind :horizon)
                                  (do (swap! written conj (:horizon-steps r))
                                      (case mutation :none r
                                        :absent (anticipate {:events-loaded? false} (second args))
                                        :different (update r :horizon-steps inc))) r)))
                            belief/r3d-aggregate-driver
                            (fn [& args]
                              (let [r (apply aggregate args)]
                                (if (= kind :driver)
                                  (do (swap! written conj (:driver r))
                                      (case mutation :none r :absent (aggregate {:annotation-health {:status :absent :reason :not-carried}})
                                        :different (update r :driver -))) r)))]
                (run))
              (catch Exception e {:wire-error {:class (.getName (class e)) :message (ex-message e)}}))
            reader (case kind :brief (:belief result) :horizon (:policy-depth-used result)
                         :driver (get-in result [:micro-step-trace 0 :aggregated-signed-error])
                         :carry (:belief-pre result))]
        {:writer (first @written) :reader reader :result result :fresh fresh
         :carrier (when persisted (:path persisted))})
      (finally (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f))))))
