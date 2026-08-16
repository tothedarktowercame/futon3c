(ns futon3c.agency.frame-seats
  "Deterministic, invoke-ready Agency seats for one APM frame."
  (:require [clojure.string :as str]
            [futon3c.agency.registry :as registry]))

(def ^:private seat-specs
  [[:reg/solver-seat "solver" :codex]
   [:reg/student-seat "student" :zai]
   [:reg/guide-seat "guide" :claude]
   [:reg/proctor-seat "proctor" :codex]
   [:reg/scribe-seat "scribe" :codex]])

(def ^:private mint-lock (Object.))

(defn seat-map
  "Return the deterministic registration-shaped seat map for FRAME-ID."
  [frame-id]
  (into {}
        (map (fn [[registration-key suffix _agent-type]]
               [registration-key (str frame-id "-" suffix)]))
        seat-specs))

(defn- readiness [agent-id]
  (get-in (registry/registry-status) [:agents agent-id]))

(defn- mint-one!
  [prepare-seat-fn frame-id [registration-key suffix agent-type]]
  (let [agent-id (str frame-id "-" suffix)]
    (when-not (registry/get-agent agent-id)
      (let [{:keys [invoke-fn session-reset-fn metadata] :as prepared}
            (prepare-seat-fn {:agent-id agent-id :agent-type agent-type})]
        (if-not (fn? invoke-fn)
          {:finding :seat-not-invoke-ready
           :seat registration-key
           :agent-id agent-id
           :agent-type agent-type
           :detail (dissoc prepared :invoke-fn :session-reset-fn)}
          (let [registered
                (registry/register-agent!
                 {:agent-id {:id/value agent-id :id/type :continuity}
                  :type agent-type
                  :invoke-fn invoke-fn
                  :session-reset-fn session-reset-fn
                  :capabilities [:explore :edit :test :coordination/execute]
                  :metadata (merge {:frame-id frame-id
                                    :frame-seat registration-key
                                    :fresh-session? true}
                                   metadata)})]
            (when (and (map? registered) (= false (:ok registered)))
              {:finding :seat-registration-failed
               :seat registration-key
               :agent-id agent-id
               :agent-type agent-type
               :detail registered})))))))

(defn mint-seats!
  "Register five fresh, locally invocable identities for FRAME-ID.

   PREPARE-SEAT-FN receives {:agent-id :agent-type} and must return an
   :invoke-fn plus optional :session-reset-fn and :metadata.  Keeping the
   execution adapter injectable lets the HTTP boundary use the same local
   Claude/Codex/ZAI constructors as ordinary Agency registration.

   Existing deterministic identities make the operation idempotent.  A seat
   map is returned only when every identity is roster-visible and invoke-ready."
  [{:keys [prepare-seat-fn]} frame-id]
  (let [frame-id (some-> frame-id str str/trim not-empty)]
    (cond
      (nil? frame-id)
      {:ok false
       :error :missing-frame-id
       :findings [{:finding :missing-frame-id}]}

      (not (fn? prepare-seat-fn))
      {:ok false
       :error :missing-seat-preparer
       :findings [{:finding :missing-seat-preparer}]}

      :else
      (locking mint-lock
        (let [seats (seat-map frame-id)
              findings
              (into [] (keep (partial mint-one! prepare-seat-fn frame-id)) seat-specs)
              readiness-findings
              (->> seat-specs
                   (keep (fn [[registration-key suffix agent-type]]
                           (let [agent-id (str frame-id "-" suffix)
                                 agent (registry/get-agent agent-id)
                                 info (readiness agent-id)]
                             (cond
                               (and agent (not= agent-type (:agent/type agent)))
                               {:finding :seat-type-mismatch
                                :seat registration-key
                                :agent-id agent-id
                                :expected-type agent-type
                                :actual-type (:agent/type agent)}

                               (not (true? (:invoke-ready? info)))
                               {:finding :seat-not-invoke-ready
                                :seat registration-key
                                :agent-id agent-id
                                :agent-type agent-type
                                :diagnostic (:invoke-diagnostic info)}))))
                   vec)
              all-findings (into findings readiness-findings)]
          (if (seq all-findings)
            {:ok false
             :error :seat-mint-incomplete
             :frame-id frame-id
             :findings all-findings}
            {:ok true
             :frame-id frame-id
             :seats seats}))))))
