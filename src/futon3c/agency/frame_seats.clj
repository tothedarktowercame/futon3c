(ns futon3c.agency.frame-seats
  "Deterministic, invoke-ready Agency seats for one APM frame."
  (:require [clojure.string :as str]
            [futon3c.agency.registry :as registry]))

(def ^:private seat-specs
  [[:reg/solver-seat "solver" :codex nil]
   [:reg/student-seat "student" :zai :mathematics]
   [:reg/guide-seat "guide" :claude nil]
   [:reg/proctor-seat "proctor" :codex nil]
   [:reg/promotion-proctor-seat "promotion-proctor" :codex nil]
   [:reg/scribe-seat "scribe" :codex nil]
   [:reg/zai-scribe-seat "zai-scribe" :zai nil]
   [:reg/analyst-seat "analyst" :claude nil]])

(def ^:private accepted-seat-suffixes
  (set (map second seat-specs)))

(def ^:private accepted-agent-types
  #{:claude :codex :zai})

(def ^:private accepted-override-keys
  #{:type :model})

(def ^:private mint-lock (Object.))

(defn seat-map
  "Return the deterministic registration-shaped seat map for FRAME-ID."
  [frame-id]
  (into {}
        (map (fn [[registration-key suffix _agent-type _memory-domain]]
               [registration-key (str frame-id "-" suffix)]))
        seat-specs))

(defn- readiness [agent-id]
  (get-in (registry/registry-status) [:agents agent-id]))

(defn- mint-one!
  [prepare-seat-fn frame-id
   [registration-key suffix agent-type memory-domain model]]
  (let [agent-id (str frame-id "-" suffix)]
    (when-not (registry/get-agent agent-id)
      (let [{:keys [invoke-fn session-reset-fn metadata] :as prepared}
            (prepare-seat-fn (cond-> {:agent-id agent-id :agent-type agent-type}
                               model (assoc :model model)
                               memory-domain (assoc :memory-domain memory-domain)))]
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
                                   (when (some? model) {:model model})
                                   metadata)})]
            (when (and (map? registered) (= false (:ok registered)))
              {:finding :seat-registration-failed
               :seat registration-key
               :agent-id agent-id
               :agent-type agent-type
               :detail registered})))))))

(defn- normalize-cast [cast]
  (when (map? cast)
    (into {} (map (fn [[seat override]] [(name seat) override])) cast)))

(defn- cast-findings [cast]
  (let [cast (normalize-cast cast)]
    (cond
      (nil? cast)
      [{:finding :invalid-seat-cast
        :accepted-seats (vec (sort accepted-seat-suffixes))}]

      :else
      (into []
            (mapcat
             (fn [[seat override]]
               (cond
                 (not (contains? accepted-seat-suffixes seat))
                 [{:finding :unknown-seat
                   :seat seat
                   :accepted-seats (vec (sort accepted-seat-suffixes))}]

                 (not (map? override))
                 [{:finding :invalid-seat-override :seat seat}]

                 :else
                 (let [unknown-keys (remove accepted-override-keys (keys override))
                       raw-type (:type override)]
                   (cond
                     ;; A misspelled override key must not be accepted and
                     ;; dropped. Same shape as the ?tag= filter and ?df=:
                     ;; {:guide {:tpye "zai"}} would otherwise return 200 and
                     ;; silently mint the default Claude guide.
                     (seq unknown-keys)
                     [{:finding :unknown-override-key
                       :seat seat
                       :keys (vec (sort (map name unknown-keys)))
                       :accepted-keys (vec (sort (map name accepted-override-keys)))}]

                     ;; keyword returns nil for a non-string/symbol, so a
                     ;; numeric or boolean type would read as "no override".
                     (and (some? raw-type)
                          (not (or (string? raw-type) (keyword? raw-type))))
                     [{:finding :invalid-agent-type
                       :seat seat
                       :agent-type (pr-str raw-type)
                       :accepted-types (vec (sort (map name accepted-agent-types)))}]

                     :else
                     (let [agent-type (some-> raw-type keyword)]
                       (when (and (some? agent-type)
                                  (not (contains? accepted-agent-types agent-type)))
                         [{:finding :unknown-agent-type
                           :seat seat
                           :agent-type (name agent-type)
                           :accepted-types (vec (sort (map name accepted-agent-types)))}]))))))
             cast)))))

(defn- effective-seat-specs [model cast]
  (let [cast (or (normalize-cast cast) {})]
    (mapv (fn [[registration-key suffix agent-type memory-domain]]
            (let [override (get cast suffix)
                  effective-type (or (some-> (:type override) keyword) agent-type)
                  effective-model (if (some? (:model override))
                                    (:model override)
                                    model)]
              [registration-key suffix effective-type memory-domain effective-model]))
          seat-specs)))

(defn- effective-casting [specs]
  (into {}
        (map (fn [[_registration-key suffix agent-type _memory-domain model]]
               [suffix (cond-> {:agent-type agent-type}
                         (some? model) (assoc :model model))]))
        specs))

(defn mint-seats!
  "Register seven fresh, locally invocable identities for FRAME-ID.

   PREPARE-SEAT-FN receives {:agent-id :agent-type} and must return an
   :invoke-fn plus optional :session-reset-fn and :metadata.  Keeping the
   execution adapter injectable lets the HTTP boundary use the same local
   Claude/Codex/ZAI constructors as ordinary Agency registration.

   Existing deterministic identities make the operation idempotent.  A seat
   map is returned only when every identity is roster-visible and invoke-ready."
  [{:keys [prepare-seat-fn model cast]} frame-id]
  (let [frame-id (some-> frame-id str str/trim not-empty)
        cast-provided? (some? cast)
        cast-errors (when cast-provided? (cast-findings cast))]
    (cond
      (nil? frame-id)
      {:ok false
       :error :missing-frame-id
       :findings [{:finding :missing-frame-id}]}

      (not (fn? prepare-seat-fn))
      {:ok false
       :error :missing-seat-preparer
       :findings [{:finding :missing-seat-preparer}]}

      (seq cast-errors)
      {:ok false
       :error :invalid-seat-cast
       :findings cast-errors}

      :else
      (locking mint-lock
        (let [specs (effective-seat-specs model cast)
              seats (seat-map frame-id)
              findings
              (into [] (keep (partial mint-one! prepare-seat-fn frame-id)) specs)
              readiness-findings
              (->> specs
                   (keep (fn [[registration-key suffix agent-type _memory-domain
                               _model]]
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
             :seats seats
             :casting (effective-casting specs)}))))))

(defn mint-analyst!
  "Register the fresh, locally invocable Analyst identity for TENURE.

   An existing identity is returned without calling PREPARE-SEAT-FN. This is
   the idempotency boundary: re-minting an active tenure never resets or
   deletes its Analyst session."
  [{:keys [prepare-seat-fn model]} tenure]
  (cond
    (not (and (integer? tenure) (pos? tenure)))
    {:ok false
     :error :invalid-tenure
     :findings [{:finding :invalid-tenure :tenure tenure}]}

    (not (fn? prepare-seat-fn))
    {:ok false
     :error :missing-seat-preparer
     :findings [{:finding :missing-seat-preparer}]}

    :else
    (locking mint-lock
      (let [agent-id (str "analyst-" tenure)
            existing (registry/get-agent agent-id)
            finding
            (cond
              (and existing (not= :claude (:agent/type existing)))
              {:finding :seat-type-mismatch
               :agent-id agent-id
               :expected-type :claude
               :actual-type (:agent/type existing)}

              existing
              nil

              :else
              (let [{:keys [invoke-fn session-reset-fn metadata] :as prepared}
                    (prepare-seat-fn (cond-> {:agent-id agent-id :agent-type :claude}
                                       model (assoc :model model)))]
                (if-not (fn? invoke-fn)
                  {:finding :seat-not-invoke-ready
                   :agent-id agent-id
                   :agent-type :claude
                   :detail (dissoc prepared :invoke-fn :session-reset-fn)}
                  (let [registered
                        (registry/register-agent!
                         {:agent-id {:id/value agent-id :id/type :continuity}
                          :type :claude
                          :invoke-fn invoke-fn
                          :session-reset-fn session-reset-fn
                          :capabilities [:explore :edit :test :coordination/execute]
                          :metadata (merge {:analyst-tenure tenure
                                            :fresh-session? true}
                                           metadata)})]
                    (when (and (map? registered) (= false (:ok registered)))
                      {:finding :seat-registration-failed
                       :agent-id agent-id
                       :agent-type :claude
                       :detail registered})))))
            info (readiness agent-id)
            readiness-finding
            (when (and (nil? finding) (not (true? (:invoke-ready? info))))
              {:finding :seat-not-invoke-ready
               :agent-id agent-id
               :agent-type :claude
               :diagnostic (:invoke-diagnostic info)})
            findings (cond-> [] finding (conj finding)
                                      readiness-finding (conj readiness-finding))]
        (if (seq findings)
          {:ok false
           :error :analyst-mint-incomplete
           :tenure tenure
           :findings findings}
          {:ok true
           :tenure tenure
           :analyst-seat agent-id})))))
