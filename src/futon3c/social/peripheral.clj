(ns futon3c.social.peripheral
  "Peripheral specs + hop protocol.

   Loads peripheral definitions, validates peripheral hops (entry/exit),
   and transfers context across boundaries. Pure functions; no side effects."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.social.shapes :as shapes])
  (:import [java.time Instant]))

(defn- now-str []
  (str (Instant/now)))

(defn- social-error
  [code message & {:as context}]
  (cond-> {:error/component :peripheral
           :error/code code
           :error/message message
           :error/at (now-str)}
    (seq context) (assoc :error/context context)))

(defn load-peripherals
  "Load and validate peripheral specs from EDN file.
   Returns {:peripherals {id PeripheralSpec}} or SocialError."
  [path]
  (try
    (let [f (io/file path)]
      (when-not (.exists f)
        (throw (ex-info "file-not-found" {:path path})))
      (let [data (edn/read-string (slurp f))
            per (get data :peripherals)]
        (cond
          (not (map? data))
          (social-error :invalid-edn "Peripheral EDN must be a map" :path path :data data)

          (not (map? per))
          (social-error :invalid-edn "Missing or invalid :peripherals map" :path path :data data)

          :else
          (let [errors (->> per
                            (keep (fn [[pid spec]]
                                    (when-let [err (shapes/validate shapes/PeripheralSpec spec)]
                                      {:peripheral/id pid :error err})))
                            vec)]
            (if (seq errors)
              (social-error :invalid-peripheral-spec
                            "One or more peripheral specs failed validation"
                            :path path
                            :errors errors)
              {:peripherals per})))))
    (catch clojure.lang.ExceptionInfo e
      (let [{:keys [path]} (ex-data e)]
        (social-error :file-not-found "Peripheral EDN file not found" :path (or path path))))
    (catch Exception e
      (social-error :load-failed "Failed to load peripherals EDN" :path path :exception (str e)))))

(defn get-peripheral
  "Look up a peripheral by ID.
   Returns PeripheralSpec or SocialError."
  [peripherals peripheral-id]
  (cond
    (not (map? peripherals))
    (social-error :invalid-peripherals "Peripherals must be a map from load-peripherals" :peripherals peripherals)

    (not (shapes/valid? shapes/PeripheralId peripheral-id))
    (social-error :invalid-peripheral-id "Invalid peripheral id" :peripheral-id peripheral-id)

    :else
    (if-let [spec (get-in peripherals [:peripherals peripheral-id])]
      (if (shapes/valid? shapes/PeripheralSpec spec)
        spec
        (social-error :invalid-peripheral-spec
                      "Peripheral spec did not conform to PeripheralSpec shape"
                      :peripheral-id peripheral-id
                      :validation (or (:error (shapes/validate shapes/PeripheralSpec spec)) {})))
      (social-error :peripheral-not-found "Peripheral not found" :peripheral-id peripheral-id))))

(defn- entry-allows?
  [entry-set current-id user-request?]
  (or (contains? entry-set :from-any)
      (contains? entry-set (keyword (str "from-" (name current-id))))
      (and user-request? (contains? entry-set :user-request))))

(defn- resolve-exit-condition
  "Resolve exit condition from a hop request.
   Requires explicit :hop/exit-condition — no substring inference.
   Priority: 1) :hop/exit-condition (top-level, preferred)
             2) :hop/context {:hop/exit-condition ...} (nested, backwards compat)
   Returns keyword or nil."
  [hop-request]
  (let [top-level (:hop/exit-condition hop-request)
        ctx (:hop/context hop-request)
        nested (when (map? ctx) (:hop/exit-condition ctx))]
    (cond
      (keyword? top-level) top-level
      (keyword? nested) nested
      :else nil)))

(defn validate-hop
  "Validate a hop request: is this transition allowed?
   Checks: source peripheral's exit conditions permit leaving,
   target peripheral's entry conditions permit entering.
   Returns HopResult or SocialError."
  [peripherals current-peripheral-id hop-request]
  (cond
    (not (map? peripherals))
    (social-error :invalid-peripherals "Peripherals must be a map from load-peripherals" :peripherals peripherals)

    (not (shapes/valid? shapes/PeripheralId current-peripheral-id))
    (social-error :invalid-peripheral-id "Invalid current peripheral id" :peripheral-id current-peripheral-id)

    (not (shapes/valid? shapes/HopRequest hop-request))
    (social-error :invalid-hop-request
                  "Invalid HopRequest input"
                  :hop-request hop-request
                  :validation (or (:error (shapes/validate shapes/HopRequest hop-request)) {}))

    :else
    (let [to (:hop/to hop-request)
          from-spec (get-peripheral peripherals current-peripheral-id)
          to-spec (get-peripheral peripherals to)]
      (cond
        (shapes/valid? shapes/SocialError from-spec) from-spec
        (shapes/valid? shapes/SocialError to-spec) to-spec

        :else
        (let [entry (:peripheral/entry to-spec)
              exit (:peripheral/exit from-spec)
              exit-cond (resolve-exit-condition hop-request)
              user-request? (= :user-request exit-cond)
              entry-ok? (and (set? entry) (entry-allows? entry current-peripheral-id user-request?))
              ;; Overrides are only active when the hop is actually a user-request hop.
              entry-override? (or (contains? entry :from-any)
                                  (and user-request? (contains? entry :user-request)))
              exit-ok? (or entry-override?
                           (and (keyword? exit-cond) (contains? exit exit-cond)))]
          (cond
            (not entry-ok?)
            (social-error :hop-not-allowed
                          "Target peripheral entry conditions do not allow this hop"
                          :from current-peripheral-id
                          :to to
                          :entry entry)

            (not exit-ok?)
            (social-error :hop-not-allowed
                          "Source peripheral exit conditions do not allow leaving"
                          :from current-peripheral-id
                          :to to
                          :exit exit
                          :exit-condition exit-cond
                          :reason (:hop/reason hop-request))

            :else
            (let [hop-result (cond-> {:hop/from current-peripheral-id
                                      :hop/to to
                                      :hop/session-id (:hop/session-id hop-request)
                                      :hop/at (now-str)
                                      :hop/success? true}
                               (map? (:hop/context hop-request))
                               (assoc :hop/context (:hop/context hop-request)))]
              (if (shapes/valid? shapes/HopResult hop-result)
                hop-result
                (social-error :invalid-hop-result
                              "Internal error: HopResult did not conform to shape"
                              :hop-result hop-result
                              :validation (or (:error (shapes/validate shapes/HopResult hop-result)) {}))))))))))

(defn transfer-context
  "Build context for the target peripheral.
   Carries :session-id (always) and any keys specified in
   the target peripheral's :peripheral/context map.
   Returns context map."
  [hop-result source-context target-peripheral]
  (let [sid (:hop/session-id hop-result)
        ctx-keys (keys (or (:peripheral/context target-peripheral) {}))]
    (->> ctx-keys
         (reduce (fn [m k]
                   (if (and (not= k :session-id)
                            (map? source-context)
                            (contains? source-context k))
                     (assoc m k (get source-context k))
                     m))
                 {:session-id sid}))))
