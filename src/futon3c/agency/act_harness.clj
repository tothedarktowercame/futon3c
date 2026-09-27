(ns futon3c.agency.act-harness
  "Closed execution-harness stamps for minted-act command-line producers."
  (:require [clojure.string :as str]))

(def kinds #{:war-machine :zai :none :unknown})
(def allowed-keys #{:kind :basis :execution-id :reason :source-ref})

(defn- text? [value]
  (and (string? value) (not (str/blank? value))))

(defn validate!
  "Return HARNESS or throw the same typed reasons as futon1b's evidence contract."
  [harness]
  (let [reason
        (cond
          (not (map? harness)) :invalid-harness-map
          (not-every? allowed-keys (keys harness)) :unexpected-harness-key
          (not (contains? kinds (:kind harness))) :unknown-harness-kind
          (not= :producer-context (:basis harness)) :invalid-harness-basis
          (= :zai (:kind harness)) :zai-harness-not-deployed
          (and (= :war-machine (:kind harness))
               (not (text? (:execution-id harness)))) :missing-execution-id
          (and (= :unknown (:kind harness))
               (not (text? (:reason harness)))) :missing-harness-reason
          (some #(and (contains? harness %) (not (text? (get harness %))))
                [:execution-id :reason :source-ref]) :invalid-harness-string)]
    (when reason
      (throw (ex-info "Invalid act execution harness"
                      {:reason reason :field :act/harness})))
    harness))

(defn plain [source-ref]
  (validate! {:kind :none :basis :producer-context :source-ref source-ref}))

(defn parse-cli
  "Parse the shared act CLI flags and return write/file/harness values."
  [args source-ref usage]
  (loop [remaining (seq args) result {:write? false}]
    (if-let [arg (first remaining)]
      (cond
        (= "--write" arg)
        (if (:write? result)
          (throw (ex-info usage {:reason :duplicate-option :option arg}))
          (recur (next remaining) (assoc result :write? true)))

        (= "--harness-kind" arg)
        (if-let [value (second remaining)]
          (recur (nnext remaining) (assoc result :harness-kind value))
          (throw (ex-info usage {:reason :missing-option-value :option arg})))

        (= "--harness-execution-id" arg)
        (if-let [value (second remaining)]
          (recur (nnext remaining) (assoc result :harness-execution-id value))
          (throw (ex-info usage {:reason :missing-option-value :option arg})))

        (str/starts-with? arg "--")
        (throw (ex-info usage {:reason :unknown-option :option arg}))

        (:file result)
        (throw (ex-info usage {:reason :too-many-files}))

        :else (recur (next remaining) (assoc result :file arg)))
      (let [{:keys [file harness-kind harness-execution-id]} result]
        (when-not file (throw (ex-info usage {:reason :missing-file})))
        (let [harness (if (or harness-kind harness-execution-id)
                        (validate! (cond-> {:kind (some-> harness-kind keyword)
                                           :basis :producer-context
                                           :source-ref source-ref}
                                    harness-execution-id
                                    (assoc :execution-id harness-execution-id)))
                        (plain source-ref))]
          (assoc result :harness harness))))))
