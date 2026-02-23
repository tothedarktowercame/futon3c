(ns futon3c.reflection.envelope
  "ReflectionEnvelope — structured metadata for a Clojure var.

   Every strategic claim about runtime behavior or implementation status
   should resolve to a concrete Clojure target. This schema defines the
   shape of that resolution.

   Derived from: M-self-representing-stack §Reflection-Bottomed Strategic Scholia"
  (:require [malli.core :as m])
  (:import [java.time Instant]))

;; =============================================================================
;; Schema
;; =============================================================================

(def ReflectionEnvelope
  "Structured reflection metadata for a single Clojure var.
   Produced by reflect-var, consumed by Arxana strategic scholia."
  [:map
   [:reflection/ns :symbol]
   [:reflection/symbol :symbol]
   [:reflection/file {:optional true} :string]
   [:reflection/line {:optional true} :int]
   [:reflection/arglists {:optional true} :any]
   [:reflection/doc {:optional true} [:maybe :string]]
   [:reflection/resolved-at inst?]
   [:reflection/private? {:optional true} :boolean]
   [:reflection/macro? {:optional true} :boolean]
   [:reflection/protocol? {:optional true} :boolean]
   [:reflection/dynamic? {:optional true} :boolean]
   [:reflection/tag {:optional true} :any]])

;; =============================================================================
;; Constructor
;; =============================================================================

(defn ->envelope
  "Construct a ReflectionEnvelope from a resolved var.

   Returns a validated envelope map, or {:error ...} if the var cannot be
   resolved or the resulting envelope fails validation."
  [ns-sym var-sym var-obj]
  (if-not var-obj
    {:error (str "Cannot resolve var: " ns-sym "/" var-sym)}
    (let [m (meta var-obj)
          envelope (cond-> {:reflection/ns ns-sym
                            :reflection/symbol var-sym
                            :reflection/resolved-at (Instant/now)
                            :reflection/private? (boolean (:private m))
                            :reflection/macro? (boolean (:macro m))
                            :reflection/dynamic? (boolean
                                                  (or (:dynamic m)
                                                      (when (instance? clojure.lang.Var var-obj)
                                                        (.isDynamic ^clojure.lang.Var var-obj))))}
                     (:file m)      (assoc :reflection/file (:file m))
                     (:line m)      (assoc :reflection/line (:line m))
                     (:arglists m)  (assoc :reflection/arglists (:arglists m))
                     (contains? m :doc) (assoc :reflection/doc (:doc m))
                     (:tag m)       (assoc :reflection/tag (:tag m)))]
      (if (m/validate ReflectionEnvelope envelope)
        envelope
        {:error (str "Envelope validation failed for " ns-sym "/" var-sym)
         :explain (m/explain ReflectionEnvelope envelope)}))))

(defn valid?
  "Check if a map is a valid ReflectionEnvelope."
  [m]
  (m/validate ReflectionEnvelope m))
