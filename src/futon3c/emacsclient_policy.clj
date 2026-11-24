(ns futon3c.emacsclient-policy
  "One process-wide authority for spawning emacsclient."
  (:require [clojure.string :as str]))

(defn enabled-value?
  [value]
  (not (contains? #{"1" "true" "yes" "on"}
                  (some-> value str str/trim str/lower-case))))

(defn enabled?
  []
  (enabled-value? (System/getenv "FUTON3C_DISABLE_EMACSCLIENT")))
