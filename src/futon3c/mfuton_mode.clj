(ns futon3c.mfuton-mode
  "Thin mfuton mode helpers. Keep mfuton-specific detection and transport-side
   compatibility helpers out of the generic futon owners."
  (:require [clojure.string :as str]))

(def ^:private mfuton-mode-env "FUTON3C_MFUTON_MODE")
(def ^:private default-mfuton-mode "futon")

(defn mfuton-mode
  []
  (or (System/getenv mfuton-mode-env) default-mfuton-mode))

(defn mfuton-mode?
  []
  (= "mfuton" (mfuton-mode)))

(def ^:private frontiermath-local-artifact-ref-re
  #"(?ix)
    \bmfuton/data/frontiermath-local/FM-\d{3}/[^\s\]\[)>,;\"']+/?")

(defn first-frontiermath-local-artifact-ref
  [text]
  (when (string? text)
    (some-> (re-find frontiermath-local-artifact-ref-re text)
            (str/replace #"[.,:;]+$" "")
            str/trim
            not-empty)))
