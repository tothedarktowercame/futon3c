(ns futon3c.apm.toolchain-port
  "Single producer of typed Lean elaboration observations."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.authority-port :as authority-port]))

(def blocking-warning-kinds #{:error-class})

(defn diagnostic-kind [line]
  (cond
    (str/includes? line "declaration uses `sorry`") :sorry
    (or (str/includes? line "has been deprecated")
        (str/includes? line "deprecated:")) :deprecation
    (or (str/includes? line "try 'simp'")
        (str/includes? line "linter")) :linter
    (str/includes? line "warning:") :compiler-warning
    :else nil))

(defn classify-output [exit output]
  (let [lines (str/split-lines (str output))
        errors (filterv #(str/includes? % "error:") lines)
        warnings (keep (fn [line]
                         (when-let [kind (diagnostic-kind line)]
                           {:kind kind :msg line}))
                       lines)
        sorry-warnings (count (filter #(= :sorry (:kind %)) warnings))
        other-warnings (vec (remove #(= :sorry (:kind %)) warnings))]
    {:exit exit :errors (count errors) :warnings (count warnings)
     :sorry-warnings sorry-warnings
     :other-warnings other-warnings
     :blocking-warnings (count (filter #(contains? blocking-warning-kinds
                                                  (:kind %))
                                       other-warnings))
     :output (str output)}))

(defn acceptable-preflight? [observation]
  (and (= 0 (:exit observation)) (= 0 (:errors observation))
       (pos-int? (:sorry-warnings observation))
       (zero? (:blocking-warnings observation))))

(defn elaborate
  ([authority problem-path]
   (elaborate authority problem-path
              (fn [workspace path]
                (apply shell/sh ["lake" "env" "lean" path :dir workspace]))))
  ([authority problem-path run-fn]
   (let [resolved (authority-port/require-path authority :problem-file
                                               problem-path)]
     (if-not (:ok resolved)
       resolved
       (let [workspace (:workspace authority)
             relative (str (.relativize
                            (java.nio.file.Path/of workspace
                                                   (make-array String 0))
                            (java.nio.file.Path/of (:path resolved)
                                                   (make-array String 0))))
             result (run-fn workspace relative)]
         (assoc (classify-output (:exit result)
                                 (str (:out result) (:err result)))
                :ok true :problem-path (:path resolved)))))))
