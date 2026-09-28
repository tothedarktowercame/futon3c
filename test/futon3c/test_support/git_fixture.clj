(ns futon3c.test-support.git-fixture
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]))

(def routing-environment-keys
  ["GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_COMMON_DIR"])

(defn environment []
  (apply dissoc (into {} (System/getenv)) routing-environment-keys))

(defn git-result [repo & args]
  (apply shell/sh
         (concat ["git" "-C" (str repo)] args [:env (environment)])))

(defn git! [repo & args]
  (let [result (apply git-result repo args)]
    (when-not (zero? (:exit result))
      (throw (ex-info "Git fixture command failed"
                      (assoc result :repo (str repo) :args args))))
    (str/trim (:out result))))
