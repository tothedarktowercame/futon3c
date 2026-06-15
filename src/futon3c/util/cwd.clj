(ns futon3c.util.cwd
  "Resolve an agent-supplied working directory for process launch.

   Agent `:cwd` metadata can carry a literal tilde (e.g. \"~/\"). Java's
   `ProcessBuilder`/`java.io.File` does NOT shell-expand `~`, so calling
   `.directory` with it throws \"No such file or directory\" and the launch
   fails (e.g. the kangaroo warm pouch silently falling back to cold). Expand a
   leading `~` to the user's home at every launch site so this can't happen.

   A blank value or a bare \"~\" returns nil — the caller should then OMIT
   `.directory`, leaving the JVM's own working directory in effect."
  (:require [clojure.string :as str]))

(defn resolve-cwd
  "Return CWD with a leading ~ / ~/ expanded to the user's home, the string
   unchanged if it's already a normal path, or nil for blank / bare \"~\"."
  [cwd]
  (let [s (some-> cwd str str/trim)]
    (cond
      (or (str/blank? s) (= s "~")) nil
      (str/starts-with? s "~/") (str (System/getProperty "user.home") (subs s 1))
      :else s)))
