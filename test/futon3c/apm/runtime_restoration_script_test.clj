(ns futon3c.apm.runtime-restoration-script-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(deftest restoration-is-canonical-complete-and-recovering
  (let [script (slurp "scripts/restore-apm-runtime.sh")]
    (is (str/includes? script "git branch --show-current"))
    (is (str/includes? script "git status --porcelain -- src/futon3c/apm"))
    (is (str/includes? script "require n :reload"))
    (is (str/includes? script "runtime source mismatch"))
    (is (not (str/includes? script "load-file")))
    (is (str/includes? script "recover-all!"))
    (is (str/includes? script ":restored"))
    (is (str/includes? script ":mismatched"))
    (is (str/includes? script ":readopted"))))
