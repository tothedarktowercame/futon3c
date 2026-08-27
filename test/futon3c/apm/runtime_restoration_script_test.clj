(ns futon3c.apm.runtime-restoration-script-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(deftest restoration-is-canonical-complete-and-recovering
  (let [script (slurp "scripts/restore-apm-runtime.sh")]
    (is (str/includes? script "git branch --show-current"))
    (is (str/includes? script "git status --porcelain -- src/futon3c/apm"))
    ;; The script formerly reloaded via a loop variable named `n`; it now emits
    ;; a `(require '<ns> :reload)` per concrete namespace, each preceded by a
    ;; resource-path check against the canonical checkout. Assert the property
    ;; — reload, never load-file — rather than the former literal.
    (is (str/includes? script ":reload"))
    (is (not (str/includes? script "(load-file")))
    (is (str/includes? script "runtime source mismatch"))
    (is (not (str/includes? script "load-file")))
    (is (str/includes? script "recover-all!"))
    (is (str/includes? script ":restored"))
    (is (str/includes? script ":mismatched"))
    (is (str/includes? script ":readopted"))))
