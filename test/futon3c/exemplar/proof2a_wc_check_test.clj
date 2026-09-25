(ns futon3c.exemplar.proof2a-wc-check-test
  "The W_c half of holes/labs/M-futon-seams/exemplar/proof2a_check.clj, run as
  the bb script it is (`--wc` exits after W_c), so its outcome can be
  registered. The script re-runs futon2.aif.grain-gate (loaded from futon2's
  checkout in the bb process) on click-001-enactment.edn; the records are read
  as committed and never rewritten."
  (:require [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def dir "holes/labs/M-futon-seams/exemplar")

(defn- run-wc []
  (sh/sh "bb" (str dir "/proof2a_check.clj") (str dir "/click-001.edn")
         (str dir "/click-001-enactment.edn") "--wc"))

(deftest wc-reads-g-c-and-the-selection-join
  (let [{:keys [exit out err]} (run-wc)
        lines (str/split-lines out)
        line (fn [prefix] (first (filter #(str/includes? % prefix) lines)))]
    (is (zero? exit) (str out err))
    (testing "X_c(d): click-001's recorded enactment fails W_c on its grain attempt"
      (is (str/includes? out "no successful attempt names a G_c pass (X_c(d))"))
      (is (not (re-find #"(?m)^    PASS$" out)) "the recorded enactment no longer passes"))
    (testing "the positive control passes, so each bad case fails for its own condition"
      (is (str/ends-with? (str (line "W_c positive control")) "PASS")))
    (testing "every bad case is caught"
      (doseq [label ["X_c(first)" "X_c(check)" "X_c(untyped)"
                     "X_c(d) the recorded enactment"
                     "X_c(d) G_c at the outcome's provider grain"
                     "X_c(d) G_c recorded as a pass, re-run refuses :arglist-mismatch"
                     "X_c(join) selection law names a DIFFERENT candidate"]]
        (is (str/starts-with? (str/trim (str (line label))) "caught") label)))
    (testing "A5: no candidate id on the selection law reads :join-unverifiable, neither pass nor fail"
      (is (str/starts-with? (str/trim (str (line "selection law with no candidate id"))) "typed")))
    (is (not (str/includes? out "VACUOUS")))))
