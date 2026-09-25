(ns futon3c.exemplar.proof2a-wc-check-test
  "The W_c half of holes/labs/M-futon-seams/exemplar/proof2a_check.clj, run as
  the bb script it is (`--wc` exits after W_c), so its outcome can be
  registered. The script re-runs futon2.aif.grain-gate (loaded from futon2's
  checkout in the bb process) on click-001-enactment.edn; the records are read
  as committed and never rewritten."
  (:require [clojure.edn]
            [clojure.java.shell :as sh]
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

;; --wc --edn (M-wm-wiring step 11): the verdict as one EDN form, the form the
;; flight reads; --edn-cases the control and each bad case, agreeing with the
;; text mode's lines
(defn- run-edn [flag]
  (sh/sh "bb" (str dir "/proof2a_check.clj") (str dir "/click-001.edn")
         (str dir "/click-001-enactment.edn") "--wc" flag))

(deftest wc-edn-gives-the-verdict-as-data
  (let [{:keys [exit out]} (run-edn "--edn")
        v (clojure.edn/read-string out)]
    (is (zero? exit))
    (is (= ["W_c: no successful attempt names a G_c pass (X_c(d)): the grain attempt's check is not grain-gate"] v))
    (is (= 1 (count (str/split-lines (str/trim out)))) "one form, nothing else on stdout")))

(deftest wc-edn-cases-agree-with-the-text-mode
  (let [{:keys [out]} (run-edn "--edn-cases")
        {:keys [verdict control unverifiable cases]} (clojure.edn/read-string out)
        text (:out (run-wc))]
    (is (= [] control) "the positive control passes")
    (is (= {:status :join-unverifiable :failures []} (select-keys unverifiable [:status :failures])))
    (is (= 7 (count cases)))
    (is (every? (fn [[_ v]] (and (vector? v) (seq v))) cases) "every bad case fails, as data")
    (doseq [[label _] cases]
      (is (str/includes? text (str "caught  " label)) label))
    (is (every? #(str/includes? text %) verdict) "the text mode lists the same failures")))

;; The tick's run-record shape (WM-PRESPIKE-I): check-c reads the candidate's
;; derivation from [:decision :selection-certificate :candidates n :id] when
;; the hand key is absent. Fixtures: a run record reduced from a real one
;; (provenance in its header) and an enactment in enact-fn's shape.
(def machine-record "test/futon3c/exemplar/fixtures/tick-run-record-reduced@a4b4fc78.edn")
(def machine-enactment "test/futon3c/exemplar/fixtures/enactment-machine@a4b4fc78.edn")

(defn- verdict [rec-path enact-path]
  (let [{:keys [exit out err]} (sh/sh "bb" (str dir "/proof2a_check.clj") rec-path enact-path "--wc" "--edn")]
    (is (zero? exit) err)
    (clojure.edn/read-string out)))

(defn- variant [path f]
  (let [tmp (java.io.File/createTempFile "wc-variant" ".edn")]
    (spit tmp (pr-str (f (clojure.edn/read-string (slurp path)))))
    (str tmp)))

(deftest the-tick-run-record-shape-gives-a-verdict
  (is (= [] (verdict machine-record machine-enactment)) "a consistent pair passes")
  (is (= ["W_c: enactment candidate :C1 differs from the click's selected candidate :C9"]
         (verdict (variant machine-record #(assoc-in % [:decision :selection-law :candidate] :C9))
                  machine-enactment))
      "the candidate mismatch fails")
  (let [v (verdict (variant machine-record #(update % :decision dissoc :selection-certificate)) machine-enactment)]
    (is (= :join-unverifiable (:status v)))
    (is (str/includes? (:reason v) "carries neither")))
  (is (= ["W_c: the run record's precedence names patterns with no interpretation receipt: [:apparatus/one-authority-per-question]"]
         (verdict (variant machine-record
                           #(update-in % [:decision :selection-certificate :candidates 0 :id]
                                       dissoc :interpretation-receipts))
                  machine-enactment))
      "the bad case names the pattern, not a nil"))
