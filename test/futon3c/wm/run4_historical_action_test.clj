(ns futon3c.wm.run4-historical-action-test
  "A guard refusing to act on the wrong obligation must not stop the line.

  futon2's repair-class-for files an untyped failure as :machine-failure,
  which mints a stop-line -- and under Joe's rule of 2026-09-24 a stop-line
  stops the machine until it is repaired from outside. On 2026-09-11 this
  guard refused correctly and cost a stop-line that is still open. These
  tests pin the typing that keeps a correct refusal an environmental hold."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.wm.run4-historical-action :as historical])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (str (Files/createTempDirectory "run4-historical" (make-array FileAttribute 0))))

(def ^:private repair-class-for
  (requiring-resolve 'futon2.aif.full-loop-runner/repair-class-for))
(def ^:private outcome-from
  (requiring-resolve 'futon2.aif.full-loop-runner/outcome-from))

(defn- class-of
  "The repair class a throw from this namespace would be filed under, by the
  exact route the runner takes: the failure-kind if typed, else the outcome."
  [^Throwable e]
  (let [data (ex-data e)]
    (repair-class-for (or (:failure-kind data) (outcome-from e)))))

(deftest refusing-with-no-open-stop-line-is-not-itself-a-stop-line
  (let [repair-root (temp-dir)
        verification-root (temp-dir)]
    (.mkdirs (io/file repair-root "findings"))
    (let [e (try (historical/validate-applicable!
                  {:repair-root repair-root
                   :verification-root verification-root
                   :verification-path "v.edn"
                   :verification-sha256 (apply str (repeat 64 "a"))})
                 nil
                 (catch clojure.lang.ExceptionInfo ex ex))]
      (is (some? e) "the guard must refuse when there is no open stop-line")
      (is (= :trigger-ineligible (:failure-kind (ex-data e))))
      (is (= :no-open-stop-line (:failure-detail (ex-data e))))
      (is (= :environmental-hold (class-of e))
          "a refusal with nothing to verify is a hold, never a machine failure"))))

(deftest the-untyped-throw-is-the-bad-case
  (testing "an empty ex-data is filed as a machine failure -- the 2026-09-11 defect"
    ;; This is why the throws in run4_historical_action carry data at all.
    ;; If this assertion ever flips -- if an untyped throw stops being filed
    ;; as :machine-failure -- then the typing above is no longer what keeps
    ;; these guards off the board, and the next person to add a bare
    ;; (ex-info "..." {}) here will not find out from a red test.
    (is (= :machine-failure (class-of (ex-info "untyped guard refusal" {})))))
  (testing "and the typed refusals this namespace throws are held"
    (is (= :environmental-hold
           (class-of (ex-info "x" {:failure-kind :guardrail-refusal}))))
    (is (= :environmental-hold
           (class-of (ex-info "x" {:failure-kind :trigger-ineligible}))))))
