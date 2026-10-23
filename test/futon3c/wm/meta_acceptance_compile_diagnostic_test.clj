(ns futon3c.wm.meta-acceptance-compile-diagnostic-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.meta-self-heal-adapters :as sut]))

(def fixture "test/resources/wm/cfbcbad0-compile-error.log")
;; SHA of the retained live acceptance log, copied byte-for-byte.
(def fixture-sha "708cf3d6707cbf32910f2c1ae866d9aefa36373f8042b217e231ee9cbd8f1b2c")

(defn result [output]
  (binding [sut/*acceptance-runs-root* "/home/joe/notes/M2f-1-test-runs"]
    (#'sut/write-run-result "compile-diagnostic" nil output :failed 1)))

(deftest retained-live-compiler-error
  (let [output (slurp fixture)
        r (result output)]
    (is (= fixture-sha (#'sut/hex-sha256
                       (java.nio.file.Files/readAllBytes (.toPath (io/file fixture))))))
    (is (= {:file "futon2/aif/meta_acceptance_injury_cfbcbad0_test.clj"
            :line 92 :col 3 :message "No such var: falsifier/want-check-refusal"}
           (:compile-diagnostic r)))
    (is (= {:status :ran :outcome :failed :exit 1}
           (select-keys r [:status :outcome :exit])))
    (is (= (#'sut/acceptance-summary output) (:summary r)))
    (is (= fixture-sha (get-in r [:run-pin :sha256])))
    (is (= output (slurp (get-in r [:run-pin :path]))))))

(deftest only-compiler-header-lines-classify
  (doseq [output ["FAIL in (ordinary-test)\nexpected: true\n  actual: false\nRan 1 tests containing 1 assertions.\n1 failures, 0 errors."
                  "Process exited 1\nNo such var: example/nope"
                  "quoted: Syntax error compiling at (x.clj:1:2).\nNo such var: x/y"]]
    (is (not (contains? (result output) :compile-diagnostic))))
  (is (= {:file "some/path.clj" :line 12 :col 9 :message " bad form"}
         (:compile-diagnostic
          (result "Syntax error (IllegalArgumentException) compiling at (some/path.clj:12:9).\n\n bad form\n"))))
  (is (= 2000 (count (:message (:compile-diagnostic
                               (result (str "Syntax error compiling at (x.clj:1:2).\n"
                                            (apply str (repeat 2001 "x"))))))))))

(deftest reader-and-macro-syntax-headers
  (let [path "test/resources/wm/cfbcbad0-reader-error.log"
        sha "8e92c61b34b7039ba2fb78b8cfb3f64a047f367b783efc39d9513917b8b00b85"]
    (is (= sha (#'sut/hex-sha256
                (java.nio.file.Files/readAllBytes (.toPath (io/file path)))))))
  (is (= {:file "futon2/aif/meta_reviewer_falsifier_injury_acceptance_test.clj"
          :line 91 :col 39 :message "Unmatched delimiter: ]"}
         (:compile-diagnostic (result (slurp "test/resources/wm/cfbcbad0-reader-error.log")))))
  ;; Actual Clojure 1.11.1 run of (let [x] x), not an invented header.
  (is (= {:file "/home/joe/notes/M2f-3-malformed-let.clj" :line 2 :col 1
          :message "[x] - failed: even-number-of-forms? at: [:bindings] spec: :clojure.core.specs.alpha/bindings"}
         (:compile-diagnostic (result (slurp "test/resources/wm/m2f3-macroexpansion.log")))))
  ;; clojure.main/ex-str also emits this form when :symbol is absent.
  (is (= {:file "x.clj" :line 1 :col 2 :message "invalid macro form"}
         (:compile-diagnostic (result "Syntax error macroexpanding at (x.clj:1:2).\ninvalid macro form"))))
  (is (not (contains? (result "quoted: Syntax error reading source at (x.clj:1:2).\nUnmatched delimiter: ]")
                      :compile-diagnostic))))
