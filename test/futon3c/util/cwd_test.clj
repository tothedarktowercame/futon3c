(ns futon3c.util.cwd-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.util.cwd :as cwd]))

;; Regression for the 2026-06-15 kangaroo warm-pouch failure: an agent :cwd of
;; literal "~/" reached ProcessBuilder.directory un-expanded → "No such file or
;; directory". resolve-cwd must expand the tilde; blank / bare "~" => nil so the
;; caller omits .directory (JVM cwd).

(deftest resolve-cwd-expands-tilde
  (let [home (System/getProperty "user.home")]
    (testing "leading ~/ expands to the user's home (slash preserved)"
      (is (= (str home "/") (cwd/resolve-cwd "~/")))
      (is (= (str home "/code") (cwd/resolve-cwd "~/code")))
      (is (= (str home "/code/futon3c") (cwd/resolve-cwd "~/code/futon3c"))))))

(deftest resolve-cwd-blank-and-bare-tilde-are-nil
  (testing "blank, whitespace, bare ~, and nil all mean no directory"
    (doseq [in [nil "" "   " "~"]]
      (is (nil? (cwd/resolve-cwd in)) (str "expected nil for " (pr-str in))))))

(deftest resolve-cwd-passes-normal-paths-through
  (testing "absolute and relative non-tilde paths are returned trimmed, unchanged"
    (is (= "/abs/path" (cwd/resolve-cwd "/abs/path")))
    (is (= "/abs/path" (cwd/resolve-cwd "  /abs/path  ")))
    (is (= "relative/x" (cwd/resolve-cwd "relative/x")))))
