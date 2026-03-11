(ns futon3c.peripheral.tools-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.tools :as tools]))

(def scoped-spec
  {:peripheral/id :edit
   :peripheral/tools #{:read :write :edit :grep :musn-log}
   :peripheral/scope {:paths ["src/" "test/"]}})

(deftest in-scope-file-tools-only-check-path-arg
  (testing ":edit only treats the first arg as a path"
    (is (true? (tools/in-scope? :edit
                                ["src/futon3c/core.clj" "old text" "new text"]
                                scoped-spec)))
    (is (false? (tools/in-scope? :edit
                                 ["resources/outside.clj" "old text" "new text"]
                                 scoped-spec))))
  (testing ":write only treats the first arg as a path"
    (is (true? (tools/in-scope? :write
                                ["test/futon3c/core_test.clj" "(ns demo)"]
                                scoped-spec)))
    (is (false? (tools/in-scope? :write
                                 ["resources/generated.edn" "{:ok true}"]
                                 scoped-spec)))))

(deftest in-scope-grep-uses-second-arg-as-path
  (testing ":grep uses the target path rather than the pattern string"
    (is (true? (tools/in-scope? :grep
                                ["session-id" "src/futon3c"]
                                scoped-spec)))
    (is (false? (tools/in-scope? :grep
                                 ["session-id" "resources/"]
                                 scoped-spec)))))

(deftest in-scope-fallback-checks-all-string-args
  (testing "unknown tools conservatively validate every string arg"
    (is (true? (tools/in-scope? :unknown-tool
                                ["src/futon3c" "test/futon3c"]
                                scoped-spec)))
    (is (false? (tools/in-scope? :unknown-tool
                                 ["src/futon3c" "resources/"]
                                 scoped-spec)))))
