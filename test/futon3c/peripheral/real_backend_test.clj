(ns futon3c.peripheral.real-backend-test
  "Tests for RealBackend — actual tool execution through peripheral constraints.

   These tests use real file I/O in a temp directory. The key validation:
   tools execute against the real filesystem, but the peripheral spec's
   structural enforcement still governs what's allowed."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.peripheral.real-backend :as rb]
            [futon3c.peripheral.tools :as tools]
            [futon3c.peripheral.explore :as explore]
            [futon3c.peripheral.edit :as edit]
            [futon3c.peripheral.test-runner :as test-runner]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.common :as common]
            [futon3c.evidence.store :as estore])
  (:import [java.io File]
           [java.util UUID]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defn- temp-dir! []
  (let [d (File. "/tmp" (str "futon3c-rb-test-" (UUID/randomUUID)))]
    (.mkdirs d)
    d))

(defn- cleanup! [^File dir]
  (doseq [f (reverse (file-seq dir))]
    (.delete ^File f)))

(defn- write-test-file! [^File dir path content]
  (let [f (io/file dir path)]
    (.mkdirs (.getParentFile f))
    (spit f content)
    (.getPath f)))

(defmacro with-temp-dir [sym & body]
  `(let [~sym (temp-dir!)]
     (try ~@body
       (finally (cleanup! ~sym)))))

;; =============================================================================
;; 1. Read tool — real file contents
;; =============================================================================

(deftest read-tool-reads-real-file
  (testing ":read tool returns actual file contents"
    (with-temp-dir dir
      (let [path (write-test-file! dir "hello.txt" "line one\nline two\nline three")
            backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :read [path])]
        (is (true? (:ok result)))
        (is (str/includes? (:result result) "line one"))
        (is (str/includes? (:result result) "line three"))))))

(deftest read-tool-with-offset-and-limit
  (testing ":read tool respects offset and limit"
    (with-temp-dir dir
      (let [path (write-test-file! dir "lines.txt" "a\nb\nc\nd\ne")
            backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :read [path {:offset 1 :limit 2}])]
        (is (true? (:ok result)))
        (is (= "b\nc" (:result result)))))))

(deftest read-tool-missing-file-returns-error
  (testing ":read tool returns error for missing file"
    (with-temp-dir dir
      (let [backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :read ["/nonexistent/file.txt"])]
        (is (false? (:ok result)))
        (is (str/includes? (:error result) "not found"))))))

;; =============================================================================
;; 2. Glob tool — real file matching
;; =============================================================================

(deftest glob-tool-finds-matching-files
  (testing ":glob tool finds files by pattern"
    (with-temp-dir dir
      (write-test-file! dir "src/foo.clj" "(ns foo)")
      (write-test-file! dir "src/bar.clj" "(ns bar)")
      (write-test-file! dir "test/baz.txt" "test")
      (let [backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :glob ["**/*.clj" (.getPath dir)])]
        (is (true? (:ok result)))
        (is (= 2 (count (:result result))))
        (is (every? #(str/ends-with? % ".clj") (:result result)))))))

;; =============================================================================
;; 3. Grep tool — real content search
;; =============================================================================

(deftest grep-tool-finds-matches
  (testing ":grep tool searches file contents"
    (with-temp-dir dir
      (write-test-file! dir "a.clj" "(ns a)\n(defn greet [] \"hello\")")
      (write-test-file! dir "b.clj" "(ns b)\n(defn farewell [] \"goodbye\")")
      (let [backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :grep ["defn" (.getPath dir)])]
        (is (true? (:ok result)))
        (is (= 2 (count (:result result))))
        (is (every? #(str/includes? (:content %) "defn") (:result result)))))))

;; =============================================================================
;; 4. Edit tool — real file modification
;; =============================================================================

(deftest edit-tool-replaces-in-file
  (testing ":edit tool performs find/replace on real file"
    (with-temp-dir dir
      (let [path (write-test-file! dir "src/code.clj" "(defn old-name [] :ok)")
            backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :edit [path "old-name" "new-name"])]
        (is (true? (:ok result)))
        (is (= "(defn new-name [] :ok)" (slurp path)))))))

(deftest edit-tool-missing-string-returns-error
  (testing ":edit tool returns error when old-string not found"
    (with-temp-dir dir
      (let [path (write-test-file! dir "src/code.clj" "(defn foo [] :ok)")
            backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :edit [path "nonexistent" "replacement"])]
        (is (false? (:ok result)))
        (is (str/includes? (:error result) "not found"))))))

;; =============================================================================
;; 5. Write tool — real file creation
;; =============================================================================

(deftest write-tool-creates-file
  (testing ":write tool creates a new file with content"
    (with-temp-dir dir
      (let [path (str (.getPath dir) "/new-file.txt")
            backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :write [path "hello world"])]
        (is (true? (:ok result)))
        (is (= "hello world" (slurp path)))))))

(deftest write-tool-creates-parent-dirs
  (testing ":write tool creates parent directories"
    (with-temp-dir dir
      (let [path (str (.getPath dir) "/deep/nested/dir/file.txt")
            backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :write [path "nested content"])]
        (is (true? (:ok result)))
        (is (= "nested content" (slurp path)))))))

;; =============================================================================
;; 6. Bash tools — real command execution
;; =============================================================================

(deftest bash-tool-executes-command
  (testing ":bash tool runs a real command"
    (with-temp-dir dir
      (let [backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :bash ["echo hello"])]
        (is (true? (:ok result)))
        (is (= "hello\n" (get-in result [:result :out])))
        (is (zero? (get-in result [:result :exit])))))))

(deftest bash-readonly-rejects-destructive-commands
  (testing ":bash-readonly rejects rm, mv, etc."
    (with-temp-dir dir
      (let [backend (rb/make-real-backend {:cwd (.getPath dir)})]
        (doseq [cmd ["rm -rf /" "mv foo bar" "kill -9 1"]]
          (let [result (tools/execute-tool backend :bash-readonly [cmd])]
            (is (false? (:ok result))
                (str "Should reject: " cmd))
            (is (str/includes? (:error result) "destructive"))))))))

(deftest bash-readonly-allows-read-commands
  (testing ":bash-readonly allows ls, cat, wc, etc."
    (with-temp-dir dir
      (write-test-file! dir "test.txt" "hello")
      (let [backend (rb/make-real-backend {:cwd (.getPath dir)})
            result (tools/execute-tool backend :bash-readonly ["ls"])]
        (is (true? (:ok result)))
        (is (str/includes? (get-in result [:result :out]) "test.txt"))))))

;; =============================================================================
;; 7. Musn-log tool — evidence store access
;; =============================================================================

(deftest musn-log-reads-evidence-by-session
  (testing ":musn-log tool queries evidence store by session-id"
    (let [store (atom {:entries {} :order []})
          _ (estore/append* store {:evidence/id "e-1"
                                   :evidence/subject {:ref/type :session :ref/id "sess-1"}
                                   :evidence/type :coordination
                                   :evidence/claim-type :goal
                                   :evidence/author "test"
                                   :evidence/at (str (java.time.Instant/now))
                                   :evidence/body {:test true}
                                   :evidence/tags [:test]
                                   :evidence/session-id "sess-1"})
          _ (estore/append* store {:evidence/id "e-2"
                                   :evidence/subject {:ref/type :session :ref/id "sess-2"}
                                   :evidence/type :coordination
                                   :evidence/claim-type :goal
                                   :evidence/author "test"
                                   :evidence/at (str (java.time.Instant/now))
                                   :evidence/body {:test true}
                                   :evidence/tags [:test]
                                   :evidence/session-id "sess-2"})
          backend (rb/make-real-backend {:evidence-store store})
          result (tools/execute-tool backend :musn-log ["sess-1"])]
      (is (true? (:ok result)))
      (is (= 1 (count (:result result))))
      (is (= "e-1" (:evidence/id (first (:result result))))))))

;; =============================================================================
;; 8. Explore peripheral with RealBackend — full lifecycle
;; =============================================================================

(deftest explore-peripheral-with-real-backend
  (testing "explore peripheral lifecycle with real file I/O"
    (with-temp-dir dir
      (write-test-file! dir "src/core.clj" "(ns core)\n(defn main [] :ok)")
      (write-test-file! dir "src/util.clj" "(ns util)\n(defn helper [] :help)")
      (write-test-file! dir "README.md" "# Project\nA test project")
      (let [evidence-store (atom {:entries {} :order []})
            backend (rb/make-real-backend {:cwd (.getPath dir)})
            spec (common/load-spec :explore)
            peripheral (explore/make-explore spec backend)
            ;; Start
            start-result (runner/start peripheral {:session-id "sess-explore"
                                                   :evidence-store evidence-store})
            _ (is (true? (:ok start-result)))
            state (:state start-result)

            ;; Step 1: glob for clj files
            step1 (runner/step peripheral state {:tool :glob
                                                 :args ["**/*.clj" (.getPath dir)]})
            _ (is (true? (:ok step1)))
            _ (is (= 2 (count (get-in step1 [:result]))))

            ;; Step 2: grep for defn
            step2 (runner/step peripheral (:state step1) {:tool :grep
                                                          :args ["defn" (.getPath dir)]})
            _ (is (true? (:ok step2)))
            _ (is (= 2 (count (:result step2))))

            ;; Step 3: read a specific file
            step3 (runner/step peripheral (:state step2)
                    {:tool :read
                     :args [(str (.getPath dir) "/src/core.clj")]})
            _ (is (true? (:ok step3)))
            _ (is (str/includes? (:result step3) "(defn main"))

            ;; Stop
            stop-result (runner/stop peripheral (:state step3) "found-target")]
        (is (true? (:ok stop-result)))
        ;; Fruit should report found files
        (is (seq (get-in stop-result [:fruit :found])))
        ;; Evidence store has entries
        (is (>= (count (:order @evidence-store)) 5)
            (str "Expected 5+ evidence entries (start + 3 steps + stop), got "
                 (count (:order @evidence-store))))))))

;; =============================================================================
;; 9. Edit peripheral with RealBackend — scope enforcement
;; =============================================================================

(deftest edit-peripheral-scope-enforcement-with-real-backend
  (testing "edit peripheral enforces path scope on real files"
    (with-temp-dir dir
      (write-test-file! dir "src/code.clj" "(defn foo [] :old)")
      (write-test-file! dir "secrets/creds.txt" "password=hunter2")
      (let [backend (rb/make-real-backend {:cwd (.getPath dir)})
            ;; Scope paths must be absolute to match the absolute args
            spec (assoc (common/load-spec :edit)
                        :peripheral/scope {:paths [(str (.getPath dir) "/src/")
                                                   (str (.getPath dir) "/docs/")
                                                   (str (.getPath dir) "/scripts/")]})
            peripheral (edit/make-edit spec backend)
            start-result (runner/start peripheral {:session-id "sess-edit"})
            state (:state start-result)

            ;; Edit within scope (src/) — should succeed
            step1 (runner/step peripheral state
                    {:tool :edit
                     :args [(str (.getPath dir) "/src/code.clj") ":old" ":new"]})
            _ (is (true? (:ok step1)))
            _ (is (= "(defn foo [] :new)" (slurp (io/file dir "src/code.clj"))))

            ;; Read is also allowed in edit peripheral
            step2 (runner/step peripheral (:state step1)
                    {:tool :read
                     :args [(str (.getPath dir) "/src/code.clj")]})
            _ (is (true? (:ok step2)))

            ;; Edit outside scope (secrets/) — should be rejected by dispatch
            step3 (runner/step peripheral (:state step2)
                    {:tool :edit
                     :args [(str (.getPath dir) "/secrets/creds.txt") "hunter2" "****"]})
            ;; Should fail — out of scope
            _ (is (contains? step3 :error/code)
                  "editing secrets/ should be rejected as out-of-scope")

            ;; Stop
            stop-result (runner/stop peripheral (:state step2) "tests-pass")]
        (is (true? (:ok stop-result)))
        (is (= 1 (:changes (:fruit stop-result))))
        ;; Secrets file untouched
        (is (= "password=hunter2" (slurp (io/file dir "secrets/creds.txt"))))))))

;; =============================================================================
;; 10. Explore cannot use :edit tool — structural enforcement
;; =============================================================================

(deftest explore-cannot-use-edit-tool
  (testing "explore peripheral structurally prevents :edit tool"
    (with-temp-dir dir
      (write-test-file! dir "src/code.clj" "(defn foo [] :ok)")
      (let [backend (rb/make-real-backend {:cwd (.getPath dir)})
            spec (common/load-spec :explore)
            peripheral (explore/make-explore spec backend)
            start-result (runner/start peripheral {:session-id "sess-ro"})
            ;; Try to edit — should be rejected (not in explore's tool set)
            step (runner/step peripheral (:state start-result)
                   {:tool :edit
                    :args [(str (.getPath dir) "/src/code.clj") "foo" "bar"]})]
        (is (contains? step :error/code))
        (is (= :tool-not-allowed (:error/code step))
            "explore must reject :edit tool")
        ;; File untouched
        (is (= "(defn foo [] :ok)" (slurp (io/file dir "src/code.clj"))))))))

;; =============================================================================
;; 11. Test peripheral with real bash-test execution
;; =============================================================================

(deftest test-peripheral-with-real-bash
  (testing "test peripheral executes real test commands"
    (with-temp-dir dir
      (write-test-file! dir "run-tests.sh" "#!/bin/bash\necho 'Tests: 5 pass, 0 fail'\nexit 0")
      (.setExecutable (io/file dir "run-tests.sh") true)
      (let [backend (rb/make-real-backend {:cwd (.getPath dir)})
            spec (common/load-spec :test)
            peripheral (test-runner/make-test-runner spec backend)
            start-result (runner/start peripheral {:session-id "sess-test"})
            ;; Run test command
            step (runner/step peripheral (:state start-result)
                   {:tool :bash-test
                    :args ["bash run-tests.sh"]})
            _ (is (true? (:ok step)))
            _ (is (str/includes? (get-in step [:result :out]) "5 pass"))
            ;; Stop
            stop-result (runner/stop peripheral (:state step) "pass")]
        (is (true? (:ok stop-result)))))))

;; =============================================================================
;; 12. Reflect peripheral reads evidence via musn-log
;; =============================================================================

(deftest reflect-peripheral-reads-evidence
  (testing "reflect peripheral uses :musn-log to read session evidence"
    (let [evidence-store (atom {:entries {} :order []})
          ;; Pre-populate some evidence
          _ (estore/append* evidence-store
              {:evidence/id "e-reflect-1"
               :evidence/subject {:ref/type :session :ref/id "sess-reflect"}
               :evidence/type :coordination
               :evidence/claim-type :step
               :evidence/author "test"
               :evidence/at (str (java.time.Instant/now))
               :evidence/body {:tool :read :event :step}
               :evidence/tags [:test]
               :evidence/session-id "sess-reflect"})
          backend (rb/make-real-backend {:evidence-store evidence-store})
          spec (common/load-spec :reflect)
          peripheral (futon3c.peripheral.reflect/make-reflect spec backend)
          start-result (runner/start peripheral {:session-id "sess-reflect"
                                                 :evidence-store evidence-store})
          ;; Read session log
          step (runner/step peripheral (:state start-result)
                 {:tool :musn-log
                  :args ["sess-reflect"]})
          _ (is (true? (:ok step)))
          _ (is (>= (count (:result step)) 1)
                "musn-log returns evidence for this session")
          ;; Stop produces PAR
          stop-result (runner/stop peripheral (:state step) "session-close")]
      (is (true? (:ok stop-result)))
      (is (some? (get-in stop-result [:fruit :par]))
          "reflect fruit includes PAR entry"))))
