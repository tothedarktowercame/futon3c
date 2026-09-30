(ns futon3c.test-registry-pytest-test
  "Pytest runner support in the test registry (requisition 2026-09-30).
  Real processes in a temp git repo: a module, a data file the test opens by
  path, and one test file; then the planted-change checks (c: module edit,
  data edit) name the changed path, an unrelated file stays current, and a
  failing test still registers a record (not a warrant) with outcomes."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.test-registry :as registry])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private pytest-python
  (delay (or (when (.isFile (io/file "/tmp/mfs-venv/bin/python"))
              "/tmp/mfs-venv/bin/python")
             (let [sys (io/file "/usr/bin/python3")]
               (when (.isFile sys)
                 (let [r (shell/sh (str sys) "-c" "import pytest")]
                   (when (zero? (:exit r)) (str sys))))))))

(defn- sh [root & argv]
  (let [r (apply shell/sh (concat argv [:dir root]))]
    (assert (zero? (:exit r)) (pr-str r))))

(deftest pytest-command-shape-is-validated-exactly
  (let [python "/tmp/mfs-venv/bin/python"
        reason (fn [command]
                 (try (registry/validate-command! command)
                      (catch Exception e (:reason (ex-data e)))))]
    (is (nil? (registry/validate-command! [python "-m" "pytest" "test/test_a.py"])))
    (is (= :invalid-pytest-command (reason [python "-m" "pytest"])))
    (is (= :invalid-pytest-command (reason [python "-m" "pytest" "test/"])))
    (is (= :invalid-pytest-command (reason [python "-m" "pytest" "/abs/test_a.py"])))
    (is (= :invalid-pytest-command (reason [python "-m" "pytest" "test/a.py" "-k" "x"])))
    ;; A bare `pytest` is not an absolute python path, so it falls to the
    ;; Clojure clause and is refused as :explicit-namespace-required.
    (is (= :explicit-namespace-required (reason ["pytest" "-m" "pytest" "test/a.py"])))))

(deftest parse-pytest-results-reads-the-final-summary-and-outcomes
  (let [log (str "PASSED test/x.py::test_one\n"
                 "FAILED test/x.py::test_two - assert 1 == 2\n"
                 "=================================== FAILURES ====================================\n"
                 "1 failed, 2 passed, 1 skipped in 0.05s\n")]
    (let [r (registry/parse-pytest-results 1 log 77)]
      (is (= 1 (:exit r))) (is (= 4 (:tests r)))
      (is (= 2 (:passed r))) (is (= 1 (:failures r)))
      (is (= 0 (:errors r))) (is (= 1 (:skipped r)))
      (is (= 77 (:duration-ms r)))
      (is (= 2 (count (:outcomes r))))
      (is (some #(str/starts-with? % "FAILED test/x.py::test_two") (:outcomes r))))
    (is (false? (registry/pytest-successful? (registry/parse-pytest-results 1 log 77))))
    (let [green (str "PASSED test/x.py::test_one\n"
                     "============================= 3 passed in 0.02s =============================\n")
          r (registry/parse-pytest-results 0 green 20)]
      (is (= 3 (:tests r)))
      (is (zero? (:failures r)))
      (is (true? (registry/pytest-successful? r))))
    (let [r (registry/parse-pytest-results 0 "no summary here\n" 5)]
      (is (= :unparsed (get-in r [:tests :reason]))))))

(deftest ^:slow pytest-register-check-roundtrip-with-planted-changes
  (when-let [python @pytest-python]
    (let [dir (.toFile (Files/createTempDirectory "registry-pytest-" (make-array FileAttribute 0)))
          root (str dir)
          backend (atom {:entries {} :order []})
          check (fn [run changed]
                  (registry/check-record! backend {:entry-id (:evidence/id run)
                                                   :repo-root root :changed-paths changed}))]
      (try
        (let [write (fn [path text] (let [f (io/file dir path)] (io/make-parents f) (spit f text)))]
          (write "src/mymod.py" "VALUE = 1\n")
          (write "data/data.txt" "v1\n")
          (write "unrelated/notes.md" "n1\n")
          (write "test/test_thing.py"
                 (str "import pathlib\n"
                      "import mymod\n\n"
                      "def test_one():\n"
                      "    assert mymod.VALUE == 1\n"
                      "    assert pathlib.Path('data/data.txt').read_text().strip() == 'v1'\n"))
          (write "conftest.py"
                 "import sys, os\nsys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))\n")
          (sh root "git" "init" "-q") (sh root "git" "add" ".")
          (sh root "git" "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" "fixture")
          (let [run (registry/register-run!
                     backend {:repo-root root :command [python "-m" "pytest" "test/test_thing.py"]
                              :code-paths ["conftest.py"]
                              :test-paths ["test/test_thing.py"]
                              :author "author" :artifact-dir (str root "/.artifacts")
                              :ledger-root (str root "/.ledger")})
                payload (:payload run)
                paths (set (map :path (:load-closure payload)))]
            ;; a. register: closure carries the module AND the opened data file.
            (is (true? (:warrant? payload)) (pr-str (dissoc payload :env-fingerprint)))
            (is (contains? paths "src/mymod.py") (pr-str paths))
            (is (contains? paths "data/data.txt") (pr-str paths))
            (is (contains? paths "test/test_thing.py") (pr-str paths))
            (is (map? (get-in payload [:env-fingerprint :toolchain])))
            (is (= 1 (get-in payload [:results :tests])))
            (is (true? (registry/pytest-successful? (:results payload))))
            ;; b. nothing changed -> current.
            (is (true? (:warrant? (check run []))))
            ;; c. PLANTED module edit -> stale, naming the path.
            (write "src/mymod.py" "VALUE = 2\n")
            (let [r (check run ["src/mymod.py"])]
              (is (= :environment-mismatch (:reason r)) (pr-str r))
              (is (= ["src/mymod.py"] (get-in r [:details :changed-files]))))
            ;; c'. PLANTED data-file edit -> stale, naming the path.
            (write "src/mymod.py" "VALUE = 1\n")
            (write "data/data.txt" "v2\n")
            (let [r (check run ["data/data.txt"])]
              (is (= :environment-mismatch (:reason r)) (pr-str r))
              (is (= ["data/data.txt"] (get-in r [:details :changed-files]))))
            ;; d. unrelated file change -> still current.
            (write "data/data.txt" "v1\n")
            (write "unrelated/notes.md" "n2\n")
            (let [r (check run ["unrelated/notes.md"])]
              (is (true? (:warrant? r)) (pr-str r))
              (is (= ["unrelated/notes.md"] (:outside-closure r))))
            (write "data/data.txt" "v1\n")
            ;; e. a failing test still registers a record, not a warrant, with
            ;; recoverable outcomes.
            (write "test/test_thing.py"
                   (str "import pathlib\n"
                        "import mymod\n\n"
                        "def test_one():\n"
                        "    assert mymod.VALUE == 1\n"
                        "    assert pathlib.Path('data/data.txt').read_text().strip() == 'v1'\n\n"
                        "def test_failing():\n"
                        "    assert mymod.VALUE == 99\n"))
            (sh root "git" "add" ".")
            (sh root "git" "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" "failing")
            (let [bad (registry/register-run!
                       backend {:repo-root root :command [python "-m" "pytest" "test/test_thing.py"]
                                :code-paths ["conftest.py"]
                                :test-paths ["test/test_thing.py"]
                                :author "author" :artifact-dir (str root "/.artifacts")
                                :ledger-root (str root "/.ledger")})
                  bad-payload (:payload bad)]
              (is (map? bad-payload) "a failing run still appends a record")
              (is (not (true? (:warrant? bad-payload))))
              (is (= 2 (get-in bad-payload [:results :tests])))
              (is (= 1 (get-in bad-payload [:results :failures])))
              (is (some #(str/starts-with? % "FAILED test/test_thing.py::test_failing")
                        (:outcomes (get bad-payload :results)))
                  (pr-str (:outcomes (get bad-payload :results))))
              ;; check-record! still refuses a non-warrant, before any
              ;; closure/env comparison — the case a careless refactor of
              ;; check-record! into a mode would break.
              (is (= :not-a-warrant (:reason (check bad []))))
              ;; check-currency!: the failing record can still be shown
              ;; UNCHANGED, returning its recorded results/outcomes.
              (let [currency (registry/check-currency!
                              backend {:entry-id (:evidence/id bad) :repo-root root
                                       :changed-paths []})]
                (is (true? (:current? currency)) (pr-str currency))
                (is (= 1 (get-in currency [:results :failures])))
                (is (some #(str/starts-with? % "FAILED test/test_thing.py::test_failing")
                          (:outcomes (:results currency)))))
              ;; PLANTED: edit the module the failing test imports -> stale,
              ;; naming the path; revert, edit an unrelated file -> current.
              (write "src/mymod.py" "VALUE = 3\n")
              (let [r (registry/check-currency!
                       backend {:entry-id (:evidence/id bad) :repo-root root
                                :changed-paths ["src/mymod.py"]})]
                (is (= :environment-mismatch (:reason r)) (pr-str r))
                (is (= ["src/mymod.py"] (get-in r [:details :changed-files]))))
              (write "src/mymod.py" "VALUE = 1\n")
              (write "unrelated/notes.md" "n3\n")
              (let [r (registry/check-currency!
                       backend {:entry-id (:evidence/id bad) :repo-root root
                                :changed-paths ["unrelated/notes.md"]})]
                (is (true? (:current? r)) (pr-str r))
                (is (= ["unrelated/notes.md"] (:outside-closure r)))))
            ;; A run killed before pytest's session end (OOM, os._exit) has
            ;; no load closure; with nothing pinned it must not read current.
            (write "test/test_killed.py"
                   "import os\nimport mymod\n\ndef test_killed():\n    os._exit(137)\n")
            (sh root "git" "add" ".")
            (sh root "git" "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" "killed")
            (let [killed (registry/register-run!
                          backend {:repo-root root :command [python "-m" "pytest" "test/test_killed.py"]
                                   :code-paths ["conftest.py"]
                                   :test-paths ["test/test_killed.py"]
                                   :author "author" :artifact-dir (str root "/.artifacts")
                                   :ledger-root (str root "/.ledger")})
                  r (registry/check-currency!
                     backend {:entry-id (:evidence/id killed) :repo-root root :changed-paths []})]
              (is (not (true? (:current? r))) (pr-str r))
              (is (= :no-load-closure (:reason r)) (pr-str r))
              ;; and check-record! refuses it as a non-warrant, not a crash.
              (is (= :not-a-warrant (:reason (check killed [])))))))
        (finally
          (doseq [f (reverse (file-seq dir))] (io/delete-file f true)))))))

(deftest ^:slow out-of-repo-closure-inputs-are-pinned-by-content
  ;; claude-4 requisition 2026-09-30: a closure entry whose path resolves
  ;; (through symlinks) OUTSIDE the repo is pinned by its content sha and
  ;; exempt from committed-scope, like ~/.m2 jars; in-repo entries —
  ;; including a symlink to another in-repo file — keep today's rule.
  (when-let [python @pytest-python]
    (let [dir (.toFile (Files/createTempDirectory "registry-ext-" (make-array FileAttribute 0)))
          ext (.toFile (Files/createTempDirectory "registry-ext-out-" (make-array FileAttribute 0)))
          root (str dir)
          backend (atom {:entries {} :order []})
          check (fn [f run changed]
                  (f backend {:entry-id (:evidence/id run) :repo-root root :changed-paths changed}))]
      (try
        (let [write (fn [d path text] (let [f (io/file d path)] (io/make-parents f) (spit f text)))]
          (write dir "src/mymod.py" "VALUE = 1\n")
          (write dir "data/data.txt" "v1\n")
          ;; the external sibling: OUTSIDE the repo, reached through a symlink
          (write ext "target.txt" "ext-v1\n")
          (Files/createSymbolicLink (.toPath (io/file dir "linked"))
                                    (.toPath ext) (make-array FileAttribute 0))
          ;; a symlink INSIDE the repo to another in-repo file: in-repo rule
          (.mkdirs (io/file dir "inrepo"))
          (Files/createSymbolicLink (.toPath (io/file dir "inrepo/alias.txt"))
                                    (.toPath (io/file dir "data/data.txt"))
                                    (make-array FileAttribute 0))
          (write dir "test/test_ext.py"
                 (str "import pathlib\n"
                      "import mymod\n\n"
                      "def test_one():\n"
                      "    assert mymod.VALUE == 1\n"
                      "    assert pathlib.Path('linked/target.txt').read_text().strip() == 'ext-v1'\n"
                      "    assert pathlib.Path('inrepo/alias.txt').read_text().strip() == 'v1'\n"))
          (write dir "conftest.py"
                 "import sys, os\nsys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))\n")
          (sh root "git" "init" "-q") (sh root "git" "add" ".")
          (sh root "git" "-c" "user.email=t@t" "-c" "user.name=t" "commit" "-qm" "fixture")
          (let [opts {:repo-root root :command [python "-m" "pytest" "test/test_ext.py"]
                      :code-paths ["conftest.py"] :test-paths ["test/test_ext.py"]
                      :author "author" :artifact-dir (str root "/.artifacts")
                      :ledger-root (str root "/.ledger")}
                run (registry/register-run! backend opts)
                payload (:payload run)
                paths (set (map :path (:load-closure payload)))]
            ;; a. register -> warrant, external path listed as exempted.
            (is (true? (:warrant? payload)) (pr-str (select-keys payload [:warrant? :postcheck])))
            (is (= ["linked/target.txt"] (:external-closure-paths payload)) (pr-str (:external-closure-paths payload)))
            (is (contains? paths "linked/target.txt") (pr-str paths))
            ;; d. the in-repo symlink is NOT exempt (resolves back inside).
            (is (contains? paths "inrepo/alias.txt") (pr-str paths))
            (is (not (some #(= % "inrepo/alias.txt") (:external-closure-paths payload))))
            ;; b. PLANTED: edit the EXTERNAL target -> both checks stale,
            ;; naming the LINK path (the sha is re-hashed through it).
            (write ext "target.txt" "ext-v2\n")
            (let [r (check registry/check-record! run [])
                  c (check registry/check-currency! run [])]
              (is (= :environment-mismatch (:reason r)) (pr-str r))
              (is (= ["linked/target.txt"] (get-in r [:details :changed-files])))
              (is (= :environment-mismatch (:reason c)) (pr-str c))
              (is (= ["linked/target.txt"] (get-in c [:details :changed-files]))))
            (write ext "target.txt" "ext-v1\n")
            (is (true? (:warrant? (check registry/check-record! run []))))
            ;; c. PLANTED: an uncommitted IN-repo closure file still refuses
            ;; the warrant — the case a too-broad exemption breaks.
            (write dir "src/mymod.py" "VALUE = 1  # uncommitted edit\n")
            (let [bad (registry/register-run! backend opts)
                  bad-payload (:payload bad)]
              (is (not (true? (:warrant? bad-payload))))
              (is (= :scope-not-committed (:reason (:postcheck bad-payload))) (pr-str (:postcheck bad-payload)))
              (is (contains? (set (get-in bad-payload [:postcheck :details :paths])) "src/mymod.py")))
            (write dir "src/mymod.py" "VALUE = 1\n")
            (is (true? (:warrant? (:payload (registry/register-run! backend opts)))))))
        (finally
          (doseq [f (reverse (file-seq dir))] (io/delete-file f true))
          (doseq [f (reverse (file-seq ext))] (io/delete-file f true)))))))
