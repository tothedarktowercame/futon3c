(ns futon3c.peripheral.night-shift-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.night-shift :as ns]
            [futon3c.peripheral.night-shift-backend :as nsb]
            [futon3c.peripheral.runner :as runner]))

(defn- sh!
  [& argv]
  (let [{:keys [exit err] :as result} (apply shell/sh argv)]
    (when-not (zero? exit)
      (throw (ex-info "command failed" {:argv argv :result result :stderr err})))
    result))

(defn- temp-dir-path
  []
  (let [f (io/file (str (System/getProperty "java.io.tmpdir")
                        "/night-shift-test-"
                        (System/nanoTime)))]
    (.mkdirs f)
    (.getAbsolutePath f)))

(defn- init-repo!
  []
  (let [repo (temp-dir-path)
        remote (str repo "-origin.git")
        app-file (io/file repo "README.md")]
    (sh! "git" "init" repo)
    (sh! "git" "-C" repo "config" "user.email" "night-shift@example.com")
    (sh! "git" "-C" repo "config" "user.name" "Night Shift")
    (spit app-file "hello\n")
    (sh! "git" "-C" repo "add" "README.md")
    (sh! "git" "-C" repo "commit" "-m" "seed")
    (sh! "git" "init" "--bare" remote)
    (sh! "git" "-C" repo "remote" "add" "origin" remote)
    {:repo repo
     :remote remote}))

(defn- start-shift
  [backend]
  (let [shift (ns/make-night-shift backend)
        start (runner/start shift {:session-id "night-shift-test" :agent-id "codex-2"})]
    (is (:ok start))
    {:shift shift :state (:state start)}))

(deftest spec-loads-and-spike-check-passes
  (let [spec (common/load-spec :night-shift)
        spike (ns/spike-check)]
    (is (= :night-shift (:peripheral/id spec)))
    (is (contains? (:peripheral/tools spec) :frame-provision))
    (is (contains? (:peripheral/tools spec) :branch-create))
    (is (contains? (:peripheral/tools spec) :pr-create))
    (is (true? (:valid-config? spike)))
    (is (true? (:make-night-shift-ok? spike)))))

(deftest frame-provision-isolates-dirty-source-repo
  (let [{:keys [repo]} (init-repo!)
        _ (spit (io/file repo "README.md") "dirty source checkout\n")
        provision (nsb/frame-provision {:source-repo-path repo
                                        :agenda-id "ag-frame"
                                        :short-slug "demo"
                                        :consent-gate-event-id "cg-frame"})
        worktree (:repo-path (:result provision))
        branch (:branch (:result provision))
        metadata-path (:metadata-path (:result provision))
        worktree-status (:out (sh! "git" "-C" worktree "status" "--porcelain"))
        source-status (:out (sh! "git" "-C" repo "status" "--porcelain"))]
    (is (:ok provision))
    (is (not= repo worktree))
    (is (.exists (io/file metadata-path)))
    (is (= "" (str/trim worktree-status)))
    (is (str/includes? source-status "README.md"))
    (is (= branch (str/trim (:out (sh! "git" "-C" worktree "rev-parse" "--abbrev-ref" "HEAD")))))))

(deftest branch-naming-and-protected-branch-invariants
  (let [{:keys [repo]} (init-repo!)
        invalid (nsb/branch-create {:repo-path repo
                                    :branch-name "oops"
                                    :consent-gate-event-id "cg-invalid"})
        protected-edit (nsb/edit-file {:repo-path repo
                                       :path "README.md"
                                       :change-set [{:op :replace-first
                                                     :old-string "hello"
                                                     :new-string "hullo"}]
                                       :consent-gate-event-id "cg-edit"})]
    (is (false? (:ok invalid)))
    (is (= :invalid-branch-name (:reason invalid)))
    (is (false? (:ok protected-edit)))
    (is (= :protected-branch (:reason protected-edit)))))

(deftest dirty-worktree-and-branch-collision-rejected
  (let [{:keys [repo]} (init-repo!)
        _ (spit (io/file repo "README.md") "dirty\n")
        dirty (nsb/branch-create {:repo-path repo
                                  :branch-name "e-night-shift/a1/dirty"
                                  :consent-gate-event-id "cg-dirty"})]
    (is (false? (:ok dirty)))
    (is (= :dirty-worktree (:reason dirty))))
  (let [{:keys [repo]} (init-repo!)
        first (nsb/branch-create {:repo-path repo
                                  :branch-name "e-night-shift/a1/once"
                                  :consent-gate-event-id "cg-once"})
        collision (nsb/branch-create {:repo-path repo
                                      :branch-name "e-night-shift/a1/once"
                                      :consent-gate-event-id "cg-two"})]
    (is (:ok first))
    (is (false? (:ok collision)))
    (is (= :branch-collision (:reason collision)))))

(deftest one-cg-one-branch-and-no-cross-repo
  (let [{:keys [repo]} (init-repo!)
        backend (nsb/make-night-shift-backend)
        {:keys [shift state]} (start-shift backend)
        step1 (runner/step shift state
                           {:tool :branch-create
                            :args [{:repo-path repo
                                    :branch-name "e-night-shift/ag1/fix-readme"
                                    :agenda-id "ag1"
                                    :source-recommendation "PI ranked action"
                                    :consent-gate-event-id "cg-ag1"}]})
        second-branch (runner/step shift (:state step1)
                                   {:tool :branch-create
                                    :args [{:repo-path repo
                                            :branch-name "e-night-shift/ag2/other"
                                            :agenda-id "ag2"
                                            :source-recommendation "PI ranked action"
                                            :consent-gate-event-id "cg-ag2"}]})
        {:keys [other]} (let [{_repo-1 :repo} (init-repo!)
                              {repo-2 :repo} (init-repo!)]
                          {:other repo-2})
        cross-repo (runner/step shift (:state step1)
                                {:tool :repo-stage
                                 :args [{:repo-path other
                                         :paths ["README.md"]
                                         :consent-gate-event-id "cg-ag1"}]})]
    (is (:ok step1))
    (is (false? (:ok second-branch)))
    (is (str/includes? (str second-branch) "one-cg-one-branch"))
    (is (false? (:ok cross-repo)))
    (is (str/includes? (str cross-repo) "no-cross-repo"))))

(deftest secret-patterns-and-commit-provenance-enforced
  (let [{:keys [repo]} (init-repo!)
        _ (nsb/branch-create {:repo-path repo
                              :branch-name "e-night-shift/ag3/provenance"
                              :consent-gate-event-id "cg-ag3"})
        secret-file (io/file repo ".env")
        _ (spit secret-file "TOKEN=1\n")
        secret-stage (nsb/repo-stage {:repo-path repo
                                      :paths [".env"]
                                      :expected-branch "e-night-shift/ag3/provenance"
                                      :consent-gate-event-id "cg-ag3"})]
    (is (false? (:ok secret-stage)))
    (is (= :secret-pattern-match (:reason secret-stage))))
  (let [{:keys [repo]} (init-repo!)
        _ (nsb/branch-create {:repo-path repo
                              :branch-name "e-night-shift/ag4/provenance"
                              :consent-gate-event-id "cg-ag4"})
        _ (spit (io/file repo "README.md") "night shift\n")
        _ (nsb/repo-stage {:repo-path repo
                           :paths ["README.md"]
                           :expected-branch "e-night-shift/ag4/provenance"
                           :consent-gate-event-id "cg-ag4"})
        commit (nsb/repo-commit {:repo-path repo
                                 :message "Tighten README"
                                 :source-agenda "ag4"
                                 :source-recommendation "night shift test"
                                 :consent-gate-event-id "cg-ag4"
                                 :expected-branch "e-night-shift/ag4/provenance"
                                 :author "codex-2"})
        log-msg (:out (sh! "git" "-C" repo "log" "-1" "--pretty=%B"))]
    (is (:ok commit))
    (is (str/includes? (:message (:result commit)) "Source-Agenda: ag4"))
    (is (str/includes? log-msg "Source-Recommendation: night shift test"))
    (is (str/includes? log-msg "Consent-Gate: cg-ag4"))
    (is (str/includes? log-msg "Co-Authored-By: codex-2 via :night-shift"))))

(deftest test-integrity-no-force-and-pr-as-deliverable
  (let [{:keys [repo]} (init-repo!)
        backend (nsb/make-night-shift-backend)
        {:keys [shift state]} (start-shift backend)
        branch (runner/step shift state
                            {:tool :branch-create
                             :args [{:repo-path repo
                                     :branch-name "e-night-shift/ag5/pr"
                                     :agenda-id "ag5"
                                     :source-recommendation "test path"
                                     :consent-gate-event-id "cg-ag5"}]})
        test-r (runner/step shift (:state branch)
                            {:tool :run-tests
                             :args [{:repo-path repo
                                     :test-command "false"
                                     :consent-gate-event-id "cg-ag5"}]})
        pr-r (runner/step shift (:state test-r)
                          {:tool :pr-create
                           :args [{:repo-path repo
                                   :title "Demo PR"
                                   :body "body"
                                   :consent-gate-event-id "cg-ag5"
                                   :ready-for-review? true}]})
        push-r (nsb/push-feature-branch {:repo-path repo
                                         :branch-name "e-night-shift/ag5/pr"
                                         :expected-branch "e-night-shift/ag5/pr"
                                         :consent-gate-event-id "cg-ag5"
                                         :force true})
        spec (common/load-spec :night-shift)]
    (is (:ok branch))
    (is (:ok test-r))
    (is (false? (get-in test-r [:result :passed?])))
    (is (false? (:ok pr-r)))
    (is (str/includes? (str pr-r) "test-hook-integrity"))
    (is (false? (:ok push-r)))
    (is (= :force-unreachable (:reason push-r)))
    (is (not (contains? (:peripheral/tools spec) :repo-merge)))
    (is (not (contains? (:peripheral/tools spec) :repo-rebase)))
    (is (not (contains? (:peripheral/tools spec) :repo-amend)))))

(deftest reap-check-emits-discard-candidates-without-deleting
  (let [repo-path (temp-dir-path)
        bell-calls (atom [])
        shell-fn (fn [& argv]
                   (let [cmd (vec argv)]
                     (cond
                       (= ["git" "-C" repo-path "rev-parse" "--show-toplevel"] cmd)
                       {:exit 0 :out (str repo-path "\n") :err ""}

                       (= ["git" "-C" repo-path "for-each-ref" "refs/heads/e-night-shift/*"
                           "--format=%(refname:short)|%(committerdate:iso8601)"] cmd)
                       {:exit 0
                        :out "e-night-shift/ag7/stale|2026-05-01T10:00:00Z\n"
                        :err ""}

                       :else
                       {:exit 1 :out "" :err (str "unexpected command " cmd)})))
        bell-fn (fn [payload]
                  (swap! bell-calls conj payload)
                  {:ok true :result {:queued true}})
        result (nsb/reap-check {:repo-path repo-path
                                :max-age-days 14
                                :shell-fn shell-fn
                                :bell-fn bell-fn
                                :agent-id "claude-1"})]
    (is (:ok result))
    (is (= ["e-night-shift/ag7/stale"]
           (mapv :branch (:discard-candidates (:result result)))))
    (is (= 1 (count @bell-calls)))
    (is (= :night-shift/discard-candidates
           (get-in (first @bell-calls) [:payload :event])))))
