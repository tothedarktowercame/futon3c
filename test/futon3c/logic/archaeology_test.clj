(ns futon3c.logic.archaeology-test
  "Tests for `futon3c.logic.archaeology` — the three sibling
   `obsolescence-recognition/*` check-fns under family
   `archaeology-control`.

  Mission: M-archaeology-control (futon3c/holes/missions/)."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [xtdb.api :as xtdb]
            [futon3c.evidence.store :as store]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.logic.archaeology :as arch]
            [futon3c.logic.probe :as probe]
            [futon3c.logic.tracer :as tracer]))

(def ^:dynamic *xtdb-backend* nil)

(defn- temp-dir
  [prefix]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      prefix
                      (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.deleteOnExit dir)
    dir))

(defn- init-git-repo!
  [^java.io.File dir]
  (let [{:keys [exit err]} (shell/sh "git" "init" (.getPath dir))]
    (is (zero? exit) (str "git init failed: " err))
    dir))

(use-fixtures
  :each
  (fn [f]
    (let [node (xtdb/start-node {})]
      (try
        (binding [*xtdb-backend* (xb/make-xtdb-backend node)]
          (f))
        (finally
          (.close node))))))

;; -----------------------------------------------------------------------------
;; obsolescence-recognition/autostash
;; -----------------------------------------------------------------------------

(deftest autostash-empty-repo-list-is-ok
  (testing "no repos to scan → :ok with zero obsolete artifacts"
    (let [check (arch/check-autostash-obsolescence [])
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r)))
      (is (= 0 (get-in r [:detail :obsolete-count]))))))

(deftest autostash-no-stashes-is-ok
  (testing "scanning real repos with no stashes → :ok"
    ;; Use the actual futon-stack repos. Joe's machine currently has
    ;; zero stashes (verified during M-archaeology-control IDENTIFY),
    ;; so this exercise the happy path against live state.
    (let [check (arch/check-autostash-obsolescence
                 ["/home/joe/code/futon3c"])
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r))
          (str "expected :ok with no stashes, got " (pr-str r))))))

(deftest autostash-nonexistent-repo-is-skipped
  (testing "non-git directories are skipped, not errored"
    (let [check (arch/check-autostash-obsolescence
                 ["/tmp/definitely-not-a-repo-xyz123"])
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r))
          "non-existent repo path produces no violations"))))

;; -----------------------------------------------------------------------------
;; obsolescence-recognition/deferred-stub
;; -----------------------------------------------------------------------------

(deftest deferred-stub-empty-registry-is-ok
  (testing "no registrations → :ok"
    (let [old @probe/family-check-fns]
      (try
        (reset! probe/family-check-fns {})
        (let [check (arch/check-deferred-stub-obsolescence nil)
              r (check *xtdb-backend*)]
          (is (= :ok (:outcome r)))
          (is (= 0 (get-in r [:detail :obsolete-count]))))
        (finally
          (reset! probe/family-check-fns old))))))

(deftest deferred-stub-without-inventory-is-ok
  (testing "with deferred registrations but no inventory cross-check → :ok"
    (let [old @probe/family-check-fns]
      (try
        (reset! probe/family-check-fns {})
        (probe/register-family-check!
         :test/deferred-thing
         (fn [_] {:outcome :inactive :detail {:deferred? true}}))
        (let [check (arch/check-deferred-stub-obsolescence nil)
              r (check *xtdb-backend*)]
          (is (= :ok (:outcome r))
              "no inventory means no cross-check; can't detect obsolescence"))
        (finally
          (reset! probe/family-check-fns old))))))

(deftest deferred-stub-non-deferred-fns-not-flagged
  (testing "real check-fns are never flagged as obsolete"
    (let [old @probe/family-check-fns
          inv-path "/home/joe/code/futon3c/docs/structural-law-inventory.sexp"]
      (try
        (reset! probe/family-check-fns {})
        (probe/register-family-check!
         :single-boundary
         (fn [_] {:outcome :ok :detail {:checked true}}))
        (let [check (arch/check-deferred-stub-obsolescence inv-path)
              r (check *xtdb-backend*)]
          (is (= :ok (:outcome r))
              "non-deferred check-fns can't be obsolescence-stubs"))
        (finally
          (reset! probe/family-check-fns old))))))

;; -----------------------------------------------------------------------------
;; obsolescence-recognition/pipeline-tracer
;; -----------------------------------------------------------------------------

(deftest pipeline-tracer-no-tracers-is-ok
  (testing "no open or closed tracers → :ok"
    (let [check (arch/check-pipeline-tracer-obsolescence)
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r)))
      (is (= 0 (get-in r [:detail :open-count]))))))

(deftest pipeline-tracer-closed-track-flagged-as-obsolete
  (testing "an open tracer with a matching :closed entry is flagged as obsolete"
    (tracer/emit-tracer!
     *xtdb-backend*
     {:track-id :test/track-flagged
      :title "test"
      :mission :M-test
      :target-date "2099-12-31"
      :expected-outcome "test"
      :owner nil})
    (tracer/emit-tracer-closed!
     *xtdb-backend*
     {:track-id :test/track-flagged
      :resolution "test-closed"
      :closed-by "archaeology-test"})
    (let [check (arch/check-pipeline-tracer-obsolescence)
          r (check *xtdb-backend*)]
      (is (= :violation (:outcome r))
          (str "expected violation, got " (pr-str r)))
      (is (= 1 (get-in r [:detail :obsolete-count]))))))

(deftest pipeline-tracer-past-target-flagged-as-obsolete
  (testing "open tracer past target-date with no close → flagged"
    (tracer/emit-tracer!
     *xtdb-backend*
     {:track-id :test/past-due
      :title "test"
      :mission :M-test
      :target-date "2020-01-01"
      :expected-outcome "test"
      :owner nil})
    (let [check (arch/check-pipeline-tracer-obsolescence)
          r (check *xtdb-backend*)]
      (is (= :violation (:outcome r))
          (str "expected violation, got " (pr-str r)))
      (is (= 1 (get-in r [:detail :obsolete-count]))))))

(deftest pipeline-tracer-fresh-open-not-flagged
  (testing "open tracer with future target-date and no close → not flagged"
    (tracer/emit-tracer!
     *xtdb-backend*
     {:track-id :test/future
      :title "test"
      :mission :M-test
      :target-date "2099-12-31"
      :expected-outcome "test"
      :owner nil})
    (let [check (arch/check-pipeline-tracer-obsolescence)
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r))
          (str "expected ok for future-dated open tracer, got " (pr-str r))))))

;; -----------------------------------------------------------------------------
;; bounded-disposition/stash — different shape (per-artifact + bound)
;; -----------------------------------------------------------------------------

(deftest stash-disposition-empty-repo-list-is-ok
  (testing "no repos to scan → :ok"
    (let [check (arch/check-stash-disposition [])
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r)))
      (is (= 0 (get-in r [:detail :scanned-repos]))))))

(deftest stash-disposition-no-stashes-is-ok
  (testing "real repo with no stashes → :ok"
    (let [check (arch/check-stash-disposition ["/home/joe/code/futon3c"])
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r))
          (str "expected :ok with no stashes, got " (pr-str r))))))

(deftest stash-disposition-nonexistent-repo-is-skipped
  (testing "non-git directories skipped, not errored"
    (let [check (arch/check-stash-disposition
                 ["/tmp/definitely-not-a-repo-xyz123"])
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r))))))

(deftest parse-stash-disposition-vocabulary
  (testing "parses each vocabulary keyword"
    (is (= :kept (arch/parse-stash-disposition "[disposition: kept] WIP")))
    (is (= :parked-on-branch
           (arch/parse-stash-disposition "WIP [disposition: parked-on-branch]")))
    (is (= :dropped (arch/parse-stash-disposition "[disposition: dropped]")))
    (is (= :awaiting-decision
           (arch/parse-stash-disposition "[disposition: awaiting-decision]"))))
  (testing "case-insensitive parsing"
    (is (= :kept (arch/parse-stash-disposition "[Disposition: KEPT] WIP"))))
  (testing "missing tag → default"
    (is (= arch/default-stash-disposition
           (arch/parse-stash-disposition "WIP message no tag"))))
  (testing "unknown tag → default"
    (is (= arch/default-stash-disposition
           (arch/parse-stash-disposition "[disposition: weird-thing]")))))

(deftest disposition-canonical-statement-grep-verifiable
  (is (string? arch/I-bounded-disposition))
  (is (re-find #"I-bounded-disposition" arch/I-bounded-disposition)))

(deftest stash-disposition-vocabulary-fixed
  (testing "vocabulary set has exactly the documented members"
    (is (= #{:kept :parked-on-branch :dropped :awaiting-decision}
           arch/stash-disposition-vocabulary))))

;; -----------------------------------------------------------------------------
;; bounded-disposition/branch
;; -----------------------------------------------------------------------------

(deftest parse-branch-disposition-vocabulary
  (testing "parses each vocabulary keyword"
    (is (= :active (arch/parse-branch-disposition "[disposition: active] WIP")))
    (is (= :merged-not-yet-deleted
           (arch/parse-branch-disposition
            "desc [disposition: merged-not-yet-deleted]")))
    (is (= :parked (arch/parse-branch-disposition "[disposition: parked]")))
    (is (= :long-lived-release
           (arch/parse-branch-disposition "[disposition: long-lived-release]")))
    (is (= :abandoned
           (arch/parse-branch-disposition "[disposition: abandoned]"))))
  (testing "case-insensitive parsing"
    (is (= :active
           (arch/parse-branch-disposition "[Disposition: ACTIVE] feature branch"))))
  (testing "missing or unknown tag -> default"
    (is (= arch/default-branch-disposition
           (arch/parse-branch-disposition "feature branch")))
    (is (= arch/default-branch-disposition
           (arch/parse-branch-disposition "[disposition: unknown]")))))

(deftest branch-disposition-empty-repo-list-is-ok
  (testing "no repos to scan -> :ok"
    (let [check (arch/check-branch-disposition [])
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r)))
      (is (= 0 (get-in r [:detail :scanned-repos]))))))

(deftest branch-disposition-no-branches-is-ok
  (testing "empty git repo with no local branches beyond init state -> :ok"
    (let [repo (init-git-repo! (temp-dir "branch-disposition-empty"))
          check (arch/check-branch-disposition [(.getPath repo)])
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r))
          (str "expected :ok with no branches, got " (pr-str r))))))

;; -----------------------------------------------------------------------------
;; bounded-disposition/mission-doc
;; -----------------------------------------------------------------------------

(deftest parse-mission-doc-status-with-status-line
  (let [repo (temp-dir "mission-doc-status")
        missions-dir (io/file repo "holes" "missions")
        mission-file (io/file missions-dir "M-sample.md")]
    (.mkdirs missions-dir)
    (spit mission-file (str "**Status:** PARKED\n"
                            "body\n"))
    (is (= :parked
           (arch/parse-mission-doc-status (.getPath mission-file))))))

(deftest parse-mission-doc-status-without-status-line
  (let [repo (temp-dir "mission-doc-no-status")
        missions-dir (io/file repo "holes" "missions")
        mission-file (io/file missions-dir "M-sample.md")]
    (.mkdirs missions-dir)
    (spit mission-file "No front matter here\n")
    (is (= arch/default-mission-doc-disposition
           (arch/parse-mission-doc-status (.getPath mission-file))))))

(deftest mission-doc-disposition-empty-repo-list-is-ok
  (testing "no repos to scan -> :ok"
    (let [check (arch/check-mission-doc-disposition [])
          r (check *xtdb-backend*)]
      (is (= :ok (:outcome r)))
      (is (= 0 (get-in r [:detail :scanned-repos]))))))

;; -----------------------------------------------------------------------------
;; Canonical statement is grep-verifiable (subsumption-witness)
;; -----------------------------------------------------------------------------

(deftest canonical-statement-grep-verifiable
  (is (string? arch/I-obsolescence-recognition))
  (is (re-find #"I-obsolescence-recognition" arch/I-obsolescence-recognition)))

;; -----------------------------------------------------------------------------
;; register-archaeology-control-taps! integrates with the probe
;; -----------------------------------------------------------------------------

(deftest register-installs-six-taps
  (testing "the convenience registrar installs three obsolescence-recognition/* siblings PLUS three bounded-disposition siblings"
    (let [old @probe/family-check-fns]
      (try
        (reset! probe/family-check-fns {})
        (let [registered (arch/register-archaeology-control-taps!
                          {:repo-paths []
                           :inventory-path nil})]
          (is (= 6 (count registered)))
          (is (contains? registered :obsolescence-recognition/autostash))
          (is (contains? registered :bounded-disposition/stash))
          (is (contains? registered :bounded-disposition/branch))
          (is (contains? registered :bounded-disposition/mission-doc))
          (is (contains? registered :obsolescence-recognition/deferred-stub))
          (is (contains? registered :obsolescence-recognition/pipeline-tracer)))
        (finally
          (reset! probe/family-check-fns old))))))

;; -----------------------------------------------------------------------------
;; Load-time checks — emit :family-fired evidence on JVM boot
;; -----------------------------------------------------------------------------

(defn- family-fired-entries
  [backend family-id]
  (store/query* backend
                {:query/type :coordination
                 :query/tags [:family-fired :load-time family-id]}))

(deftest check-autostash-on-load-emits-family-fired-evidence
  (let [result (arch/check-autostash-on-load!
                *xtdb-backend*
                {:print? false :repo-paths []})
        entries (family-fired-entries *xtdb-backend*
                                      :obsolescence-recognition/autostash)]
    (is (#{:ok :violation :inactive} (:outcome result)))
    (is (= {:ok true} (select-keys (:emit-receipt result) [:ok])))
    (is (= 1 (count entries)))))

(deftest check-autostash-on-load-respects-emit-flag
  (let [result (arch/check-autostash-on-load!
                *xtdb-backend*
                {:print? false :emit? false :repo-paths []})]
    (is (nil? (:emit-receipt result)))
    (is (empty? (family-fired-entries *xtdb-backend*
                                      :obsolescence-recognition/autostash)))))

(deftest check-deferred-stub-on-load-emits-family-fired-evidence
  (let [result (arch/check-deferred-stub-on-load!
                *xtdb-backend*
                {:print? false :inventory-path nil})
        entries (family-fired-entries *xtdb-backend*
                                      :obsolescence-recognition/deferred-stub)]
    (is (#{:ok :violation :inactive} (:outcome result)))
    (is (= {:ok true} (select-keys (:emit-receipt result) [:ok])))
    (is (= 1 (count entries)))))

(deftest check-deferred-stub-on-load-respects-emit-flag
  (let [result (arch/check-deferred-stub-on-load!
                *xtdb-backend*
                {:print? false :emit? false :inventory-path nil})]
    (is (nil? (:emit-receipt result)))
    (is (empty? (family-fired-entries *xtdb-backend*
                                      :obsolescence-recognition/deferred-stub)))))

(deftest check-pipeline-tracer-on-load-emits-family-fired-evidence
  (let [result (arch/check-pipeline-tracer-on-load!
                *xtdb-backend*
                {:print? false})
        entries (family-fired-entries *xtdb-backend*
                                      :obsolescence-recognition/pipeline-tracer)]
    (is (#{:ok :violation :inactive} (:outcome result)))
    (is (= {:ok true} (select-keys (:emit-receipt result) [:ok])))
    (is (= 1 (count entries)))))

(deftest check-pipeline-tracer-on-load-respects-emit-flag
  (let [result (arch/check-pipeline-tracer-on-load!
                *xtdb-backend*
                {:print? false :emit? false})]
    (is (nil? (:emit-receipt result)))
    (is (empty? (family-fired-entries *xtdb-backend*
                                      :obsolescence-recognition/pipeline-tracer)))))

(deftest check-stash-disposition-on-load-emits-family-fired-evidence
  (let [result (arch/check-stash-disposition-on-load!
                *xtdb-backend*
                {:print? false :repo-paths []})
        entries (family-fired-entries *xtdb-backend*
                                      :bounded-disposition/stash)]
    (is (#{:ok :violation :inactive} (:outcome result)))
    (is (= {:ok true} (select-keys (:emit-receipt result) [:ok])))
    (is (= 1 (count entries)))))

(deftest check-stash-disposition-on-load-respects-emit-flag
  (let [result (arch/check-stash-disposition-on-load!
                *xtdb-backend*
                {:print? false :emit? false :repo-paths []})]
    (is (nil? (:emit-receipt result)))
    (is (empty? (family-fired-entries *xtdb-backend*
                                      :bounded-disposition/stash)))))

(deftest check-branch-disposition-on-load-emits-family-fired-evidence
  (let [result (arch/check-branch-disposition-on-load!
                *xtdb-backend*
                {:print? false :repo-paths []})
        entries (family-fired-entries *xtdb-backend*
                                      :bounded-disposition/branch)]
    (is (#{:ok :violation :inactive} (:outcome result)))
    (is (= {:ok true} (select-keys (:emit-receipt result) [:ok])))
    (is (= 1 (count entries)))))

(deftest check-branch-disposition-on-load-respects-emit-flag
  (let [result (arch/check-branch-disposition-on-load!
                *xtdb-backend*
                {:print? false :emit? false :repo-paths []})]
    (is (nil? (:emit-receipt result)))
    (is (empty? (family-fired-entries *xtdb-backend*
                                      :bounded-disposition/branch)))))

(deftest check-mission-doc-disposition-on-load-emits-family-fired-evidence
  (let [result (arch/check-mission-doc-disposition-on-load!
                *xtdb-backend*
                {:print? false :repo-paths []})
        entries (family-fired-entries *xtdb-backend*
                                      :bounded-disposition/mission-doc)]
    (is (#{:ok :violation :inactive} (:outcome result)))
    (is (= {:ok true} (select-keys (:emit-receipt result) [:ok])))
    (is (= 1 (count entries)))))

(deftest check-mission-doc-disposition-on-load-respects-emit-flag
  (let [result (arch/check-mission-doc-disposition-on-load!
                *xtdb-backend*
                {:print? false :emit? false :repo-paths []})]
    (is (nil? (:emit-receipt result)))
    (is (empty? (family-fired-entries *xtdb-backend*
                                      :bounded-disposition/mission-doc)))))
