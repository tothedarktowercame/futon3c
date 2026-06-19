(ns futon3c.logic.snapshot-test
  "Tests for `futon3c.logic.snapshot` state-snapshot-witness siblings."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [xtdb.api :as xtdb]
            [futon3c.evidence.store :as estore]
            [futon3c.evidence.xtdb-backend :as xb]
            [futon3c.logic.probe :as probe]
            [futon3c.logic.snapshot :as snapshot])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:dynamic *xtdb-backend* nil)

(defn- temp-dir
  [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- sh!
  [& args]
  (let [{:keys [exit out err]} (apply shell/sh args)]
    (is (zero? exit)
        (str "command failed: " (pr-str args) "\n" err))
    out))

(defn- init-git-repo!
  []
  (let [dir (temp-dir "snapshot-repo-")
        path (.getAbsolutePath dir)]
    (sh! "git" "init" path)
    (sh! "git" "-C" path "config" "user.email" "snapshot-test@example.com")
    (sh! "git" "-C" path "config" "user.name" "Snapshot Test")
    (spit (io/file dir "tracked.txt") "tracked\n")
    (sh! "git" "-C" path "add" "tracked.txt")
    (sh! "git" "-C" path "commit" "-m" "init")
    (spit (io/file dir "stashed.txt") "stashed\n")
    (sh! "git" "-C" path "add" "stashed.txt")
    (sh! "git" "-C" path "stash" "push" "-m" "snapshot-test-stash")
    (spit (io/file dir "dirty.txt") "dirty\n")
    path))

(use-fixtures
  :each
  (fn [f]
    (let [node (xtdb/start-node {})
          old-registry @probe/family-check-fns
          old-hud (snapshot/current-hud-render)]
      (try
        (reset! probe/family-check-fns {})
        (snapshot/clear-hud-render!)
        (binding [*xtdb-backend* (xb/make-xtdb-backend node)]
          (f))
        (finally
          (reset! probe/family-check-fns old-registry)
          (if old-hud
            (snapshot/record-hud-render! old-hud)
            (snapshot/clear-hud-render!))
          (.close node))))))

(deftest project-inventory-real-stack-yields-records
  (testing "real inventory parses and projects to >=10 family records"
    (let [snap (snapshot/project-inventory)]
      (is (some? snap))
      (is (>= (:families snap) 10))
      (is (vector? (:family-records snap)))
      (is (some :status (:family-records snap))))))

(deftest project-inventory-nonexistent-path-is-nil
  (testing "nonexistent path returns nil tolerantly"
    (is (nil? (snapshot/project-inventory "/tmp/definitely-does-not-exist.sexp")))))

(deftest project-registry-empty-yields-zero-records
  (let [snap (snapshot/project-registry {})]
    (is (= 0 (:registered-families snap)))
    (is (= 0 (:deferred-families snap)))
    (is (= [] (:family-records snap)))))

(deftest project-registry-real-yields-deferred-state-and-timestamps
  (probe/register-family-check! :always-ok
                                (fn [_] {:outcome :ok}))
  (probe/register-family-check! :later
                                (fn [_] {:outcome :inactive
                                         :detail {:deferred? true}}))
  (let [snap (snapshot/project-registry)
        by-id (into {} (map (juxt :family-id identity) (:family-records snap)))]
    (is (= 2 (:registered-families snap)))
    (is (= 1 (:deferred-families snap)))
    (is (= false (get-in by-id [:always-ok :deferred?])))
    (is (= true (get-in by-id [:later :deferred?])))
    (is (string? (get-in by-id [:always-ok :registered-at])))
    (is (string? (get-in by-id [:later :registered-at])))))

(deftest project-repo-refs-empty-yields-zero-records
  (let [snap (snapshot/project-repo-refs [])]
    (is (= 0 (:repos snap)))
    (is (= 0 (:dirty-repos snap)))
    (is (= [] (:repo-records snap)))))

(deftest project-repo-refs-real-yields-git-state
  (let [repo (init-git-repo!)
        snap (snapshot/project-repo-refs [repo])
        record (first (:repo-records snap))]
    (is (= 1 (:repos snap)))
    (is (= repo (:repo record)))
    (is (string? (:branch record)))
    (is (= 40 (count (:HEAD-sha record))))
    (is (= 1 (:stash-count record)))
    (is (true? (:dirty? record)))
    (is (contains? record :ahead-of-origin))
    (is (contains? record :behind-origin))))

(deftest project-hud-render-inactive-when-empty
  (let [snap (snapshot/project-hud-render)]
    (is (= :inactive (:outcome snap)))
    (is (string? (:captured-at snap)))
    (is (string? (:reason snap)))))

(deftest project-hud-render-real-yields-recorded-state
  (snapshot/record-hud-render! {:widget-id :invariant-queue
                                :rendered-at "2026-05-01T12:00:00Z"
                                :counts {:open 5 :closed 1}
                                :motion-flag :moving})
  (let [snap (snapshot/project-hud-render)]
    (is (= :ok (:outcome snap)))
    (is (= :invariant-queue (:widget-id snap)))
    (is (= "2026-05-01T12:00:00Z" (:rendered-at snap)))
    (is (= {:open 5 :closed 1} (:counts snap)))
    (is (= :moving (:motion-flag snap)))))

(deftest snapshot-emits-queryable-evidence-for-all-siblings
  (let [repo (init-git-repo!)]
    (probe/register-family-check! :emit-ok
                                  (fn [_] {:outcome :ok}))
    (snapshot/record-hud-render! {:widget-id :invariant-queue
                                  :counts {:open 5 :closed 1}
                                  :motion-flag :moving})
    (doseq [{:keys [label emit tags event container]}
            [{:label "inventory"
              :emit #(snapshot/snapshot-inventory! *xtdb-backend*)
              :tags [:invariant-queue :state-snapshot :inventory]
              :event :inventory-snapshot
              :container :inventory}
             {:label "registry"
              :emit #(snapshot/snapshot-registry! *xtdb-backend*)
              :tags [:invariant-queue :state-snapshot :registry]
              :event :registry-snapshot
              :container :registry}
             {:label "repo-refs"
              :emit #(snapshot/snapshot-repo-refs! *xtdb-backend* [repo])
              :tags [:invariant-queue :state-snapshot :repo-refs]
              :event :repo-refs-snapshot
              :container :repo-refs}
             {:label "hud-render"
              :emit #(snapshot/snapshot-hud-render! *xtdb-backend*)
              :tags [:invariant-queue :state-snapshot :hud-render]
              :event :hud-render-snapshot
              :container :hud-render}]]
      (testing (str label " emits a queryable snapshot evidence entry")
        (let [r (emit)]
          (is (:ok r) (str "expected emit success, got " (pr-str r))))
        (let [hits (estore/query* *xtdb-backend*
                                  {:query/type :coordination
                                   :query/tags tags})
              entry (first hits)]
          (is (= 1 (count hits)))
          (is (= event (get-in entry [:evidence/body :event])))
          (is (= container (get-in entry [:evidence/body :container])))
          (is (some? (get-in entry [:evidence/body :state]))))))))

(deftest snapshot-on-load-wrappers-return-receipts
  (let [repo (init-git-repo!)]
    (probe/register-family-check! :on-load-ok
                                  (fn [_] {:outcome :ok}))
    (snapshot/record-hud-render! {:widget-id :invariant-queue
                                  :counts {:open 1 :closed 1}
                                  :motion-flag :flowing})
    (doseq [{:keys [label run]}
            [{:label "inventory"
              :run #(snapshot/snapshot-inventory-on-load! *xtdb-backend* {:print? false})}
             {:label "registry"
              :run #(snapshot/snapshot-registry-on-load! *xtdb-backend* {:print? false})}
             {:label "repo-refs"
              :run #(snapshot/snapshot-repo-refs-on-load! *xtdb-backend*
                                                          {:repo-paths [repo]
                                                           :print? false})}
             {:label "hud-render"
              :run #(snapshot/snapshot-hud-render-on-load! *xtdb-backend* {:print? false})}]]
      (testing (str label " on-load wrapper returns a receipt")
        (let [r (run)]
          (is (:ok r) (str "expected on-load success, got " (pr-str r))))))))

(deftest canonical-statement-grep-verifiable
  (is (string? snapshot/I-state-snapshot-witness))
  (is (re-find #"I-state-snapshot-witness" snapshot/I-state-snapshot-witness)))
