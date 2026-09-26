(ns futon3c.apm.workspace-lifecycle-policy-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]))

(def policy
  (edn/read-string
   (slurp "holes/labs/M-apm-demonstration/workspace-lifecycle-policy-v1.edn")))

(deftest retirement-preserves-history-and-removes-only-the-exact-worktree
  (let [effects (mapv :effect (:retirement/effects policy))]
    (is (= :git-worktree-remove-exact-registered-path (first effects)))
    (is (some #{:retain-branch-and-recorded-commit} effects))
    (is (contains? (get-in policy [:safety :forbidden])
                   :delete-branch-as-part-of-worktree-retirement))
    (is (contains? (get-in policy [:safety :forbidden])
                   :recursive-delete-unregistered-path))))

(deftest active-and-ambiguous-worktrees-fail-closed
  (is (contains? (:retirable/preconditions policy)
                 :no-running-or-parked-job-references-workspace))
  (is (= :quarantine-and-report (get-in policy [:safety :ambiguous-action])))
  (is (contains? (get-in policy [:countdown/rule
                                  :before-opening-next-frame])
                 :all-quarantined-workspaces-reported)))

(deftest inventory-does-not-pretend-to-be-a-deletion-manifest
  (is (= 90 (get-in policy [:inventory/baseline
                             :apm-lean-registered-worktrees])))
  (is (= 2 (count (get-in policy [:inventory/baseline
                                   :dirty-apm-worktrees]))))
  (is (string? (get-in policy [:inventory/baseline :interpretation]))))
