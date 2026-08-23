(ns futon3c.apm.bank-driver
  "Effectful, fail-closed execution of one certified frame-bank ruling.

  Landed rulings are first merged and checked in a detached candidate
  worktree.  The named trunk is advanced only after post-merge axioms, the
  ConstructionTargets roll-up, and problem elaboration all pass.  This keeps
  a failed candidate out of the trunk while ensuring every gate observes the
  exact merged tree that would land."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.bank :as bank]
            [futon3c.apm.toolchain-port :as toolchain-port])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def landed-rulings #{:closed :partial-banked})

(defn- run-default [dir argv]
  (apply shell/sh (concat (map str argv) [:dir (str dir)])))

(defn- link-substrate!
  "Point the candidate worktree's `.lake` at the repository's built substrate.

  `.lake` is gitignored, so a fresh `git worktree add` has no oleans and every
  lake command inside it would rebuild Mathlib from source (observed
  2026-08-23: the post-merge axiom command cloned mathlib and then failed on
  `unknown module prefix 'Mathlib'`). Workspaces get the same symlink from
  workspace-lifecycle/provision!; the candidate must too. Returns the
  substrate path linked, or nil when the repository has no `.lake`."
  [repository candidate]
  (let [substrate (io/file repository ".lake")
        link (io/file candidate ".lake")]
    (when (and (.isDirectory substrate) (not (.exists link)))
      (Files/createSymbolicLink (.toPath link)
                                (.toPath (.getCanonicalFile substrate))
                                (make-array FileAttribute 0))
      (str substrate))))

(defn- execute-command [run-fn dir argv]
  (let [result (run-fn dir argv)]
    (merge {:exit -1 :out "" :err ""} result)))

(defn- git [run-fn repository & args]
  (execute-command run-fn repository
                   (into ["git" "-C" (str repository)] args)))

(defn- output [result]
  (str/trim (or (:out result) "")))

(defn- success? [result]
  (zero? (:exit result)))

(defn- detach-worktrees-on!
  "Detach every worktree of REPOSITORY that has BRANCH checked out, so the
  branch can be deleted. The solver's workspace is such a worktree: git
  refuses `branch -d` while it is checked out there (library lane,
  bg-1787485260849-9: trunk had already advanced, so the refusal left a
  landed merge with no receipt). Detaching keeps the workspace at its head;
  the next launch at the new trunk revision provisions a fresh frame anyway.
  Returns the first failing command result, or nil."
  [run-fn repository branch]
  (let [listing (git run-fn repository "worktree" "list" "--porcelain")
        blocks (str/split (str (:out listing)) #"\n\n")
        wanted (str "branch refs/heads/" branch)]
    (some (fn [block]
            (let [lines (str/split-lines block)]
              (when (some #(= wanted %) lines)
                (when-let [path (some #(when (str/starts-with? % "worktree ")
                                         (subs % 9))
                                      lines)]
                  (let [r (git run-fn path "checkout" "--detach")]
                    (when-not (success? r) r))))))
          blocks)))

(defn- command-finding [code result]
  {:finding code :exit (:exit result)
   :stdout (or (:out result) "") :stderr (or (:err result) "")})

(defn- parse-axioms [result]
  (when (success? result)
    (when-let [inside (second (re-find #"depends on axioms: \[([^]]*)\]"
                                      (str (:out result) "\n" (:err result))))]
      (let [found (->> (str/split inside #",")
                       (map str/trim)
                       (remove str/blank?)
                       (map symbol)
                       set)]
        ;; Canonical order: the permitted axioms as bank/permitted-axioms lists
        ;; them, then anything else sorted. Lean prints collection order, which
        ;; differs between `#print axioms` and a NameSet walk.
        (vec (concat (filter found bank/permitted-axioms)
                     (sort (remove (set bank/permitted-axioms) found))))))))

(defn- read-status [candidate status-path]
  (let [file (io/file candidate status-path)]
    (when (.isFile file)
      (json/parse-string (slurp file) true))))

(defn- status-classification [status]
  (or (:classification status) (:status status) "unknown"))

(defn- status-sorry-count [status]
  (or (:sorry_count_total status) (:sorry-count status) 0))

(defn- recomputed-status [candidate status-path ruling sorry-count]
  (let [previous (or (read-status candidate status-path) {})
        classification (get bank/ruling->classification ruling)
        updated (assoc previous :classification classification
                       :sorry_count_total sorry-count)]
    (spit (io/file candidate status-path)
          (str (json/generate-string updated {:pretty true}) "\n"))
    {:evidence {:previous-classification (str (status-classification previous))
                :classification classification
                :previous-sorry-count (long (status-sorry-count previous))
                :sorry-count sorry-count
                :method :elaboration}
     :updated updated}))

(defn- common-body [{:keys [frame-id problem-id verify-receipt-id
                            lane-transition]}]
  {:receipt/type :frame-bank
   :receipt/frame-id frame-id
   :receipt/problem-id problem-id
   :receipt/verify-receipt-id verify-receipt-id
   :receipt/lane-transition lane-transition})

(defn- non-landed-receipt [request]
  (bank/build-receipt
   (merge (common-body request)
          (select-keys request [:receipt/defect-witness
                                :receipt/refuted-statement-sha
                                :receipt/seam])
          {:receipt/ruling (:ruling request)})))

(defn- blocked-body [request seam]
  (assoc (common-body request)
         :receipt/ruling :blocked
         :receipt/lane-transition
         {:from (get-in request [:lane-transition :from]) :to :library}
         :receipt/seam (name seam)))

(defn- refuse [request seam finding]
  (let [built (bank/build-receipt (blocked-body request seam))]
    {:ok false :error/code :frame-bank-refused :finding finding
     :ruling :blocked :receipt (:receipt built)}))

(defn- branch-head [run-fn repository branch]
  (let [result (git run-fn repository "rev-parse" "--verify"
                    (str "refs/heads/" branch))]
    (when (success? result) (output result))))

(defn- ancestor? [run-fn repository older newer]
  (success? (git run-fn repository "merge-base" "--is-ancestor" older newer)))

(defn- remove-candidate! [run-fn repository candidate]
  (git run-fn repository "worktree" "remove" "--force" (str candidate)))

(defn- execute-landed!
  [{:keys [repository source-branch source-head trunk-branch ruling
           axiom-command rollup-command status-command status-path run-fn]
    :as request}]
  (let [run-fn (or run-fn run-default)
        trunk-head (branch-head run-fn repository trunk-branch)
        observed-source (branch-head run-fn repository source-branch)
        source-head (or observed-source source-head)]
    (cond
      (not (and trunk-head source-head))
      (refuse request :bank-source-or-trunk-missing
              {:finding :bank-source-or-trunk-missing
               :trunk-head trunk-head :source-head source-head})

      (and (nil? observed-source)
           (ancestor? run-fn repository source-head trunk-head))
      ;; Completed rerun: recheck the landed trunk and reproduce a fresh
      ;; receipt without attempting to redirect or recreate the source ref.
      (execute-landed! (assoc request :source-branch trunk-branch
                              :source-head trunk-head))

      :else
      (let [candidate (str (Files/createTempDirectory
                            "futon3c-bank-candidate-"
                            (make-array FileAttribute 0)))
            add-result (git run-fn repository "worktree" "add" "--detach"
                            candidate trunk-head)]
        (if-not (success? add-result)
          (refuse request :bank-candidate-worktree-failed
                  (command-finding :bank-candidate-worktree-failed add-result))
          (try
            (link-substrate! repository candidate)
            (let [merge-result (git run-fn candidate "merge" "--no-ff" "--no-edit"
                                    source-head)]
              (if-not (success? merge-result)
                (refuse request :bank-merge-failed
                        (command-finding :bank-merge-failed merge-result))
                (let [axiom-result (execute-command run-fn candidate axiom-command)
                      axioms (parse-axioms axiom-result)]
                  (cond
                    (not (success? axiom-result))
                    (refuse request :post-merge-axiom-command-failed
                            (command-finding :post-merge-axiom-command-failed
                                             axiom-result))

                    (not= bank/permitted-axioms axioms)
                    (refuse request :post-merge-axiom-mismatch
                            {:finding :post-merge-axiom-mismatch
                             :expected bank/permitted-axioms :actual axioms})

                    :else
                    (let [rollup-result (execute-command run-fn candidate
                                                         rollup-command)
                          rollup-sorries
                          (toolchain-port/sorry-warning-count rollup-result)]
                      (cond
                        (not (success? rollup-result))
                        (refuse request :post-merge-rollup-failed
                                (command-finding :post-merge-rollup-failed
                                                 rollup-result))

                        (pos? rollup-sorries)
                        (refuse request :post-merge-rollup-carries-sorry
                                {:finding :post-merge-rollup-carries-sorry
                                 :sorry-warnings rollup-sorries})

                        :else
                        (let [status-result (execute-command run-fn candidate
                                                             status-command)
                              status-sorries
                              (toolchain-port/sorry-warning-count status-result)]
                          (if-not (success? status-result)
                            (refuse request :post-merge-status-elaboration-failed
                                    (command-finding
                                     :post-merge-status-elaboration-failed
                                     status-result))
                            (let [{:keys [evidence]}
                                  (recomputed-status candidate status-path ruling
                                                     status-sorries)
                                  add-status (git run-fn candidate "add" "--" status-path)
                                  staged-status (when (success? add-status)
                                                  (git run-fn candidate "diff"
                                                       "--cached" "--quiet"))
                                  commit-status
                                  (cond
                                    (not (success? add-status)) add-status
                                    (success? staged-status)
                                    {:exit 0 :out "" :err ""}
                                    :else
                                    (git run-fn candidate "commit" "-m"
                                         "Recompute problem status after bank merge"))
                                  clean-status
                                  (git run-fn candidate "status" "--porcelain")]
                              (cond
                                (not (success? commit-status))
                                (refuse request :status-recompute-commit-failed
                                        (command-finding
                                         :status-recompute-commit-failed
                                         commit-status))

                                (not (and (success? clean-status)
                                          (str/blank? (output clean-status))))
                                (refuse request :bank-candidate-dirty
                                        {:finding :bank-candidate-dirty
                                         :status (:out clean-status)})

                                :else
                                (let [candidate-head
                                      (output (git run-fn candidate "rev-parse" "HEAD"))
                                      ;; Advance through the checked-out trunk,
                                      ;; not by moving its ref behind Git's
                                      ;; back. update-ref changes history while
                                      ;; leaving the index and visible files at
                                      ;; the old tree, which makes the next lane
                                      ;; cycle observe stale corpus contents.
                                      ;; merge --ff-only updates all three
                                      ;; together and refuses dirty/racing state.
                                      publish (git run-fn repository "merge"
                                                   "--ff-only" candidate-head)]
                                  (if-not (success? publish)
                                    (refuse request :bank-trunk-advance-failed
                                            (command-finding
                                             :bank-trunk-advance-failed publish))
                                    (let [delete (if (= source-branch trunk-branch)
                                                   {:exit 0 :out "" :err ""}
                                                   (or (detach-worktrees-on!
                                                        run-fn repository source-branch)
                                                       (git run-fn repository "branch" "-d"
                                                            source-branch)))]
                                      (if-not (success? delete)
                                        (refuse request :bank-source-delete-failed
                                                (command-finding
                                                 :bank-source-delete-failed delete))
                                        (bank/build-receipt
                                         (cond->
                                          (merge (common-body request)
                                                 {:receipt/ruling ruling
                                                  :receipt/trunk-branch trunk-branch
                                                  :receipt/merge-sha candidate-head
                                                  :receipt/post-merge-axioms axioms
                                                  :receipt/rollup-sorry-warnings
                                                  rollup-sorries
                                                  :receipt/status-recomputed evidence
                                                  :receipt/branch-deleted true})
                                           (= :partial-banked ruling)
                                           (assoc :receipt/boundary
                                                  (:receipt/boundary request))))))))))))))))))
            (finally
              (remove-candidate! run-fn repository candidate))))))))

(defn execute!
  "Execute one bank ruling.

  Commands are argv vectors and always run with the candidate worktree as
  their directory. `repository` is the only Git repository mutated. For a
  landed ruling `source-head` makes completed reruns possible after the source
  branch has been deleted."
  [{:keys [ruling repository source-branch trunk-branch
           axiom-command rollup-command status-command status-path]
    :as request}]
  (let [landed? (contains? landed-rulings ruling)
        required (when landed?
                   [repository source-branch trunk-branch
                    axiom-command rollup-command status-command status-path])]
    (cond
      (not (contains? bank/rulings ruling))
      {:ok false :error/code :bank-driver-ruling-invalid}

      (and landed? (not (every? some? required)))
      {:ok false :error/code :bank-driver-configuration-invalid}

      landed? (execute-landed! request)
      :else (non-landed-receipt request))))
