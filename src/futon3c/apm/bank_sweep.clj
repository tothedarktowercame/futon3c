(ns futon3c.apm.bank-sweep
  "Move verified, pinned solver files onto a fresh branch from origin/master."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.bank-audit :as bank-audit])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time LocalDate)
           (java.util UUID)))

(defn- default-git
  [repo & args]
  (apply shell/sh (concat ["git" "-C" repo] args)))

(defn- git-reader
  [git repo]
  (fn [rev path]
    (let [result (git repo "show" (str rev ":" path))]
      (when (zero? (:exit result)) (:out result)))))

(defn- pin-ref
  [{:keys [frame problem-id head]}]
  (str "refs/apm/banked-solves/" frame "/" problem-id "/" head))

(defn- pinned?
  [git repo solve]
  (let [result (git repo "rev-parse" "--verify" (pin-ref solve))]
    (and (zero? (:exit result))
         (= (:head solve) (str/trim (:out result))))))

(defn- status-json
  [content {:keys [frame problem-id head]} campaign-id date subject]
  (let [prior (json/parse-string content true)
        previous-classification (:classification prior)
        previous-sorries (get-in prior [:lean :sorry_count_total])
        boundary (str "Closed by frame " frame " (campaign " campaign-id ", " date
                      "): \"" subject "\". Banked verbatim from the " frame
                      " Solver terminal head " head " on branch exp/countdown-"
                      frame "-" problem-id
                      "-solver; content identical to that head, no re-derivation. "
                      "Frozen statement apm_" (str/lower-case problem-id)
                      " unchanged from the pre-solve base.")
        audit {:method "lake env lean elaboration; exact clean axiom set required by solve pin"
               :at date
               :elaborated_exit 0
               :sorry_declarations 0
               :previous_classification previous-classification
               :previous_sorry_count_total previous-sorries
               :axioms (str "apm_" (str/lower-case problem-id)
                            " depends on axioms: [propext, Classical.choice, Quot.sound]")
               :frame frame}]
    (-> prior
        (assoc-in [:lean :sorry_count_main] 0)
        (assoc-in [:lean :sorry_count_total] 0)
        (assoc :classification "solved" :boundary boundary :sorry_audit audit)
        (dissoc :checked_at :checked_lean_state)
        (json/generate-string {:pretty true})
        (str "\n"))))

(defn- commit-file!
  [git worktree path message]
  (let [added (git worktree "add" "--" path)]
    (if-not (zero? (:exit added))
      {:ok false :reason :git-add-failed :exit (:exit added)}
      (let [committed (git worktree "commit" "-m" message "--" path)]
        (if-not (zero? (:exit committed))
          {:ok false :reason :git-commit-failed :exit (:exit committed)}
          (let [head (git worktree "rev-parse" "HEAD")]
            (if (zero? (:exit head))
              {:ok true :commit (str/trim (:out head))}
              {:ok false :reason :git-head-unreadable :exit (:exit head)})))))))

(defn- bank-one!
  [git read-at-rev worktree campaign-id date solve]
  (let [{:keys [frame problem-id head]} solve
        proof-path (format "problems/%s/lean/Main.lean" problem-id)
        status-path (format "problems/%s/status.json" problem-id)
        proof (read-at-rev head proof-path)
        status (read-at-rev "origin/master" status-path)
        subject-result (git worktree "show" "-s" "--format=%s" head)
        subject (str/trim (:out subject-result))]
    (cond
      (nil? proof) {:ok false :reason :head-unresolvable}
      (nil? status) {:ok false :reason :status-unreadable}
      (not (zero? (:exit subject-result))) {:ok false :reason :subject-unreadable}
      :else
      (let [proof-file (io/file worktree proof-path)
            status-file (io/file worktree status-path)
            _ (.mkdirs (.getParentFile proof-file))
            _ (spit proof-file proof)
            solve-message (str subject "\n\nBanked from " frame " solver head " head
                               " (branch exp/countdown-" frame "-" problem-id
                               "-solver), campaign " campaign-id
                               ". Content identical to that head; no re-derivation.")
            solve-commit (commit-file! git worktree proof-path solve-message)]
        (if-not (:ok solve-commit)
          solve-commit
          (let [_ (spit status-file
                        (status-json status solve campaign-id date subject))
                metadata-commit
                (commit-file!
                 git worktree status-path
                 (str "Recompute " problem-id " metadata after banking the "
                      frame " solve"))]
            (if-not (:ok metadata-commit)
              metadata-commit
              {:ok true
               :banked {:frame frame :problem-id problem-id :head head
                        :commits [(:commit solve-commit)
                                  (:commit metadata-commit)]}})))))))

(defn- non-fast-forward?
  [result]
  (boolean (re-find #"(?i)non-fast-forward|fetch first"
                    (str (:out result) "\n" (:err result)))))

(defn sweep-to-master!
  "Create a fresh origin/master worktree and bank every pinned, content-unbanked
  solve. Push is opt-in and is always a normal fast-forward push."
  [{:keys [campaign-dir repo push? git read-at-rev date]
    :or {push? false git default-git date (str (LocalDate/now))}}]
  (let [read-at-rev (or read-at-rev (git-reader git repo))
        fetched (git repo "fetch" "origin")]
    (if-not (zero? (:exit fetched))
      {:banked [] :skipped [] :refused [] :pushed? false
       :reason :fetch-failed}
      (let [classified (bank-audit/unbanked-solved
                        {:campaign-dir campaign-dir
                         :read-at-rev read-at-rev
                         :master-rev "origin/master"})
            skipped (mapv #(assoc % :reason (:status %))
                          (remove #(= :unbanked (:status %)) classified))
            unbanked (filterv #(= :unbanked (:status %)) classified)
            pin-checks (mapv (fn [solve] [solve (pinned? git repo solve)])
                             unbanked)
            pinned (mapv first (filter second pin-checks))
            refused (mapv #(assoc % :reason :not-pinned)
                          (map first (remove second pin-checks)))]
        (if (empty? pinned)
          {:banked [] :skipped skipped :refused refused :pushed? false
           :reason :nothing-to-bank}
          (let [temp-root (.toFile
                           (Files/createTempDirectory
                            "apm-bank-sweep-" (make-array FileAttribute 0)))
                worktree (io/file temp-root "worktree")
                branch (str "bank/sweep-" (UUID/randomUUID))
                created? (atom false)]
            (try
              (let [added (git repo "worktree" "add" "-b" branch
                               (.getPath worktree) "origin/master")]
                (if-not (zero? (:exit added))
                  {:banked [] :skipped skipped :refused refused :pushed? false
                   :reason :worktree-create-failed :branch branch}
                  (do
                    (reset! created? true)
                    (let [campaign-id (.getName (io/file campaign-dir))
                          results (mapv #(bank-one! git read-at-rev
                                                   (.getPath worktree)
                                                   campaign-id date %)
                                        pinned)
                          banked (mapv :banked (filter :ok results))
                          bank-refused
                          (mapv (fn [[solve result]]
                                  (assoc solve :reason (:reason result)))
                                (filter (comp not :ok second)
                                        (map vector pinned results)))
                          refused (into refused bank-refused)]
                      (cond
                        (not= (count banked) (count pinned))
                        {:banked banked :skipped skipped :refused refused
                         :pushed? false :reason :bank-commit-failed :branch branch}

                        (not push?)
                        {:banked banked :skipped skipped :refused refused
                         :pushed? false :reason :push-disabled :branch branch}

                        :else
                        (let [pushed (git (.getPath worktree) "push" "origin"
                                          "HEAD:master")]
                          (cond
                            (zero? (:exit pushed))
                            {:banked banked :skipped skipped :refused refused
                             :pushed? true :branch branch}

                            (non-fast-forward? pushed)
                            {:banked banked :skipped skipped :refused refused
                             :pushed? false :reason :non-fast-forward
                             :branch branch}

                            :else
                            {:banked banked :skipped skipped :refused refused
                             :pushed? false :reason :push-failed
                             :branch branch})))))))
              (finally
                (when @created?
                  (git repo "worktree" "remove" "--force" (.getPath worktree)))
                (doseq [file (reverse (file-seq temp-root))]
                  (io/delete-file file true))))))))))
