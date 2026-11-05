(ns futon3c.file-change.settlement-consumer
  "Consume new file-change hints at declared generated-output settlement roles.

  Registration baselines existing dirt. A role authorizes only its repo root
  and relative path prefix. Every other new change is retained as a nag."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [futon3c.file-change.journal :as journal])
  (:import [java.security MessageDigest]))

(def consumer-id :settlement)

(def default-roles
  "Initial narrow automatic settlement role. Keeping the consumer armed also
  makes every non-role file change a retained nag rather than invisible dirt."
  [{:role/id :futon0-inbox-zero-live
    :repo-root "/home/joe/code/futon0"
    :path-prefix "holes/missions/M-inbox-zero-live-readiness.md"}])

(defn empty-state [roles max-retained]
  {:roles (vec roles) :max-retained max-retained
   :commits [] :nags [] :refusals []})

(defn- retain [state key value]
  (update state key #(->> (conj (vec %) value)
                          (take-last (:max-retained state)) vec)))

(defn- sha256 [file]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (with-open [in (io/input-stream file)]
      (let [buf (byte-array 65536)]
        (loop []
          (let [n (.read in buf)]
            (when (pos? n) (.update digest buf 0 n) (recur))))))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest digest)))))

(defn- git! [root & args]
  (let [{:keys [exit out err]} (apply sh/sh "git" "-C" root args)]
    (when-not (zero? exit)
      (throw (ex-info "Settlement git command failed"
                      {:root root :args (vec args) :exit exit :stderr err})))
    (str/trim out)))

(defn matching-role [roles {:keys [worktree path]}]
  (some (fn [{:keys [repo-root path-prefix] :as role}]
          (let [root (.getCanonicalPath (io/file repo-root))
                file (.getCanonicalPath (io/file path))
                prefix (.getCanonicalPath (io/file root path-prefix))]
            (when (and (= root (.getCanonicalPath (io/file worktree)))
                       (or (= file prefix) (str/starts-with? file (str prefix java.io.File/separator))))
              role)))
        roles))

(defn commit-entry!
  "Commit one exact current file through git commit --only. The journal hash
  must still match, and the resulting commit must name exactly that path."
  [{:keys [worktree path content-id operation seq]} role]
  (when-not (= :write operation)
    (throw (ex-info "Settlement role only commits present files"
                    {:reason :unsupported-operation :operation operation})))
  (let [root (.getCanonicalPath (io/file worktree))
        file (io/file path)
        rel (.toString (.relativize (.toPath (io/file root)) (.toPath (.getCanonicalFile file))))]
    (when-not (and (.isFile file) (= content-id (sha256 file)))
      (throw (ex-info "Settled file changed before commit"
                      {:reason :content-changed :path path :expected content-id})))
    (when (str/blank? (git! root "status" "--porcelain=v1" "--" rel))
      (throw (ex-info "Settled file is already clean" {:reason :already-clean :path path})))
    (let [before (git! root "rev-parse" "HEAD")
          message (format "inbox-zero: settle %s (%s)" rel (name (:role/id role)))]
      (when-not (zero? (:exit (sh/sh "git" "-C" root "diff" "--cached" "--quiet" "--" rel)))
        (throw (ex-info "Settlement path already has staged content"
                        {:reason :preexisting-staged-content :path path})))
      (git! root "add" "--" rel)
      (try
        (when-not (= content-id (sha256 file))
          (throw (ex-info "Settled file changed while staging"
                          {:reason :content-changed-during-stage :path path})))
        (git! root "commit" "--only" "-m" message "--" rel)
        (catch Exception e
          (sh/sh "git" "-C" root "reset" "-q" "HEAD" "--" rel)
          (throw e)))
      (let [sha (git! root "rev-parse" "HEAD")
            names (-> (git! root "diff-tree" "--root" "--no-commit-id" "--name-only" "-r" sha)
                      str/split-lines vec)]
        (when (or (= before sha) (not= [rel] names))
          (throw (ex-info "Settlement commit scope mismatch"
                          {:reason :commit-scope-mismatch :before before :after sha
                           :expected [rel] :actual names})))
        {:outcome :committed :role/id (:role/id role) :repo-root root
         :path rel :seq seq :commit/sha sha :content-id content-id}))))

(defn run-once!
  [journal-state consumer-state {:keys [now-ms commit-fn nag-fn read-limit deadline-ms]
                                 :or {commit-fn commit-entry! nag-fn (constantly nil)
                                      read-limit 128 deadline-ms 300000}}]
  (when-not (get-in @journal-state [:consumers consumer-id])
    (swap! journal-state journal/register-consumer consumer-id now-ms deadline-ms
           {:authority :git-status-and-declared-settlement-roles}))
  (when (= :rebuild-required (:status (journal/read-next @journal-state consumer-id read-limit)))
    (let [basis (journal/begin-rebuild @journal-state consumer-id)]
      (swap! journal-state
             #(first (journal/rebuild-consumer % consumer-id (:through-seq basis) now-ms
                                               {:authority :git-status-and-declared-settlement-roles})))
      (swap! consumer-state assoc :baseline-through-seq (:through-seq basis))))
  (let [{:keys [entries]} (journal/read-next @journal-state consumer-id read-limit)]
    (when (seq entries)
      (doseq [entry entries]
        (if-let [role (matching-role (:roles @consumer-state) entry)]
          (try
            (swap! consumer-state retain :commits (assoc (commit-fn entry role) :at-ms now-ms))
            (catch Exception e
              (swap! consumer-state retain :refusals
                     {:at-ms now-ms :seq (:seq entry) :path (:path entry)
                      :reason (or (:reason (ex-data e)) :commit-failed)
                      :message (.getMessage e)})))
          (let [nag {:at-ms now-ms :seq (:seq entry) :repo (:repo entry)
                     :path (:path entry) :reason :unsettled-file-change}]
            (nag-fn nag)
            (swap! consumer-state retain :nags nag))))
      (swap! journal-state journal/acknowledge consumer-id (:seq (last entries)) now-ms)))
  {:status :live :roles (mapv :role/id (:roles @consumer-state))
   :baseline-through-seq (:baseline-through-seq @consumer-state)
   :commit-count (count (:commits @consumer-state))
   :nag-count (count (:nags @consumer-state))
   :refusal-count (count (:refusals @consumer-state))
   :last-commit (last (:commits @consumer-state))
   :last-nag (last (:nags @consumer-state))
   :last-refusal (last (:refusals @consumer-state))})
