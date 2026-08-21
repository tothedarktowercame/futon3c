(ns futon3c.inbox-zero.witness
  "Publish exact-seat, successful tool-edit witnesses for futon3's intake.

  This namespace deliberately knows nothing about Git dirt.  It records only
  the fact witnessed by an agent tool stream: one exact session successfully
  completed an edit-shaped tool call naming one file."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import [java.net InetAddress]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]
           [java.security MessageDigest]
           [java.util Date]))

(def edit-tool-names #{"Edit" "Write" "MultiEdit"})

(defn- sha-256-value [value]
  (let [bytes (.getBytes (pr-str value) StandardCharsets/UTF_8)
        digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
    (str "sha256:" (apply str (map #(format "%02x" (bit-and % 0xff)) digest)))))

(defn- worktree-id [repo-root]
  (str "worktree:" (subs (sha-256-value repo-root) 7 23)))

(defn- record-id [record]
  (get record ({:inbox-zero/session-seat :seat/id
                :inbox-zero/session-file-claim :claim/id}
               (:record/type record))))

(defn- target-path [directory id]
  (.resolve directory (str (subs (sha-256-value id) 7) ".edn")))

(defn- publish-immutable! [witness-dir record]
  (let [directory (.toPath (io/file witness-dir))
        attrs (make-array FileAttribute 0)
        id (record-id record)]
    (Files/createDirectories directory attrs)
    (let [target (target-path directory id)]
      (if (.exists (.toFile target))
        (let [existing (edn/read-string (slurp (.toFile target)))]
          ;; A seat is established once. Later successful edits in that same
          ;; exact session reuse it without rewriting its first observation.
          (when-not (or (= existing record)
                        (and (= :inbox-zero/session-seat (:record/type record))
                             (= id (:seat/id existing))))
            (throw (ex-info "Inbox-zero witness id conflict" {:record/id id})))
          target)
        (let [tmp (Files/createTempFile directory ".witness-" ".edn" attrs)]
          (try
            (spit (.toFile tmp) (str (pr-str record) "\n"))
            (Files/move tmp target
                        (into-array StandardCopyOption
                                    [StandardCopyOption/ATOMIC_MOVE]))
            target
            (finally (Files/deleteIfExists tmp))))))))

(defn- git-root [file]
  (let [cwd (if (.isDirectory file) file (.getParentFile file))
        {:keys [exit out]} (shell/sh "git" "-C" (.getCanonicalPath cwd)
                                     "rev-parse" "--show-toplevel")]
    (when (zero? exit) (.getCanonicalPath (io/file (str/trim out))))))

(defn edit-path
  "Return the path named by an edit-shaped tool detail, otherwise nil."
  [{:keys [name input]}]
  (when (edit-tool-names name)
    (let [path (or (:file_path input) (get input "file_path"))]
      (when (and (string? path) (not (str/blank? path))) path))))

(defn publish-successful-edit!
  "Publish a seat and claim for a confirmed successful tool result.

  Returns nil when disabled or when the tool/path is not an honest Git-backed
  edit witness.  WITNESS-DIR is normally FUTON3_INBOX_ZERO_WITNESS_DIR."
  [{:keys [witness-dir agent-id session-id tool-detail observed-at host-id]
    :or {observed-at (Date.)}}]
  (when-let [named-path (and witness-dir (edit-path tool-detail))]
    (let [file (.getCanonicalFile (io/file named-path))]
      (when-let [root (git-root file)]
        (let [relative (str (.relativize (.toPath (io/file root)) (.toPath file)))
              seat-id (str "seat:" agent-id ":" session-id)
              tool-id (str (:id tool-detail))
              repo-id (.getName (io/file root))
              worktree (worktree-id root)
              seat {:record/type :inbox-zero/session-seat
                    :seat/id seat-id :agent/id agent-id :session/id session-id
                    :surface :agent-tool-stream
                    :host/id (or host-id (.getHostName (InetAddress/getLocalHost)))
                    :workspace/root root :observed-at observed-at
                    :registry-witness {:endpoint :invoke-tool-result
                                       :session/id session-id}}
              claim {:record/type :inbox-zero/session-file-claim
                     :claim/id (str "claim:" (subs (sha-256-value
                                                    [seat-id tool-id worktree relative]) 7))
                     :seat/id seat-id :repo/id repo-id :worktree/id worktree
                     :path relative :relation :edited-by-session
                     :witness/type :tool-edit :witness/id tool-id
                     :first-observed-at observed-at :last-observed-at observed-at
                     :state :active}]
          (publish-immutable! witness-dir seat)
          (publish-immutable! witness-dir claim)
          {:seat seat :claim claim})))))
