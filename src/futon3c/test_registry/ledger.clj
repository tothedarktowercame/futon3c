(ns futon3c.test-registry.ledger
  "Write-only, content-addressed store for warrant artifacts.

  A warrant pins its log. Pinning it by path made the path part of the
  evidence: move the file and the warrant is not stale but unverifiable, with
  no way back except re-running (2026-09-17: three warrants pinned logs under
  /tmp). Here the object's NAME is its sha256, so there is no path to go
  stale, the same bytes written twice are the same object, and a write can
  never overwrite anything — the only operations are put and resolve.

  Objects are made read-only on arrival. That does not stop root, and is not
  meant to: it stops the ordinary accident, which is what actually happens."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.file Files LinkOption StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermission]
           [java.security MessageDigest]
           [java.util EnumSet]))

(def default-root
  (or (System/getenv "FUTON3C_REGISTRY_LEDGER")
      "/home/joe/code/storage/registry-ledger"))

(defn- sha256-file [file]
  (let [digest (MessageDigest/getInstance "SHA-256")
        buffer (byte-array 65536)]
    (with-open [in (io/input-stream file)]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur)))))
    (str/join (map #(format "%02x" (bit-and % 0xff)) (.digest digest)))))

(defn object-file
  "Where SHA lives, sharded two hex wide so one directory never grows huge."
  [root sha]
  (io/file root "objects" (subs sha 0 2) (str sha ".log")))

(defn resolve-file
  "The stored object for SHA, or nil when this ledger does not hold it."
  ([sha] (resolve-file default-root sha))
  ([root sha]
   (when (and (string? sha) (<= 2 (count sha)))
     (let [file (object-file root sha)]
       (when (.isFile file) file)))))

(defn holds?
  ([sha] (holds? default-root sha))
  ([root sha] (some? (resolve-file root sha))))

(defn- read-only!
  "Make FILE unwritable. A filesystem without POSIX permissions is a reason to
  carry on — the object is still content-addressed — but any other failure is
  worth seeing, so it is not swallowed."
  [file]
  (try
    (Files/setPosixFilePermissions
     (.toPath ^java.io.File file)
     (EnumSet/of PosixFilePermission/OWNER_READ
                 PosixFilePermission/GROUP_READ
                 PosixFilePermission/OTHERS_READ))
    (catch UnsupportedOperationException _ nil)))

(defn put!
  "Store FILE under its own sha256 and return the sha.

  Idempotent by construction: if the object is already here the bytes are by
  definition identical, so this is a no-op. The copy lands through a temp file
  in the same directory and an atomic move, so a reader never sees a partial
  object."
  ([file] (put! default-root file))
  ([root file]
   (let [sha (sha256-file file)
         target (object-file root sha)]
     (when-not (.isFile target)
       (let [dir (.toPath (io/file (.getParent target)))
             attrs (make-array FileAttribute 0)]
         (Files/createDirectories dir attrs)
         (let [tmp (Files/createTempFile dir ".incoming-" ".log" attrs)]
           (try
             (Files/copy (.toPath (io/file file)) tmp
                         (into-array StandardCopyOption
                                     [StandardCopyOption/REPLACE_EXISTING]))
             (Files/move tmp (.toPath target)
                         (into-array StandardCopyOption
                                     [StandardCopyOption/ATOMIC_MOVE]))
             (read-only! target)
             (finally (Files/deleteIfExists tmp))))))
     sha)))

(defn artifact
  "The `:log-artifact` value for FILE: its path, its sha, and the ledger now
  holding it. The path stays recorded so a record written here can still be
  read by a checker that predates the ledger.

  Throws if the ledger cannot take the object. It used to fall back to hashing
  in place and record `:ledger nil`, which minted a path-pinned warrant —
  exactly the failure the ledger exists to prevent — and made it
  indistinguishable from a pre-ledger record. A caller that wants a warrant
  should refuse instead (zai-1's ruling, 2026-09-17)."
  ([file] (artifact default-root file))
  ([root file]
   (let [sha (put! root file)]
     (when-not (holds? root sha)
       (throw (ex-info "ledger did not take the object"
                       {:sha256 sha :root (str root) :file (str file)})))
     {:path (.getCanonicalPath (io/file file))
      :sha256 sha
      :ledger (str root)})))

(defn locate
  "Resolve a recorded `:log-artifact` to a readable file: the ledger first,
  which cannot have moved, then the recorded path for records written before
  the ledger existed. Returns nil when neither holds it."
  ([log] (locate (or (:ledger log) default-root) log))
  ([root log]
   (or (resolve-file (or (:ledger log) root) (:sha256 log))
       (let [path (:path log)]
         (when (and (string? path) (not (str/blank? path)))
           (let [file (io/file path)]
             (when (Files/isRegularFile (.toPath file)
                                        (make-array LinkOption 0))
               file)))))))
