(ns futon3c.evidence.payload-store
  "Content-addressed store for exact text payloads referenced by evidence."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file FileAlreadyExistsException Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermission]
           [java.security MessageDigest]
           [java.util EnumSet]))

(def default-root
  (or (System/getenv "FUTON3C_DISPATCH_PAYLOAD_STORE")
      "/home/joe/code/storage/dispatch-payloads"))

(defn- hex [bytes]
  (str/join (map #(format "%02x" (bit-and % 0xff)) bytes)))

(defn- sha256 [bytes]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.update digest bytes)
    (hex (.digest digest))))

(defn object-file [root sha]
  (io/file root "objects" (subs sha 0 2) sha))

(defn- valid-sha? [sha]
  (and (string? sha) (boolean (re-matches #"[0-9a-f]{64}" sha))))

(defn- read-only! [file]
  (try
    (Files/setPosixFilePermissions
     (.toPath ^java.io.File file)
     (EnumSet/of PosixFilePermission/OWNER_READ
                 PosixFilePermission/GROUP_READ
                 PosixFilePermission/OTHERS_READ))
    (catch UnsupportedOperationException _
      (.setWritable ^java.io.File file false false)))
  file)

(defn put-text!
  "Store S as UTF-8 under its SHA-256. Returns its content coordinates."
  ([s] (put-text! default-root s))
  ([root s]
   (let [s (str s)
         bytes (.getBytes s StandardCharsets/UTF_8)
         sha (sha256 bytes)
         target (object-file root sha)]
     (when-not (.isFile target)
       (let [dir (.toPath (io/file (.getParent target)))
             attrs (make-array FileAttribute 0)]
         (Files/createDirectories dir attrs)
         (let [tmp (Files/createTempFile dir ".incoming-" ".payload" attrs)]
           (try
             (with-open [out (io/output-stream (.toFile tmp))]
               (.write out bytes))
             (try
               (Files/move tmp (.toPath target)
                           (into-array StandardCopyOption
                                       [StandardCopyOption/ATOMIC_MOVE]))
               (catch FileAlreadyExistsException _ nil))
             (finally (Files/deleteIfExists tmp))))))
     (read-only! target)
     {:sha256 sha :chars (count s) :bytes (alength bytes)})))

(defn resolve-text
  "Resolve SHA as UTF-8 text, or nil when ROOT does not hold it."
  ([sha] (resolve-text default-root sha))
  ([root sha]
   (when (valid-sha? sha)
     (let [file (object-file root sha)]
       (when (.isFile file)
         (Files/readString (.toPath file) StandardCharsets/UTF_8))))))
