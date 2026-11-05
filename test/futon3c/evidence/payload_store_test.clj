(ns futon3c.evidence.payload-store-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.evidence.payload-store :as payload-store])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute PosixFilePermission]))

(defn- temp-root []
  (str (Files/createTempDirectory "dispatch-payload-test-"
                                  (make-array FileAttribute 0))))

(deftest text-payloads-round-trip-idempotently-and-read-only
  (let [root (temp-root)
        text "象 receives exact UTF-8 — καλημέρα"
        first-put (payload-store/put-text! root text)
        second-put (payload-store/put-text! root text)
        object (payload-store/object-file root (:sha256 first-put))
        permissions (Files/getPosixFilePermissions
                     (.toPath object)
                     (make-array java.nio.file.LinkOption 0))]
    (is (= first-put second-put))
    (is (= text (payload-store/resolve-text root (:sha256 first-put))))
    (is (= (count text) (:chars first-put)))
    (is (= (alength (.getBytes text java.nio.charset.StandardCharsets/UTF_8))
           (:bytes first-put)))
    (is (= 1 (count (filter #(.isFile ^java.io.File %)
                            (file-seq (io/file root "objects"))))))
    (is (not (contains? permissions PosixFilePermission/OWNER_WRITE)))
    (is (nil? (payload-store/resolve-text root (apply str (repeat 64 "0")))))))
