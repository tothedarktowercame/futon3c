(ns futon3c.wm.r10-commission-binding-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.wm.r10-commission :as commission]
            [futon3c.wm.r10-commission-binding :as binding])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files StandardOpenOption)))

(deftest server-owned-authority-pin-and-readback
  (testing "committed bytes match the code-owned pin"
    (let [bytes (Files/readAllBytes (java.nio.file.Path/of
                                     binding/authority-path
                                     (make-array String 0)))]
      (is (= binding/authority-sha256 (commission/sha256-bytes bytes)))))
  (testing "binding returns the exact adopted commission"
    (let [record (binding/authorized-commission)]
      (is (= "r10-click-commission-2026-09-14-01" (:commission/id record)))
      (is (= binding/authority-sha256 (:authority/sha256 record))))))

(deftest drifted-copy-refuses-against-server-pin
  (let [dir (Files/createTempDirectory
             "r10-binding-drift" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "authority.edn")
        original (Files/readAllBytes (java.nio.file.Path/of
                                      binding/authority-path
                                      (make-array String 0)))
        tampered (byte-array (concat original (.getBytes " " StandardCharsets/UTF_8)))]
    (Files/write path tampered
                 (into-array StandardOpenOption
                             [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE]))
    (try
      (commission/load-authorized-commission
       {:authority-path (str path)
        :authority-sha256 binding/authority-sha256})
      (is false "drifted authority unexpectedly accepted")
      (catch clojure.lang.ExceptionInfo e
        (is (= :r10/invalid-commission (:error/code (ex-data e))))
        (is (= :authority-sha256-mismatch (:reason (ex-data e))))))))
