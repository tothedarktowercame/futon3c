(ns futon3c.apm.bank-audit
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell])
  (:import (java.math BigInteger)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)))

(def ^:private proof-path-template "problems/%s/lean/Main.lean")

(defn- default-read-at-rev
  [rev path]
  (let [repo (io/file (System/getProperty "user.home") "code" "apm-lean")
        {:keys [exit out]} (shell/sh "git" "show" (str rev ":" path)
                                     :dir (.getPath repo))]
    (when (zero? exit)
      out)))

(defn- sha256
  [content]
  (format "%064x"
          (BigInteger. 1 (.digest (MessageDigest/getInstance "SHA-256")
                                  (.getBytes content StandardCharsets/UTF_8)))))

(defn- terminal-receipts
  [campaign-dir]
  (->> (or (.listFiles (io/file campaign-dir)) (make-array java.io.File 0))
       (filter #(.isDirectory ^java.io.File %))
       (map #(io/file % "terminal" "frame-terminal.edn"))
       (filter #(.isFile ^java.io.File %))
       (sort-by #(.getPath ^java.io.File %))
       (map #(edn/read-string (slurp %)))))

(defn unbanked-solved
  "Classify solved frame receipts by comparing solver-head proof content to master."
  [{:keys [campaign-dir read-at-rev]
    :or {read-at-rev default-read-at-rev}}]
  (->> (terminal-receipts campaign-dir)
       (filter #(= :solved (:problem/outcome %)))
       (mapv (fn [receipt]
               (let [frame (:frame/id receipt)
                     problem-id (:problem/id receipt)
                     head (get-in receipt [:workspace/terminal-heads :solver])
                     path (format proof-path-template problem-id)
                     head-content (read-at-rev head path)
                     master-content (when (some? head-content)
                                      (read-at-rev "master" path))
                     status (cond
                              (nil? head-content) :head-unresolvable
                              (and (some? master-content)
                                   (= (sha256 head-content) (sha256 master-content))) :banked
                              :else :unbanked)]
                 {:frame frame
                  :problem-id problem-id
                  :head head
                  :status status})))))
