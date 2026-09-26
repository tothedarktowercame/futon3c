(ns futon3c.apm.campaign-projection-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-projection :as projection]
            [futon3c.apm.campaign-snapshot :as snapshot])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]))

(defn temp-dir []
  (Files/createTempDirectory "campaign-projection-"
                             (make-array FileAttribute 0)))

(defn delete-tree! [^Path dir]
  (with-open [paths (Files/walk dir (make-array java.nio.file.FileVisitOption 0))]
    (doseq [path (reverse (sort-by #(.getNameCount ^Path %)
                                  (iterator-seq (.iterator paths))))]
      (Files/deleteIfExists path))))

(defn certificate [version generated-at ledger-digest status]
  (let [body {:certificate/type :campaign-projection :certificate/version 1
              :generated-at generated-at :snapshot/status status
              :campaign/id "apm-200" :campaign/series :apm
              :campaign/status :running :campaign/version version
              :ledger/digest ledger-digest :ledger/event-count version
              :facts/digest (str "facts-" generated-at)
              :active/block "b1"
              :active/frame {:frame-id (str "f" version) :problem-id "p"
                             :phase :solve}
              :counts {:blocks 1 :frames version :closed-frames 0
                       :stopped-frames 0}
              :reconciliation {:observed-at generated-at :findings []}}]
    (assoc body :certificate/id (machine/ledger-digest [body]))))

(defn persist [dir certificate]
  (snapshot/persist! (.resolve ^Path dir "certificates") certificate))

(deftest rendering-is-stable-and-identifies-authority
  (let [certificate (certificate 3 "2026-08-20T12:00:00Z" "ledger-3" :valid)
        rendered (projection/render certificate)]
    (is (str/starts-with? rendered "CAMPAIGN VALID"))
    (is (str/includes? rendered (:certificate/id certificate)))
    (is (str/includes? rendered "frame=f3 problem=p phase=solve"))
    (is (str/includes? rendered "Findings: none"))))

(deftest pointer-accepts-newer-and-refuses-regression
  (let [dir (temp-dir)]
    (try
      (let [old-path (:path (persist dir (certificate 1 "2026-08-20T12:00:00Z"
                                                        "ledger-1" :valid)))
            new-path (:path (persist dir (certificate 2 "2026-08-20T12:01:00Z"
                                                        "ledger-2" :valid)))
            pointer-dir (.resolve ^Path dir "projection")
            first-result (projection/publish-pointer! pointer-dir old-path)
            newer (projection/publish-pointer! pointer-dir new-path)
            regression (projection/publish-pointer! pointer-dir old-path)]
        (is (:published? first-result))
        (is (:published? newer))
        (is (= :campaign-projection-regression (:error/code regression)))
        (is (= 2 (get-in (projection/read-latest pointer-dir)
                         [:pointer :campaign/version]))))
      (finally (delete-tree! dir)))))

(deftest same-version-allows-newer-observation-of-same-ledger
  (let [dir (temp-dir)]
    (try
      (let [first-path (:path (persist dir (certificate 2 "2026-08-20T12:00:00Z"
                                                          "ledger-2" :valid)))
            later-path (:path (persist dir (certificate 2 "2026-08-20T12:05:00Z"
                                                          "ledger-2" :stale)))
            pointer-dir (.resolve ^Path dir "projection")]
        (is (:published? (projection/publish-pointer! pointer-dir first-path)))
        (is (:published? (projection/publish-pointer! pointer-dir later-path)))
        (is (= :stale (get-in (projection/read-latest pointer-dir)
                              [:pointer :snapshot/status]))))
      (finally (delete-tree! dir)))))

(deftest same-version-different-ledger-is-conflict
  (let [dir (temp-dir)]
    (try
      (let [a (:path (persist dir (certificate 2 "2026-08-20T12:00:00Z"
                                               "ledger-a" :valid)))
            b (:path (persist dir (certificate 2 "2026-08-20T12:01:00Z"
                                               "ledger-b" :valid)))
            pointer-dir (.resolve ^Path dir "projection")]
        (is (:ok (projection/publish-pointer! pointer-dir a)))
        (is (= :campaign-projection-ledger-conflict
               (:error/code (projection/publish-pointer! pointer-dir b)))))
      (finally (delete-tree! dir)))))

(deftest project-uses-injected-sink-and-is-idempotent
  (let [dir (temp-dir) calls (atom [])]
    (try
      (let [certificate (certificate 1 "2026-08-20T12:00:00Z" "ledger" :valid)
            certificate-path (:path (persist dir certificate))
            pointer-dir (.resolve ^Path dir "projection")
            sink #(swap! calls conj %)
            first-result (projection/project! pointer-dir certificate-path sink)
            second-result (projection/project! pointer-dir certificate-path sink)]
        (is (:projected? first-result))
        (is (false? (:published? second-result)))
        (is (= 2 (count @calls)))
        (is (= "*problem*" (:buffer-name (first @calls))))
        (is (= (:certificate/id certificate)
               (get-in @calls [0 :certificate :certificate/id]))))
      (finally (delete-tree! dir)))))
