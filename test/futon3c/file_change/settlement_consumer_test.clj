(ns futon3c.file-change.settlement-consumer-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.file-change.journal :as journal]
            [futon3c.file-change.settlement-consumer :as sut]))

(def role {:role/id :generated-test :repo-root "/repo" :path-prefix "generated"})
(defn entry [seq path]
  {:repo "repo" :worktree "/repo" :path path :content-id (str "h" seq)
   :operation :write :observed-at-ms seq :seq seq})

(deftest activation-baselines-old-dirt-then-commits-role-and-nags-other
  (let [j (atom (journal/empty-state 20))
        s (atom (sut/empty-state [role] 10))
        commits (atom []) nags (atom [])]
    (swap! j #(first (journal/observe % (dissoc (entry 1 "/repo/generated/old") :seq))))
    (sut/run-once! j s {:now-ms 10 :commit-fn #(swap! commits conj [%1 %2])
                        :nag-fn #(swap! nags conj %)})
    (is (empty? @commits) "activation cannot sweep existing dirt")
    (swap! j #(first (journal/observe % (dissoc (entry 2 "/repo/generated/new") :seq))))
    (swap! j #(first (journal/observe % (dissoc (entry 3 "/repo/other") :seq))))
    (let [health (sut/run-once! j s {:now-ms 20
                                     :commit-fn (fn [e r]
                                                  (swap! commits conj [e r])
                                                  {:outcome :committed :commit/sha "abc"
                                                   :path (:path e)})
                                     :nag-fn #(swap! nags conj %)})]
      (is (= 1 (:commit-count health)))
      (is (= 1 (:nag-count health)))
      (is (= "/repo/generated/new" (get-in health [:last-commit :path])))
      (is (= :unsettled-file-change (get-in health [:last-nag :reason]))))))

(deftest role-match-is-root-and-prefix-bounded
  (is (= :generated-test (:role/id (sut/matching-role [role] (entry 1 "/repo/generated/x")))))
  (is (nil? (sut/matching-role [role] (entry 1 "/repo/generated-elsewhere/x"))))
  (is (nil? (sut/matching-role [role] (assoc (entry 1 "/other/generated/x")
                                             :worktree "/other")))))

(defn- digest [file]
  (let [d (java.security.MessageDigest/getInstance "SHA-256")]
    (.update d (java.nio.file.Files/readAllBytes (.toPath file)))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.digest d)))))

(deftest exact-commit-leaves-unrelated-dirt-outside-the-commit
  (let [dir (doto (java.io.File/createTempFile "settlement-git-" "")
              (.delete) (.mkdirs) (.deleteOnExit))
        generated (doto (io/file dir "generated") .mkdirs)
        target (io/file generated "one.txt")
        other (io/file dir "other.txt")]
    (sh/sh "git" "init" "-q" :dir (.getPath dir))
    (sh/sh "git" "config" "user.name" "Settlement Test" :dir (.getPath dir))
    (sh/sh "git" "config" "user.email" "settlement@example.invalid" :dir (.getPath dir))
    (spit target "old\n") (spit other "old\n")
    (sh/sh "git" "add" "." :dir (.getPath dir))
    (sh/sh "git" "commit" "-q" "-m" "base" :dir (.getPath dir))
    (spit target "new\n") (spit other "also new\n")
    (let [result (sut/commit-entry!
                  {:worktree (.getPath dir) :path (.getPath target)
                   :content-id (digest target) :operation :write :seq 2}
                  (assoc role :repo-root (.getPath dir)))]
      (is (= :committed (:outcome result)))
      (is (= ["generated/one.txt"]
             (str/split-lines
              (:out (sh/sh "git" "show" "--format=" "--name-only" "HEAD"
                           :dir (.getPath dir))))))
      (is (str/includes?
           (:out (sh/sh "git" "status" "--porcelain" :dir (.getPath dir)))
           "other.txt")))))
