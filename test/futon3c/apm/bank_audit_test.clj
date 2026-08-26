(ns futon3c.apm.bank-audit-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.bank-audit :as bank-audit]))

(defn- write-terminal!
  [campaign-dir frame receipt]
  (let [terminal-dir (io/file campaign-dir frame "terminal")]
    (.mkdirs terminal-dir)
    (spit (io/file terminal-dir "frame-terminal.edn") (pr-str receipt))))

(defn- receipt
  [frame problem-id outcome head]
  {:frame/id frame
   :problem/id problem-id
   :problem/outcome outcome
   :workspace/terminal-heads {:solver head}})

(defn- with-temp-dir
  [f]
  (let [path (java.nio.file.Files/createTempDirectory
              "bank-audit-test-"
              (make-array java.nio.file.attribute.FileAttribute 0))
        dir (.toFile path)]
    (try
      (f dir)
      (finally
        (doseq [file (reverse (file-seq dir))]
          (io/delete-file file true))))))

(deftest classifies-solved-terminal-receipts
  (with-temp-dir
    (fn [campaign-dir]
      (write-terminal! campaign-dir "campaign-f35"
                       (receipt "f35" "a95J04" :solved "different-head"))
      (write-terminal! campaign-dir "campaign-f36"
                       (receipt "f36" "a95J05" :solved "banked-head"))
      (write-terminal! campaign-dir "campaign-f37"
                       (receipt "f37" "a95J06" :partial "partial-head"))
      (write-terminal! campaign-dir "campaign-f38"
                       (receipt "f38" "a95J07" :solved "missing-head"))
      (let [content {"different-head" "new proof"
                     "banked-head" "same proof"
                     "master" {"a95J04" "old proof"
                               "a95J05" "same proof"
                               "a95J07" "old proof"}}
            read-at-rev (fn [rev path]
                          (let [problem-id (second (re-find #"problems/([^/]+)/" path))]
                            (if (= rev "master")
                              (get-in content [rev problem-id])
                              (get content rev))))
            results (bank-audit/unbanked-solved
                     {:campaign-dir (.getPath campaign-dir)
                      ;; The production default is "origin/master" (the ref the
                      ;; sweep pushes to); fixtures key on "master".
                      :master-rev "master"
                      :read-at-rev read-at-rev})]
        (is (= [{:frame "f35" :problem-id "a95J04"
                 :head "different-head" :status :unbanked}
                {:frame "f36" :problem-id "a95J05"
                 :head "banked-head" :status :banked}
                {:frame "f38" :problem-id "a95J07"
                 :head "missing-head" :status :head-unresolvable}]
               results))
        ;; Regression: banking copies content into a new commit, so reachability
        ;; cannot distinguish this banked proof from an unbanked solver head.
        (is (= :banked (:status (second results))))
        (is (not-any? #(= "f37" (:frame %)) results))))))

(deftest empty-campaign-has-no-unbanked-solves
  (with-temp-dir
    (fn [campaign-dir]
      (is (= [] (bank-audit/unbanked-solved
                 {:campaign-dir (.getPath campaign-dir)
                  :master-rev "master"
                  :read-at-rev (constantly nil)}))))))

(defn- verification-input
  [repo status run-lean git]
  {:frame "f42"
   :problem-id "a97J07"
   :head "d84e28b164f3355c53089c19a58f2056e8c1b6db"
   :status status
   :repo (.getPath repo)
   :run-lean run-lean
   :git git})

(defn- stub-git
  [calls]
  (fn [_repo & args]
    (swap! calls conj args)
    (case (first args)
      "show" {:exit 0 :out "theorem apm_a97j07 : True := by trivial" :err ""}
      "update-ref" {:exit 0 :out "" :err ""})))

(deftest verified-clean-head-is-pinned
  (with-temp-dir
    (fn [repo]
      (let [calls (atom [])
            result (bank-audit/verify-and-pin!
                    (verification-input
                     repo :unbanked
                     (fn [_repo _file]
                       {:exit 0
                        :out "'apm_a97j07' depends on axioms: [propext, Classical.choice, Quot.sound]"
                        :err ""})
                     (stub-git calls)))]
        (is (= :pinned (:status result)))
        (is (= (str "refs/apm/banked-solves/f42/a97J07/"
                    "d84e28b164f3355c53089c19a58f2056e8c1b6db")
               (:ref result)))
        (is (= "update-ref" (first (last @calls))))
        (is (not (.exists (io/file repo ".lake/build/lib/Mathlib.olean"))))))))

(deftest sorry-ax-and-elaboration-failure-never-pin
  (with-temp-dir
    (fn [repo]
      (let [sorry-calls (atom [])
            sorry-result
            (bank-audit/verify-and-pin!
             (verification-input
              repo :unbanked
              (fn [_repo _file]
                {:exit 0
                 :out "'apm_a97j07' depends on axioms: [propext, sorryAx, Classical.choice, Quot.sound]"
                 :err ""})
              (stub-git sorry-calls)))
            failed-calls (atom [])
            failed-result
            (bank-audit/verify-and-pin!
             (verification-input
              repo :unbanked
              (fn [_repo _file] {:exit 1 :out "" :err "type mismatch"})
              (stub-git failed-calls)))]
        (is (= {:status :refused :reason :sorry-ax
                :axioms ["propext" "sorryAx" "Classical.choice" "Quot.sound"]}
               sorry-result))
        (is (not-any? #(= "update-ref" (first %)) @sorry-calls))
        (is (= {:status :refused :reason :elaboration-failed :exit 1}
               failed-result))
        (is (not-any? #(= "update-ref" (first %)) @failed-calls))))))

(deftest non-unbanked-inputs-skip-elaboration-and-git
  (with-temp-dir
    (fn [repo]
      (doseq [status [:banked :head-unresolvable]]
        (let [calls (atom [])
              fail-if-called (fn [& _] (swap! calls inc))]
          (is (= {:status :skipped :reason status}
                 (bank-audit/verify-and-pin!
                  (verification-input repo status fail-if-called fail-if-called))))
          (is (empty? @calls)))))))
