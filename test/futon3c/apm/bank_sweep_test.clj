(ns futon3c.apm.bank-sweep-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.bank-sweep :as sut]))

(def head "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(def proof-path "problems/a97J07/lean/Main.lean")
(def status-path "problems/a97J07/status.json")

(defn- with-temp-dir
  [f]
  (let [dir (.toFile
             (java.nio.file.Files/createTempDirectory
              "bank-sweep-test-"
              (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (f dir)
      (finally
        (doseq [file (reverse (file-seq dir))]
          (io/delete-file file true))))))

(defn- write-terminal!
  [campaign]
  (let [dir (io/file campaign "campaign-f42" "terminal")]
    (.mkdirs dir)
    (spit (io/file dir "frame-terminal.edn")
          (pr-str {:frame/id "f42" :problem/id "a97J07"
                   :problem/outcome :solved
                   :workspace/terminal-heads {:solver head}}))))

(def initial-status
  (json/generate-string
   {:problem_id "a97J07"
    :lean {:sorry_count_main 1 :sorry_count_total 1}
    :classification "partial"
    :checked_at "old"
    :checked_lean_state {:sorry_count 1}}))

(defn- fixture
  [pinned? push-result]
  (let [calls (atom [])
        commits (atom [])
        master-proof (atom "old proof")
        commit-counter (atom 0)
        read-at-rev
        (fn [rev path]
          (cond
            (and (= rev head) (= path proof-path)) "solved proof\n"
            (and (= rev "origin/master") (= path proof-path)) @master-proof
            (and (= rev "origin/master") (= path status-path)) initial-status))
        git
        (fn [repo & args]
          (swap! calls conj {:repo repo :args args})
          (cond
            (= ["fetch" "origin"] args) {:exit 0 :out "" :err ""}
            (and (= "rev-parse" (first args)) (= "--verify" (second args)))
            {:exit (if pinned? 0 1) :out (if pinned? (str head "\n") "") :err ""}
            (= ["show" "-s" "--format=%s" head] args)
            {:exit 0 :out "Solve a97J07 exactly\n" :err ""}
            (and (= "worktree" (first args)) (= "add" (second args)))
            (do (.mkdirs (io/file (nth args 4))) {:exit 0 :out "" :err ""})
            (and (= "worktree" (first args)) (= "remove" (second args)))
            {:exit 0 :out "" :err ""}
            (= "add" (first args)) {:exit 0 :out "" :err ""}
            (= "commit" (first args))
            (let [path (last args)
                  content (slurp (io/file repo path))]
              (swap! commits conj {:path path :content content :args args})
              (when (= path proof-path) (reset! master-proof content))
              {:exit 0 :out "" :err ""})
            (= ["rev-parse" "HEAD"] args)
            {:exit 0
             :out (format "%040x" (swap! commit-counter inc))
             :err ""}
            (= ["push" "origin" "HEAD:master"] args) push-result
            :else {:exit 1 :out "" :err (str "unexpected " args)}))]
    {:calls calls :commits commits :master-proof master-proof
     :read-at-rev read-at-rev :git git}))

(defn- sweep
  [campaign repo fixture push?]
  (sut/sweep-to-master!
   {:campaign-dir (.getPath campaign) :repo (.getPath repo)
    :push? push? :git (:git fixture) :read-at-rev (:read-at-rev fixture)
    :date "2026-08-26"}))

(deftest pinned-unbanked-solve-makes-two-single-file-commits-without-push
  (with-temp-dir
    (fn [root]
      (let [campaign (io/file root "campaign")
            repo (io/file root "repo")
            _ (write-terminal! campaign)
            fixture (fixture true {:exit 0 :out "" :err ""})
            result (sweep campaign repo fixture false)
            commits @(:commits fixture)
            metadata (json/parse-string (:content (second commits)) true)]
        (is (= :push-disabled (:reason result)))
        (is (= [{:frame "f42" :problem-id "a97J07" :head head
                 :commits [(format "%040x" 1) (format "%040x" 2)]}]
               (:banked result)))
        (is (= [proof-path status-path] (mapv :path commits)))
        (is (every? (fn [commit]
                      (= 1 (count (filter #{(:path commit)} (:args commit)))))
                    commits))
        (is (= "solved proof\n" (:content (first commits))))
        (is (= 0 (get-in metadata [:lean :sorry_count_main])))
        (is (= 0 (get-in metadata [:lean :sorry_count_total])))
        (is (= "solved" (:classification metadata)))
        (is (not (contains? metadata :checked_at)))
        (is (not (contains? metadata :checked_lean_state)))
        (is (= "f42" (get-in metadata [:sorry_audit :frame])))
        (is (not-any? #(= "push" (first (:args %))) @(:calls fixture)))))))

(deftest unpinned-unbanked-solve-is-refused-without-commits
  (with-temp-dir
    (fn [root]
      (let [campaign (io/file root "campaign")
            _ (write-terminal! campaign)
            fixture (fixture false {:exit 0 :out "" :err ""})
            result (sweep campaign (io/file root "repo") fixture false)]
        (is (= :nothing-to-bank (:reason result)))
        (is (= :not-pinned (get-in result [:refused 0 :reason])))
        (is (empty? @(:commits fixture)))))))

(deftest already-banked-solve-is-skipped-without-push
  (with-temp-dir
    (fn [root]
      (let [campaign (io/file root "campaign")
            _ (write-terminal! campaign)
            fixture (fixture true {:exit 0 :out "" :err ""})
            _ (reset! (:master-proof fixture) "solved proof\n")
            result (sweep campaign (io/file root "repo") fixture true)]
        (is (= :nothing-to-bank (:reason result)))
        (is (= :banked (get-in result [:skipped 0 :reason])))
        (is (empty? @(:commits fixture)))
        (is (not-any? #(= "push" (first (:args %))) @(:calls fixture)))))))

(deftest non-fast-forward-push-is-reported-without-force
  (with-temp-dir
    (fn [root]
      (let [campaign (io/file root "campaign")
            _ (write-terminal! campaign)
            fixture (fixture true {:exit 1 :out "" :err "rejected (non-fast-forward)"})
            result (sweep campaign (io/file root "repo") fixture true)
            args (mapcat :args @(:calls fixture))]
        (is (= :non-fast-forward (:reason result)))
        (is (false? (:pushed? result)))
        (is (= 2 (count @(:commits fixture))))
        (is (not-any? #(and (= "push" (first %))
                            (some #{"--force"} %))
                      @(:calls fixture)))
        (is (not-any? #(str/starts-with? % "+") args))))))

(deftest second-sweep-is-idempotent-after-master-content-matches
  (with-temp-dir
    (fn [root]
      (let [campaign (io/file root "campaign")
            _ (write-terminal! campaign)
            fixture (fixture true {:exit 0 :out "" :err ""})
            first-result (sweep campaign (io/file root "repo") fixture false)
            commit-count (count @(:commits fixture))
            second-result (sweep campaign (io/file root "repo") fixture false)]
        (is (= 1 (count (:banked first-result))))
        (is (empty? (:banked second-result)))
        (is (= :banked (get-in second-result [:skipped 0 :reason])))
        (is (= commit-count (count @(:commits fixture))))))))
