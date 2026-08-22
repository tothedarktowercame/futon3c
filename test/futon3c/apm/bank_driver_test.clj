(ns futon3c.apm.bank-driver-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.apm.bank-driver :as driver])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def verify-id (apply str (repeat 64 "a")))

(defn- sh [dir & argv]
  (apply shell/sh (concat (map str argv) [:dir (str dir)])))

(defn- git [repo & args]
  (apply sh repo "git" "-C" (str repo) args))

(defn- write! [repo path content]
  (let [file (io/file repo path)]
    (.mkdirs (.getParentFile file))
    (spit file content)))

(defn- commit! [repo message]
  (git repo "add" "-A")
  (git repo "commit" "-q" "-m" message)
  (str/trim (:out (git repo "rev-parse" "HEAD"))))

(defn- delete-tree! [root]
  (doseq [file (reverse (file-seq (io/file root)))]
    (.delete file)))

(defn- fixture-repo []
  (let [repo (Files/createTempDirectory
              "futon3c-bank-driver-test-" (make-array FileAttribute 0))
        repo (str repo)]
    (git repo "init" "-q" "-b" "trunk")
    (git repo "config" "user.email" "bank-driver@example.test")
    (git repo "config" "user.name" "Bank Driver Test")
    (write! repo "Main.lean" "theorem fixture : True := by\n  sorry\n")
    (write! repo "Library.lean" "theorem libraryFixture : True := by trivial\n")
    (write! repo "ConstructionTargets.lean" "import Library\n")
    (write! repo "Axioms.lean" "import Main\n#print axioms fixture\n")
    (write! repo "status.json"
            (json/generate-string {:classification "partial"
                                   :sorry_count_total 1}))
    (let [base (commit! repo "fixture base")]
      (git repo "checkout" "-q" "-b" "solver")
      (write! repo "Main.lean" "theorem fixture : True := by trivial\n")
      (let [source (commit! repo "solve fixture")]
        (git repo "checkout" "-q" "trunk")
        {:repo repo :base base :source source}))))

(defn- request [{:keys [repo source]}]
  {:repository (str repo)
   :source-branch "solver"
   :source-head source
   :trunk-branch "trunk"
   :frame-id "f21"
   :problem-id "fixture"
   :verify-receipt-id verify-id
   :ruling :closed
   :lane-transition {:from :proof :to :done}
   ;; Each gate first invokes Lean on a tiny fixture file. The axiom wrapper
   ;; then emits the canonical permitted list because a truly axiom-free tiny
   ;; theorem would correctly print an empty list.
   :axiom-command
   ["bash" "-lc"
    (str "lean -R . Main.lean >/dev/null && "
         "printf 'fixture depends on axioms: "
         "[propext, Classical.choice, Quot.sound]\\n'")]
   :rollup-command ["lean" "-R" "." "Library.lean"]
   :status-command ["lean" "-R" "." "Main.lean"]
   :status-path "status.json"})

(defn- ref-head [repo branch]
  (str/trim (:out (git repo "rev-parse" branch))))

(deftest closed-happy-path-recomputes-status-and-is-rerunnable
  (let [{:keys [repo base source] :as fixture} (fixture-repo)]
    (try
      (let [first-result (driver/execute! (request fixture))
            receipt (:receipt first-result)
            status (:receipt/status-recomputed receipt)]
        (is (:ok first-result) (pr-str first-result))
        (is (= :closed (:receipt/ruling receipt)))
        (is (= source (:source-head (request fixture))))
        (is (not= base (ref-head repo "trunk")))
        (is (= {:previous-classification "partial"
                :classification "solved"
                :previous-sorry-count 1
                :sorry-count 0
                :method :elaboration}
               status))
        (is (not (zero? (:exit (git repo "show-ref" "--verify" "--quiet"
                                    "refs/heads/solver")))))
        (testing "a completed merge with a deleted source branch is safe"
          (let [rerun (driver/execute! (request fixture))]
            (is (:ok rerun))
            (is (= (ref-head repo "trunk")
                   (get-in rerun [:receipt :receipt/merge-sha]))))))
      (finally (delete-tree! repo)))))

(deftest rollup-sorry-refuses-to-advance-trunk
  (let [{:keys [repo base] :as fixture} (fixture-repo)]
    (try
      (git repo "checkout" "-q" "solver")
      (write! repo "Library.lean" "theorem libraryFixture : True := by\n  sorry\n")
      (let [source (commit! repo "introduce partial library")
            result (driver/execute! (request (assoc fixture :source source)))]
        (is (false? (:ok result)))
        (is (= :post-merge-rollup-carries-sorry
               (get-in result [:finding :finding])))
        (is (= base (ref-head repo "trunk")))
        (is (= :blocked (get-in result [:receipt :receipt/ruling]))))
      (finally (delete-tree! repo)))))

(deftest post-merge-axiom-mismatch-refuses-to-advance-trunk
  (let [{:keys [repo base] :as fixture} (fixture-repo)]
    (try
      (let [bad (assoc (request fixture) :axiom-command
                       ["bash" "-lc"
                            (str "lean -R . Main.lean >/dev/null && "
                             "printf 'fixture depends on axioms: "
                             "[propext, unsafeAxiom]\\n'")])
            result (driver/execute! bad)]
        (is (false? (:ok result)))
        (is (= :post-merge-axiom-mismatch
               (get-in result [:finding :finding])))
        (is (= base (ref-head repo "trunk")))
        (is (= :blocked (get-in result [:receipt :receipt/ruling]))))
      (finally (delete-tree! repo)))))

(deftest status-is-counted-from-lean-warning-output
  (let [{:keys [repo] :as fixture} (fixture-repo)]
    (try
      (let [request (assoc (request fixture) :ruling :partial-banked
                           :lane-transition {:from :proof :to :library}
                           :receipt/boundary "fixture theorem remains open")
            ;; The status command elaborates the merged Main, then emits one
            ;; compiler-shaped sorry warning. No textual scan of Main occurs.
            request (assoc request :status-command
                           ["bash" "-lc"
                            (str "lean -R . Main.lean >/dev/null && "
                                 "printf 'warning: declaration uses "
                                 "`sorry`\\n' >&2")])
            result (driver/execute! request)]
        (is (:ok result) (pr-str result))
        (is (= 1 (get-in result
                         [:receipt :receipt/status-recomputed :sorry-count])))
        (is (= "partial-banked"
               (get-in result
                       [:receipt :receipt/status-recomputed :classification]))))
      (finally (delete-tree! repo)))))
