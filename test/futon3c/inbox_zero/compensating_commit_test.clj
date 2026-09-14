(ns futon3c.inbox-zero.compensating-commit-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.promote-exec :as executor]
            [futon3c.inbox-zero.board-consumer :as consumer]
            [futon3c.inbox-zero.compensating-commit :as comp])
  (:import [java.nio.file Files] [java.nio.file.attribute FileAttribute]))

(defn git! [root & args]
  (let [r (apply shell/sh (concat ["git" "-C" root] args))]
    (when-not (zero? (:exit r)) (throw (ex-info "Fixture Git failed" r)))
    (str/trim (:out r))))

(defn fixture [f]
  (let [dir (.toFile (Files/createTempDirectory "inbox-zero-compensation-" (make-array FileAttribute 0)))
        root (.getPath dir) path (io/file dir "README.md")]
    (try
      (git! root "init" "-q")
      (git! root "config" "user.name" "Compensation fixture")
      (git! root "config" "user.email" "fixture@example.invalid")
      (spit path "base\n")
      (git! root "add" "README.md")
      (git! root "commit" "-qm" "base")
      (let [base (git! root "rev-parse" "HEAD") records (atom [])]
        (spit path "reviewed work\n")
        (f {:root root :path path :base base :records records
            :plan {:verdict :proposed :include [{:path "README.md" :git/status :modified}]}
            :options {:repo-root root :message "Fixture reviewed work"
                      :reviewed-blobs {"README.md" (comp/blob-id root "README.md")}
                      :idle-check! (fn []) :record! #(swap! records conj %)
                      :certificate {:fixture true}}}))
      (finally (doseq [p (reverse (file-seq dir))] (io/delete-file p))))))

(deftest strict-caller-still-refuses
  (is (= :atomic-feel-commit-unavailable
         (:held/reason (consumer/atomic-commit! {} {:safety :compensating}))))
  (is (= 5000 comp/detector-response-budget-ms))
  (is (false? (:hard-real-time-guarantee? comp/assumptions))))

(deftest ^:slow quiet-plan-survives-with-measured-postcheck
  (fixture
   (fn [{:keys [root plan options records]}]
     (let [r (comp/execute! plan options)]
       (is (= :committed (:verdict r)))
       (is (:compensating/verified? r))
       (is (<= (get-in r [:postcheck :elapsed-ms]) 5000))
       (is (= "" (git! root "status" "--porcelain")))
       (is (= :inbox-zero/commit-witness (:record/type (last @records))))))))

(deftest ^:slow edit-after-snapshot-is-reverted-without-losing-the-edit
  (doseq [where [:before-stage :after-commit :edit-and-restore]]
    (fixture
     (fn [{:keys [root path base records plan options]}]
       (let [real-execute executor/execute-plan!
             r (with-redefs [executor/execute-plan!
                             (fn [p opts]
                               (when (contains? #{:before-stage :edit-and-restore} where)
                                 (spit path "concurrent edit\n"))
                               (when (= :edit-and-restore where) (spit path "reviewed work\n"))
                               (let [result (real-execute p opts)]
                                 (when (= :after-commit where) (spit path "concurrent edit\n"))
                                 result))]
                 (comp/execute! plan options))]
         (is (= :compensated (:verdict r)) (pr-str r))
         (is (= :raced-with-edit (:finding/type r)))
         (is (= (git! root "rev-parse" (str base "^{tree}")) (git! root "rev-parse" "HEAD^{tree}")))
         (is (= (if (= :edit-and-restore where) "reviewed work\n" "concurrent edit\n") (slurp path)))
         (is (get-in r [:compensation :index/preserved?]))
         (is (seq (get-in r [:postcheck :paths])))
         (is (= :inbox-zero/finding (:record/type (last @records)))))))))

(deftest ^:slow inconclusive-detector-also-compensates
  (doseq [mode [:timeout :overflow]]
    (fixture
     (fn [{:keys [root base plan options]}]
       (let [drain comp/drain! calls (atom 0)
             r (with-redefs [comp/drain! (fn [d]
                                          (if (= 2 (swap! calls inc))
                                            (if (= :timeout mode)
                                              (throw (ex-info "deadline" {:reason :detector-timeout}))
                                              {:complete true :invalid true :event_count 1 :events []})
                                            (drain d)))]
                 (comp/execute! plan options))]
         (is (= :compensated (:verdict r)))
         (is (= :detector-inconclusive (:finding/type r)))
         (is (= (git! root "rev-parse" (str base "^{tree}")) (git! root "rev-parse" "HEAD^{tree}"))))))))

(deftest ^:slow moved-head-is-not-rewritten
  (fixture
   (fn [{:keys [root plan options]}]
     (let [real-execute executor/execute-plan! other (atom nil)
           r (with-redefs [executor/execute-plan!
                           (fn [p opts]
                             (let [r (real-execute p opts)]
                               (git! root "commit" "--allow-empty" "-qm" "other writer")
                               (reset! other (git! root "rev-parse" "HEAD")) r))]
               (comp/execute! plan options))]
       (is (= :compensation-failed (:verdict r)))
       (is (= @other (git! root "rev-parse" "HEAD")))
       (is (= :compensation-head-moved (get-in r [:compensation :data :reason])))))))

(deftest ^:slow wrapper-failure-after-git-commit-is-compensated
  (fixture
   (fn [{:keys [root base plan options]}]
     (let [real-execute executor/execute-plan!
           r (with-redefs [executor/execute-plan! (fn [p opts]
                                                  (real-execute p opts)
                                                  (throw (Exception. "wrapper failed")))]
               (comp/execute! plan options))]
       (is (= :compensated (:verdict r)))
       (is (= :detector-inconclusive (:finding/type r)))
       (is (= (git! root "rev-parse" (str base "^{tree}")) (git! root "rev-parse" "HEAD^{tree}")))))))
