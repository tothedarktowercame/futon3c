(ns futon3c.file-change.journal-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.file-change.journal :as journal]))

(defn observation [path content-id at]
  {:repo "futon3c" :worktree "/work/futon3c" :path path
   :content-id content-id :operation :write :observed-at-ms at})

(deftest identical-save-does-not-refill-the-level
  (let [[s1 first-result] (journal/observe (journal/empty-state 3)
                                           (observation "a.clj" "h1" 1770000000000))
        [s2 duplicate-result] (journal/observe s1 (observation "a.clj" "h1" 2))]
    (is (= :appended (:outcome first-result)))
    (is (= :duplicate (:outcome duplicate-result)))
    (is (= 1 (count (:journal s2))))
    (is (= 2 (:next-seq s2)))))

(deftest stalled-cursor-does-not-block-bounded-retention
  (let [s0 (journal/register-consumer (journal/empty-state 2) :registry 0 50
                                      {:authority :registry :hash "initial"})
        [s1 _] (journal/observe s0 (observation "a" "1" 1))
        [s2 _] (journal/observe s1 (observation "b" "2" 2))
        [s3 result] (journal/observe s2 (observation "c" "3" 3))]
    (is (= 2 (count (:journal s3))))
    (is (= 1 (:watermark s3)))
    (is (= 1 (:dropped result)))
    (is (= :rebuild-required (:status (journal/read-next s3 :registry 10))))
    (testing "a fresh authority receipt resumes at the current tail"
      (let [rebuilt (journal/rebuild-consumer s3 :registry 4
                                              {:authority :registry :hash "fresh"})]
        (is (= {:status :ok :entries [] :watermark 1}
               (journal/read-next rebuilt :registry 10)))))))

(deftest expired-consumer-is-dropped-with-rebuild-receipt
  (let [s0 (journal/register-consumer (journal/empty-state 2) :giraffe 10 20
                                      {:authority :giraffe-input :hash "h"})
        [before receipts-before] (journal/expire-consumers s0 30)
        [after receipts-after] (journal/expire-consumers before 31)]
    (is (empty? receipts-before) "the exact deadline remains live")
    (is (= :ok (:status (journal/read-next before :giraffe 1))))
    (is (= [{:consumer-id :giraffe :outcome :expired-and-dropped
             :at-ms 31 :recovery :rebuild-from-authority}]
           receipts-after))
    (is (= :consumer-not-registered
           (:status (journal/read-next after :giraffe 1))))))

(deftest acknowledgement-is-monotone-and-bounded-by-tail
  (let [s0 (journal/register-consumer (journal/empty-state 3) :registry 0 50
                                      {:authority :registry})
        [s1 _] (journal/observe s0 (observation "a" "1" 1))
        s2 (journal/acknowledge s1 :registry 1 2)]
    (is (= 0 (get-in (journal/health s2 2) [:consumers :registry :lag])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (journal/acknowledge s2 :registry 2 3)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (journal/acknowledge s2 :registry 0 3)))))

(deftest authority-reconciliation-recovers-a-missed-change
  (let [[seeded seed-result]
        (journal/reconcile-snapshot
         (journal/empty-state 3)
         {:repo "futon3c" :worktree "/work/futon3c"
          :files {"a.clj" "h1" "gone.clj" "g1"} :observed-at-ms 1})
        ;; No direct observe call represents a missed notification. The next
        ;; authority read must recover both the changed and deleted levels.
        [reconciled result]
        (journal/reconcile-snapshot
         seeded
         {:repo "futon3c" :worktree "/work/futon3c"
          :files {"a.clj" "h2"} :observed-at-ms 2})
        [stable stable-result]
        (journal/reconcile-snapshot
         reconciled
         {:repo "futon3c" :worktree "/work/futon3c"
          :files {"a.clj" "h2"} :observed-at-ms 3})]
    (is (= :seeded (:outcome seed-result)))
    (is (empty? (:journal seeded)) "the initial authority read is not a change storm")
    (is (= {:changed 1 :deleted 1 :appended 2}
           (select-keys result [:changed :deleted :appended])))
    (is (= [["a.clj" :write "h2"] ["gone.clj" :delete "g1"]]
           (mapv (juxt :path :operation :content-id) (:journal reconciled))))
    (is (zero? (:appended stable-result)))
    (is (= (:journal reconciled) (:journal stable)))))
