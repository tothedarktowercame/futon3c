(ns futon3c.evidence.threads-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.evidence.store :as store]
            [futon3c.evidence.threads :as threads]
            [futon3c.social.test-fixtures :as fix]))

(use-fixtures
  :each
  (fn [f]
    (store/reset-store!)
    (f)))

(deftest project-thread-nil-for-nonexistent-subject
  (testing "project-thread returns nil when subject has no entries"
    (is (nil? (threads/project-thread store/!store (fix/make-artifact-ref :mission "M-none"))))))

(deftest project-thread-groups-entries
  (testing "project-thread groups entries for a subject with a goal"
    (let [subj (fix/make-artifact-ref :mission "M1")]
      (store/append! {:evidence-id "g" :subject subj :type :coordination :claim-type :goal :author "a" :body {} :tags [:t]})
      (store/append! {:evidence-id "s1" :subject subj :type :coordination :claim-type :step :author "b" :body {} :tags [:t]
                     :in-reply-to "g"})
      (store/append! {:evidence-id "e1" :subject subj :type :coordination :claim-type :evidence :author "a" :body {} :tags [:t]
                     :in-reply-to "s1"})
      (let [tp (threads/project-thread store/!store subj)]
        (is (= 3 (:thread/entry-count tp)))
        (is (= #{"a" "b"} (:thread/participants tp)))
        (is (= "g" (get-in tp [:thread/goal :evidence/id])))))))

(deftest thread-status-closed-when-conclusion-exists
  (testing "thread-status is :closed when a conclusion entry exists"
    (let [subj (fix/make-artifact-ref :mission "M1")]
      (store/append! {:evidence-id "g" :subject subj :type :coordination :claim-type :goal :author "a" :body {} :tags [:t]})
      (store/append! {:evidence-id "c" :subject subj :type :coordination :claim-type :conclusion :author "a" :body {} :tags [:t]
                     :in-reply-to "g"})
      (let [tp (threads/project-thread store/!store subj)]
        (is (= :closed (threads/thread-status tp)))))))

(deftest thread-status-open-for-active-thread
  (testing "thread-status is :open for a thread with no conclusion/fork and recent activity"
    (let [subj (fix/make-artifact-ref :mission "M1")]
      (store/append! {:evidence-id "g" :subject subj :type :coordination :claim-type :goal :author "a" :body {} :tags [:t]})
      (store/append! {:evidence-id "s" :subject subj :type :coordination :claim-type :step :author "a" :body {} :tags [:t]
                     :in-reply-to "g"})
      (let [tp (threads/project-thread store/!store subj)]
        (is (= :open (threads/thread-status tp)))))))

(deftest thread-forks-produce-branch-projections
  (testing "thread-forks returns projections for fork branches"
    (let [subj (fix/make-artifact-ref :mission "M1")]
      (store/append! {:evidence-id "g" :subject subj :type :coordination :claim-type :goal :author "a" :body {} :tags [:t]})
      (store/append! {:evidence-id "s" :subject subj :type :coordination :claim-type :step :author "a" :body {} :tags [:t]
                     :in-reply-to "g"})
      (store/append! {:evidence-id "f1" :subject subj :type :coordination :claim-type :step :author "b" :body {} :tags [:t]
                     :fork-of "s" :in-reply-to "s"})
      (store/append! {:evidence-id "f1c" :subject subj :type :coordination :claim-type :evidence :author "b" :body {} :tags [:t]
                     :in-reply-to "f1"})
      (let [tp (threads/project-thread store/!store subj)
            forks (threads/thread-forks tp)]
        (is (= 1 (count forks)))
        (is (= #{ "g" "s" "f1" "f1c"} (set (map :evidence/id (get (first forks) :thread/entries)))))
        (is (= :forked (:thread/status (first forks))))))))

(deftest conjectures-open-and-confirmed
  (testing "thread-conjectures tracks open and confirmed lifecycle"
    (let [subj (fix/make-artifact-ref :mission "M1")]
      (store/append! {:evidence-id "g" :subject subj :type :coordination :claim-type :goal :author "a" :body {} :tags [:t]})
      (store/append! {:evidence-id "cj" :subject subj :type :conjecture :claim-type :conjecture :author "a" :body {} :tags [:t]
                     :in-reply-to "g" :conjecture? true})
      (let [tp (threads/project-thread store/!store subj)
            cs (threads/thread-conjectures tp)]
        (is (= 1 (count cs)))
        (is (= :open (:status (first cs)))))
      (store/append! {:evidence-id "concl" :subject subj :type :coordination :claim-type :conclusion :author "b" :body {} :tags [:t]
                     :in-reply-to "cj"})
      (let [tp (threads/project-thread store/!store subj)
            cs (threads/thread-conjectures tp)]
        (is (= :confirmed (:status (first cs))))))))

