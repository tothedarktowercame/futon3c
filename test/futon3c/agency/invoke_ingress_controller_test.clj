(ns futon3c.agency.invoke-ingress-controller-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.invoke-ingress-controller :as ingress]))

(defn- refusal [f] (:refusal (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e)))))

(deftest close-drains-work-with-writer-waiter
  (let [c (ingress/controller {:auth-token "local-secret"})
        entered (promise) release (promise)
        worker (future (let [ticket (ingress/begin-creation! c)]
                         (deliver entered true) @release
                         (ingress/creation-finished! c ticket "job-1")))]
    @entered
    (is (= 1 (:waiting-writer (ingress/close-intake! c))))
    (is (= :ingress/intake-closed (refusal #(ingress/begin-creation! c))))
    (is (false? (ingress/drained? c)))
    (deliver release true) @worker
    (ingress/start-execution! c "job-1")
    (ingress/finish-execution! c "job-1")
    (is (false? (ingress/drained? c)))
    (ingress/finish-delivery! c "job-1")
    (is (ingress/drained? c))))

(deftest deferred-resumes-are-durable-idempotent-and-ordered
  (let [writes (atom []) c (ingress/controller {:auth-token "s" :persist! #(do (swap! writes conj %) true)})]
    (ingress/close-intake! c)
    (is (= "resume-1" (ingress/defer-resume! c "resume-1" {:prompt "p"})))
    (is (= "resume-1" (ingress/defer-resume! c "resume-1" {:prompt "p"})))
    (is (= :ingress/deferred-resume-conflict
           (refusal #(ingress/defer-resume! c "resume-1" {:prompt "changed"}))))
    (is (= [["resume-1" {:prompt "p"}]] (ingress/reopen! c)))
    (is (= [["resume-1" {:prompt "p"}]] (ingress/reopen! c)))
    (ingress/acknowledge-resume! c "resume-1")
    (is (= [] (ingress/reopen! c)))
    (is (= 2 (count @writes)))))

(deftest persistence-failure-does-not-publish-resume
  (let [c (ingress/controller {:auth-token "s" :persist! (constantly false)})]
    (ingress/close-intake! c)
    (is (= :ingress/deferred-persistence-failed
           (refusal #(ingress/defer-resume! c "r" {:x 1}))))
    (is (= 0 (:deferred (ingress/verification-snapshot
                         c {:remote-addr "127.0.0.1" :auth-token "s"}))))))

(deftest verification-lane-is-local-and-authenticated
  (let [c (ingress/controller {:auth-token "correct"})]
    (is (= :ingress/verification-not-loopback
           (refusal #(ingress/verification-snapshot c {:remote-addr "192.0.2.1"
                                                        :auth-token "correct"}))))
    (is (= :ingress/verification-unauthorized
           (refusal #(ingress/verification-snapshot c {:remote-addr "127.0.0.1"
                                                        :auth-token "wrong"}))))
    (is (true? (:drained? (ingress/verification-snapshot
                            c {:remote-addr "::1" :auth-token "correct"}))))))
