(ns futon3c.agency.invoke-ingress-controller-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.invoke-ingress-controller :as ingress])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files StandardOpenOption)
           (java.util.concurrent TimeUnit)))

(defn- refusal [f] (:refusal (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e)))))

(deftest close-drains-work-with-writer-waiter
  (let [c (ingress/controller {:auth-token "local-secret" :test-only? true})
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
  (let [writes (atom [])
        store {:acquire! (fn [] :lease) :release! (fn [_] true)
               :read! (fn [_] {:schema ingress/deferred-schema :order [] :records {}})
               :persist! (fn [_ p] (swap! writes conj p) true)}
        c (ingress/controller {:auth-token "s" :deferred-store store})]
    (ingress/close-intake! c)
    (is (= "resume-1" (ingress/defer-resume! c "resume-1" {:requested-job-id "resume-1" :prompt "p"})))
    (is (= "resume-1" (ingress/defer-resume! c "resume-1" {:requested-job-id "resume-1" :prompt "p"})))
    (is (= :ingress/deferred-resume-conflict
           (refusal #(ingress/defer-resume! c "resume-1" {:requested-job-id "resume-1" :prompt "changed"}))))
    (is (= [["resume-1" {:requested-job-id "resume-1" :prompt "p"}]] (ingress/reopen! c)))
    (is (= [["resume-1" {:requested-job-id "resume-1" :prompt "p"}]] (ingress/reopen! c)))
    (ingress/acknowledge-resume! c "resume-1")
    (is (= [] (ingress/reopen! c)))
    (is (= 2 (count @writes)))))

(deftest persistence-failure-does-not-publish-resume
  (let [store {:acquire! (fn [] :lease) :release! (fn [_] true)
               :read! (fn [_] {:schema ingress/deferred-schema :order [] :records {}})
               :persist! (fn [_ _] false)}
        c (ingress/controller {:auth-token "s" :deferred-store store})]
    (ingress/close-intake! c)
    (is (= :ingress/deferred-persistence-failed
           (refusal #(ingress/defer-resume! c "r" {:requested-job-id "r" :x 1}))))
    (is (= 0 (:deferred (ingress/verification-snapshot
                         c {:remote-addr "127.0.0.1" :auth-token "s"}))))))

(deftest verification-lane-is-local-and-authenticated
  (let [c (ingress/controller {:auth-token "correct" :test-only? true})]
    (is (= :ingress/verification-not-loopback
           (refusal #(ingress/verification-snapshot c {:remote-addr "192.0.2.1"
                                                        :auth-token "correct"}))))
    (is (= :ingress/verification-unauthorized
           (refusal #(ingress/verification-snapshot c {:remote-addr "127.0.0.1"
                                                        :auth-token "wrong"}))))
    (let [snapshot (ingress/verification-snapshot
                    c {:remote-addr "::1" :auth-token "correct"})]
      (is (true? (:drained? snapshot)))
      (is (false? (:restart-authorized? snapshot))))))

(deftest durable-recovery-retries-before-ack-and-not-after
  (let [dir (Files/createTempDirectory "ingress-recovery-" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "deferred.edn")
        store (ingress/file-deferred-store path)]
    (ingress/initialize-file-store! store)
    (let [c1 (ingress/controller {:auth-token "s" :deferred-store store})]
      (ingress/close-intake! c1)
      (ingress/defer-resume! c1 "park-7" {:requested-job-id "park-7" :prompt "resume"})
      (ingress/release-controller! c1))
    (let [c2 (ingress/controller {:auth-token "s" :deferred-store store})]
      (ingress/close-intake! c2)
      (is (= [["park-7" {:requested-job-id "park-7" :prompt "resume"}]]
             (ingress/reopen! c2)))
      (ingress/release-controller! c2)
      ;; Crash-before-ack: a fresh controller returns the identical stable pair.
      (let [c3 (ingress/controller {:auth-token "s" :deferred-store store})]
        (ingress/close-intake! c3)
        (is (= [["park-7" {:requested-job-id "park-7" :prompt "resume"}]]
               (ingress/reopen! c3)))
        (ingress/acknowledge-resume! c3 "park-7")
        (ingress/release-controller! c3)))
    (let [c4 (ingress/controller {:auth-token "s" :deferred-store store})]
      (ingress/close-intake! c4)
      (is (= [] (ingress/reopen! c4)))
      (ingress/release-controller! c4))))

(deftest corrupt-store-and-missing-store-refuse
  (let [dir (Files/createTempDirectory "ingress-corrupt-" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "deferred.edn")
        store (ingress/file-deferred-store path)]
    (is (= :ingress/deferred-store-missing
           (refusal #(ingress/controller {:auth-token "s" :deferred-store store}))))
    (ingress/initialize-file-store! store)
    (Files/write path (.getBytes "{:schema :agency/deferred-resume-envelope-v1 :projection {:schema :agency/deferred-resumes-v1 :order [] :records {}} :projection-sha256 \"wrong\"}"
                                 StandardCharsets/UTF_8)
                 (into-array StandardOpenOption [StandardOpenOption/TRUNCATE_EXISTING
                                                  StandardOpenOption/WRITE]))
    (is (= :ingress/deferred-digest-mismatch
           (refusal #(ingress/controller {:auth-token "s" :deferred-store store}))))))

(deftest store-is-required-outside-explicit-test-mode
  (is (= :ingress/deferred-store-required
         (refusal #(ingress/controller {:auth-token "s"}))))
  (is (map? (ingress/controller {:auth-token "s" :test-only? true}))))

(deftest invalid-payload-preserves-prior-evidence
  (let [dir (Files/createTempDirectory "ingress-payload-" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "deferred.edn") store (ingress/file-deferred-store path)]
    (ingress/initialize-file-store! store)
    (let [c (ingress/controller {:auth-token "s" :deferred-store store})]
      (ingress/close-intake! c)
      (ingress/defer-resume! c "good" {:requested-job-id "good" :prompt "kept"})
      (is (= :ingress/deferred-payload-unsupported
             (refusal #(ingress/defer-resume!
                        c "bad" {:requested-job-id "bad" :value (Object.)}))))
      (ingress/release-controller! c))
    (let [recovered (ingress/controller {:auth-token "s" :deferred-store store})]
      (ingress/close-intake! recovered)
      (is (= [["good" {:requested-job-id "good" :prompt "kept"}]]
             (ingress/reopen! recovered)))
      (ingress/release-controller! recovered))))

(deftest competing-owner-and-initialization-refuse-without-write
  (let [dir (Files/createTempDirectory "ingress-owner-" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "deferred.edn") store-a (ingress/file-deferred-store path)
        store-b (ingress/file-deferred-store path)]
    (ingress/initialize-file-store! store-a)
    (let [c (ingress/controller {:auth-token "s" :deferred-store store-a})]
      (ingress/close-intake! c)
      (ingress/defer-resume! c "a" {:requested-job-id "a" :prompt "a"})
      (is (= :ingress/deferred-store-owned
             (refusal #(ingress/controller {:auth-token "s" :deferred-store store-b}))))
      (is (= :ingress/deferred-store-owned
             (refusal #(ingress/initialize-file-store! store-b))))
      (ingress/release-controller! c)
      (is (= :ingress/controller-released
             (refusal #(ingress/defer-resume! c "late" {:requested-job-id "late"})))))
    (is (= :ingress/deferred-store-already-exists
           (refusal #(ingress/initialize-file-store! store-b))))
    (let [c (ingress/controller {:auth-token "s" :deferred-store store-b})]
      (ingress/close-intake! c)
      (is (= [["a" {:requested-job-id "a" :prompt "a"}]] (ingress/reopen! c)))
      (ingress/release-controller! c))))

(deftest competing-cross-process-owner-refuses
  (let [dir (Files/createTempDirectory "ingress-process-owner-" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "deferred.edn") store (ingress/file-deferred-store path)]
    (ingress/initialize-file-store! store)
    (let [c (ingress/controller {:auth-token "parent" :deferred-store store})
          form (str "(require '[futon3c.agency.invoke-ingress-controller :as i]) "
                    "(try (i/controller {:auth-token \"child\" :deferred-store "
                    "(i/file-deferred-store " (pr-str (str path)) ")}) "
                    "(System/exit 20) "
                    "(catch clojure.lang.ExceptionInfo e "
                    "(System/exit (if (= :ingress/deferred-store-owned (:refusal (ex-data e))) 23 24))))")
          process (.start (doto (ProcessBuilder. (into-array String ["clojure" "-M" "-e" form]))
                            (.directory (java.io.File. (System/getProperty "user.dir")))
                            (.redirectErrorStream true)))]
      (is (.waitFor process 15 TimeUnit/SECONDS))
      (is (= 23 (.exitValue process)))
      (ingress/release-controller! c))))
