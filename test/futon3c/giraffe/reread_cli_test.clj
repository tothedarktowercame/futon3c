(ns futon3c.giraffe.reread-cli-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.reread-cli :as cli]
            [futon3c.giraffe.reread-mint :as mint]
            [futon3c.giraffe.reread-queue :as queue]
            [futon3c.giraffe.reread-worker :as worker]
            [futon3c.giraffe.run :as giraffe-run])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "reread-cli-"
                                      (make-array FileAttribute 0))))

(deftest health-lists-actionable-rows-generation-and-last-mint
  (let [root (temp-dir)
        generation-dir (doto (io/file root "generation") .mkdirs)
        pointer (io/file root "corpus")
        runs (doto (io/file root "runs") .mkdirs)
        run-dir (doto (io/file runs "run-17") .mkdirs)
        store (queue/open-store (io/file root "queue.sqlite"))]
    (spit (io/file generation-dir "members.json") "[]")
    (Files/createSymbolicLink (.toPath pointer)
                              (.toPath (.getAbsoluteFile generation-dir))
                              (make-array FileAttribute 0))
    (queue/reconcile! store
                      [{:id :M-one :source-pin {:path "/M-one" :sha256 "new"}}]
                      {:members [{:id "M-one" :status :read
                                  :source {:path "/M-one" :sha256 "old"}}]}
                      100)
    (.setLastModified run-dir 900)
    (with-redefs [giraffe-run/newest-field-run
                  (fn [_] {:run-id "run-17" :input-sha256 "sha"})]
      (let [health (cli/health {:store store :pointer pointer
                                :giraffe-root runs :now-ms 1100})]
        (is (= :attention (:status health)))
        (is (= [1000] (mapv :age_ms (:requests health))))
        (is (= (generation/generation-digest generation-dir)
               (get-in health [:generation :digest])))
        (is (= {:run-id "run-17" :at-ms 900} (:last-mint health)))))))

(deftest cli-cycle-refuses-without-explicit-reader-seat
  (testing "refusal precedes authority reads or writes"
    (let [error (try (cli/run-once! {:reader-seat nil}) nil
                     (catch clojure.lang.ExceptionInfo exc exc))]
      (is (= :giraffe-reread/reader-seat-required
             (:kind (ex-data error)))))))

(deftest landed-worker-result-requests-and-awaits-mint
  (let [calls (atom 0)
        coordinator (mint/coordinator
                     (fn [] (swap! calls inc)
                       {:status :minted :run-id "rerated-1"}))]
    (with-redefs [queue/reconcile! (fn [& _] {:requests []})
                  worker/run-once! (fn [_] {:status :landed})]
      (let [result (cli/run-once!
                    {:reader-seat "reader-1" :store {}
                     :snapshot-fn (fn [_] {:open-items [] :coverage {:members []}})
                     :contact-fn (fn [& _] (throw (Exception. "unused")))
                     :mint-coordinator coordinator :now-ms 100})]
        (is (= 1 @calls))
        (is (= "rerated-1" (get-in result [:mint :last-run-id])))))))
