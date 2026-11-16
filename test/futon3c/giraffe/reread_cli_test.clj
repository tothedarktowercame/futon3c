(ns futon3c.giraffe.reread-cli-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.excursions :as excursions]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.reread-cli :as cli]
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
  (let [calls (atom 0)]
    (with-redefs [queue/reconcile! (fn [& _] {:requests []})
                  queue/requests (constantly [])
                  worker/run-once! (fn [_] {:status :landed})]
      (let [result (cli/run-once!
                    {:reader-seat "reader-1" :store {}
                     :snapshot-fn (fn [_] {:open-items [] :coverage {:members []}})
                     :contact-fn (fn [& _] (throw (Exception. "unused")))
                     :mint-fn (fn [] (swap! calls inc)
                                {:status :minted :run-id "rerated-1"})
                     :mint-state-path (io/file (temp-dir) "mint.edn")
                     :mint-window-ms 1000 :now-ms 100 :new-item-cap 0})]
        (is (= 1 @calls))
        (is (= :minted (get-in result [:mint :status])))))))

(deftest cli-cycle-refuses-without-new-item-cap
  (let [error (try (cli/run-once! {:reader-seat "reader-1"}) nil
                   (catch clojure.lang.ExceptionInfo exc exc))]
    (is (= :giraffe-reread/new-item-cap-required
           (:kind (ex-data error))))))

(deftest usage-pause-skips-only-that-seat-and-expires
  (let [root (temp-dir)
        store (queue/open-store (io/file root "queue.sqlite"))
        pauses (atom {})
        calls (atom [])
        opts {:store store :pointer root :reader-seats ["reader-a"]
              :seat-pauses pauses :parallel? false :new-item-cap 1
              :mint-window-ms 1000 :mint-state-path (io/file root "mint.edn")
              :mint-fn (fn [] (throw (Exception. "no landed reading")))
              :contact-fns {"reader-a" (fn [& _] (swap! calls conj :a)
                                         {:ok false :reason :reader-usage-exhausted :retry-at-ms 200})
                            "reader-b" (fn [& _] (swap! calls conj :b)
                                         {:ok false :reason :agency-job-failed})}}]
    (queue/reconcile! store [{:id "M-toy" :source-pin {:path "/invented" :sha256 "new"}}]
                      {:members []} 1)
    (with-redefs [worker/reading-prompt (fn [& _] "invented prompt")]
      (let [result (cli/drain-once! (assoc opts :now-ms 100))]
        (is (= :reader-usage-exhausted (get-in result [:reader-health "reader-a" :reason])))
        (is (= 200 (get-in result [:reader-health "reader-a" :retry-at-ms]))))
      (cli/drain-once! (assoc opts :now-ms 150))
      (is (= [:a] @calls) "all paused: no claims")
      (cli/drain-once! (assoc opts :now-ms 150 :reader-seats ["reader-a" "reader-b"]))
      (is (= [:a :b] @calls) "other seat still claims")
      (is (= 1 (:attempts (first (queue/requests store)))))
      (cli/drain-once! (assoc opts :now-ms 200))
      (is (= [:a :b :a] @calls) "claimable at retry deadline")
      (is (= 1 (:attempts (first (queue/requests store))))))))

(deftest every-snapshot-checks-fresh
  ;; codex-7 review finding 4: a cache keyed by generation hid changed cited
  ;; evidence. With one checker process per pass, every snapshot rechecks.
  (let [root (doto (io/file (System/getProperty "java.io.tmpdir")
                            (str "c13-cov-" (System/nanoTime))) .mkdirs)
        gen (doto (io/file root "gen") .mkdirs)
        pointer (io/file root "corpus")
        calls (atom 0)]
    (java.nio.file.Files/createSymbolicLink
     (.toPath pointer) (.toPath gen) (make-array java.nio.file.attribute.FileAttribute 0))
    (with-redefs [readings/consume (fn [_] (swap! calls inc) {:coverage {:members [] :n @calls}})
                  excursions/missions-and-excursions->input (fn [_] {:input {:items []}})]
      (let [a (cli/authority-snapshot {:pointer (str pointer) :edges "unused"})
            b (cli/authority-snapshot {:pointer (str pointer) :edges "unused"})]
        (is (= 2 @calls) "same generation, still checked each time")
        (is (not= (:coverage a) (:coverage b)))))))
