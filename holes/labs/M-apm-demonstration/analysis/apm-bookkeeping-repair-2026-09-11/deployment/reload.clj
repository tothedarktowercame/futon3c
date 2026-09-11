(do
  (require 'clojure.edn 'clojure.java.io)
  (let [root "/home/joe/code/futon3c/"
        receipt (str root "holes/labs/M-apm-demonstration/analysis/apm-bookkeeping-repair-2026-09-11/deployment/applied.edn")
        campaign (str root "data/apm-campaigns/jit-all-open-v3/")
        pins {"src/futon3c/apm/promotion_pipeline.clj" "8b88b4ac5d70eca41873086208b88c82290199f9ee0ffa85f4c8d573c0b92180" "src/futon3c/apm/typed_role_submission.clj" "80444c3a6788ee37d81189a9a7de70ae7000f329a1d19de66fbf2b9758b041da" "src/futon3c/apm/live_promotion.clj" "616a23ce32faacd25b845affd79a186e13caa9a6e167e171f6aa119431d0aaec" "src/futon3c/apm/live_proof_phases.clj" "fcb89017e48d623772dcfc304de01b38c6a9b2dadf62f04d28d8824542dbd05c" "src/futon3c/apm/live_learning_phases.clj" "fbf6fc3af6dedad8c200e1669fda24c5039178559e547f1567a9990e75acbe9c" "scripts/apm-read-job.py" "51040a7bd36e7d5a5f312db8dbdaade5b0ccf9eb123d184ea2fd9b233625203a"}
        sha (fn [path] (format "%064x" (java.math.BigInteger. 1
                          (.digest (java.security.MessageDigest/getInstance "SHA-256")
                                   (java.nio.file.Files/readAllBytes
                                    (.toPath (clojure.java.io/file path)))))))
        read-doc #(clojure.edn/read-string (slurp %))]
    (if (.exists (clojure.java.io/file receipt))
      {:status :already-recorded :receipt (read-doc receipt)}
      (try
        (with-open [channel (java.nio.channels.FileChannel/open
                             (.toPath (clojure.java.io/file
                                       (str campaign "coordinator.edn.tick-claim.lock")))
                             (into-array java.nio.file.OpenOption
                                         [java.nio.file.StandardOpenOption/WRITE]))]
          (if-let [lock (.tryLock channel)]
            (try
              (let [state (read-doc (str campaign "coordinator.edn"))
                    drift (vec (for [[path expected] pins
                                     :let [actual (sha (str root path))]
                                     :when (not= expected actual)] path))]
                (cond
                  (:regulator/tick-claim state) {:status :waiting :reason :claimed-tick}
                  (seq drift) {:status :blocked :reason :source-drift :paths drift}
                  :else
                  (do
                    (require 'futon3c.apm.promotion-pipeline :reload)
                    (require 'futon3c.apm.typed-role-submission :reload)
                    (require 'futon3c.apm.live-promotion :reload)
                    (require 'futon3c.apm.live-proof-phases :reload)
                    (require 'futon3c.apm.live-learning-phases :reload)
                    (let [identity ((requiring-resolve 'futon3c.apm.live-promotion/transport-implementation-identity))
                          sample ((requiring-resolve 'futon3c.apm.typed-role-submission/command)
                                  {:role :student :agency-base "http://agency-check.invalid:9999"
                                   :submission/token "diagnostic-not-a-token"}
                                  {:job-id "diagnostic-no-dispatch"})
                          result {:status :loaded :source-commit "17720cc6"
                                  :at (str (java.time.Instant/now))
                                  :frame (get-in (read-doc (str campaign "queue-state.edn"))
                                                 [:active :frame :frame/id])
                                  :tick-lock-held true :tick-claim-absent true
                                  :source-pins pins
                                  :promotion-runtime-matches-source?
                                  (= (:source-id identity) (:loaded-runtime-id identity))
                                  :configured-address-in-role-command?
                                  (.contains sample "--agency-base 'http://agency-check.invalid:9999'")}]
                      (spit receipt (str (pr-str result) "\n"))
                      result))))
              (finally (.release lock)))
            {:status :waiting :reason :tick-lock-busy}))
        (catch java.nio.channels.OverlappingFileLockException _
          {:status :waiting :reason :tick-lock-busy})))))
