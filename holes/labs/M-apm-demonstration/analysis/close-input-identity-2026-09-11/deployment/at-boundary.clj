(do
  (require 'clojure.edn 'clojure.java.io)
(let [root "/home/joe/code/futon3c/"
      campaign (str root "data/apm-campaigns/jit-all-open-v3/")
      receipt (str root "holes/labs/M-apm-demonstration/analysis/close-input-identity-2026-09-11/deployment/applied.edn")
      read-doc #(clojure.edn/read-string (slurp %))]
  (if (.exists (clojure.java.io/file receipt))
    {:status :already-recorded}
    (try
      (with-open [channel (java.nio.channels.FileChannel/open
                          (java.nio.file.Path/of (str campaign "coordinator.edn.tick-claim.lock")
                                                 (make-array String 0))
                          (into-array java.nio.file.OpenOption
                                      [java.nio.file.StandardOpenOption/WRITE]))]
        (if-let [lock (.tryLock channel)]
          (try
            (let [state (read-doc (str campaign "coordinator.edn"))
                  registry (read-doc (str root "data/apm-coordinators/registry.edn"))
                  frame (get-in (read-doc (str campaign "queue-state.edn"))
                                [:active :frame :frame/id])
                  close-file (clojure.java.io/file
                              (str campaign "jit-all-open-v3-" frame "/live/close-frame.edn"))]
              (cond
                (not (get-in registry [:entries "jit-queue:jit-all-open-v3" :coordinator/enabled?]))
                {:status :blocked :reason :coordinator-disabled}
                (:regulator/tick-claim state) {:status :waiting :reason :claimed-tick}
                (or (nil? frame) (= "f221" frame)) {:status :waiting :reason :same-frame}
                (.exists close-file) {:status :blocked :reason :new-frame-close-request-exists}
                :else
                (let [source (slurp (str root "src/futon3c/apm/live_learning_phases.clj"))
                      sha (format "%064x" (java.math.BigInteger.
                                           1 (.digest (java.security.MessageDigest/getInstance "SHA-256")
                                                      (.getBytes source "UTF-8"))))]
                  (if (not= sha "36a769c9c374d4b2abda336c1420589ff67858444ebe4b82755c155e35e40246")
                    {:status :blocked :reason :source-drift :sha256 sha}
                    (do
                      (require 'futon3c.apm.live-learning-phases :reload)
                      (let [result {:status :loaded :frame frame :previous-frame "f221"
                                    :at (str (java.time.Instant/now)) :source-sha256 sha
                                    :source-commit "33b56d65" :tick-lock-held true
                                    :tick-claim-absent true :close-request-absent true}]
                        (spit receipt (str (pr-str result) "\n"))
                        result))))))
            (finally (.release lock)))
          {:status :waiting :reason :tick-lock-busy}))
      (catch java.nio.channels.OverlappingFileLockException _
        {:status :waiting :reason :tick-lock-busy})))))
