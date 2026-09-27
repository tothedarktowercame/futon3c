(ns futon3c.agency.promise-crash-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files Path]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]))

(def driver "scripts/xiang2000_p2c_driver.clj")

(defn temp-dir []
  (str (Files/createTempDirectory "p2c-crash-" (make-array FileAttribute 0))))

(defn delete-tree! [dir]
  (when (.exists (io/file dir))
    (with-open [paths (Files/walk (.toPath (io/file dir)) (make-array java.nio.file.FileVisitOption 0))]
      (doseq [^Path path (reverse (sort-by #(.getNameCount ^Path %) (iterator-seq (.iterator paths))))]
        (Files/deleteIfExists path)))))

(defn start! [dir & args]
  (let [log (io/file dir (str "child-" (System/nanoTime) ".log"))
        builder (doto (ProcessBuilder. ^java.util.List
                                       (into ["clojure" "-Sdeps"
                                              "{:paths [\"src\" \"resources\" \"library\" \"dev\"]}"
                                              "-M" driver] args))
                  (.directory (io/file "."))
                  (.redirectErrorStream true)
                  (.redirectOutput log))]
    {:process (.start builder) :log log}))

(defn await-file! [file process log]
  (let [deadline (+ (System/nanoTime) (* 30 1000000000))]
    (loop []
      (cond
        (.exists (io/file file)) true
        (not (.isAlive process))
        (throw (ex-info "Crash driver exited before its boundary"
                        {:exit (.exitValue process) :log (slurp log)}))
        (> (System/nanoTime) deadline)
        (throw (ex-info "Crash driver boundary timeout" {:log (slurp log)}))
        :else (do (Thread/sleep 10) (recur))))))

(defn kill-at-boundary! [case dir]
  (let [{:keys [process log]} (start! dir "mutate" case dir)]
    (await-file! (str dir "/ready") process log)
    (.destroyForcibly process)
    (when-not (.waitFor process 10 TimeUnit/SECONDS)
      (throw (ex-info "SIGKILL did not terminate crash driver" {:case case})))
    {:exit (.exitValue process) :log log}))

(defn restart! [dir]
  (let [{:keys [process log]} (start! dir "restart" dir)]
    (when-not (.waitFor process 30 TimeUnit/SECONDS)
      (.destroyForcibly process)
      (throw (ex-info "Restart driver timeout" {:log (slurp log)})))
    (when-not (zero? (.exitValue process))
      (throw (ex-info "Restart driver failed" {:exit (.exitValue process) :log (slurp log)})))
    (edn/read-string (slurp (str dir "/result.edn")))))

(defn crash-report [case]
  (let [dir (temp-dir)]
    (try
      (kill-at-boundary! case dir)
      (restart! dir)
      (finally (delete-tree! dir)))))

(defn truncate! [path]
  (let [text (slurp path)]
    (spit path (subs text 0 (max 1 (quot (count text) 2))))))

(defn difference-roots [report]
  (set (map (fn [{:keys [store path]}] [store (first path)]) (:differences report))))

(defn truncated-report [control store-file]
  (let [dir (temp-dir)]
    (try
      (kill-at-boundary! control dir)
      (truncate! (str dir "/" store-file))
      (restart! dir)
      (finally (delete-tree! dir)))))

(deftest park-made-persist-before-history-currently-disagrees
  (let [report (crash-report "park-made")]
    (is (false? (:equal? report)) (pr-str report))
    (is (:readable? report))
    (is (= [:no-history] (mapv :reason (:issues report))) (pr-str report))
    (is (= #{[:parked :records] [:parked :index] [:parked :coalesced]}
           (difference-roots report)) (pr-str report))))

(deftest park-released-persist-before-history-currently-disagrees
  (let [report (crash-report "park-released")]
    (is (false? (:equal? report)) (pr-str report))
    (is (:readable? report))
    (is (empty? (:issues report)) (pr-str report))
    (is (= #{[:parked :records] [:parked :index]}
           (difference-roots report)) (pr-str report))))

(deftest followup-enqueued-persist-before-history-currently-disagrees
  (let [report (crash-report "followup-enqueued")]
    (is (false? (:equal? report)) (pr-str report))
    (is (:readable? report))
    (is (= [:no-history] (mapv :reason (:issues report))) (pr-str report))
    (is (= #{[:followup :queued] [:followup :dedupe]}
           (difference-roots report)) (pr-str report))))

(deftest truncated-park-is-preserved-but-currently-disagrees
  (let [report (truncated-report "control-park" "parked.edn")]
    (is (:readable? report) (pr-str report))
    (is (false? (:equal? report)) (pr-str report))
    (is (= 1 (get-in report [:corruption-stats :by-store :parked])) (pr-str report))
    (is (= 1 (count (:corrupt-files report))) (pr-str report))
    (is (re-find #"^parked\.edn\.corrupt-" (first (:corrupt-files report))))
    (is (= #{[:parked :records] [:parked :index] [:parked :coalesced]}
           (difference-roots report)) (pr-str report))))

(deftest truncated-followup-boots-and-currently-disagrees
  (let [report (truncated-report "control-followup" "followups.edn")]
    (is (:readable? report) (pr-str report))
    (is (false? (:equal? report)) (pr-str report))
    (is (= 1 (get-in report [:corruption-stats :by-store :followup])) (pr-str report))
    (is (= 1 (count (:corrupt-files report))) (pr-str report))
    (is (re-find #"^followups\.edn\.corrupt-" (first (:corrupt-files report))))
    (is (= #{[:followup :queued] [:followup :dedupe]}
           (difference-roots report)) (pr-str report))))

(deftest sigkill-after-temp-force-keeps-old-file-parseable
  (let [dir (temp-dir)]
    (try
      (kill-at-boundary! "atomic-park-write" dir)
      (let [old-state (edn/read-string (slurp (str dir "/parked.edn")))
            report (restart! dir)]
        (is (map? old-state))
        (is (empty? (:records old-state)) (pr-str old-state))
        (is (:equal? report) (pr-str report)))
      (finally (delete-tree! dir)))))

(deftest clean-kill-with-empty-writer-queue-agrees
  (testing "the marker is emitted only after await-writes! drains the writer"
    (let [report (crash-report "control-park")]
      (is (:readable? report) (pr-str report))
      (is (:equal? report) (pr-str report))
      (is (empty? (:issues report)) (pr-str report))
      (is (empty? (:differences report)) (pr-str report)))))
