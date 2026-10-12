(ns futon3c.agency.turn-queue-journal-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.turn-queue :as turn-queue])
  (:import [java.io RandomAccessFile]
           [java.nio.file Files]))

(defn- empty-state [] (#'turn-queue/empty-state))
(defn- persist! [state] (#'turn-queue/persist! state))
(defn- load-state [] (#'turn-queue/load-state))

(defn- with-store [f]
  (let [dir (.toFile (Files/createTempDirectory "turn-queue-journal-"
                                                 (make-array java.nio.file.attribute.FileAttribute 0)))
        store (io/file dir "queue.edn")]
    (with-redefs [turn-queue/queue-store-path (constantly (.getAbsolutePath store))]
      (try (f store (io/file (str (.getAbsolutePath store) ".journal")))
           (finally
             (doseq [file (reverse (file-seq dir))] (.delete file)))))))

(defn- entry [id n prompt-size]
  {:id id :to "agent" :from "joe" :surface "bell" :msg-id (str "m-" id)
   :seq n :status (if (zero? (mod n 3)) :processed :queued)
   :prompt (apply str (repeat prompt-size (char (+ 65 (mod n 26)))))} )

(defn- mutate [state n]
  (let [id (str "turn-" (mod n 37))
        item {:id id :n n :result (str "result-" n)}]
    (-> state
        (assoc-in [:entries id] (entry id n 80))
        (assoc-in [:seqs "agent"] n)
        (update :history #(->> (conj (vec %) item) (take-last 200) vec)))))

(deftest randomized-round-trip-replays-exactly
  (with-store
    (fn [_ _]
      (let [rng (java.util.Random. 19)
            final (reduce (fn [state n]
                            (let [state (mutate state (+ n (.nextInt rng 10000)))
                                  state (if (zero? (mod n 11))
                                          (update state :entries dissoc (str "turn-" (.nextInt rng 37)))
                                          state)]
                              (persist! state)
                              state))
                          (empty-state) (range 260))]
        (is (= final (load-state)))
        (is (= 200 (count (:history final))) "history crossed and retained its cap")))))

(deftest write-volume-is-sublinear
  (with-store
    (fn [store journal]
      (let [blob (apply str (repeat 10500 "x"))
            entries (into {} (map (fn [n] [(str "e-" n) (entry (str "e-" n) n 10500)])
                                  (range 225)))
            history (mapv (fn [n] {:id (str "e-" n) :prompt blob :status :processed})
                          (range 200))
            initial (assoc (empty-state) :entries entries :history history)]
        (persist! initial)
        (let [snapshot-size (.length store)]
          (loop [state initial n 0]
            (when (< n 100)
              (let [id (str "e-" (mod n 225))
                    next (-> state
                             (assoc-in [:entries id :status] (keyword (str "changed-" n)))
                             (update :history #(conj (subvec % 1) {:id id :result n})))]
                (persist! next)
                (recur next (inc n)))))
          (let [ratio (/ (double (.length journal)) (* 100.0 snapshot-size))]
            (println (format "turn-queue journal write-volume ratio: %.4f%%" (* 100 ratio)))
            (is (< ratio 0.03))))))))

(deftest torn-tail-is-ignored-and-corrupt-middle-falls-back-empty
  (testing "torn tail"
    (with-store
      (fn [_ journal]
        (let [s0 (assoc-in (empty-state) [:entries "padding"] (entry "padding" 0 20000))
              s1 (mutate s0 1) s2 (mutate s1 2)]
          (persist! s0)
          (persist! s1)
          (let [before-last (.length journal)]
            (persist! s2)
            (with-open [raf (RandomAccessFile. journal "rw")]
              (.setLength raf (+ before-last (quot (- (.length journal) before-last) 2))))
            (is (= s1 (load-state)) "torn final record is ignored"))))))
  (testing "corrupt middle"
    (with-store
      (fn [_ journal]
        (let [s0 (assoc-in (empty-state) [:entries "padding"] (entry "padding" 0 20000))]
          (persist! s0)
          (persist! (mutate s0 1))
          (persist! (mutate (mutate s0 1) 2))
          (let [lines (str/split-lines (slurp journal))]
            (spit journal (str/join "\n" (concat [(first lines) "{broken"] (drop 2 lines) [""])))
            (is (= (empty-state) (load-state))
                "corrupt middle line follows corrupt-store fallback")))))))

(deftest every-rotation-crash-point-loads-pre-or-post-state
  (doseq [stage [:snapshot-temp :snapshot-rename :journal-temp :journal-rename]]
    (testing (name stage)
      (with-store
        (fn [_ _]
          (let [before (mutate (empty-state) 1)
                after (mutate before 2)]
            (persist! before)
            (swap! @#'turn-queue/!journal-state dissoc
                   (str (.toAbsolutePath (java.nio.file.Path/of
                                          (turn-queue/queue-store-path)
                                          (make-array String 0)))))
            (binding [turn-queue/*persist-stage-hook*
                      (fn [seen] (when (= seen stage) (throw (ex-info "crash" {}))))]
              (persist! after))
            (is (contains? #{before after} (load-state)))))))))

(deftest failed-append-forces-next-write-to-snapshot
  (with-store
    (fn [_ _]
      (let [s0 (empty-state) s1 (mutate s0 1) s2 (mutate s1 2)]
        (persist! s0)
        (with-redefs-fn {#'turn-queue/append-journal!
                         (fn [_] (throw (ex-info "forced append failure" {})))}
          #(persist! s1))
        (persist! s2)
        (is (= s2 (load-state)))))))

(deftest old-format-and-draining-reset
  (with-store
    (fn [store _]
      (let [old (-> (empty-state) (assoc :entries {"old" {:id "old"}})
                    (assoc :draining #{"agent"}))]
        (spit store (pr-str old))
        (is (= (assoc old :draining #{}) (load-state)))
        (let [next (assoc old :entries {"new" {:id "new"}})]
          (persist! next)
          (is (= (assoc next :draining #{}) (load-state))))))))
