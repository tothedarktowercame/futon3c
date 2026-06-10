(ns futon3c.aif.calibration
  "Calibration evidence reader for play-out -> outcome records."
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.aif.discipline-events :as discipline]))

(def ^:private home (System/getProperty "user.home"))

(def default-paths
  ;; All absolute (anchored at user.home) — the harness must read the same
  ;; evidence regardless of caller cwd (CLI from futon3c vs the serving JVM).
  {:traces-dir (str home "/code/futon3c/data/repl-traces")
   :pilots-log (str home "/code/futon3c/holes/PILOTS-LOG.md")
   :closure-folds (str home "/code/futon6/holes/closure-folds.edn")
   :ch2-events (str home "/code/futon3a/data/ch2-discharge-events.edn")
   :discipline-events (str home "/code/futon3c/data/discipline-events.edn")})

(defn- warn [source message]
  {:source (str source) :warning message})

(defn- parse-number [x]
  (cond
    (number? x) (double x)
    (string? x) (let [s (-> x
                            (str/replace "−" "-")
                            (str/replace #"[,)]$" "")
                            str/trim)]
                  (try (Double/parseDouble s)
                       (catch Throwable _ nil)))
    :else nil))

(defn- abs-error [pred realised explicit]
  (or (parse-number explicit)
      (when (and (number? pred) (number? realised))
        (Math/abs (- realised pred)))))

(defn- maps-in [x]
  (filter map? (tree-seq coll? seq x)))

(defn- read-edn-file [path warnings]
  (let [file (io/file path)]
    (cond
      (not (.exists file))
      (do (swap! warnings conj (warn path "missing source")) nil)

      :else
      (try (edn/read-string (slurp file))
           (catch Throwable t
             (swap! warnings conj (warn path (str "unreadable source: " (.getMessage t))))
             nil)))))

(defn- gamma-records [traces-dir warnings]
  (let [dir (io/file traces-dir)]
    (cond
      (not (.exists dir))
      (do (swap! warnings conj (warn traces-dir "missing source")) [])

      (not (.isDirectory dir))
      (do (swap! warnings conj (warn traces-dir "not a directory")) [])

      :else
      (mapcat
       (fn [file]
         (try
           (let [data (edn/read-string (slurp file))
                 run-id (:run-id data)
                 date (:date data)]
             (->> (maps-in data)
                  (filter #(or (contains? % :predicted-discharge)
                               (contains? % :realised-discharge)
                               (contains? % :prediction-error)))
                  (map (fn [m]
                         (let [pred (parse-number (:predicted-discharge m))
                               real (parse-number (:realised-discharge m))]
                           {:kind :gamma-frame
                            :at (or (:at m) date)
                            :predicted pred
                            :realised real
                            :error (abs-error pred real (:prediction-error m))
                            :success nil
                            :source (.getPath file)
                            :ref (or (:run-id m) run-id)})))))
           (catch Throwable t
             (swap! warnings conj (warn (.getPath file) (str "unreadable source: " (.getMessage t))))
             [])))
       (sort-by #(.getName %) (filter #(.isFile %) (file-seq dir)))))))

(def ^:private pilot-g-re
  #"(?i)(?:predicted\s+G\s*=\s*([-+−]?[0-9]+(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?)).*?(?:realised\s+G\s*=\s*([-+−]?[0-9]+(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?))")

(defn- pilots-records [path warnings]
  (let [file (io/file path)]
    (cond
      (not (.exists file))
      (do (swap! warnings conj (warn path "missing source")) [])

      :else
      (try
        (->> (str/split-lines (slurp file))
             (keep-indexed
              (fn [idx line]
                (when-let [[_ p r] (re-find pilot-g-re line)]
                  (let [pred (parse-number p)
                        real (parse-number r)]
                    {:kind :pilots-log-turn
                     :at nil
                     :predicted pred
                     :realised real
                     :error (abs-error pred real nil)
                     :success nil
                     :source (str path)
                     :ref (str "line-" (inc idx))}))))
             vec)
        (catch Throwable t
          (swap! warnings conj (warn path (str "unreadable source: " (.getMessage t))))
          [])))))

(defn- closure-records [path warnings]
  (if-let [data (read-edn-file path warnings)]
    (->> data
         (filter map?)
         (mapv (fn [m]
                 {:kind :closure-fold
                  :at nil
                  :predicted nil
                  :realised nil
                  :error nil
                  :success (when (contains? m :success) (boolean (:success m)))
                  :source (str path)
                  :ref (:scope m)})))
    []))

(defn- edn-lines [path warnings]
  (let [file (io/file path)]
    (cond
      (not (.exists file))
      (do (swap! warnings conj (warn path "missing source")) [])

      :else
      (try
        (with-open [r (io/reader file)]
          (->> (line-seq r)
               (remove str/blank?)
               (map edn/read-string)
               vec))
        (catch Throwable t
          (swap! warnings conj (warn path (str "unreadable source: " (.getMessage t))))
          [])))))

(defn- ch2-records [path warnings]
  (->> (edn-lines path warnings)
       (filter map?)
       (mapv (fn [m]
               {:kind :ch2-discharge
                :at (:at m)
                :predicted nil
                :realised nil
                :error nil
                :success (when (contains? m :discharged?) (boolean (:discharged? m)))
                :source (str path)
                :ref (:move/id m)}))))

(defn- discipline-records [path warnings]
  (let [file (io/file path)]
    (if-not (.exists file)
      []
      (try
        (mapv (fn [m]
                {:kind :discipline-event
                 :at (:at m)
                 :predicted (parse-number (:predicted m))
                 :realised nil
                 :error nil
                 :success false
                 :source (str path)
                 :ref (:run-id m)})
              (discipline/read-events path))
        (catch Throwable t
          (swap! warnings conj (warn path (str "unreadable source: " (.getMessage t))))
          [])))))

(defn load-evidence
  "Load normalized calibration evidence. Missing/unreadable sources are warnings."
  ([] (load-evidence default-paths))
  ([{:keys [traces-dir pilots-log closure-folds ch2-events discipline-events]
     :or {traces-dir (:traces-dir default-paths)
          pilots-log (:pilots-log default-paths)
          closure-folds (:closure-folds default-paths)
          ch2-events (:ch2-events default-paths)
          discipline-events (:discipline-events default-paths)}}]
   (let [warnings (atom [])
         sourced [[:gamma-frame traces-dir (gamma-records traces-dir warnings)]
                  [:pilots-log-turn pilots-log (pilots-records pilots-log warnings)]
                  [:closure-fold closure-folds (closure-records closure-folds warnings)]
                  [:ch2-discharge ch2-events (ch2-records ch2-events warnings)]
                  [:discipline-event discipline-events (discipline-records discipline-events warnings)]]
         ;; A source that exists but yields zero records is a coverage gap that
         ;; must not look identical to a healthy parse (no silent caps): warn.
         _ (doseq [[kind path records] sourced
                   :when (and (empty? records)
                              (.exists (io/file path))
                              (not-any? #(= (str path) (:source %)) @warnings))]
             (swap! warnings conj (warn path (str "source present, 0 " (name kind) " records parsed"))))
         records (vec (mapcat peek sourced))]
     (with-meta records {:warnings @warnings}))))

(defn- mean [xs]
  (when (seq xs)
    (/ (reduce + 0.0 xs) (double (count xs)))))

(defn calibration-report [evidence]
  (let [warnings (vec (:warnings (meta evidence)))
        n-by-kind (frequencies (map :kind evidence))
        paired (filter #(and (number? (:predicted %))
                             (number? (:realised %)))
                       evidence)
        errors (mapv #(double (or (:error %) (Math/abs (- (:realised %) (:predicted %))))) paired)
        zero-count (count (filter zero? errors))
        nonzero-count (- (count errors) zero-count)
        near-zero-count (count (filter #(< (Math/abs (double %)) 1.0e-3) errors))
        paired-count (count paired)
        degenerate? (and (pos? paired-count)
                         (> (/ near-zero-count (double paired-count)) 0.8))
        verdict (cond
                  (< paired-count 10) :insufficient-evidence
                  degenerate? :degenerate
                  :else :calibratable)]
    {:n-by-kind n-by-kind
     :paired-count paired-count
     :error-stats {:zero-count zero-count
                   :nonzero-count nonzero-count
                   :max (if (seq errors) (apply max errors) 0.0)
                   :mean (or (mean errors) 0.0)}
     :degenerate? (boolean degenerate?)
     :verdict verdict
     :warnings warnings}))

(defn -main [& _]
  (prn (calibration-report (load-evidence))))
