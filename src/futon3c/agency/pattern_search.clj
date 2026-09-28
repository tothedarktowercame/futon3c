(ns futon3c.agency.pattern-search
  "Single-owner resident futon3a pattern search with fail-safe one-shot fallback."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader OutputStreamWriter]
           [java.lang ProcessBuilder$Redirect]
           [java.util.concurrent TimeUnit]))

(def ^:dynamic *query-timeout-ms* 2000)
(def ^:dynamic *startup-timeout-ms* 15000)
(def ^:dynamic *resident-command* nil)
(def ^:dynamic *fallback-search* nil)

(defonce ^:private owner-lock (Object.))
(defonce ^:private !resident (atom nil))
(defonce ^:private !started-before? (atom false))
(defonce ^:private !stats
  (atom {:resident-hits 0 :fallbacks 0 :restarts 0 :last-latency-ms nil}))

(defn- futon3a-root []
  (or (System/getenv "FUTON3A_ROOT")
      (str (System/getProperty "user.home") "/code/futon3a")))

(defn- resident-command []
  (or *resident-command*
      (let [root (futon3a-root)]
        [(str root "/.venv/bin/python3") "-u"
         (str root "/scripts/notions_search.py") "--resident"
         "--embeddings" (str root "/resources/notions/minilm_pattern_embeddings.json")])))

(defn- destroy! [{:keys [^Process process ^BufferedReader reader ^BufferedWriter writer]}]
  (when writer (try (.close writer) (catch Throwable _)))
  (when process
    (try
      (when (.isAlive process)
        (.destroyForcibly process)
        (.waitFor process 500 TimeUnit/MILLISECONDS))
      (catch Throwable _)))
  (when reader (try (.close reader) (catch Throwable _)))
  nil)

(defn stop! []
  (locking owner-lock
    (destroy! @!resident)
    (reset! !resident nil)))

(defn reset-state! []
  (stop!)
  (reset! !started-before? false)
  (reset! !stats {:resident-hits 0 :fallbacks 0 :restarts 0 :last-latency-ms nil}))

(defn stats [] @!stats)

(defn- alive? [resident]
  (and resident (.isAlive ^Process (:process resident))))

(defn- start-resident! []
  (let [builder (doto (ProcessBuilder. ^java.util.List (vec (resident-command)))
                  (.redirectError ProcessBuilder$Redirect/INHERIT))
        environment (.environment builder)
        _ (.put environment "HF_HUB_OFFLINE" "1")
        _ (.put environment "TRANSFORMERS_OFFLINE" "1")
        process (.start builder)
        resident {:process process
                  :reader (BufferedReader. (InputStreamReader. (.getInputStream process)))
                  :writer (BufferedWriter. (OutputStreamWriter. (.getOutputStream process)))}
        pending (future (.readLine ^BufferedReader (:reader resident)))
        timeout-token (Object.)
        ready-line (deref pending *startup-timeout-ms* timeout-token)
        ready? (try (= {:resident "ready"} (json/parse-string ready-line true))
                    (catch Throwable _ false))]
    (when (or (identical? timeout-token ready-line)
              (nil? ready-line)
              (not ready?))
      (future-cancel pending)
      (destroy! resident)
      (throw (ex-info "Resident pattern search failed startup handshake"
                      {:error/code :resident-startup :line ready-line})))
    (when @!started-before?
      (swap! !stats update :restarts inc))
    (reset! !started-before? true)
    (reset! !resident resident)
    resident))

(defn- ensure-resident! []
  (let [current @!resident]
    (if (alive? current)
      current
      (do
        (when current
          (destroy! current)
          (reset! !resident nil))
        (start-resident!)))))

(defn- parse-results [line]
  (let [parsed (json/parse-string line true)]
    (when (and (sequential? parsed)
               (every? #(and (map? %) (string? (:id %)) (number? (:score %))) parsed))
      (vec parsed))))

(defn- resident-search! [query-text top]
  (let [{:keys [^BufferedReader reader ^BufferedWriter writer] :as resident}
        (ensure-resident!)
        started (System/nanoTime)]
    (.write writer (json/generate-string {:query query-text :top top}))
    (.newLine writer)
    (.flush writer)
    (let [pending (future (.readLine reader))
          timeout-token (Object.)
          line (deref pending *query-timeout-ms* timeout-token)]
      (when (identical? timeout-token line)
        (future-cancel pending)
        (destroy! resident)
        (reset! !resident nil)
        (throw (ex-info "Resident pattern search timed out"
                        {:error/code :resident-timeout})))
      (when (nil? line)
        (reset! !resident nil)
        (throw (ex-info "Resident pattern search exited"
                        {:error/code :resident-exited})))
      (let [results (parse-results line)
            latency-ms (/ (- (System/nanoTime) started) 1000000.0)]
        (when-not results
          (destroy! resident)
          (reset! !resident nil)
          (throw (ex-info "Resident pattern search returned malformed output"
                          {:error/code :resident-malformed :line line})))
        (swap! !stats assoc :last-latency-ms latency-ms)
        (swap! !stats update :resident-hits inc)
        results))))

(defn- default-fallback [query-text top]
  (let [root (futon3a-root)
        process (.start
                 (doto (ProcessBuilder.
                        ^java.util.List
                        [(str root "/.venv/bin/python3")
                         (str root "/scripts/notions_search.py")
                         "--query" query-text "--top" (str top)
                         "--embeddings"
                         (str root "/resources/notions/minilm_pattern_embeddings.json")
                         "--json"])
                   (.redirectErrorStream true)))]
    (if-not (.waitFor process 15 TimeUnit/SECONDS)
      (do (.destroyForcibly process) nil)
      (let [line (->> (slurp (.getInputStream process)) str/split-lines
                      (filter #(str/starts-with? % "[")) first)]
        (when line (parse-results line))))))

(defn search
  "Return TOP ranked maps. Resident failures use the historical one-shot path."
  [query-text top]
  (locking owner-lock
    (try
      (resident-search! query-text top)
      (catch Throwable t
        (swap! !stats assoc :last-error (.getMessage t)
               :last-error-data (ex-data t))
        (swap! !stats update :fallbacks inc)
        ((or *fallback-search* default-fallback) query-text top)))))
