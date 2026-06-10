(ns futon3c.aif.discipline-events
  "Out-of-band loss events for calibration evidence."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def allowed-kinds
  #{:teleport-refused :guardrail-trip :operator-decline :operator-merge})

(defn default-path []
  ;; Absolute — the sink must be the same file regardless of caller cwd
  ;; (CLI vs serving JVM); matches calibration/default-paths.
  (str (System/getProperty "user.home") "/code/futon3c/data/discipline-events.edn"))

(defn discipline-event
  "Validate and normalize one out-of-band discipline event."
  [m]
  (let [kind (:discipline/event m)]
    (cond
      (not (contains? allowed-kinds kind))
      (throw (ex-info "invalid discipline event kind"
                      {:kind kind :allowed allowed-kinds}))

      (str/blank? (str (:run-id m)))
      (throw (ex-info "discipline event requires run-id" {:event m}))

      (str/blank? (str (:at m)))
      (throw (ex-info "discipline event requires at" {:event m}))

      (not (map? (:action m)))
      (throw (ex-info "discipline event requires action map" {:event m}))

      :else
      (select-keys m [:discipline/event :run-id :at :action :predicted :note]))))

(defn append-event!
  "Append one validated event as an EDN line."
  ([ev] (append-event! ev (default-path)))
  ([ev path]
   (let [event (discipline-event ev)
         file (io/file path)]
     (when-let [parent (.getParentFile file)]
       (.mkdirs parent))
     (spit file (str (pr-str event) "\n") :append true)
     event)))

(defn read-events
  ([] (read-events (default-path)))
  ([path]
   (let [file (io/file path)]
     (if-not (.exists file)
       []
       (with-open [r (io/reader file)]
         (->> (line-seq r)
              (remove str/blank?)
              (map edn/read-string)
              (mapv discipline-event)))))))
