#!/usr/bin/env bb
;; flight_scope_view.bb — project one WM flight's ORGANS as scopes (JSON).
;;
;; The flight analogue of mission-scope-view: the anatomy's organ vocabulary
;; (futon6/holes/anatomy-of-a-wm-flight.md §2) read live from the flight's
;; artifacts. An organ that hasn't happened yet is a GHOST — so an
;; in-progress flight (begin-state present, no frame) renders its organs
;; filling in, the way a young mission shows ghost phases.
;;
;; Usage: flight_scope_view.bb [--run-id live-...|--latest|--list]
;; Output: {"run-id".. "turn".. "organs":[{"organ","state","value","artifact","anchor"}..]}
(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[cheshire.core :as json])

(def traces-dir (str (System/getProperty "user.home") "/code/futon3c/data/repl-traces"))
(def pilots-log (str (System/getProperty "user.home") "/code/futon3c/holes/PILOTS-LOG.md"))
(def discipline-file (str (System/getProperty "user.home") "/code/futon3c/data/discipline-events.edn"))

(defn read-edn-file [f]
  (try (edn/read-string (slurp f)) (catch Exception _ nil)))

(defn run-ids []
  (->> (file-seq (io/file traces-dir))
       (filter #(.isFile %))
       (map #(.getName %))
       (keep #(second (re-matches #"(live-[0-9a-f-]+?)(\.begin)?\.edn" %)))
       distinct sort))

(defn latest-run-id []
  (->> (file-seq (io/file traces-dir))
       (filter #(.isFile %))
       (sort-by #(.lastModified %))
       (map #(.getName %))
       (keep #(second (re-matches #"(live-[0-9a-f-]+?)(\.begin)?\.edn" %)))
       last))

(defn frame-path [rid] (str traces-dir "/" rid ".edn"))
(defn begin-path [rid] (str traces-dir "/" rid ".begin.edn"))

(defn turn-record [frame] (first (:trace frame)))

(defn discipline-events-for [rid]
  (when (.exists (io/file discipline-file))
    (->> (str/split-lines (slurp discipline-file))
         (remove str/blank?)
         (map edn/read-string)
         (filterv #(= rid (:run-id %))))))

(defn pilots-log-anchor
  "Best-effort: the PILOTS-LOG line number whose Turn block mentions RID's
   predicted G (fallback: nil)."
  [pred]
  (when (and pred (.exists (io/file pilots-log)))
    (let [needle (format "%.2f" (double pred))
          lines (str/split-lines (slurp pilots-log))]
      (first (keep-indexed (fn [i l] (when (str/includes? l needle) (inc i)))
                           lines)))))

(defn organ [name state value artifact anchor]
  {:organ name :state state :value value :artifact artifact :anchor anchor})

(defn project [rid]
  (let [begin (read-edn-file (begin-path rid))
        frame (read-edn-file (frame-path rid))
        tr (when frame (turn-record frame))
        devs (discipline-events-for rid)
        pre (:pre begin)
        v (or (:v tr) (:v pre))
        ;; state helpers
        present (fn [x] (if (some? x) "present" "ghost"))
        fmt (fn [x] (cond (nil? x) nil
                          (number? x) (format "%.4f" (double x))
                          (map? x) (pr-str (select-keys x [:type :target]))
                          :else (str x)))]
    {:run-id rid
     :complete? (some? frame)
     :organs
     [(organ "field-read"
             (present (or (:dT-snapshot tr) (:dT-snapshot pre)))
             (when-let [dt (or (:dT-snapshot tr) (:dT-snapshot pre))]
               (str (count dt) " ranked actions"))
             (if frame (frame-path rid) (begin-path rid)) nil)
      (organ "velocity"
             (present v) (fmt v)
             (if frame (frame-path rid) (begin-path rid)) nil)
      (organ "attribution"
             (present (or (:v-attribution tr) (:v-attribution begin)))
             (some-> (or (:v-attribution tr) (:v-attribution begin)) str)
             (if frame (frame-path rid) (begin-path rid)) nil)
      (organ "prediction"
             (present (or (:predicted-discharge tr) (:predicted-discharge pre)))
             (fmt (or (:predicted-discharge tr) (:predicted-discharge pre)))
             (if frame (frame-path rid) (begin-path rid)) nil)
      (organ "counterfactual"
             (present (or (:predicted-constant tr) (:predicted-constant pre)))
             (fmt (or (:predicted-constant tr) (:predicted-constant pre)))
             (if frame (frame-path rid) (begin-path rid)) nil)
      (organ "begin-state"
             (if (.exists (io/file (begin-path rid))) "present" "ghost")
             nil (begin-path rid) nil)
      (organ "act+witness"
             (cond (:evidence-ref tr) "present"
                   (seq (filter #(= :teleport-refused (:discipline/event %)) devs)) "refused"
                   frame "absent (proposal-mode)"
                   :else "ghost")
             (or (:evidence-ref tr)
                 (some-> (first devs) :discipline/event str))
             (if (:evidence-ref tr) (frame-path rid) discipline-file) nil)
      (organ "measurement"
             (cond (nil? frame) "ghost"
                   (= :measured (:realised-source tr)) "measured"
                   (:realised-source tr) (name (:realised-source tr))
                   (:realised-discharge tr) "untagged"
                   :else "ghost")
             (when (:realised-discharge tr)
               (str "realised " (fmt (:realised-discharge tr))
                    " | error " (fmt (:prediction-error tr))
                    (when (:realised-read tr) (str " | " (name (:realised-read tr))))
                    (when (:independent? tr) " | independent")))
             (frame-path rid) nil)
      (organ "out-of-band"
             (if (seq devs) "present" (if frame "none" "ghost"))
             (when (seq devs) (str/join ", " (map #(name (:discipline/event %)) devs)))
             discipline-file nil)
      (organ "self-record"
             (if frame "present" "ghost")
             (when frame (str "γ frame, " (count (:trace frame)) " turn-record(s)"))
             pilots-log
             (pilots-log-anchor (or (:predicted-discharge tr)
                                    (:predicted-discharge pre))))]}))

(let [args *command-line-args*]
  (cond
    (some #{"--list"} args)
    (println (json/generate-string {:run-ids (run-ids)}))

    :else
    (let [rid (or (second (drop-while #(not= "--run-id" %) args))
                  (latest-run-id))]
      (if (and rid (or (.exists (io/file (frame-path rid)))
                       (.exists (io/file (begin-path rid)))))
        (println (json/generate-string (project rid)))
        (println (json/generate-string {:error (str "no artifacts for run-id " rid)}))))))
