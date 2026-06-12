#!/usr/bin/env bb
;; flight_scope_view.bb — project one WM flight's ORGANS as scopes (JSON).
;;
;; Schema-v0.4 flight records are derivations: each organ is already a
;; term-with-ground or a typed sorry. This renderer reads those cells and does
;; not adjudicate them. Pre-schema runs fall back to the old projection and are
;; explicitly labelled derivation-thin.
;;
;; Usage:
;;   flight_scope_view.bb [--run-id live-...|--latest|--list]
;;   flight_scope_view.bb --file path/to/<run-id>.flight.edn
;; Output: {"run-id".. "derivation".. "organs":[...]}
(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[cheshire.core :as json])

(def traces-dir (str (System/getProperty "user.home") "/code/futon3c/data/repl-traces"))
(def pilots-log (str (System/getProperty "user.home") "/code/futon3c/holes/PILOTS-LOG.md"))
(def discipline-file (str (System/getProperty "user.home") "/code/futon3c/data/discipline-events.edn"))

(def organ-order [:field-read :velocity :warrant :verification :attribution
                  :prediction :counterfactual :begin-state :act :measurement
                  :window :out-of-band :self-record])

(defn read-edn-file [f]
  (try (edn/read-string (slurp f)) (catch Exception _ nil)))

(defn run-ids []
  (->> (file-seq (io/file traces-dir))
       (filter #(.isFile %))
       (map #(.getName %))
       (keep #(or (second (re-matches #"(live-[0-9a-f-]+?)\.flight\.edn" %))
                  (second (re-matches #"(live-[0-9a-f-]+?)(\.begin)?\.edn" %))))
       distinct sort))

(defn latest-run-id []
  (->> (file-seq (io/file traces-dir))
       (filter #(.isFile %))
       (sort-by #(.lastModified %))
       (map #(.getName %))
       (keep #(or (second (re-matches #"(live-[0-9a-f-]+?)\.flight\.edn" %))
                  (second (re-matches #"(live-[0-9a-f-]+?)(\.begin)?\.edn" %))))
       last))

(defn frame-path [rid] (str traces-dir "/" rid ".edn"))
(defn begin-path [rid] (str traces-dir "/" rid ".begin.edn"))
(defn flight-path [rid] (str traces-dir "/" rid ".flight.edn"))

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

(defn term-cell? [cell]
  (and (map? cell) (contains? cell :judgment) (contains? cell :ground)))

(defn sorry-cell? [cell]
  (and (map? cell) (map? (:sorry cell))))

(defn fmt [x]
  (cond
    (nil? x) nil
    (number? x) (format "%.4f" (double x))
    (keyword? x) (name x)
    (map? x) (pr-str (select-keys x [:type :target :state :class :determined? :ref :g :g-grain :hole :why]))
    :else (str x)))

(defn g-summary [g]
  (when (map? g)
    (str (fmt (:g g)) " " (name (:g-grain g)))))

(defn term-summary [organ-key judgment derivation]
  (case organ-key
    :field-read
    (let [gauge (:gauge judgment)
          nbh (:neighbourhood judgment)]
      (str (get gauge :count "?") " ranked actions; chosen "
           (or (get-in nbh [:chosen :target]) (fmt (:chosen nbh)))
           "; rejected " (count (:rejected nbh))))
    :velocity (fmt (:action judgment))
    :warrant (if (:determined? judgment)
               (str "determined by " (name (get-in judgment [:determined-by :kind])))
               (str "queued " (get-in judgment [:queued :queue-ref])))
    :verification (str (count (:holes judgment)) " hole check(s); chosen " (fmt (:chosen-hole judgment)))
    :attribution (fmt judgment)
    :prediction (str "scaled " (g-summary (:scaled judgment))
                     "; constant " (g-summary (:constant judgment))
                     (when-let [s (get-in judgment [:policy :sorry :kind])]
                       (str "; policy sorry " (name s))))
    :counterfactual (fmt judgment)
    :begin-state (str "begin " (:begin-at judgment) "; target " (g-summary (:target-g judgment)))
    :act (str (name (:state judgment))
              (when-let [r (get-in judgment [:witness :ref])] (str " " r)))
    :measurement (str (if-let [c (:class judgment)]
                        (name c)
                        ;; fable-2's review edge: a nil class must say WHY —
                        ;; on a thin backfill it is prose-lost; on a full
                        ;; record it is a judgment the pilot has not made
                        (if (= :thin derivation)
                          "class absent (derivation-thin: prose-lost)"
                          "class NOT-YET-JUDGED"))
                      "; predicted " (g-summary (:predicted judgment))
                      "; realised " (g-summary (:realised judgment))
                      "; error " (fmt (:error judgment)))
    :window (str "scans " (count (:scans judgment))
                 "; epsilon " (:epsilon judgment)
                 "; agreement " (:agreement judgment))
    :out-of-band (fmt judgment)
    :self-record (str "gamma " (:gamma-ref judgment)
                      "; turns " (:turn-record-count judgment))
    (fmt judgment)))

(defn cell->organ [record-path derivation organ-key cell]
  (let [organ-name (name organ-key)]
    (cond
      (sorry-cell? cell)
      {:organ organ-name
       :state (str "sorry/" (name (get-in cell [:sorry :kind])))
       :value (or (get-in cell [:sorry :note]) (pr-str (:sorry cell)))
       :artifact record-path
       :anchor nil
       :sorry (:sorry cell)
       :ground nil}

      (term-cell? cell)
      {:organ organ-name
       :state "present"
       :value (term-summary organ-key (:judgment cell) derivation)
       :artifact record-path
       :anchor nil
       :judgment (:judgment cell)
       :ground (:ground cell)}

      :else
      {:organ organ-name
       :state "invalid-cell"
       :value (pr-str cell)
       :artifact record-path
       :anchor nil})))

(defn project-flight-record [record-path record]
  (let [organs (:organs record)]
    {:run-id (:flight/id record)
     :complete? true
     :derivation (:flight/derivation record)
     :derivation-label (if (= :thin (:flight/derivation record)) "derivation-thin" "grounded-record")
     :record-path record-path
     :organs (->> (concat organ-order (remove (set organ-order) (keys organs)))
                  distinct
                  (keep #(when-let [cell (get organs %)]
                           (cell->organ record-path (:flight/derivation record) % cell)))
                  vec)}))

(defn project-thin [rid]
  (let [begin (read-edn-file (begin-path rid))
        frame (read-edn-file (frame-path rid))
        tr (when frame (turn-record frame))
        devs (discipline-events-for rid)
        pre (:pre begin)
        v (or (:v tr) (:v pre))
        present (fn [x] (if (some? x) "present" "ghost"))]
    {:run-id rid
     :complete? (some? frame)
     :derivation :thin
     :derivation-label "derivation-thin fallback; pre-schema record, grounds projected not recorded"
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
                   frame "sorry/proposal-mode"
                   :else "ghost")
             (or (:evidence-ref tr)
                 (some-> (first devs) :discipline/event str)
                 (when frame "pre-schema proposal-mode; derivation-thin"))
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

(defn project-run-id [rid]
  (let [fp (flight-path rid)]
    (if (.exists (io/file fp))
      (project-flight-record fp (read-edn-file fp))
      (project-thin rid))))

(defn project-file [path]
  (let [record (read-edn-file path)]
    (if (:flight/id record)
      (project-flight-record path record)
      (let [name (.getName (io/file path))
            rid (second (re-matches #"(live-[0-9a-f-]+?)(?:\.begin)?\.edn" name))]
        (if rid
          (project-thin rid)
          {:error (str "not a flight record or known pre-schema frame: " path)})))))

(defn arg-after [flag args]
  (second (drop-while #(not= flag %) args)))

(let [args *command-line-args*]
  (cond
    (some #{"--list"} args)
    (println (json/generate-string {:run-ids (run-ids)}))

    (arg-after "--file" args)
    (println (json/generate-string (project-file (arg-after "--file" args))))

    :else
    (let [rid (or (arg-after "--run-id" args)
                  (when-let [p (first (remove #(str/starts-with? % "--") args))]
                    (second (re-matches #"(live-[0-9a-f-]+).*" (.getName (io/file p)))))
                  (latest-run-id))]
      (if (and rid (or (.exists (io/file (flight-path rid)))
                       (.exists (io/file (frame-path rid)))
                       (.exists (io/file (begin-path rid)))))
        (println (json/generate-string (project-run-id rid)))
        (println (json/generate-string {:error (str "no artifacts for run-id " rid)}))))))
