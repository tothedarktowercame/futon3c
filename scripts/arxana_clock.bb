#!/usr/bin/env bb
;; arxana_clock.bb — the Arxana Clock aggregator (E-arxana-clock, car 1).
;;
;; One read-only surface over the system's THREE disjoint time-driver mechanisms:
;;   - cron        (crontab -l)
;;   - systemd     (systemctl --user list-timers)
;;   - cyder       (futon3c.cyder/list-processes, in-JVM, via Drawbridge)
;; normalised to {:name :mechanism :what :cadence :last-fired :next-fire :status}.
;; Absent fields render "—" (never fabricated — the substrate-2 discipline).
;;
;;   bb scripts/arxana_clock.bb            # print the unified clock + write snapshot
;;   bb scripts/arxana_clock.bb --quiet    # write snapshot only
(require '[babashka.http-client :as http]
         '[babashka.process :refer [shell sh]]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[clojure.pprint :as pp])
(import '[java.time ZonedDateTime LocalTime LocalDate ZoneId]
        '[java.time.format DateTimeFormatter])

(def TOKEN (str/trim (slurp "/home/joe/code/futon3c/.admintoken")))
(def DRAWBRIDGE "http://127.0.0.1:6768/eval")
(def OUT "/home/joe/code/futon3c/holes/excursions/arxana-clock-snapshot.edn")

(defn- sh-out [& args]
  (try (let [{:keys [out exit]} (apply sh args)] (when (zero? exit) out)) (catch Exception _ nil)))

(defn- futon? [s] (boolean (and s (re-find #"(?i)futon|/code/|war.?machine|mana|vitality" s))))

;; ---- cron ------------------------------------------------------------------
(defn- cron-cadence [m h dom mon dow]
  (cond
    (and (= m "0") (= h "*") (= dom "*") (= mon "*") (= dow "*")) "hourly"
    (and (re-matches #"\d+" m) (re-matches #"\d+" h) (= dom "*") (= mon "*") (= dow "*"))
    (format "daily %02d:%02d" (parse-long h) (parse-long m))
    :else (str/join " " [m h dom mon dow])))

(defn- cron-next-fire [m h dom mon dow]
  ;; compute next fire for the two common shapes (hourly; daily at HH:MM); else nil
  (try
    (let [now (ZonedDateTime/now (ZoneId/systemDefault))]
      (cond
        (and (= m "0") (= h "*") (= dom "*") (= mon "*") (= dow "*"))
        (-> now (.withMinute 0) (.withSecond 0) (.withNano 0) (.plusHours 1))
        (and (re-matches #"\d+" m) (re-matches #"\d+" h) (= dom "*") (= mon "*") (= dow "*"))
        (let [t (-> now (.withHour (parse-long h)) (.withMinute (parse-long m)) (.withSecond 0) (.withNano 0))]
          (if (.isAfter t now) t (.plusDays t 1)))
        :else nil))
    (catch Exception _ nil)))

(defn- cron-name [cmd]
  (or (some->> (re-find #":[a-z0-9-]+" cmd) (str))                  ; a deps alias like :wm-scheduled
      (some->> (re-find #"/([a-zA-Z0-9_.-]+\.(?:sh|bb|py|clj))" cmd) second) ; a script basename
      (subs cmd 0 (min 32 (count cmd)))))

(defn cron-drivers []
  (let [raw (or (sh-out "crontab" "-l") "")]
    (for [line (str/split-lines raw)
          :let [l (str/trim line)]
          :when (and (seq l) (not (str/starts-with? l "#")) (re-find #"^[\d*/,-]+\s" l))
          :let [toks (str/split l #"\s+" 6)
                [m h dom mon dow cmd] toks]
          :when (= 6 (count toks))]
      {:name (cron-name cmd) :mechanism :cron :what cmd
       :cadence (cron-cadence m h dom mon dow)
       :last-fired nil
       :next-fire (some-> (cron-next-fire m h dom mon dow) str)
       :futon? (futon? cmd)
       :status :installed})))

;; ---- systemd timers --------------------------------------------------------
(def ^:private dt-re #"\w{3} \d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} \w+")
(defn- timer-descriptions
  "Map of timer-unit -> Description (one show call for all timers)."
  []
  (let [raw (or (sh-out "systemctl" "--user" "show" "*.timer" "-p" "Id,Description") "")]
    (->> (str/split raw #"\n\n")
         (keep (fn [block]
                 (let [m (into {} (for [l (str/split-lines block)
                                        :let [[k v] (str/split l #"=" 2)]
                                        :when v] [k v]))]
                   (when (get m "Id") [(get m "Id") (get m "Description")]))))
         (into {}))))

(defn systemd-drivers []
  (let [raw (or (sh-out "systemctl" "--user" "list-timers" "--all" "--no-pager") "")
        descs (timer-descriptions)]
    (for [line (str/split-lines raw)
          :let [dts (re-seq dt-re line)
                unit (second (re-find #"(\S+\.timer)" line))
                svc  (second (re-find #"(\S+\.service)" line))
                desc (get descs unit)
                exec (some-> (sh-out "systemctl" "--user" "show" svc "-p" "ExecStart" "--value") str/trim)]
          :when (and unit svc)]
      {:name (str/replace svc #"\.service$" "") :mechanism :systemd
       :what (or desc svc)
       :cadence "timer"
       :next-fire (when (>= (count dts) 1) (first dts))
       :last-fired (when (>= (count dts) 2) (second dts))
       ;; relevance from the ExecStart PATH (ground truth), not just the description
       :futon? (futon? (str unit " " desc " " exec))
       :status :installed})))

;; ---- cyder (in-JVM, via Drawbridge) ----------------------------------------
(defn cyder-drivers []
  (try
    (let [resp (http/post DRAWBRIDGE
                          {:headers {"x-admin-token" TOKEN "Content-Type" "text/plain"}
                           :body "(mapv (fn [p] {:id (:process/id p) :type (str (:process/type p))
                                                 :layer (str (:process/layer p))
                                                 :last (str (:process/last-active p))
                                                 :cadence (str (or (:cadence (:process/metadata p)) \"\"))})
                                       (futon3c.cyder/list-processes))"
                           :throw false})
          out (when (= 200 (:status resp)) (edn/read-string (:body resp)))]
      (for [p (:value out)]
        {:name (:id p) :mechanism :cyder :what (str (:type p) " / " (:layer p))
         :cadence (if (str/blank? (:cadence p)) nil (:cadence p))
         :last-fired (:last p) :next-fire nil
         :futon? true   ; everything in cyder is in-JVM futon-stack
         :status :running}))
    (catch Exception _ [])))

;; ---- aggregate + render ----------------------------------------------------
(defn- f [v] (or v "—"))
(defn -main [& args]
  (let [drivers (vec (concat (cron-drivers) (systemd-drivers) (cyder-drivers)))
        by-mech (group-by :mechanism drivers)]
    (spit OUT (with-out-str (pp/pprint {:source "futon3c/scripts/arxana_clock.bb"
                                        :n (count drivers) :drivers drivers})))
    (when-not (some #{"--quiet"} args)
      (println (format "=== Arxana Clock — %d time-drivers across %d mechanisms (%d futon-relevant) ===\n"
                       (count drivers) (count by-mech) (count (filter :futon? drivers))))
      (doseq [[mech ds] (sort-by (comp name key) by-mech)]
        (println (format "── %s (%d) ──" (name mech) (count ds)))
        (doseq [d (sort-by #(or (:next-fire %) "zzz") ds)]
          (println (format "  %-26s cadence=%-12s next=%-26s last=%-26s%s"
                           (f (:name d)) (f (:cadence d)) (f (:next-fire d)) (f (:last-fired d))
                           (if (:futon? d) "" "  · non-futon"))))
        (println))
      (println (str "wrote snapshot → " OUT)))))

(apply -main *command-line-args*)
