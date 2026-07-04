#!/usr/bin/env bb
;; arxana_clock.bb — the Arxana Clock aggregator (E-arxana-clock, cars 1 + 4).
;;
;; One read-only surface over the system's time-drivers.  CAR 4 reframes it from
;; "registry + partial scans" (circumventable) to SCAN-PRIMARY + RECONCILE +
;; MANIFEST (auditable), because a registry can be skipped — a process that
;; never calls cyder/register! is invisible.  So we scan GROUND TRUTH and flag
;; what the registry missed:
;;
;;   cron     — ALL locations: user crontab + /etc/crontab + /etc/cron.d/* +
;;              /etc/cron.{hourly,daily,weekly,monthly}/*
;;   systemd  — user AND system timers
;;   cyder    — the in-JVM registry (the SEMANTIC layer: names/cadence/stop)
;;   jvm      — Thread.getAllStackTraces (ground truth a registry can't escape);
;;              periodic-looking threads not matched to a cyder driver are
;;              surfaced as ⚠ UNACCOUNTED residue (the completeness signal).
;;
;; Output carries `:drivers` (the accounted, semantic list), `:unaccounted`
;; (scan residue), and `:sources` (the manifest — exactly what was scanned, so
;; coverage is auditable).  Absent fields render "—" (never fabricated).  The
;; honesty bound: you cannot prove zero hidden timers, but the thread scan is
;; ground-truth-complete for what exists and the manifest makes coverage
;; inspectable.
;;
;;   bb scripts/arxana_clock.bb            # print the clock + write snapshot
;;   bb scripts/arxana_clock.bb --quiet    # write snapshot only
(require '[babashka.http-client :as http]
         '[babashka.process :refer [sh]]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[clojure.set :as set]
         '[clojure.java.io :as io]
         '[clojure.pprint :as pp])
(import '[java.time ZonedDateTime ZoneId])

(def TOKEN (str/trim (slurp "/home/joe/code/futon3c/.admintoken")))
(def DRAWBRIDGE "http://127.0.0.1:6768/eval")
(def OUT "/home/joe/code/futon3c/holes/excursions/arxana-clock-snapshot.edn")

(defn- sh-out [& args]
  (try (let [{:keys [out exit]} (apply sh args)] (when (zero? exit) out)) (catch Exception _ nil)))
(defn- futon? [s] (boolean (and s (re-find #"(?i)futon|/code/|war.?machine|mana|vitality" s))))

;; ---- cron (ALL locations) --------------------------------------------------
(defn- cron-cadence [m h dom mon dow]
  (cond
    (and (= m "0") (= h "*") (= dom "*") (= mon "*") (= dow "*")) "hourly"
    (and (re-matches #"\d+" m) (re-matches #"\d+" h) (= dom "*") (= mon "*") (= dow "*"))
    (format "daily %02d:%02d" (parse-long h) (parse-long m))
    :else (str/join " " [m h dom mon dow])))

(defn- cron-next-fire [m h dom mon dow]
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
  (or (some->> (re-find #":[a-z0-9-]+" cmd) (str))
      (some->> (re-find #"/([a-zA-Z0-9_.-]+\.(?:sh|bb|py|clj))" cmd) second)
      (some->> (re-find #"/([a-zA-Z0-9_.-]+)\s*$" cmd) second)
      (subs cmd 0 (min 32 (count cmd)))))

(defn- cron-line->driver
  "Parse one cron line.  SIXTH-FIELD-USER? true for /etc/crontab + /etc/cron.d
   (which carry a user column before the command)."
  [line source sixth-field-user?]
  (let [l (str/trim line)]
    (when (and (seq l) (not (str/starts-with? l "#"))
               (re-find #"^(@|[\d*/,-]+\s)" l)
               (not (re-find #"^\w+\s*=" l)))         ; skip env-var lines (SHELL=, PATH=)
      (if (str/starts-with? l "@")
        (let [[sched & more] (str/split l #"\s+" (if sixth-field-user? 3 2))
              cmd (str/join " " (if sixth-field-user? (drop 1 more) more))] ; drop user col if present
          {:name (cron-name cmd) :mechanism :cron :what cmd :source source
           :cadence (str/replace sched "@" "") :last-fired nil :next-fire nil
           :futon? (futon? cmd) :status :installed})
        (let [n (if sixth-field-user? 7 6)
              toks (str/split l #"\s+" n)]
          (when (= n (count toks))
            (let [[m h dom mon dow] toks
                  cmd (if sixth-field-user? (nth toks 6) (nth toks 5))]
              {:name (cron-name cmd) :mechanism :cron :what cmd :source source
               :cadence (cron-cadence m h dom mon dow)
               :last-fired nil
               :next-fire (some-> (cron-next-fire m h dom mon dow) str)
               :futon? (futon? cmd) :status :installed})))))))

(defn cron-drivers []
  (let [from-text (fn [raw source sixth?]
                    (keep #(cron-line->driver % source sixth?) (str/split-lines (or raw ""))))
        user   (from-text (sh-out "crontab" "-l") "user crontab" false)
        etc    (from-text (and (.exists (io/file "/etc/crontab")) (slurp "/etc/crontab")) "/etc/crontab" true)
        cron-d (mapcat (fn [f] (from-text (slurp (str f)) (str "/etc/cron.d/" (.getName f)) true))
                       (when (.isDirectory (io/file "/etc/cron.d")) (seq (.listFiles (io/file "/etc/cron.d")))))
        dirs   (for [period ["hourly" "daily" "weekly" "monthly"]
                     :let [d (io/file (str "/etc/cron." period))]
                     :when (.isDirectory d)
                     f (.listFiles d)
                     :when (.isFile f)]
                 {:name (.getName f) :mechanism :cron :what (.getPath f)
                  :source (str "/etc/cron." period) :cadence period
                  :last-fired nil :next-fire nil :futon? (futon? (.getPath f)) :status :installed})]
    (vec (concat user etc cron-d dirs))))

;; ---- systemd timers (user AND system) --------------------------------------
(def ^:private dt-re #"\w{3} \d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} \w+")
(defn- timer-descs [scope-args]
  (let [raw (or (apply sh-out (concat ["systemctl"] scope-args ["show" "*.timer" "-p" "Id,Description"])) "")]
    (->> (str/split raw #"\n\n")
         (keep (fn [b] (let [m (into {} (for [l (str/split-lines b) :let [[k v] (str/split l #"=" 2)] :when v] [k v]))]
                         (when (get m "Id") [(get m "Id") (get m "Description")]))))
         (into {}))))

(defn- systemd-drivers-for [scope-args scope-label]
  (let [raw (or (apply sh-out (concat ["systemctl"] scope-args ["list-timers" "--all" "--no-pager"])) "")
        descs (timer-descs scope-args)]
    (for [line (str/split-lines raw)
          :let [dts (re-seq dt-re line)
                unit (second (re-find #"(\S+\.timer)" line))
                svc  (second (re-find #"(\S+\.service)" line))
                desc (get descs unit)
                exec (some-> (apply sh-out (concat ["systemctl"] scope-args ["show" svc "-p" "ExecStart" "--value"])) str/trim)]
          :when (and unit svc)]
      {:name (str/replace svc #"\.service$" "") :mechanism :systemd
       :what (or desc svc) :source (str "systemd " scope-label " timers")
       :cadence "timer"
       :next-fire (when (>= (count dts) 1) (first dts))
       :last-fired (when (>= (count dts) 2) (second dts))
       :futon? (futon? (str unit " " desc " " exec)) :status :installed})))

(defn systemd-drivers []
  (vec (concat (systemd-drivers-for ["--user"] "user")
               (systemd-drivers-for [] "system"))))

;; ---- systemd transient/running user services (campaign drivers) ------------
;; Bounded foreground campaigns started via `systemd-run --user` (e.g. the
;; wm-clicks regulated click loop, futon2/scripts/wm_click_loop.sh +
;; README-clicks-and-ticks) are SERVICES, not timers — `list-timers` never
;; sees them, so an ACTIVE campaign was invisible to the Clock (observed
;; 2026-07-04, the first campaign's own day; the E-arxana-clock
;; "completeness is bounded" issue made concrete). Scan running user
;; services, keep the transient ones.
(defn- transient-service-drivers []
  (let [raw (or (sh-out "systemctl" "--user" "list-units" "--type=service"
                        "--state=running" "--no-legend" "--no-pager" "--plain") "")]
    (vec
     (for [line (str/split-lines raw)
           :let [unit (first (str/split (str/trim line) #"\s+"))]
           :when (and unit (str/ends-with? unit ".service"))
           :let [transient* (some-> (sh-out "systemctl" "--user" "show" unit
                                            "-p" "Transient" "--value") str/trim)]
           :when (= "yes" transient*)
           :let [exec (some-> (sh-out "systemctl" "--user" "show" unit
                                      "-p" "ExecStart" "--value") str/trim)
                 since (some-> (sh-out "systemctl" "--user" "show" unit
                                       "-p" "ActiveEnterTimestamp" "--value") str/trim)]]
       {:name (str/replace unit #"\.service$" "") :mechanism :systemd-transient
        :what (or (not-empty exec) unit)
        :source "systemd user transient services (running)"
        :cadence "campaign" :next-fire nil :last-fired (not-empty since)
        :futon? (futon? (str unit " " exec)) :status :running}))))

;; ---- JVM: cyder registry + ground-truth thread scan (one Drawbridge call) ---
(def ^:private jvm-form "
(let [names (map #(.getName %) (keys (Thread/getAllStackTraces)))
      worker? (fn [n] (re-find #\"(?i)ForkJoinPool|agent-send-off|^pool-\\d|Reference Handler|Finalizer|Common-Cleaner|GC |Notification|Signal Dispatch|process reaper|nREPL|Acceptor|qtp|HttpClient|Keep-Alive|Connection|Attach|JDWP|DestroyJavaVM|main|Poller|timeout-daemon|core\\.async\" n))
      periodic? (fn [n] (re-find #\"(?i)sched|timer|watch|poll|tick|cron|feeder|replicat|probe|drainer|heartbeat\" n))
      periodic (->> names (filter periodic?) (remove worker?))
      cyder (mapv (fn [p] {:id (:process/id p) :type (str (:process/type p)) :layer (str (:process/layer p))
                           :last (str (:process/last-active p)) :cadence (str (or (:cadence (:process/metadata p)) \"\"))})
                  (futon3c.cyder/list-processes))]
  {:threads-total (count names)
   :worker-pool-count (count (filter worker? names))
   :periodic-threads (mapv (fn [[n c]] {:name n :count c}) (frequencies periodic))
   :cyder cyder})")

(defn jvm-scan []
  (try
    (let [resp (http/post DRAWBRIDGE {:headers {"x-admin-token" TOKEN "Content-Type" "text/plain"}
                                      :body jvm-form :throw false})]
      (when (= 200 (:status resp))
        (let [out (edn/read-string (:body resp))] (when (:ok out) (:value out)))))
    (catch Exception _ nil)))

(defn cyder-drivers [jvm]
  (for [p (:cyder jvm)]
    {:name (:id p) :mechanism :cyder :what (str (:type p) " / " (:layer p))
     :source "cyder registry (in-JVM)"
     :cadence (if (str/blank? (:cadence p)) nil (:cadence p))
     :last-fired (:last p) :next-fire nil :futon? true :status :running}))

(defn- tokens [s] (set (remove str/blank? (str/split (str/lower-case (str s)) #"[^a-z0-9]+"))))
(defn unaccounted
  "Periodic JVM threads NOT represented by a registered cyder driver — the scan
   residue (the completeness signal). Loose token-overlap match marks a thread
   accounted; the rest are flagged."
  [jvm]
  (let [cyder-toks (apply clojure.set/union #{} (map (comp tokens :id) (:cyder jvm)))]
    (->> (:periodic-threads jvm)
         (remove (fn [{:keys [name]}] (seq (clojure.set/intersection (tokens name) cyder-toks))))
         (mapv (fn [{:keys [name count]}]
                 {:name name :count count :mechanism :jvm
                  :note "periodic thread — not a registered cyder driver"})))))

;; ---- assemble + render -----------------------------------------------------
(defn- f [v] (or v "—"))
(defn -main [& args]
  (let [jvm      (jvm-scan)
        cron     (cron-drivers)
        systemd  (systemd-drivers)
        cyder    (cyder-drivers jvm)
        transients (transient-service-drivers)
        drivers  (vec (concat cron systemd transients cyder))
        residue  (if jvm (unaccounted jvm) [])
        sources  [{:source "user crontab (crontab -l)" :mechanism :cron :count (count (filter #(= "user crontab" (:source %)) cron))}
                  {:source "/etc/crontab + /etc/cron.d/*" :mechanism :cron
                   :count (count (filter #(or (= "/etc/crontab" (:source %)) (str/starts-with? (str (:source %)) "/etc/cron.d")) cron))}
                  {:source "/etc/cron.{hourly,daily,weekly,monthly}" :mechanism :cron
                   :count (count (filter #(re-find #"cron\.(hourly|daily|weekly|monthly)" (str (:source %))) cron))}
                  {:source "systemctl --user list-timers" :mechanism :systemd :count (count (filter #(= "systemd user timers" (:source %)) systemd))}
                  {:source "systemctl list-timers (system)" :mechanism :systemd :count (count (filter #(= "systemd system timers" (:source %)) systemd))}
                  {:source "systemctl --user list-units --state=running (transient)" :mechanism :systemd-transient :count (count transients)}
                  {:source "cyder/list-processes (registry)" :mechanism :cyder :count (count cyder)}
                  {:source "Thread.getAllStackTraces (JVM scan)" :mechanism :jvm
                   :count (:threads-total jvm 0) :worker-pool (:worker-pool-count jvm 0)
                   :periodic (count (:periodic-threads jvm))}]]
    (spit OUT (with-out-str (pp/pprint {:source "futon3c/scripts/arxana_clock.bb"
                                        :n (count drivers) :drivers drivers
                                        :unaccounted residue :sources sources})))
    (when-not (some #{"--quiet"} args)
      (println (format "=== Arxana Clock — %d accounted drivers (%d futon) · %d UNACCOUNTED ===\n"
                       (count drivers) (count (filter :futon? drivers)) (count residue)))
      (doseq [[mech label] [[:cron "⏰ cron — wall-clock"] [:systemd "⏱ systemd — timers"] [:systemd-transient "🖲 systemd — transient services (running campaigns)"] [:cyder "⚙ cyder — in-JVM (registry)"]]]
        (let [ds (sort-by #(or (:next-fire %) "zzz") (filter #(= mech (:mechanism %)) drivers))]
          (println (format "── %s (%d) ──" label (count ds)))
          (doseq [d ds]
            (println (format "  %-26s cadence=%-14s next=%-26s last=%-24s%s"
                             (f (:name d)) (f (:cadence d)) (f (:next-fire d)) (f (:last-fired d))
                             (if (:futon? d) "" "  ·non-futon"))))
          (println)))
      (when (seq residue)
        (println "⚠ UNACCOUNTED — periodic JVM threads with no registered cyder driver (scan residue):")
        (doseq [r residue] (println (format "  %-34s ×%d" (:name r) (:count r))))
        (println))
      (println "── sources scanned (coverage manifest) ──")
      (doseq [s sources]
        (println (format "  %-44s %s" (:source s)
                         (str "count=" (:count s)
                              (when (:worker-pool s) (format " worker-pool=%d periodic=%d" (:worker-pool s) (:periodic s)))))))
      (println (str "\nwrote snapshot → " OUT)))))

(apply -main *command-line-args*)
