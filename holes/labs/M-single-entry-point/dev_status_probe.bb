#!/usr/bin/env bb
;; dev_status_probe.bb — read-only probe of dev-laptop-env managed subsystems.
;;
;; Reads the PID files and process table (ps) that dev-laptop-env maintains
;; and lists each subsystem as running/not-running. Does NOT launch, signal,
;; or stop anything — pure observation.
;;
;; Managed subsystems (from scripts/dev-laptop-env):
;;   1. futon3c JVM         — port 7070 (FUTON3C_PORT)
;;   2. futon1a             — port 7071 (FUTON1A_PORT), lock at ~/code/storage/futon1a/default
;;   3. war-machine watch   — PID /tmp/war-machine-web.pid, dir ~/code/futon2/web/war-machine
;;   4. webarxana watch     — PID /tmp/webarxana-web.pid, dir ~/code/futon4/dev/web/webarxana
;;   5. multi-watcher       — PID /tmp/multi-watcher.pid (JVM-based, autostart deprecated)
;;   6. Ring server (in-JVM) — webarxana Ring at port 3100 (start-webarxana!, FUTON3C_WEBARXANA_SERVER_AUTOSTART)
;;   7. shadow-cljs (in-JVM) — start-shadow!, gated OFF (FUTON3C_SHADOW_AUTOSTART=false)
;;
;; Usage: bb dev_status_probe.bb
;; Acceptance: real probe output pasted; subsystem list matches the script's managed set; any cwd.

(require '[babashka.process :refer [sh]])
(require '[clojure.string :as str])
(require '[babashka.fs :as fs])

(defn pid-alive? [pid]
  (when (and pid (re-matches #"\d+" pid))
    (let [result (sh "ps" "-p" pid "-o" "comm=")]
      (zero? (:exit result)))))

(defn read-pidfile [path]
  (when (fs/exists? path)
    (-> (slurp path) str/trim)))

(defn port-listening? [port]
  (when port
    (try
      (let [result (sh "curl" "-s" "-o" "/dev/null" "-w" "%{http_code}"
                       "--connect-timeout" "2"
                       (str "http://127.0.0.1:" port "/"))]
        (and (zero? (:exit result))
             (not= (:out result) "000")))
      (catch Exception _ false))))

(defn pid-for-port [port]
  (when port
    (let [result (sh "ss" "-ltnp")]
      (->> (:out result)
           str/split-lines
           (some #(let [m (re-find #"pid=(\d+)" %)]
                    (when (and m (re-find (re-pattern (str ":" port "\\s")) %))
                      (second m))))))))

(defn proc-name-for-pid [pid]
  (when (and pid (re-matches #"\d+" pid))
    (let [result (sh "ps" "-p" pid "-o" "args=")]
      (when (zero? (:exit result))
        (str/trim (:out result))))))

(defn find-proc-by-pattern [pattern]
  (let [result (sh "bash" "-c" (str "ps -ww -eo pid=,args= | grep '" pattern "' | grep -v grep"))]
    (when (zero? (:exit result))
      (->> (:out result)
           str/split-lines
           (map str/trim)
           (filter not-empty)))))

(defn report-subsystem [name details]
  (let [status (:status details)
        marker (case status
                 :running "[RUNNING]"
                 :not-running "[NOT RUNNING]"
                 :unknown "[UNKNOWN]")
        info (:info details)]
    (println (format "%-28s %s %s" name marker (or info "")))))

(println "=== Dev Status Probe — dev-laptop-env managed subsystems ===")
(println "(Read-only observation — no launch/signal/stop)")
(println "")

;; 1. futon3c JVM — port 7070
(let [port 7070
      listening (port-listening? port)
      pid (pid-for-port port)]
  (report-subsystem
   "futon3c JVM (port 7070)"
   {:status (if listening :running :not-running)
    :info (cond
            (and pid (proc-name-for-pid pid))
            (str "pid=" pid " (" (-> (proc-name-for-pid pid) (str/split #"\s+") first) ")")
            listening "port open, pid not resolved"
            :else "port not listening")}))

;; 2. futon1a — port 7071, lock file
(let [port 7071
      listening (port-listening? port)
      pid (pid-for-port port)
      lock-dir (str (System/getenv "HOME") "/code/storage/futon1a/default")
      lock-file (fs/file lock-dir ".lock")]
  (report-subsystem
   "futon1a (port 7071)"
   {:status (if listening :running :not-running)
    :info (cond
            (and pid (proc-name-for-pid pid))
            (str "pid=" pid " (" (-> (proc-name-for-pid pid) (str/split #"\s+") first) ")")
            listening "port open, pid not resolved"
            :else (str "port not listening"
                       (when (fs/exists? (str lock-file))
                         (str "; lock file exists at " lock-dir))))}))

;; 3. war-machine shadow watch — PID file /tmp/war-machine-web.pid
(let [pidfile "/tmp/war-machine-web.pid"
      pid (read-pidfile pidfile)
      alive (pid-alive? pid)]
  (report-subsystem
   "war-machine shadow watch"
   {:status (if alive :running :not-running)
    :info (cond
            (and pid alive) (str "pid=" pid " (" (proc-name-for-pid pid) ")")
            pid (str "stale PID file: " pidfile " has " pid " but process is dead")
            :else (str "no PID file at " pidfile))}))

;; 4. webarxana shadow watch — PID file /tmp/webarxana-web.pid
(let [pidfile "/tmp/webarxana-web.pid"
      pid (read-pidfile pidfile)
      alive (pid-alive? pid)]
  (report-subsystem
   "webarxana shadow watch"
   {:status (if alive :running :not-running)
    :info (cond
            (and pid alive) (str "pid=" pid " (" (proc-name-for-pid pid) ")")
            pid (str "stale PID file: " pidfile " has " pid " but process is dead")
            :else (str "no PID file at " pidfile))}))

;; 5. multi-watcher — PID file /tmp/multi-watcher.pid (JVM-based)
(let [pidfile "/tmp/multi-watcher.pid"
      pid (read-pidfile pidfile)
      alive (pid-alive? pid)
      jvm-procs (find-proc-by-pattern "multi_watcher")]
  (report-subsystem
   "multi-watcher"
   {:status (cond
              alive :running
              (seq jvm-procs) :running
              :else :not-running)
    :info (cond
            (and pid alive) (str "pid=" pid " (PID file)")
            (seq jvm-procs) (str "JVM process found (no PID file): " (first jvm-procs))
            pid (str "stale PID file: " pidfile)
            :else (str "no PID file, no JVM multi_watcher process found"))}))

;; 6. Ring server (webarxana in-JVM) — port 3100 per psi
(let [port 3100
      listening (port-listening? port)]
  (report-subsystem
   "Ring server / webarxana (:3100)"
   {:status (if listening :running :not-running)
    :info (if listening "port 3100 listening" "port 3100 not listening (in-JVM, may be on different port)")}))

;; 7. shadow-cljs in-JVM — gated OFF by default (FUTON3C_SHADOW_AUTOSTART=false)
;;    Check for npx shadow-cljs process in the watched dirs
(let [war-procs (find-proc-by-pattern "shadow-cljs.*war-machine")
      web-procs (find-proc-by-pattern "shadow-cljs.*webarxana")
      any-shadow (find-proc-by-pattern "shadow-cljs")]
  (report-subsystem
   "shadow-cljs (in-JVM, gated OFF)"
   {:status (if (seq any-shadow) :running :not-running)
    :info (cond
            (seq war-procs) (str "war-machine shadow process found")
            (seq web-procs) (str "webarxana shadow process found")
            (seq any-shadow) (str "shadow-cljs process(es) found: " (count any-shadow))
            :else "no shadow-cljs processes (expected: gated OFF by default)")}))

;; 8. Tickle watchdog — autostart=false by default
(let [tickle-procs (find-proc-by-pattern "tickle")]
  (report-subsystem
   "Tickle watchdog (autostart=false)"
   {:status (if (seq tickle-procs) :running :not-running)
    :info (if (seq tickle-procs) "tickle process found" "not running (expected: autostart=false)")}))

;; 9. FM conductor — autostart=false by default
(let [fm-procs (find-proc-by-pattern "fm.conductor")
      fm-env-procs (find-proc-by-pattern "FUTON3C_FM")
      all-fm (concat fm-procs fm-env-procs)]
  (report-subsystem
   "FM conductor (autostart=false)"
   {:status (if (seq all-fm) :running :not-running)
    :info (if (seq all-fm) "FM conductor process found" "not running (expected: autostart=false)")}))

(println "")
(println "=== Summary ===")
(let [port-7070 (port-listening? 7070)
      port-7071 (port-listening? 7071)
      port-3100 (port-listening? 3100)]
  (println (format "futon3c JVM (:7070): %s | futon1a (:7071): %s | Ring (:3100): %s"
                   (if port-7070 "UP" "DOWN")
                   (if port-7071 "UP" "DOWN")
                   (if port-3100 "UP" "DOWN"))))
(println "")
(println "=== curl cross-check ===")
(doseq [port [7070 7071 3100]]
  (let [result (sh "curl" "-s" "-o" "/dev/null" "-w" "%{http_code}"
                   "--connect-timeout" "2"
                   (str "http://127.0.0.1:" port "/"))]
    (println (format "  :%s -> HTTP %s" port (:out result)))))
