(ns futon3c.wm.meta-self-heal-adapters
  "Concrete serving adapters for the dedicated META repair transaction.

   The adapters deliberately expose readiness separately from execution.  A
   caller can therefore prove the exact cast/repository/reload configuration
   without dispatching an agent or accepting a click."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.wm.code-identity :as code-identity])
  (:import (java.security MessageDigest)
           (java.time Instant)))

(def default-cast
  {:author "wm-author" :reviewer "wm-reviewer"
   :repair-reviewer "wm-repair-reviewer"})
(def default-repository "/home/joe/code/futon2")
(def ^:dynamic *acceptance-repository* default-repository)
(def ^:dynamic *acceptance-runs-root*
  "/home/joe/code/futon2/data/wm-meta-injuries/acceptance-runs")
(def ^:private default-acceptance-timeout-ms (* 15 60 1000))
(def ^:private bounded-test-dir "/tmp/futon-bounded-tests")
(def ^:private bounded-test-runner "/home/joe/code/futon3c/scripts/bounded_test_job.py")
(def ^:private command-name-pattern #"(?!-)[A-Za-z0-9._-]+")

(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))
(defn- available-seat-state? [status]
  (contains? #{:idle :restored "idle" "restored"} status))
(defn- invoke-capable? [agent]
  ;; get-agent returns the stored record, whereas the HTTP projection derives
  ;; :invoke-ready? from its registered local/WS route.  The dedicated WM
  ;; seats are local handlers, so their stored invoke function is the exact
  ;; authority the adapter will call after a restart.
  (or (true? (:agent/invoke-ready? agent))
      (ifn? (:agent/invoke-fn agent))))
(defn- git [repo & args]
  (let [{:keys [exit out err]} (apply shell/sh "git" "-C" repo args)]
    {:ok? (zero? exit) :out (str/trim out) :err (str/trim err)}))

(defn readiness
  "Read-only readiness receipt.  It names every resolved authority and never
   resets a session, dispatches work, reloads code, or queues a click."
  [opts]
  (let [cast (merge default-cast (select-keys opts (keys default-cast)))
        seats (vals cast)
        repo (or (:target-repository opts) default-repository)
        records (into {} (map (fn [seat] [seat (reg/get-agent seat)]) seats))
        tracked (git repo "status" "--porcelain" "--untracked-files=no")
        defects (cond-> []
                  (not= 3 (count (set seats))) (conj :cast-not-distinct)
                  (not-every? #(and % (invoke-capable? %)
                                    (available-seat-state? (:agent/status %)))
                              (vals records))
                  (conj :cast-not-ready)
                  (not (.isDirectory (io/file repo))) (conj :repository-unreadable)
                  (or (not (:ok? tracked)) (seq (:out tracked)))
                  (conj :tracked-worktree-dirty)
                  (not (nonblank? (:meta-injury-ledger-path opts)))
                  (conj :injury-ledger-path)
                  (not (nonblank? (:meta-repair-transaction-path opts)))
                  (conj :transaction-path)
                  (not (ifn? (:queue-retry-fn opts))) (conj :queued-retry-authority))]
    {:schema :wm/meta-self-heal-serving-readiness-v1
     :status (if (seq defects) :refused :ready)
     :defects defects :cast cast :repository repo
     :tracked-head (:out (git repo "rev-parse" "HEAD"))
     :ledger-path (:meta-injury-ledger-path opts)
     :transaction-path (:meta-repair-transaction-path opts)
     :dispatch :futon3c.agency.registry/invoke-agent!
     :session-reset :futon3c.agency.registry/reset-session!
     :reload :futon3c.wm.code-identity/load-file-recorded!
     :retry :queued-after-parent-release}))

(defn reset-cast!
  "Reset exactly the explicit three-seat cast and return the fresh session ids."
  [cast]
  (reduce-kv
   (fn [acc role seat]
     (let [result (reg/reset-session! seat)
           current (reg/get-agent seat)]
       (if-not (:ok result)
         (reduced {:status :refused :reason :cast-reset-failed
                   :role role :seat seat :result result})
         (assoc-in acc [:sessions role] (:agent/session-id current)))))
   {:status :reset :cast cast :sessions {}} cast))

(defn- one-edn-map [text]
  (try
    (let [text (str/trim (str text))
          fenced (re-matches #"(?s)```(?:edn)?\s*(.*?)\s*```" text)
          value (edn/read-string (or (second fenced) text))]
      (when (map? value) value))
    (catch Throwable _ nil)))

(defn agency-invoke!
  "Synchronous Agency port used inside the already-checkpointed coordinator.
   The prompt requires a single EDN result so phase evidence is machine typed."
  [{:keys [seat session-id request]}]
  (let [reset (reg/reset-session! seat)
        prompt (str "Dedicated META self-heal phase. Do not select ordinary work. "
                    "Execute only the sealed request and return exactly one EDN map "
                    "with :status and :payload (no prose).\n\n" (pr-str request))
        result (when (:ok reset)
                 (reg/invoke-agent! seat prompt
                                    {:mode "work" :caller "war-machine"
                                     :session-id session-id}))
        parsed (when (:ok result) (one-edn-map (:result result)))
        job-id (or (get-in result [:invoke-meta :job-id]) (:job-id parsed))]
    (cond
      (not (:ok reset)) {:status :failed :payload {:reason :agency-session-reset-failed
                                                    :reset reset}}
      (not (:ok result)) {:status :failed :payload {:reason :agency-invoke-failed
                                                     :response result}}
      (not (and parsed (map? (:payload parsed))))
      {:status :failed :payload {:reason :agency-response-not-one-edn-map}
       :session-id (:session-id result)}
      (not (#{:succeeded :failed :rejected} (:status parsed)))
      {:status :failed :payload {:reason :agency-response-status-invalid}}
      (not (nonblank? (:session-id result)))
      {:status :failed :payload {:reason :agency-session-missing}}
      (or (not (nonblank? job-id)) (= (:old-session-id reset) (:session-id result)))
      {:status :failed :payload {:reason :agency-session-or-job-not-fresh}}
      :else {:status (:status parsed) :payload (:payload parsed)
             :session-id (:session-id result) :job-id job-id})))

(defn- producer-prompt [operation request]
  (str "Dedicated META " (name operation) ". Return exactly one EDN map and no prose. "
       "Do not select ordinary work, execute any command, or claim a repair. "
       (case operation
         :diagnosis
         "Success must be {:status :succeeded :diagnosis <map>}. A refusal/failure must be typed. "
         :acceptance-proposal
         (str "Success must be {:status :proposed :acceptance-test {:id <nonblank-string> :command [<string> ...] :expected <value> "
              ":source {:namespace <namespace-string> :path <test/... .clj> :text <complete-file-text>}}}. "
              "The source must be a complete clojure.test file that pins the injury from the diagnosis: it fails for the injury's reason on injured code and passes after repair, using only the real production functions named by the diagnosis, with no stubs. "
              "The command must be [\"clojure\" \"-M:test\" \"-n\" <that-namespace>] optionally followed by [\"-v\" <var>]. The specification is not run and is not accepted. ")
         :acceptance-review
         (str "This acceptance test was run before any repair, in the canonical checkout; the result is in :result. "
              "Admit it if it pins the injury (fails for the injury's reason) and is the right test for the repair; refuse with a typed reason otherwise. "
              "Accept only with {:status :accepted :injury-id ... :source-record-pin ... :diagnosis-evidence-pin ... :proposal-evidence-pin ... :acceptance-test ...}, echoing those five exact request values. A refusal/failure must be typed. ")
         "Unknown producer operation; return typed refusal. ")
       "Bind every returned identity and pin to this request.\n\n" (pr-str request)))

(defn- producer-invoke!
  "Serving adapter for evidence production/admission, separate from repair
  execution.  It returns only the typed map the futon2 producer expects; it
  never executes a proposed acceptance command."
  [seat operation request]
  (let [agent (reg/get-agent seat)
        prompt (producer-prompt operation request)
        result (when (and agent (invoke-capable? agent)
                          (available-seat-state? (:agent/status agent)))
                 (reg/invoke-agent! seat prompt {:mode "work" :caller "war-machine"
                                                  :session-id (:agent/session-id agent)}))
        parsed (when (:ok result) (one-edn-map (:result result)))]
    (cond
      (not agent) {:status :failed :reason :meta-self-heal/producer-seat-unavailable}
      ;; F9 (click wm-click-2b1d568c, 2026-10-19): the transport's own error
      ;; ("Exit 1: exceeded retry limit, last status: 429 Too Many Requests")
      ;; was dropped here, so the record could not say the seat was
      ;; rate-limited; it is retained verbatim as :detail.
      (not (:ok result)) {:status :failed :reason :meta-self-heal/producer-invoke-failed
                          :detail (or (:error result) (:message result))}
      (not (map? parsed)) {:status :failed :reason :meta-self-heal/producer-response-malformed}
      :else parsed)))

(defn- hex-sha256 [^bytes content]
  (format "%064x" (java.math.BigInteger. 1
                                          (.digest (doto (MessageDigest/getInstance "SHA-256")
                                                     (.update content))))))

(defn- safe-test-id [test-id]
  (str/replace test-id #"[^A-Za-z0-9._-]" "-"))

(defn- acceptance-summary [output]
  (let [lines (str/split-lines output)
        test-summary (->> lines
                          (filter #(or (re-find #"^Ran [0-9]+ tests" %)
                                       (re-find #"^[0-9]+ failures, [0-9]+ errors" %)))
                          (str/join "\n"))]
    (if (seq test-summary)
      test-summary
      (str/trim (or (last (remove str/blank? lines)) "")))))

(defn- admissible-acceptance-command? [command]
  (and (vector? command)
       (every? string? command)
       (= ["clojure" "-M:test" "-n"] (subvec command 0 (min 3 (count command))))
       (or (and (= 4 (count command))
                (re-matches command-name-pattern (nth command 3)))
           (and (= 6 (count command))
                (= "-v" (nth command 4))
                (re-matches command-name-pattern (nth command 3))
                (re-matches command-name-pattern (nth command 5))))))

(defn- source-location [{:keys [namespace path text]}]
  (let [segments (when (string? namespace) (str/split namespace #"\." -1))
        valid? (and (seq segments)
                    (every? #(and (re-matches command-name-pattern %)
                                  (not (#{"." ".."} %)))
                            segments))
        derived (when valid?
                  (str "test/"
                       (str/join "/" (map #(str/replace % "-" "_") segments))
                       ".clj"))
        test-root (.getCanonicalFile (io/file *acceptance-repository* "test"))
        file (when derived (.getCanonicalFile (io/file *acceptance-repository* derived)))
        confined? (and file
                       (.startsWith (.toPath file) (.toPath test-root))
                       (not= file test-root))]
    (when (and confined? (= derived path) (string? text))
      {:file file :test-root test-root :derived derived :text text})))

(defn- create-parent-dirs! [^java.io.File parent]
  (let [missing (loop [dir parent acc []]
                  (if (.exists dir)
                    acc
                    (recur (.getParentFile dir) (conj acc dir))))]
    (.mkdirs parent)
    missing))

(defn- remove-materialized! [^java.io.File file created-dirs]
  (when (.exists file) (.delete file))
  (doseq [^java.io.File dir created-dirs]
    (when (and (.isDirectory dir) (empty? (.listFiles dir)))
      (.delete dir))))

(defn- write-run-result [id output-file output outcome exit]
  (let [instant (str/replace (str (Instant/now)) #"[:.]" "-")
        log-file (io/file *acceptance-runs-root*
                          (str (safe-test-id id) "-" instant ".log"))
        bytes (.getBytes output "UTF-8")]
    (.mkdirs (.getParentFile log-file))
    (with-open [stream (io/output-stream log-file)] (.write stream bytes))
    (when (and output-file (.exists (io/file output-file)))
      (.delete (io/file output-file)))
    {:status :ran :outcome outcome :exit exit
     :summary (acceptance-summary output)
     :run-pin {:path (.getCanonicalPath log-file) :sha256 (hex-sha256 bytes)}}))

(defn- launch-bounded-acceptance! [timeout-ms id command]
  (let [job-id (str "wm-acceptance-" (safe-test-id id) "-" (System/currentTimeMillis))
        unit (str job-id ".service")
        receipt-file (str bounded-test-dir "/" job-id ".receipt.json")
        output-file (str *acceptance-runs-root* "/." job-id ".pending.log")
        timeout-seconds (max 1 (long (Math/ceil (/ timeout-ms 1000.0))))
        command-text (str/join " " command)
        ;; --collect: a unit that exits nonzero (the expected result of a
        ;; test that pins an injury) is unloaded instead of retained as
        ;; "loaded failed" for every admission attempt (codex-2 review of
        ;; 21057475f: three retained units after one test).
        launch ["systemd-run" "--user" "--collect" (str "--unit=" unit)
                "--slice=futon-testing.slice" "--property=TasksMax=1280"
                (str "--property=RuntimeMaxSec=" timeout-seconds "s")
                "--property=KillMode=control-group" "python3" bounded-test-runner
                "--receipt" receipt-file "--output" output-file
                "--cwd" *acceptance-repository* command-text]
        _receipt-dir (.mkdirs (io/file bounded-test-dir))
        _output-dir (.mkdirs (io/file *acceptance-runs-root*))
        submitted (apply shell/sh launch)]
    (if-not (zero? (:exit submitted))
      {:status :failed :reason :meta-self-heal/acceptance-unit-launch-failed}
      (let [deadline (+ (System/currentTimeMillis) timeout-ms)
            receipt (loop []
                      (cond
                        (.isFile (io/file receipt-file))
                        (json/read-str (slurp receipt-file) :key-fn keyword)
                        (< (System/currentTimeMillis) deadline)
                        (do (Thread/sleep 100) (recur))
                        :else nil))]
        (if receipt
          (let [output (if (.isFile (io/file output-file)) (slurp output-file) "")
                exit (long (or (:inner-exit receipt) -1))
                outcome (cond
                          (zero? (long (or (:outer-exit receipt) 125))) :passed
                          (= "test-failure" (:reason receipt)) :failed
                          :else :errored)]
            (.delete (io/file receipt-file))
            (write-run-result id output-file output outcome exit))
          (let [_ (shell/sh "systemctl" "--user" "stop" unit)
                summary (str "bounded unit " job-id " did not finish within "
                             timeout-seconds "s")]
            (write-run-result id output-file (str summary "\n") :errored -1)))))))

(defn- run-acceptance!
  "Bounded, argv-only acceptance runner for wm-click-6cdf2a7c. It runs before
   review and records evidence; it neither interprets :expected nor claims
   acceptance."
  [timeout-ms {:keys [id command source]}]
  (let [location (source-location source)]
    (cond
    (not location)
    {:status :failed :reason :meta-self-heal/acceptance-source-path-refused}
    (.exists ^java.io.File (:file location))
    {:status :failed :reason :meta-self-heal/acceptance-source-path-refused}
    (not (nonblank? id)) {:status :failed :reason :acceptance-test-id-invalid}
    (not (admissible-acceptance-command? command))
    {:status :failed :reason :meta-self-heal/acceptance-command-not-admissible
     :command command}
    (not= (:namespace source) (nth command 3))
    {:status :failed :reason :meta-self-heal/acceptance-command-not-admissible
     :command command}
    :else
    (let [file ^java.io.File (:file location)
          created-dirs (create-parent-dirs! (.getParentFile file))
          result (atom nil)]
      (try
        (spit file (:text location))
        (reset! result (launch-bounded-acceptance! timeout-ms id command))
        (catch Throwable t
          (reset! result {:status :failed :reason :acceptance-runner-error
                          :summary (or (.getMessage t) (.getName (class t))) }))
        (finally
          (remove-materialized! file created-dirs)))
      (assoc @result :source-clean? (not (.exists file)))))))

(defn enact!
  "Verify the reviewed patch is canonical HEAD, then reload only its changed
   Clojure sources through the serving JVM's recorded reload authority."
  [{:keys [work-order] :as request}]
  (let [repo (:target-repository work-order)
        patch (get-in request [:transaction :receipts 0 :payload :patch-sha])
        patch (or patch (:patch-sha request))
        head (:out (git repo "rev-parse" "HEAD"))]
    (if-not (= patch head)
      {:status :failed :payload {:reason :patch-not-canonical-head
                                 :patch-sha patch :canonical-head head}}
      (let [parent (:out (git repo "rev-parse" (str patch "^")))
            changed (:out (git repo "diff" "--name-only" parent patch))
            sources (filter #(and (str/starts-with? % "src/")
                                  (str/ends-with? % ".clj"))
                            (str/split-lines changed))
            receipts (mapv #(code-identity/load-file-recorded!
                             (str repo "/" %)) sources)
            retry-click (str "wm-retry-" (:repair-id work-order) "-"
                             (:attempt work-order))]
        {:status :succeeded
         :payload {:patch-sha patch :retry-click retry-click
                   :integration-commit head :reload-receipt receipts}}))))

(defn ports
  "Resolve concrete ports after a successful readiness check. Retry only
   persists/enqueues; the supplied service callback runs it after release."
  [opts]
  (let [ready (readiness opts)]
    (if-not (= :ready (:status ready)) ready
      {:status :ready :readiness ready
       :agency-invoke-fn agency-invoke!
       :meta-diagnosis-fn #(producer-invoke! (:author (:cast ready)) :diagnosis %)
       :meta-acceptance-fn #(producer-invoke! (:author (:cast ready)) :acceptance-proposal %)
       :meta-acceptance-run-fn #(run-acceptance!
                                 (or (:meta-acceptance-timeout-ms opts)
                                     default-acceptance-timeout-ms)
                                 %)
       :meta-acceptance-review-fn #(producer-invoke! (:reviewer (:cast ready)) :acceptance-review %)
       :repair-enact-fn enact!
       :repair-retry-fn
       (fn [{:keys [work-order] :as request}]
         ((:queue-retry-fn opts)
          {:repair-id (:repair-id work-order)
           :injury-id (:injury-id work-order)
           :attempt (:attempt work-order)
           :patch-sha (get-in request [:transaction :receipts 0 :payload :patch-sha])
           :retry-click (get-in request [:transaction :receipts 2 :payload :retry-click])
           :request request})
         {:status :queued
          :payload {:repair-id (:repair-id work-order)
                    :patch-sha (get-in request [:transaction :receipts 0 :payload :patch-sha])
                    :retry-click (get-in request [:transaction :receipts 2 :payload :retry-click])}})})))
