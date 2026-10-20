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

(def ^:private response-preview-head 384)
(def ^:private response-preview-tail 128)

(defn- response-preview
  "F18 (click wm-click-8df4e8d2, 2026-10-19, the first click cast on Zai
   seats): the author's acceptance proposal was refused as
   :producer-response-malformed and the refusal carried no :detail, so the
   record could not say whether the seat returned JSON, prose, or a fenced
   map. A bounded, whitespace-collapsed head and tail of the text are retained."
  [text]
  (let [s (str/trim (str/replace (str text) #"\s+" " "))]
    (if (> (count s) (+ response-preview-head response-preview-tail))
      (str (subs s 0 response-preview-head) "\u2026"
           (subs s (- (count s) response-preview-tail)))
      s)))

(defn- edn-type [value]
  (cond
    (nil? value) "nil"
    (map? value) "map"
    (vector? value) "vector"
    (set? value) "set"
    (sequential? value) "sequence"
    (keyword? value) "keyword"
    (symbol? value) "symbol"
    (string? value) "string"
    (number? value) "number"
    (boolean? value) "boolean"
    :else (.getSimpleName (class value))))

(defn- keyword-keyed-map? [value]
  (and (map? value) (some keyword? (keys value))))

(defn- wrong-edn-shape [value]
  (str "read a " (edn-type value) ", not a map with keyword keys"))

(def ^:private edn-eof (Object.))

(defn- read-one-edn [text]
  (let [reader (java.io.PushbackReader. (java.io.StringReader. text))
        value (edn/read {:eof edn-eof} reader)
        trailing (edn/read {:eof edn-eof} reader)]
    (when-not (identical? edn-eof trailing)
      (throw (ex-info "read more than one EDN form" {:first-value value})))
    value))

(defn- read-repair-candidate [text]
  (try
    {:value (read-one-edn text)}
    (catch clojure.lang.ExceptionInfo e
      (when (contains? (ex-data e) :first-value)
        {:value (:first-value (ex-data e))}))
    (catch Throwable _ nil)))

(defn- braces-outside-strings [text]
  (loop [chars (seq (str text))
         in-string? false
         escaped? false
         opened 0
         closed 0]
    (if-let [ch (first chars)]
      (cond
        escaped?
        (recur (next chars) in-string? false opened closed)

        (and in-string? (= ch \\))
        (recur (next chars) true true opened closed)

        (= ch \")
        (recur (next chars) (not in-string?) false opened closed)

        in-string?
        (recur (next chars) true false opened closed)

        (= ch \{)
        (recur (next chars) false false (inc opened) closed)

        (= ch \})
        (recur (next chars) false false opened (inc closed))

        :else
        (recur (next chars) false false opened closed))
      {:opened opened :closed closed})))

(defn- one-edn-map
  "One EDN map, bare or inside a code fence under any alphabetic tag (F18: a
   seat that fences its map as ```json still returned the map). The map must
   carry at least one keyword key: JSON with unquoted values, such as
   {\"x\": 1}, reads as EDN with string keys and keyword values and is not a
   map the caller can act on (claude-2's review of ca2fa4074, 2026-10-19)."
  [text]
  (let [text (str/trim (str text))
        fenced (re-matches #"(?s)```[A-Za-z]*\s*(.*?)\s*```" text)
        reader-text (or (second fenced) text)]
    (try
      ;; First form only, as before F21: a map followed by trailing prose is
      ;; the map. read-one-edn's one-form check guards the repair candidate,
      ;; where an appended brace must not close a second form.
      (let [value (edn/read-string reader-text)]
        (if (keyword-keyed-map? value)
          {:value value}
          {:read-error (wrong-edn-shape value)}))
      (catch Throwable t
        (let [message (or (ex-message t) (.getName (class t)))
              eof? (str/includes? message "EOF while reading")
              {:keys [opened closed]} (when eof?
                                        (braces-outside-strings reader-text))
              counted-error (if eof?
                              (str message "; " opened " `{` opened, " closed
                                   " `}` closed outside strings")
                              message)
              deficit (when eof? (- opened closed))
              repair (when (and deficit (pos? deficit))
                       (read-repair-candidate
                        (str reader-text (apply str (repeat deficit "}")))))]
          (cond
            (and repair (keyword-keyed-map? (:value repair)))
            (assoc repair :read-repair {:appended deficit :error counted-error})

            repair
            {:read-error (wrong-edn-shape (:value repair))}

            :else
            {:read-error counted-error}))))))

(def ^:dynamic *rate-limit-backoff-ms*
  "Wait before the one retry of a seat invoke the provider rate-limited."
  90000)

(defn- rate-limited-error? [result]
  (let [message (str (or (:error result) (:message result)))]
    (boolean (re-find #"\b429\b|Too Many Requests|rate.?limit" message))))

(defn- invoke-with-rate-limit-retry!
  "Invoke SEAT once; if the transport reports a provider rate limit, wait
   *rate-limit-backoff-ms* and invoke once more. Clicks wm-click-36b47627 and
   wm-click-fdc08aea (2026-10-19, 20:43Z and 20:50Z) were each spent on a
   429 at the review invoke seconds after the author invoke, while a one-word
   probe of the same seat answered three minutes later: the limit is a burst,
   and the CLI's own retries are too quick to outlast it. Any other failure
   is returned as it is. A retried result carries :rate-limit-retry."
  [seat prompt options]
  (let [first-result (reg/invoke-agent! seat prompt options)]
    (if (and (not (:ok first-result)) (rate-limited-error? first-result))
      (let [first-error (str (or (:error first-result) (:message first-result)))]
        (println (str "[wm-self-heal] " seat " rate-limited; one retry after "
                      *rate-limit-backoff-ms* " ms: " first-error))
        (Thread/sleep *rate-limit-backoff-ms*)
        (assoc (reg/invoke-agent! seat prompt options)
               :rate-limit-retry {:after-ms *rate-limit-backoff-ms*
                                  :first-error first-error}))
      first-result)))

(defn agency-invoke!
  "Synchronous Agency port used inside the already-checkpointed coordinator.
   The prompt requires a single EDN result so phase evidence is machine typed."
  [{:keys [seat session-id request]}]
  (let [reset (reg/reset-session! seat)
        prompt (str "Dedicated META self-heal phase. Do not select ordinary work. "
                    "Execute only the sealed request and return exactly one EDN map "
                    "with :status and :payload (no prose).\n\n" (pr-str request))
        result (when (:ok reset)
                 (invoke-with-rate-limit-retry! seat prompt
                                                {:mode "work" :caller "war-machine"
                                                 :session-id session-id}))
        parsed-result (when (:ok result) (one-edn-map (:result result)))
        parsed (:value parsed-result)
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

(defn- producer-prompt
  "Prompt text for one META producer operation.  F11b (click
  wm-click-fca1c921, 2026-10-19): the diagnosis prompt used to be one
  sentence and never asked for callable production functions, so the author
  seat refused with :production-function-identity-absent after the
  acceptance prompt demanded a test over them.  The diagnosis now carries
  :production-functions, which the acceptance prompt refers to by that name."
  [operation request]
  (str "Dedicated META " (name operation) ". Return exactly one EDN map and no prose. "
       "Do not select ordinary work, run tests, builds or code, write any file, or claim a repair. "
       (case operation
         :diagnosis
         (str "Success must be {:status :succeeded :diagnosis <map>}, where the map includes :classification, "
              ":primary-cause (the stage, the condition, and the affected nodes with reasons), and "
              ":production-functions, a vector of {:role <keyword> :var \"<ns>/<fn>\" :file \"<repo-relative path>\"}. "
              ":production-functions must contain at least one entry for each of the roles :selection-entrypoint, "
              ":required-loop-node-evaluator and :terminal-routing-entrypoint. Each :var must be a real, callable function "
              "that you found by reading the repository at /home/joe/code/futon2 (read-only shell commands on that repository, "
              "such as cat, sed -n, grep, ls, git show and git grep, are allowed and expected; nothing that runs code or writes), "
              "and :file is the repo-relative path of the file that defines it. These are the functions the acceptance test "
              "will exercise. A function you cannot find must be reported in a typed refusal "
              "({:status :failed :kind :production-function-identity-absent ...}), never invented. "
              "A refusal/failure must be typed. ")
         :acceptance-proposal
         (str "Success must be {:status :proposed :acceptance-test {:id <nonblank-string> :command [<string> ...] :expected <value> "
              ":source {:namespace <namespace-string> :path <test/... .clj> :text <complete-file-text>}}}. "
              "The source must be a complete clojure.test file that pins the injury from the diagnosis: it fails for the injury's reason on injured code and passes after repair, using only the real production functions named in the diagnosis's :production-functions, with no stubs. "
              "The command must be [\"clojure\" \"-M:test\" \"-n\" <that-namespace>] optionally followed by [\"-v\" <var>]. The specification is not run and is not accepted. ")
         :acceptance-review
         (str "This acceptance test was run before any repair, in the canonical checkout; the result is in :result. "
              "Admit it if it pins the injury (fails for the injury's reason) and is the right test for the repair; refuse with a typed reason otherwise. "
              "Accept only with {:status :accepted :injury-id ... :source-record-pin ... :diagnosis-evidence-pin ... :proposal-evidence-pin ... :acceptance-test {:id ... :command ... :expected ...}}, "
              "echoing those request values exactly. Do not echo the test source: the proposal pin identifies it "
              "(F24a, click wm-click-a8f4dea6, 2026-10-20: the reviewer's first yes left the 4 KB source out and was refused as an identity mismatch). "
              "A refusal/failure must be typed. ")
         "Unknown producer operation; return typed refusal. ")
       "Bind every returned identity and pin to this request.\n\n" (pr-str request)))

(defn- source-read-once [source]
  (let [namespace-symbol (gensym "futon3c.wm.source-read-")]
    (create-ns namespace-symbol)
    (try
      (binding [*ns* (the-ns namespace-symbol)
                *read-eval* false]
        (let [reader (clojure.lang.LineNumberingPushbackReader.
                      (java.io.StringReader. source))]
          (try
            (loop []
              (let [form (read {:eof ::eof :read-cond :allow} reader)]
                (if (= ::eof form)
                  {:status :readable}
                  (recur))))
            (catch Throwable t
              (let [data (ex-data t)
                    line (or (:clojure.error/line data) (.getLineNumber reader))
                    col (or (:clojure.error/column data) (.getColumnNumber reader))
                    message (or (ex-message t) (.getName (class t)))]
                {:status :unreadable
                 :error message
                 :line line
                 :col col
                 :formatted-error (str message " at " line ":" col)})))))
      (finally
        (remove-ns namespace-symbol)))))

(defn- remove-source-character [source line col]
  (let [line-start (loop [index 0
                          remaining-line line]
                     (cond
                       (= remaining-line 1) index
                       (>= index (count source)) nil
                       (= \newline (.charAt source index))
                       (recur (inc index) (dec remaining-line))
                       :else
                       (recur (inc index) remaining-line)))
        index (when line-start (+ line-start (dec col)))]
    (when (and index (< index (count source)) (pos? line) (pos? col))
      {:source (str (subs source 0 index) (subs source (inc index)))
       :removed (str (.charAt source index))})))

(defn- source-read-result [source]
  (let [read-result (source-read-once source)]
    (if (and (= :unreadable (:status read-result))
             (str/includes? (:error read-result) "Unmatched delimiter"))
      (let [line (:line read-result)
            col (dec (:col read-result))
            removal (remove-source-character source line col)
            ;; F23 review (claude-5): the character removed must be the one
            ;; delimiter the reader named; any other position falls back to
            ;; F22's re-ask rather than editing the author's text.
            named-delimiter (second (re-find #"Unmatched delimiter: (.)"
                                             (:error read-result)))
            candidate-read (when (and removal (= named-delimiter (:removed removal)))
                             (source-read-once (:source removal)))]
        (if (= :readable (:status candidate-read))
          {:status :repaired
           :source (:source removal)
           :source-repair {:removed (:removed removal)
                           :line line
                           :col col
                           :error (:error read-result)}}
          read-result))
      read-result)))

(defn- proposed-source-read [operation parsed]
  (let [source (get-in parsed [:acceptance-test :source :text])]
    (when (and (= :acceptance-proposal operation)
               (= :proposed (:status parsed))
               (string? source))
      (source-read-result source))))

(defn- producer-invoke!
  "Serving adapter for evidence production/admission, separate from repair
  execution.  It returns only the typed map the futon2 producer expects; it
  never executes a proposed acceptance command."
  [seat operation request]
  (let [agent (reg/get-agent seat)
        prompt (producer-prompt operation request)
        options (when agent {:mode "work" :caller "war-machine"
                             :session-id (:agent/session-id agent)})
        result (when (and agent (invoke-capable? agent)
                          (available-seat-state? (:agent/status agent)))
                 (invoke-with-rate-limit-retry! seat prompt options))
        first-read (when (:ok result) (one-edn-map (:result result)))
        first-preview (when (:ok result) (response-preview (:result result)))
        read-retry (when (and (:ok result) (:read-error first-read))
                     {:first-error (:read-error first-read)
                      :first-preview first-preview})
        complaint (when read-retry
                    (str "\n\nYour previous answer did not read as one EDN map ("
                         (:first-error read-retry)
                         "). Return the same map, complete and balanced, as one EDN map and nothing else."))
        read-second-result (when read-retry
                             (invoke-with-rate-limit-retry! seat (str prompt complaint) options))
        envelope-result (or read-second-result result)
        envelope-read (if read-second-result
                        (when (:ok read-second-result) (one-edn-map (:result read-second-result)))
                     first-read)
        envelope-parsed (:value envelope-read)
        first-source-read (proposed-source-read operation envelope-parsed)
        envelope-parsed (if (= :repaired (:status first-source-read))
                          (assoc-in envelope-parsed [:acceptance-test :source :text]
                                    (:source first-source-read))
                          envelope-parsed)
        source-retry (when (= :unreadable (:status first-source-read))
                       {:first-error (:error first-source-read)
                        :first-line (:line first-source-read)
                        :first-col (:col first-source-read)})
        source-complaint (when source-retry
                           (str "\n\nYour test source does not read: "
                                (:first-error source-retry) " at "
                                (:first-line source-retry) ":" (:first-col source-retry)
                                ". Return the same map with the source complete and balanced, as one EDN map and nothing else."))
        source-second-result (when source-retry
                               (invoke-with-rate-limit-retry! seat
                                                              (str prompt source-complaint)
                                                              options))
        final-result (or source-second-result envelope-result)
        final-read (if source-second-result
                     (when (:ok source-second-result) (one-edn-map (:result source-second-result)))
                     envelope-read)
        parsed (if source-second-result (:value final-read) envelope-parsed)
        final-source-read (if source-second-result
                            (proposed-source-read operation parsed)
                            first-source-read)
        parsed (if (= :repaired (:status final-source-read))
                 (assoc-in parsed [:acceptance-test :source :text]
                           (:source final-source-read))
                 parsed)
        source-read-error (when (and source-retry
                                     (= :unreadable (:status final-source-read)))
                            (:formatted-error final-source-read))
        source-repair (:source-repair final-source-read)
        read-repair (:read-repair final-read)
        transport-retry (or (:rate-limit-retry result)
                            (:rate-limit-retry read-second-result)
                            (:rate-limit-retry source-second-result))]
    (cond
      (not agent) {:status :failed :reason :meta-self-heal/producer-seat-unavailable}
      ;; F9 (click wm-click-2b1d568c, 2026-10-19): the transport's own error
      ;; ("Exit 1: exceeded retry limit, last status: 429 Too Many Requests")
      ;; was dropped here, so the record could not say the seat was
      ;; rate-limited; it is retained verbatim as :detail.
      (not (:ok final-result)) (cond-> {:status :failed
                                        :reason :meta-self-heal/producer-invoke-failed
                                        :detail (or (:error final-result)
                                                    (:message final-result))}
                                 read-retry (assoc :read-retry read-retry)
                                 read-repair (assoc :read-repair read-repair)
                                 source-retry (assoc :source-retry source-retry)
                                 source-read-error (assoc :source-read-error source-read-error)
                                 source-repair (assoc :source-repair source-repair)
                                 transport-retry
                                 (assoc :transport-retry transport-retry))
      (not (map? parsed)) (cond-> {:status :failed
                                   :reason :meta-self-heal/producer-response-malformed
                                   :read-error (:read-error final-read)
                                   :detail (response-preview (:result final-result))}
                            read-retry (assoc :read-retry read-retry)
                            read-repair (assoc :read-repair read-repair)
                            source-retry (assoc :source-retry source-retry)
                            source-read-error (assoc :source-read-error source-read-error)
                            source-repair (assoc :source-repair source-repair)
                            transport-retry
                            (assoc :transport-retry transport-retry))
      :else (cond-> parsed
              read-retry (assoc :read-retry read-retry)
              read-repair (assoc :read-repair read-repair)
              source-retry (assoc :source-retry source-retry)
              source-read-error (assoc :source-read-error source-read-error)
              source-repair (assoc :source-repair source-repair)
              transport-retry (assoc :transport-retry transport-retry)))))

(defn- hex-sha256 [^bytes content]
  (format "%064x" (java.math.BigInteger. 1
                                          (.digest (doto (MessageDigest/getInstance "SHA-256")
                                                     (.update content))))))

(defn- safe-test-id [test-id]
  (str/replace test-id #"[^A-Za-z0-9._-]" "-"))

(defn- acceptance-summary
  "The reviewer reads this line. F14 (off-click replay 3, 2026-10-19): a test
   that does not compile produced a summary that was the bounded runner's
   own correlation line, so the reviewer could not see the syntax error.
   Order: the clojure.test summary lines; else the first failure line
   (Syntax error, Exception, FAIL in, ERROR in, Execution error); else the
   last non-blank line that is not the runner's correlation line."
  [output]
  (let [lines (str/split-lines output)
        runner-noise? #(str/starts-with? % "bounded-test failure/resource correlation")
        test-summary (->> lines
                          (filter #(or (re-find #"^Ran [0-9]+ tests" %)
                                       (re-find #"^[0-9]+ failures, [0-9]+ errors" %)))
                          (str/join "\n"))
        failure-line (first (filter #(re-find #"^(Syntax error|Execution error|Exception|FAIL in|ERROR in)" %)
                                    lines))]
    (cond
      (seq test-summary) test-summary
      failure-line (str/trim failure-line)
      :else (str/trim (or (last (remove #(or (str/blank? %) (runner-noise? %)) lines)) "")))))

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

(defn executed-command
  "F13 (off-click replay 3, 2026-10-19): the admitted command is the
   proposal's declaration, [\"clojure\" \"-M:test\" \"-n\" ns (\"-v\" var)].
   futon2's :test alias carries cognitect.test-runner as a dependency but has
   no :main-opts, so that argv run verbatim fails before any test loads
   (\"-n (No such file or directory)\"). The port owns the real invocation:
   the runner main is inserted; the declared namespace and var are used as
   admitted, never re-parsed from text."
  [command]
  (into ["clojure" "-M:test" "-m" "cognitect.test-runner"] (subvec command 2)))

(defn- launch-bounded-acceptance! [timeout-ms id command]
  (let [job-id (str "wm-acceptance-" (safe-test-id id) "-" (System/currentTimeMillis))
        unit (str job-id ".service")
        receipt-file (str bounded-test-dir "/" job-id ".receipt.json")
        output-file (str *acceptance-runs-root* "/." job-id ".pending.log")
        timeout-seconds (max 1 (long (Math/ceil (/ timeout-ms 1000.0))))
        command-text (str/join " " (executed-command command))
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

(defn retry-click-id
  "Return the child-click identity for REPAIR-ID, ATTEMPT, and re-queue ordinal.

  Ordinal zero preserves the historical `wm-retry-<repair-id>-<attempt>`
  receipt form. Positive ordinals use the disjoint `wm-requeue-<ordinal>-<attempt>-<repair-id>`
  namespace, so no re-queued id can equal any legacy retry id even when a
  repair id itself contains hyphens or digits."
  [repair-id attempt requeue-ordinal]
  (assert (and (integer? requeue-ordinal) (not (neg? requeue-ordinal)))
          "re-queue ordinal must be a non-negative integer")
  (if (zero? requeue-ordinal)
    (str "wm-retry-" repair-id "-" attempt)
    (str "wm-requeue-" requeue-ordinal "-" attempt "-" repair-id)))

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
            retry-click (retry-click-id (:repair-id work-order)
                                        (:attempt work-order) 0)]
        {:status :succeeded
         :payload {:patch-sha patch :retry-click retry-click
                   :integration-commit head :reload-receipt receipts}}))))

(defn work-order-identities
  "The caller-supplied identities coordinator/prepare needs on its work order,
   taken from a ready readiness receipt: the three cast seats, one fresh
   distinct claim session per seat, and the target repository and worktree.

   The sessions are claim identities, not Agency session ids: reset-session!
   leaves a seat's session nil until its next invoke mints one, and
   agency-invoke! resets the seat and records the session it actually got in
   the phase receipt (:agency-session-id). The ledger requires each claim's
   sessions to be non-blank, distinct, and unused by any earlier claim; a
   per-click UUID under the seat name satisfies that without touching any
   seat. Click wm-click-c78d0f27 (2026-10-19) refused because no production
   code supplied these five fields, so prepare refused on every self-heal
   click."
  [ready]
  (let [cast (:cast ready)
        repo (:repository ready)
        claim-session (fn [seat] (str seat "/" (java.util.UUID/randomUUID)))]
    {:author (:author cast) :reviewer (:reviewer cast)
     :repair-reviewer (:repair-reviewer cast)
     :author-session (claim-session (:author cast))
     :reviewer-session (claim-session (:reviewer cast))
     :repair-reviewer-session (claim-session (:repair-reviewer cast))
     :target-repository repo
     :target-worktree repo}))

(defn ports
  "Resolve concrete ports after a successful readiness check. Retry only
   persists/enqueues; the supplied service callback runs it after release."
  [opts]
  (let [ready (readiness opts)]
    (if-not (= :ready (:status ready)) ready
      {:status :ready :readiness ready
       :work-order-identities (work-order-identities ready)
       :agency-invoke-fn agency-invoke!
       :meta-diagnosis-fn #(producer-invoke! (:author (:cast ready)) :diagnosis %)
       :meta-acceptance-fn #(producer-invoke! (:author (:cast ready)) :acceptance-proposal %)
       :meta-acceptance-run-fn #(run-acceptance!
                                 (or (:meta-acceptance-timeout-ms opts)
                                     default-acceptance-timeout-ms)
                                 %)
       :meta-acceptance-review-fn #(producer-invoke! (:reviewer (:cast ready)) :acceptance-review %)
       :repair-enact-fn enact!
       :retry-click-id-fn retry-click-id
       :repair-retry-fn
       (fn [{:keys [work-order] :as request}]
         (let [retry-click (or (:retry-click request)
                               (get-in request [:transaction :receipts 2 :payload :retry-click]))]
           ((:queue-retry-fn opts)
            (cond-> {:repair-id (:repair-id work-order)
                     :injury-id (:injury-id work-order)
                     :attempt (:attempt work-order)
                     :patch-sha (get-in request [:transaction :receipts 0 :payload :patch-sha])
                     :retry-click retry-click
                     :request request}
              (contains? request :requeue-ordinal)
              (assoc :requeue-ordinal (:requeue-ordinal request))))
           {:status :queued
            :payload {:repair-id (:repair-id work-order)
                      :patch-sha (get-in request [:transaction :receipts 0 :payload :patch-sha])
                      :retry-click retry-click}}))})))
