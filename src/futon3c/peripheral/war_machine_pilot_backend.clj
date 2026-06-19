(ns futon3c.peripheral.war-machine-pilot-backend
  "Backend wrapper for the War Machine Pilot peripheral.

   PHASE 1 of INSTANTIATE (M-war-machine-pilot, 2026-05-24).
   Implements the `:anchors-read` domain tool against the live
   `~/code/futon5a/data/wm-ui-anchors.edn` substrate.  All other tools
   delegate to a wrapped inner backend.

   Per the read-only-first-then-extend pattern (futon3/library/peripherals/),
   Phase 1 is observation-only — no write tools land here.

   Cross-refs:
   - peripherals/read-only-first-then-extend (Phase 1 anchor)
   - peripherals/constrained-execution-envelope (envelope-as-valid-action-space)
   - mission_backend.clj (clone-from shape donor)"
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.peripheral.tools :as tools]
            [futon3c.util.edn-comment-preserving :as edn-comments]))

;; =============================================================================
;; Substrate paths
;; =============================================================================

(def ^:private default-wm-ui-anchors-path
  "/home/joe/code/futon5a/data/wm-ui-anchors.edn")

(def ^:private futon3c-base "http://127.0.0.1:7070")

(def ^:private playwright-probe-whitelist
  "Phase 2 INSTANTIATE: pilot can only run named probes from this whitelist
   (per `peripherals/constrained-execution-envelope` — the envelope is the
   space of valid actions, not a list of forbidden ones).  Probes are
   read-only by claude-9's discipline (they verify rendered UI state)."
  #{"wm-anchor-0004-verify.mjs" "wm-anchor-0001-verify.mjs"
    "wm-anchor-0002-verify.mjs" "wm-anchor-0008-verify.mjs"
    "wm-anchor-0009-verify.mjs" "wm-anchor-0010-verify.mjs"
    "wm-hud-cluster-verify.mjs" "wm-tour.mjs"
    "wm-detail-probe.mjs" "wm-proxy-verify.mjs"
    "wm-ghost-repro.mjs" "wm-deprecate-verify.mjs"})

(def ^:private playwright-probe-dir
  "/home/joe/code/futon2/web/war-machine")

(def ^:private pilot-action-allowed-roots
  ["/home/joe/code/futon2/web/war-machine"
   "/home/joe/code/futon5a/data"])

;; =============================================================================
;; :anchors-read implementation
;; =============================================================================

(defn read-anchors-summary
  "Read the wm-ui-anchors.edn file and produce a structured summary.

   Returns {:total N :by-status {:open M :addressed K ...}
            :by-batch {...} :by-sub-kind {...}
            :recent-addressed [<5 ids>] :coherence-row-count N}.

   Reads the file from disk every call (no caching) — anchor state is
   live-mutated by the pilot's own discharges, so per-call freshness
   is the discipline."
  ([] (read-anchors-summary default-wm-ui-anchors-path))
  ([path]
   (let [data (-> path slurp edn/read-string)
         anchors (:anchors data)
         coherence (:coherence-evidence data)
         addressed (filter #(= :addressed (:status %)) anchors)]
     {:source-file         path
      :read-at             (str (java.time.Instant/now))
      :total               (count anchors)
      :by-status           (frequencies (map :status anchors))
      :by-disposition      (frequencies (keep :disposition anchors))
      :by-batch            (frequencies (map :batch anchors))
      :by-severity         (frequencies (keep :severity anchors))
      :coherence-row-count (count coherence)
      :coherence-sub-kinds (frequencies (keep :sub-kind coherence))
      :addressed-count     (count addressed)
      :recent-addressed    (->> addressed
                                (take-last 5)
                                (mapv :id))
      :open-substantive    (->> anchors
                                (filter #(and (= :open (:status %))
                                              (not= :recommend-deprecate (:disposition %))))
                                (mapv :id))})))

;; =============================================================================
;; PilotBackend defrecord — wraps inner backend; adds :anchors-read
;; =============================================================================

;; =============================================================================
;; Phase 2 transport tool implementations
;; =============================================================================

(defn- arg-map
  "Normalise the args sequence (per cycle.clj :args [] discipline) into a map.
   Conventions: args is either a single map, or a flat seq of k/v pairs."
  [args]
  (cond
    (and (= 1 (count args)) (map? (first args))) (first args)
    (even? (count args)) (apply hash-map args)
    :else {}))

(defn bell-emit
  "Phase 2 :bell-emit tool.  POST /api/alpha/bell with structured payload.
   Per `peripherals/canonical-typed-event-vs-side-channel`, the payload follows
   a declared schema; not free-form text.

   Schema: {:agent-id target-agent-id :prompt prompt-text}.
   Pilot-emitted bells carry :pilot-event keyword identifying the typed event."
  [{:keys [agent-id prompt pilot-event]}]
  (try
    (let [body (cond-> {:agent-id (or agent-id "claude-10")
                        :prompt   (or prompt "[pilot] empty bell")}
                 pilot-event (assoc :pilot-event pilot-event))
          resp (http/post (str futon3c-base "/api/alpha/bell")
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string body)})]
      {:ok true :result {:status (:status resp)
                         :body (json/parse-string (:body resp) true)
                         :payload-schema body}})
    (catch Throwable t
      {:ok false :error (str "bell-emit failed: " (.getMessage t))})))

(defn heartbeat-emit
  "Phase 3.5 :heartbeat-emit tool. Emits a typed heartbeat bell distinct from
   cycle completion so watchdogs can observe progress rather than cycle-open
   duration."
  [{:keys [cycle-id step-count elapsed-ms work-tag detail]}]
  (try
    (let [body (cond-> {:agent-id "claude-10"
                        :prompt (str "[pilot/heartbeat] cycle=" (or cycle-id "<none>")
                                     " step-count=" (or step-count 0)
                                     " elapsed-ms=" (or elapsed-ms 0))
                        :pilot-event :pilot/heartbeat
                        :cycle-id cycle-id
                        :step-count step-count
                        :elapsed-ms elapsed-ms}
                 work-tag (assoc :work-tag work-tag)
                 detail (assoc :detail detail))
          resp (http/post (str futon3c-base "/api/alpha/bell")
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string body)})
          parsed-body (try (json/parse-string (:body resp) true)
                           (catch Throwable _ (:body resp)))]
      {:ok true
       :result {:status (:status resp)
                :bell-job-id (when (map? parsed-body) (:job-id parsed-body))
                :body parsed-body
                :payload-schema body}})
    (catch Throwable t
      {:ok false :error (str "heartbeat-emit failed: " (.getMessage t))})))

(defn walkie-emit
  "Phase 2 :walkie-{psr,pur,par} tool.  POST /api/alpha/evidence/<kind> with
   the appropriate evidence-record body.  Kind is one of :psr :pur :par."
  [kind payload]
  (try
    (let [path (case kind :psr "/api/alpha/evidence/psr"
                          :pur "/api/alpha/evidence/pur"
                          :par "/api/alpha/evidence/par")
          resp (http/post (str futon3c-base path)
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string payload)})]
      {:ok true :result {:status (:status resp)
                         :body (try (json/parse-string (:body resp) true) (catch Throwable _ (:body resp)))
                         :kind kind}})
    (catch Throwable t
      {:ok false :error (str "walkie-" (name kind) " failed: " (.getMessage t))})))

(defn wm-api-query
  "Phase 2 :wm-api-query tool.  GET /api/alpha/war-machine (read-only)."
  [_]
  (try
    (let [resp (http/get (str futon3c-base "/api/alpha/war-machine")
                         {:headers {"Accept" "application/json"}})]
      {:ok true :result (try (json/parse-string (:body resp) true)
                             (catch Throwable _ (:body resp)))})
    (catch Throwable t
      {:ok false :error (str "wm-api-query failed: " (.getMessage t))})))

(defn playwright-probe-run
  "Phase 2 :playwright-probe-run tool.  Runs a NAMED probe from the whitelist.
   Per `peripherals/constrained-execution-envelope`, the envelope grants only
   the valid probe-run action; arbitrary node invocation is structurally
   impossible."
  [{:keys [probe]}]
  (cond
    (not (string? probe))
    {:ok false :error ":probe arg must be a string"}

    (not (contains? playwright-probe-whitelist probe))
    {:ok false :error (str "probe " probe " not in whitelist; allowed: "
                           (vec (sort playwright-probe-whitelist)))}

    :else
    (try
      (let [result (shell/sh "node" (str playwright-probe-dir "/" probe)
                             :dir playwright-probe-dir)]
        {:ok true :result {:probe probe
                           :exit (:exit result)
                           :stdout (let [s (:out result)] (if (> (count s) 4000) (str (subs s 0 4000) "\n...[truncated]") s))
                           :stderr (let [s (:err result)] (if (> (count s) 1000) (subs s 0 1000) s))}})
      (catch Throwable t
        {:ok false :error (str "playwright-probe-run failed: " (.getMessage t))}))))

;; =============================================================================
;; Phase 3 substantive tool implementations (consent-gate + anchor-flip)
;; =============================================================================

(defn consent-gate-emit
  "Phase 3 :consent-gate-emit tool.  Emits an intent-handshake bell to the
   operator carrying intent / scope / constraints / success-criteria, AND
   returns a `:consent-gate-event-id` that subsequent substantive Commands
   in the same cycle MUST cite (per Pilot-I1).

   Per `agent/intent-handshake-is-binding`: the run binds the intent and
   scope via this event; substantive actions thereafter are contracted to
   the bound scope."
  [{:keys [intent scope constraints success-criteria target-anchor-id]
    :as payload}]
  (let [event-id (str "cg-" (java.util.UUID/randomUUID))
        emitted-at (str (java.time.Instant/now))
        bell-prompt (str "[pilot/consent-gate-emit] " event-id
                         " intent=" (pr-str intent)
                         " scope=" (pr-str scope)
                         " constraints=" (pr-str constraints)
                         " success-criteria=" (pr-str success-criteria)
                         (when target-anchor-id (str " target-anchor=" target-anchor-id)))]
    (try
      (let [body {:agent-id "claude-10"
                  :prompt bell-prompt
                  :pilot-event :pilot/consent-gate-emit
                  :consent-gate-event-id event-id
                  :consent-gate-payload payload}
            resp (http/post (str futon3c-base "/api/alpha/bell")
                            {:headers {"Content-Type" "application/json"}
                             :body (json/generate-string body)})]
        {:ok true :result {:consent-gate-event-id event-id
                           :emitted-at emitted-at
                           :status (:status resp)
                           :job-id (try (-> resp :body (json/parse-string true) :job-id)
                                        (catch Throwable _ nil))
                           :payload payload}})
      (catch Throwable t
        {:ok false :error (str "consent-gate-emit failed: " (.getMessage t))}))))

(defn- substantive-arg-check
  "Pilot-I1 enforcement: substantive tool calls MUST include :consent-gate-event-id
   in their args.  Returns nil if args pass; returns an error map otherwise."
  [tool-id args-map]
  (cond
    (not (string? (:consent-gate-event-id args-map)))
    {:ok false
     :error (str "Pilot-I1 violation: substantive tool " tool-id
                 " requires :consent-gate-event-id in args (string).")
     :pilot-invariant :Pilot-I1}

    (not (str/starts-with? (:consent-gate-event-id args-map) "cg-"))
    {:ok false
     :error (str "Pilot-I1 violation: :consent-gate-event-id " (:consent-gate-event-id args-map)
                 " is not a well-formed pilot consent-gate event id (expected prefix 'cg-').")
     :pilot-invariant :Pilot-I1}

    :else nil))

(defn- canonical-path
  [path]
  (.getCanonicalPath (io/file path)))

(defn- path-under-root?
  [path root]
  (let [canon-path (canonical-path path)
        canon-root (canonical-path root)
        root-prefix (str canon-root java.io.File/separator)]
    (or (= canon-path canon-root)
        (str/starts-with? canon-path root-prefix))))

(defn- pilot-action-target-check
  [target-file]
  (cond
    (not (string? target-file))
    {:ok false :error ":target-file arg must be a string"}

    (not-any? #(path-under-root? target-file %) pilot-action-allowed-roots)
    {:ok false
     :error (str ":target-file " target-file
                 " is outside pilot-action v0 allowed roots: "
                 (vec pilot-action-allowed-roots))}

    (not (.exists (io/file target-file)))
    {:ok false :error (str ":target-file not found: " target-file)}

    :else nil))

(defn- normalize-pilot-change-set
  [{:keys [change-set diff]}]
  (cond
    (vector? change-set) change-set
    (map? change-set) [change-set]
    (map? diff) [diff]
    :else nil))

(defn- apply-pilot-change-op
  [content {:keys [op old-string new-string]}]
  (let [resolved-op (or op :replace-first)]
    (case resolved-op
      :replace-first
      (cond
        (not (string? old-string))
        {:ok false :error ":replace-first requires string :old-string"}

        (not (string? new-string))
        {:ok false :error ":replace-first requires string :new-string"}

        (not (str/includes? content old-string))
        {:ok false :error (str ":old-string not found for :replace-first: " (pr-str old-string))}

        :else
        {:ok true
         :content (str/replace-first content old-string new-string)
         :change-count 1})

      {:ok false
       :error (str "unsupported pilot-action op " (pr-str resolved-op)
                   "; v0 supports only :replace-first")})))

(defn anchor-flip
  "Phase 3 :anchor-flip tool.  Updates a specific anchor's :status + adds
   pilot-flip metadata.  SUBSTANTIVE — Pilot-I1 enforced; WM-I5 preserved
   (cannot delete an :addressed anchor without explicit re-opening).

   :anchors-path defaults to the live wm-ui-anchors.edn but the Phase 3 v0
   demo overrides to /tmp/wm-ui-anchors-pilot-test.edn — pprint-based write
   strips comments, which is unacceptable for the live substrate.  Switching
   the real file to a comment-preserving writer is a substrate-creation
   thread (Phase 3 finding)."
  [{:keys [anchor-id new-status consent-gate-event-id rationale anchors-path] :as args}]
  (or (substantive-arg-check :anchor-flip args)
      (cond
        (not (string? anchor-id))
        {:ok false :error ":anchor-id arg must be a string"}

        (nil? new-status)
        {:ok false :error ":new-status arg required"}

        :else
        (try
          (let [path (or anchors-path default-wm-ui-anchors-path)
                data (-> path slurp edn/read-string)
                anchors (:anchors data)
                target (first (filter #(= anchor-id (:id %)) anchors))]
            (cond
              (nil? target)
              {:ok false :error (str "anchor-id " anchor-id " not found")}

              ;; WM-I5 preservation: cannot delete an :addressed anchor without re-opening
              (and (= :addressed (:status target))
                   (= :deleted new-status))
              {:ok false :error "WM-I5 violation: cannot delete an :addressed anchor"
               :pilot-invariant :WM-I5}

              :else
              (let [;; Build new anchor map preserving everything; add pilot-flip metadata
                    pilot-flip-meta {:flipped-by-pilot true
                                     :flipped-at (str (java.time.Instant/now))
                                     :cited-consent-gate-event-id consent-gate-event-id
                                     :prior-status (:status target)
                                     :rationale rationale}
                    new-target (-> target
                                   (assoc :status new-status)
                                   (assoc :pilot-flip-trail
                                          (conj (or (:pilot-flip-trail target) [])
                                                pilot-flip-meta)))
                    new-anchors (mapv #(if (= (:id %) anchor-id) new-target %) anchors)
                    new-data (assoc data :anchors new-anchors)]
                (edn-comments/write-edn-preserving-comments path new-data)
                {:ok true :result {:anchor-id anchor-id
                                   :prior-status (:status target)
                                   :new-status new-status
                                   :pilot-flip-meta pilot-flip-meta
                                   :file path
                                   :file-size-bytes (.length (io/file path))}})))
          (catch Throwable t
            {:ok false :error (str "anchor-flip failed: " (.getMessage t))})))))

(defn coherence-row-author
  "Phase 3 :coherence-row-author tool. Appends a validated row map to the
   top-level :coherence-evidence vector while preserving substrate comments."
  [{:keys [row-content anchors-path] :as args}]
  (or (substantive-arg-check :coherence-row-author args)
      (let [required-keys [:id :pairs :coherence-check :evidence-kind :landed :template-status]]
        (cond
          (not (map? row-content))
          {:ok false :error ":row-content arg must be a map"}

          (seq (remove #(contains? row-content %) required-keys))
          {:ok false
           :error (str ":row-content missing required keys "
                       (vec (remove #(contains? row-content %) required-keys)))}

          (not (string? (:id row-content)))
          {:ok false :error ":row-content :id must be a string"}

          :else
          (try
            (let [path (or anchors-path default-wm-ui-anchors-path)
                  data (-> path slurp edn/read-string)
                  coherence (vec (:coherence-evidence data))]
              (if (some #(= (:id %) (:id row-content)) coherence)
                {:ok false
                 :error (str "coherence row id already exists: " (:id row-content))}
                (let [new-data (assoc data :coherence-evidence (conj coherence row-content))]
                  (edn-comments/write-edn-preserving-comments path new-data)
                  {:ok true
                   :result {:row-id (:id row-content)
                            :file path
                            :coherence-row-count (count (:coherence-evidence new-data))
                            :file-size-bytes (.length (io/file path))}})))
            (catch Throwable t
              {:ok false :error (str "coherence-row-author failed: " (.getMessage t))}))))))

(defn hop-trigger
  "Phase 3 :hop-trigger tool (E-pilot-hop-trigger-wiring §(7)).  Transitions
   the pilot agent's inhabitation from war-machine-pilot into a destination
   peripheral (e.g. street-sweeper, night-shift) using agency.registry's
   bidirectional hop primitives.

   Substantive — Pilot-I1 requires :consent-gate-event-id citation.  The
   destination peripheral's cycle inherits the pilot's cg-id binding; the
   peripheral calls (agency.registry/hop-back! agent-id) at cycle-end to
   restore the pilot inhabitation.

   Args:
     :agent-id          — required; the pilot agent id (e.g. \"claude-1\")
     :to-peripheral     — required; destination peripheral id keyword/string
     :consent-gate-event-id — required per Pilot-I1

   Returns:
     {:ok true :result {<registry hop! result + emitted hop-event id>}}
     {:ok false :error ... :pilot-invariant :Pilot-I1} on violation"
  [{:keys [agent-id to-peripheral] :as args}]
  (or (substantive-arg-check :hop-trigger args)
      (cond
        (str/blank? (some-> agent-id str))
        {:ok false :error ":hop-trigger requires :agent-id (string)"}

        (str/blank? (some-> to-peripheral str))
        {:ok false :error ":hop-trigger requires :to-peripheral (string or keyword)"}

        :else
        (try
          (let [hop! (requiring-resolve 'futon3c.agency.registry/hop!)
                r (hop! agent-id to-peripheral)]
            (if (:ok r)
              {:ok true :result r}
              {:ok false :error (str ":hop-trigger registry rejected hop: "
                                     (:error r)
                                     (when (:by r) (str " (by " (:by r) ")")))
               :registry-result r}))
          (catch Throwable t
            {:ok false :error (str "hop-trigger failed: " (.getMessage t))})))))

(defn pilot-action
  "Phase 3 :pilot-action tool. Performs a narrowly scoped substantive file edit.

   v0 semantics:
   - :target-file must already exist under one of `pilot-action-allowed-roots`
   - args carry either :change-set (vector/map) or :diff (single structured op map)
   - supported op: {:op :replace-first :old-string s :new-string s}"
  [{:keys [target-file rationale] :as args}]
  (or (substantive-arg-check :pilot-action args)
      (if-let [target-error (pilot-action-target-check target-file)]
        target-error
        (let [ops (normalize-pilot-change-set args)]
          (cond
            (not (seq ops))
            {:ok false
             :error ":pilot-action requires :change-set (vector/map) or structured :diff map"}

            :else
            (try
              (let [f (io/file target-file)
                    original (slurp f)
                    result (reduce (fn [acc op]
                                     (if-not (:ok acc)
                                       (reduced acc)
                                       (let [step (apply-pilot-change-op (:content acc) op)]
                                         (if (:ok step)
                                           {:ok true
                                            :content (:content step)
                                            :change-count (+ (:change-count acc)
                                                             (:change-count step))}
                                           (reduced step)))))
                                   {:ok true :content original :change-count 0}
                                   ops)]
                (if-not (:ok result)
                  result
                  (do
                    (spit f (:content result))
                    {:ok true
                     :result {:file (canonical-path target-file)
                              :change-count (:change-count result)
                              :rationale rationale
                              :bytes (.length f)}})))
              (catch Throwable t
                {:ok false :error (str "pilot-action failed: " (.getMessage t))})))))))

;; =============================================================================
;; PilotBackend defrecord — Phase 3: substrate-write tools land
;; =============================================================================

(defrecord PilotBackend [inner-backend anchors-path]
  tools/ToolBackend
  (execute-tool [_ tool-id args]
    (case tool-id
      :anchors-read
      (try
        {:ok true :result (read-anchors-summary (or anchors-path default-wm-ui-anchors-path))}
        (catch Throwable t
          {:ok false :error (str "anchors-read failed: " (.getMessage t))}))

      :bell-emit            (bell-emit (arg-map args))
      :heartbeat-emit       (heartbeat-emit (arg-map args))
      :walkie-psr           (walkie-emit :psr (arg-map args))
      :walkie-pur           (walkie-emit :pur (arg-map args))
      :walkie-par           (walkie-emit :par (arg-map args))
      :wm-api-query         (wm-api-query (arg-map args))
      :playwright-probe-run (playwright-probe-run (arg-map args))
      :consent-gate-emit    (consent-gate-emit (arg-map args))
      :anchor-flip          (anchor-flip (arg-map args))
      :coherence-row-author (coherence-row-author (arg-map args))
      :pilot-action         (pilot-action (arg-map args))
      :hop-trigger          (hop-trigger (arg-map args))

      ;; All other tools (cycle-control via mock-backend in v0; delegated
      ;; standard observations :read/:glob/:grep/:bash-readonly when a
      ;; RealBackend is present) → inner-backend.
      (tools/execute-tool inner-backend tool-id args))))

(defn make-pilot-backend
  "Create a PilotBackend that wraps an inner ToolBackend.
   The inner backend handles standard tools (:read, :glob, :grep, :bash-readonly,
   :cycle-begin, :cycle-advance); this wrapper handles :anchors-read against
   the live wm-ui-anchors.edn file."
  ([] (make-pilot-backend (tools/make-mock-backend) default-wm-ui-anchors-path))
  ([inner-backend] (make-pilot-backend inner-backend default-wm-ui-anchors-path))
  ([inner-backend anchors-path]
   (->PilotBackend inner-backend anchors-path)))
