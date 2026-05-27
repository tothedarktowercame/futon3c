(ns futon3c.peripheral.street-sweeper
  "Street Sweeper peripheral — working-tree-hygiene capability envelope.

   E-street-sweeper.md (2026-05-25). Hop target from :war-machine-pilot
   when WM judgement is :stop-the-line on the :working-tree channel.
   Same agent identity across hops (I-1, I-3).

   Cross-refs:
   - clone-from: war_machine_pilot.clj
   - shapes: street_sweeper_shapes.clj (data-driven invariants — modular per Joe)
   - backend: street_sweeper_backend.clj
   - mission doc: futon3c/holes/missions/E-street-sweeper.md
   - related band-aid: futon3c/holes/missions/E-wm-staleness-meta-stop.md"
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.peripheral.cycle :as cycle]
            [futon3c.peripheral.street-sweeper-shapes :as sss]
            [futon3c.peripheral.street-sweeper-backend :as ssb]
            [futon3c.peripheral.tools :as tools]))

;; =============================================================================
;; Domain state init
;; =============================================================================

(defn- state-init
  "Initialise sweeper state. Records inhabitation moment + a sweep-ts that
   the defer-manifest writer can correlate with."
  [_context]
  (let [now (str (java.time.Instant/now))]
    {:sweeper-inhabited-at now
     :sweep-ts             (clojure.string/replace now #"[:.]" "-")
     :commits-landed       []
     :deferred-packets     []
     :pressure-before      nil
     :pressure-after       nil}))

;; =============================================================================
;; Fruit / exit context
;; =============================================================================

(defn- fruit
  "Hop-back-payload-shape fruit. Per E-street-sweeper.md §Hop-back payload schema."
  [state]
  {:sweeper-inhabited-at (:sweeper-inhabited-at state)
   :sweep-ts             (:sweep-ts state)
   :commits-landed       (or (:commits-landed state) [])
   :deferred-packets     (or (:deferred-packets state) [])
   :defer-count          (count (or (:deferred-packets state) []))
   :pressure-before      (:pressure-before state)
   :pressure-after       (:pressure-after state)
   :pressure-delta       (when (and (:pressure-before state) (:pressure-after state))
                           (- (:pressure-after state) (:pressure-before state)))
   :final-phase          (:current-phase state)})

(defn- exit-context
  [state]
  {:session-id           (:session-id state)
   :sweeper-inhabited-at (:sweeper-inhabited-at state)
   :sweep-ts             (:sweep-ts state)})

;; =============================================================================
;; Domain config — the 象 entering the cycle machine
;; =============================================================================

(def sweeper-domain-config
  "CycleDomainConfig for the Street Sweeper peripheral."
  {:domain-id          :street-sweeper
   :phase-order        sss/phase-order
   :phase-tools        sss/phase-allowed-tools
   :setup-tools        sss/setup-tools
   :tool-ops           sss/sweeper-tool-operation-kinds
   :required-outputs   sss/phase-required-outputs
   :cycle-begin-tool   :cycle-begin
   :cycle-advance-tool :cycle-advance
   :state-init-fn      state-init
   :fruit-fn           fruit
   :exit-context-fn    exit-context})

;; =============================================================================
;; Factory
;; =============================================================================

(defn make-sweeper
  "Create a Street Sweeper peripheral. Backend wraps an inner ToolBackend
   (mock by default; real one when invoked via Agency)."
  ([] (make-sweeper (tools/make-mock-backend)))
  ([backend]
   (cycle/make-cycle-peripheral sweeper-domain-config (ssb/make-sweeper-backend backend)))
  ([spec backend]
   (cycle/make-cycle-peripheral sweeper-domain-config spec (ssb/make-sweeper-backend backend))))

;; =============================================================================
;; Spike validation
;; =============================================================================

(defn spike-check
  "E-street-sweeper.md C1+C2: envelope file exists + cycle accepts the config.
   Returns {:valid-config? bool :make-sweeper-ok? bool ...}.

   Call via Drawbridge nREPL:
     (require '[futon3c.peripheral.street-sweeper :as ss])
     (ss/spike-check)"
  []
  (let [valid? (cycle/valid-domain-config? sweeper-domain-config)
        make-result (try (make-sweeper)
                         (catch Throwable t {:error t}))
        make-ok? (and (not (instance? Throwable make-result))
                      (not (:error make-result)))]
    {:valid-config?      valid?
     :make-sweeper-ok?   make-ok?
     :spike-error        (cond
                           (instance? Throwable make-result)
                           (.getMessage ^Throwable make-result)
                           (:error make-result)
                           (let [e (:error make-result)]
                             (if (instance? Throwable e)
                               (str (.getName (class e)) ": " (.getMessage ^Throwable e))
                               (pr-str e))))
     :spike-stage        :phase-1
     :excursion          "E-street-sweeper"
     :verified-criteria  (cond-> []
                           valid?   (conj :C1-envelope-file-exists-and-config-valid)
                           make-ok? (conj :C2-make-sweeper-returns-without-error))
     :invariants-loaded  {:stage-invariants (count sss/stage-invariants)
                          :auto-approve-checks (count sss/auto-approve-checks)
                          :secret-patterns (count sss/secret-patterns)
                          :exclusion-patterns (count sss/exclusion-patterns)
                          :defer-reasons (count sss/defer-reasons)}
     :deferred-criteria  [:C3-agency-registration-and-inhabitation
                          :C5-first-real-commit-via-envelope
                          :C6-pressure-drops-on-next-wm-scan
                          :C7-hop-protocol-round-trip
                          :C8-no-op-safe-exit]}))

;; =============================================================================
;; Phase 3 — Full-sweep orchestration
;; =============================================================================

(defn- top-level-dir [path]
  (let [i (.indexOf ^String path "/")]
    (if (neg? i) "" (subs path 0 i))))

(defn- strip-prefix [prefix path]
  (let [pfx (str prefix "/")]
    (if (str/starts-with? path pfx)
      (subs path (count pfx))
      path)))

(defn build-packets
  "Cluster file paths by deepest-common-prefix until each cluster ≤ max-size.
   Returns vec of {:files [paths], :prefix string}.
   Files at the repo root become single-file packets (each its own cluster)."
  [files max-size]
  (cond
    (empty? files) []
    (<= (count files) max-size)
    [{:files (vec files) :prefix ""}]
    :else
    (vec
     (mapcat
      (fn [[dir dir-files]]
        (cond
          (= "" dir)
          (mapv (fn [f] {:files [f] :prefix ""}) dir-files)

          (<= (count dir-files) max-size)
          [{:files (vec dir-files) :prefix dir}]

          :else
          (let [stripped (mapv #(strip-prefix dir %) dir-files)
                sub-pkts (build-packets stripped max-size)]
            (mapv (fn [pkt]
                    {:files (mapv #(str dir "/" %) (:files pkt))
                     :prefix (if (str/blank? (:prefix pkt))
                               dir
                               (str dir "/" (:prefix pkt)))})
                  sub-pkts))))
      (group-by top-level-dir files)))))

(defn- effective-content-for-packet
  "Returns content suitable for INV-15 classification of the packet.
   Modified files contribute their `git diff`; untracked files contribute
   their full content (slurped). Concatenated.

   Closes the v0 untracked-file blindspot: when classify-packet sees only
   the diff, untracked-files-with-secrets/TODOs/auth-keywords pass the
   content checks trivially (because diff is empty). With this helper,
   the content of new files is also checked."
  [repo files entries-by-path]
  (let [status-of (fn [p] (:status (get entries-by-path p)))
        modified (filter #(= :modified (status-of %)) files)
        untracked (filter #(= :untracked (status-of %)) files)
        diff-text (when (seq modified)
                    (let [r (ssb/repo-diff {:repo repo :files modified})]
                      (when (:ok r) (:diff-text (:result r)))))
        untracked-content (str/join "\n"
                                    (for [f untracked]
                                      (try
                                        (let [path (str "/home/joe/code/" repo "/" f)
                                              fobj (io/file path)]
                                          (if (and (.isFile fobj)
                                                   (< (.length fobj) (* 2 1024 1024))) ; skip >2MB
                                            (slurp path)
                                            ""))
                                        (catch Throwable _ ""))))]
    (str (or diff-text "") "\n" untracked-content)))

(defn- approximate-loc
  "Rough line-count of combined content. Overestimates for modified files
   (counts diff context lines) but that's a conservative skew toward defer."
  [content]
  (count (str/split-lines (or content ""))))

(defn- derive-commit-message
  "Auto-derive a commit message from packet info + entry status.
   v0 shape: '<prefix>: <verb> N files (auto-sweep)' where verb is
   inferred from the proportion of new vs modified files."
  [repo {:keys [files prefix]} entries-by-path]
  (let [new-count (count (filter #(= :untracked (:status (get entries-by-path %))) files))
        mod-count (count (filter #(= :modified (:status (get entries-by-path %))) files))
        verb (cond
               (and (zero? mod-count) (pos? new-count)) "add"
               (and (zero? new-count) (pos? mod-count)) "update"
               :else "update")
        subject (if (str/blank? prefix) repo prefix)
        n (count files)
        file-noun (if (= 1 n) "file" "files")]
    (str subject ": " verb " " n " " file-noun " (auto-sweep)")))

(defn- dirty-repos
  "List dirty repo names under /home/joe/code/futon* (those with non-empty git status)."
  []
  (try
    (let [r (shell/sh "bash" "-c"
                      "for r in /home/joe/code/futon*; do n=$(basename $r); if [ -d $r/.git ]; then cd $r && [ -n \"$(git status --porcelain)\" ] && echo $n; fi; done")]
      (->> (str/split-lines (or (:out r) ""))
           (remove str/blank?)
           vec))
    (catch Throwable _ [])))

(defn- refresh-mana-snapshot []
  (try
    (let [r (shell/sh "bb" "/home/joe/code/futon0/scripts/mana-snapshot.bb")]
      (str/trim (or (:out r) "")))
    (catch Throwable t (str "snapshot-refresh-failed: " (.getMessage t)))))

(defn- current-max-pressure []
  (-> (ssb/current-metabolic-pressure {}) :result :max-pressure))

(defn run-full-sweep
  "Phase 3 — full sweep across all dirty repos.

   For each repo:
     - survey dirty files via :repo-status
     - apply stage-time invariants (INV-2/10/13) → filter excludable + flag relocations
     - cluster surviving files into packets (≤ INV-12 cap) by deepest-common-prefix
     - for each packet:
         - fetch :repo-diff
         - classify via INV-15
         - auto-approve → consent-gate-emit → repo-stage → repo-commit
         - defer → collect into manifest
     - after all repos: write defer-manifest (triggers INV-17 reflection cluster)
     - refresh mana-snapshot; compute pressure-delta

   Args:
     :dry-run? — when true, classify + report but do NOT commit (default true; explicit
                  :dry-run? false required for real commits)
     :repos    — list of repos to sweep (default: all dirty repos under /home/joe/code/futon*)
     :max-packets-per-repo — safety cap (default 100; sweep won't process more)
   "
  ([] (run-full-sweep {}))
  ([{:keys [dry-run? repos max-packets-per-repo]
     :or {dry-run? true max-packets-per-repo 100}}]
   ;; INV: hop-back lifecycle (E-pilot-hop-trigger-wiring §8 contract α).
   ;; Resolve agency.registry primitives lazily (avoid circular ns deps).
   ;; agent-id is captured at cycle start; on cycle end (success OR error
   ;; path; via finally), hop-back! is invoked unconditionally — the
   ;; agency.registry side returns :hop-stack-empty when not actually
   ;; hopped-in (e.g. top-level orchestration), which is a safe no-op.
   (let [current-inhabitant (try (requiring-resolve 'futon3c.agency.registry/current-inhabitant)
                                 (catch Throwable _ nil))
         hop-back-fn (try (requiring-resolve 'futon3c.agency.registry/hop-back!)
                          (catch Throwable _ nil))
         agent-id (when current-inhabitant
                    (try (current-inhabitant :street-sweeper)
                         (catch Throwable _ nil)))
         target-repos (or repos (dirty-repos))
         sweep-ts (str/replace (str (java.time.Instant/now)) #"[:.]" "-")
         pressure-before (current-max-pressure)
         results (atom {:sweep-ts sweep-ts
                        :dry-run? dry-run?
                        :pressure-before pressure-before
                        :target-repos target-repos
                        :commits-landed []
                        :would-commit []  ;; populated in dry-run mode
                        :deferred-packets []
                        :rejected-files []
                        :proposed-relocations []
                        :errors []})]
     (try
     (doseq [repo target-repos]
       (try
         (let [status-r (ssb/repo-status {:repo repo})
               repo-policy (ssb/read-repo-policy repo)]
           (when (:ok status-r)
             (let [entries (:entries (:result status-r))
                   entries-by-path (into {} (map (juxt :path identity) entries))
                   all-paths (mapv :path entries)
                   inv-result (ssb/apply-stage-invariants repo all-paths repo-policy)
                   stageable (:ok inv-result)
                   packets (take max-packets-per-repo
                                 (build-packets stageable sss/packet-size-cap-files))]
               (swap! results update :rejected-files into
                      (mapv #(assoc % :repo repo) (:rejected inv-result)))
               (swap! results update :proposed-relocations into
                      (mapv #(assoc % :repo repo) (:proposed inv-result)))
               (doseq [pkt packets]
                 (try
                   (let [content (effective-content-for-packet repo (:files pkt) entries-by-path)
                         loc (approximate-loc content)
                         file-statuses (into {} (for [f (:files pkt)]
                                                  [f (:status (get entries-by-path f))]))
                         classification (ssb/classify-packet
                                         {:repo repo
                                          :files (:files pkt)
                                          :diff-text content
                                          :loc loc
                                          :file-statuses file-statuses
                                          :repo-policy repo-policy})
                         msg (derive-commit-message repo pkt entries-by-path)
                         pkt-summary {:repo repo
                                      :files (:files pkt)
                                      :prefix (:prefix pkt)
                                      :loc loc
                                      :file-count (count (:files pkt))
                                      :message-draft msg}]
                     (cond
                       (:auto-approve? classification)
                       (if dry-run?
                         (swap! results update :would-commit conj
                                (assoc pkt-summary :status :would-auto-commit))
                         (let [cg (ssb/consent-gate-emit
                                   {:repo repo :intent :auto-sweep
                                    :files (:files pkt)})
                               cg-id (:consent-gate-event-id (:result cg))
                               stage (when cg-id
                                       (ssb/repo-stage
                                        {:repo repo :files (:files pkt)
                                         :consent-gate-event-id cg-id}))
                               commit (when (and cg-id (:ok stage))
                                        (ssb/repo-commit
                                         {:repo repo :message msg
                                          :consent-gate-event-id cg-id}))]
                           (swap! results update :commits-landed conj
                                  (assoc pkt-summary
                                         :cg-id cg-id
                                         :sha (get-in commit [:result :sha])
                                         :ok? (boolean (:ok commit))
                                         :error (when-not (:ok commit)
                                                  (or (:error commit)
                                                      (:error stage)
                                                      "unknown failure"))))))

                       :else
                       (swap! results update :deferred-packets conj
                              (assoc pkt-summary
                                     :defer-reason (first (:defer-reasons classification))
                                     :defer-reasons (:defer-reasons classification)
                                     :diff-text content
                                     :commit-message-draft msg))))
                   (catch Throwable t
                     (swap! results update :errors conj
                            {:repo repo :files (:files pkt)
                             :error (.getMessage t)})))))))
         (catch Throwable t
           (swap! results update :errors conj {:repo repo :error (.getMessage t)}))))

     ;; Write defer-manifest (works in dry-run too — operator can review the manifest
     ;; without any commits having landed)
     (when (seq (:deferred-packets @results))
       (let [r (ssb/write-defer-manifest
                {:sweep-ts sweep-ts
                 :deferred-packets (:deferred-packets @results)})]
         (swap! results assoc :defer-manifest r)))

     ;; Refresh pressure (skipped in dry-run since no state change occurred)
     (when-not dry-run?
       (refresh-mana-snapshot)
       (swap! results assoc :pressure-after (current-max-pressure))
       (swap! results assoc :pressure-delta
              (when (and (:pressure-after @results) pressure-before)
                (- (:pressure-after @results) pressure-before))))

     @results
     (finally
       (when (and hop-back-fn agent-id)
         (try (hop-back-fn agent-id) (catch Throwable _ nil))))))))

(defn sweep-summary
  "Compact one-line per-bucket summary suitable for hop-back payload."
  [sweep-result]
  {:sweep-ts (:sweep-ts sweep-result)
   :dry-run? (:dry-run? sweep-result)
   :target-repos (:target-repos sweep-result)
   :commits-landed-count (count (or (:commits-landed sweep-result) []))
   :would-commit-count (count (or (:would-commit sweep-result) []))
   :deferred-count (count (or (:deferred-packets sweep-result) []))
   :rejected-count (count (or (:rejected-files sweep-result) []))
   :proposed-relocations-count (count (or (:proposed-relocations sweep-result) []))
   :error-count (count (or (:errors sweep-result) []))
   :pressure-before (:pressure-before sweep-result)
   :pressure-after (:pressure-after sweep-result)
   :pressure-delta (:pressure-delta sweep-result)
   :defer-manifest-path (get-in sweep-result [:defer-manifest :result :manifest-path])
   :defer-reason-frequencies (frequencies (keep :defer-reason
                                                (:deferred-packets sweep-result)))})
