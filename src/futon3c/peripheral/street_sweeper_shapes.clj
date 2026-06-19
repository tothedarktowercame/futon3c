(ns futon3c.peripheral.street-sweeper-shapes
  "Domain shapes for the Street Sweeper peripheral.

   E-street-sweeper.md (2026-05-25) — bounded excursion from
   M-war-machine-pilot. Grants safe working-tree-hygiene capabilities
   (status / diff / stage / commit / revert-staged) to an inhabiting
   agent via hop semantics from :war-machine-pilot.

   Invariants are declared as data (see `stage-invariants` + `auto-approve-checks`)
   so adding new INVs = appending a map. Joe's modular-extension framing:
   the defer queue becomes evidence for future invariants (INV-17 reflection).

   Cross-refs:
   - clone-from shape donor: war_machine_pilot_shapes.clj
   - cycle/valid-domain-config? — the predicate this file aims to satisfy
   - INV catalog: E-street-sweeper.md §Hard structural invariants (1-9)
     + this session's additions (10-17 per emacs-claude-repl 2026-05-25)")

;; =============================================================================
;; Phases
;; =============================================================================

(def phase-order
  "Three phases:
   :survey — read repo dirty state + WM pressure (observe-only).
   :sweep  — classify, auto-approve or defer, commit; substantive actions
             gated by Pilot-I1 cg-id citation.
   :completed — terminal."
  [:survey :sweep :completed])

;; =============================================================================
;; Tool operation kinds
;; =============================================================================

(def sweeper-tool-operation-kinds
  {;; Observation tools (no cg-id required)
   :repo-status                  :observe
   :repo-diff                    :observe
   :list-repos-with-pressure     :observe
   :current-metabolic-pressure   :observe
   ;; Transport / coordination (non-substantive)
   :bell-emit                    :action
   :heartbeat-emit               :action
   :consent-gate-emit            :action
   ;; SUBSTANTIVE TOOLS (Pilot-I1: cg-id required)
   :repo-stage                   :action
   :repo-commit                  :action
   :repo-revert-staged           :action
   :write-defer-manifest         :action
   ;; Cycle control
   :cycle-begin                  :action
   :cycle-advance                :action
   ;; Delegated standard observations
   :read                         :observe
   :glob                         :observe
   :grep                         :observe
   :bash-readonly                :observe})

(def substantive-tools
  "Closed-set of tools that modify repo state and require cg-id citation.
   :write-defer-manifest is intentionally NOT in this set — it writes to
   the operator-archive at ~/code/storage/sweeper-deferred/<ts>/ rather
   than to any repo, so cg-id citation would be overzealous."
  #{:repo-stage :repo-commit :repo-revert-staged})

(def phase-allowed-tools
  {:survey    #{:read :glob :grep :bash-readonly
                :repo-status :repo-diff
                :list-repos-with-pressure :current-metabolic-pressure
                :bell-emit :heartbeat-emit
                :cycle-advance}
   :sweep     #{:read :glob :grep :bash-readonly
                :repo-status :repo-diff
                :list-repos-with-pressure :current-metabolic-pressure
                :bell-emit :heartbeat-emit :consent-gate-emit
                :repo-stage :repo-commit :repo-revert-staged
                :write-defer-manifest
                :cycle-advance}
   :completed #{}})

(def setup-tools
  #{:read :glob :grep :bash-readonly
    :list-repos-with-pressure :current-metabolic-pressure
    :bell-emit
    :cycle-begin})

(def phase-required-outputs
  {:survey    #{:survey-summary}
   :sweep     #{:sweep-summary}
   :completed #{}})

;; =============================================================================
;; Path patterns — data-driven invariant inputs
;; =============================================================================

(def secret-patterns
  "INV-2: paths matching any of these are structurally unstageable."
  [#"\.env$" #"\.envrc$" #"\.pem$" #"credentials" #"\.admintoken$"
   #"\.key$" #"_rsa$" #"_dsa$" #"_ed25519$" #"id_ecdsa$"
   #"secret" #"\.p12$" #"\.pfx$"])

(def exclusion-patterns
  "INV-13: universal non-committable patterns (data-driven).
   Add a regex here to extend the exclusion set."
  [;; data + transient
   #"(^|/)data/" #"(^|/)logs/" #"(^|/)tmp/" #"(^|/)cache/" #"(^|/)runs/"
   #"(^|/)target/" #"(^|/)build/" #"(^|/)out/" #"(^|/)coverage/"
   ;; lang-specific
   #"(^|/)node_modules/" #"(^|/)__pycache__/" #"(^|/)\.pytest_cache/"
   #"(^|/)\.venv/" #"(^|/)venv/" #"\.pyc$" #"\.class$"
   #"(^|/)\.cpcache/" #"(^|/)\.lsp/" #"(^|/)\.clj-kondo/cache/"
   #"(^|/)\.shadow-cljs/"
   ;; editor / OS
   #"(^|/)\.idea/" #"(^|/)\.vscode/" #"\.DS_Store$"
   #"\.swp$" #"~$" #"^#.*#$" #"#$" #"\.orig$" #"\.bak$" #"\.~arxana~$"])

(def generated-extensions
  "INV-10: extensions presumed regenerable → propose storage relocation."
  #{"json" "jsonl" "parquet" "npy" "pkl" "pt" "bin"
    "csv" "tsv" "html" "pdf"})

(def hand-authored-extensions
  "INV-10: extensions presumed hand-authored → don't auto-relocate even at size."
  #{"md" "org" "txt" "rst" "tex"
    "clj" "cljs" "cljc" "edn" "el" "py" "sh" "bb" "rb" "go" "rs" "ts" "tsx" "js" "jsx"
    "yml" "yaml" "toml" "ini" "conf"})

(def hand-authored-filenames
  "INV-10 refinement (post-2026-05-26 false-positive on package.json): config
   files whose CONTENT is hand-authored even though their extension is in the
   generated-extensions set. Filename match (basename) overrides the extension
   heuristic."
  #{"package.json" "package-lock.json" "tsconfig.json" "deno.json"
    "composer.json" "manifest.json" "lerna.json"})

(def prose-extensions
  "INV-23/INV-24: extensions that are prose / markdown / patterns.  These
   carry lower mutation-risk than code (no runtime execution) and
   legitimately host TODO-style content as deliberate documentation
   rather than incomplete code."
  #{"md" "org" "txt" "rst" "flexiarg"})

(def regenerable-size-threshold-bytes 262144)  ; 256 KB

(def packet-size-cap-files 10)
(def packet-size-cap-loc 200)
(def loc-budget-per-file
  "INV-29 (2026-05-26): per-file LoC budget for the multi-file packet cap.
   Effective cap = max(base, file-count × this).  Joe's signal: '5 files
   with 500 LOC isn't big'.  150 LoC/file = ~human-readable diff per file;
   a 5-file packet admits up to 750 LoC.  Genuine megapacks (3000+ LoC,
   any file count) still defer."
  150)
(def packet-size-cap-loc-additive
  "INV-21: pure-addition packets (all files untracked) carry less risk
   than mixed packets — they don't change existing behavior. Higher cap."
  500)
(def packet-size-cap-loc-uniform-bonus
  "INV-22: when all files share the same extension AND a non-empty common
   prefix (script-generated batch / library section), grant a 1.5× cap
   bonus on top of whichever base cap (additive or normal) applies."
  1.5)

(def security-sensitive-regex
  "INV-19 (revised, 2026-05-25): tighter security-adjacent patterns that
   ACTUALLY indicate security context, not bare keyword mentions.

   Old version (deprecated): matched bare \\b(auth|secret|...|verify)\\b
   which produced massive false positives — `key` in Emacs-Lisp keybindings,
   `verify` in Playwright test scripts, `exec` in eval expressions.

   New version: anchored compounds that mean the file is touching real
   credentials / privilege / signing surface."
  #"(?i)(?:secret[-_]?(?:key|token)|api[-_]?key|private[-_]?key|bearer\s+token|password\s*[=:]|chmod\s+[0-7]{3,}|sudo\s+|setuid|authentic(?:ate|ation)|authoriz(?:e|ation)|credential|access[-_]?token)")

(def test-context-path-regex
  "INV-18: paths that live in a test/verify context. The security check
   skips these because verification/test code legitimately mentions
   security keywords without being security-sensitive itself."
  #"(?i)(?:^|/)(?:tests?|spec)/|(?:^|/)[-_a-z0-9]+[-_.](?:test|verify|spec)s?\.[a-z]+$|(?:^|/)wm-(?:anchor|tour|detail|hud|proxy|ghost|deprecate)-[^/]*\.mjs$")

(def intent-marker-regex
  "INV-15: defer auto-approve if diff adds TODO/FIXME/HACK/XXX/REVIEW comments
   (sign of incomplete thought worth operator eyes)."
  #"(?i)\b(TODO|FIXME|XXX|HACK|REVIEW)\b")

(def dep-manifest-files
  "INV-15: defer auto-approve if any of these files are modified
   (touching deps = intent-carrying)."
  #{"deps.edn" "package.json" "requirements.txt" "pom.xml" "Cargo.toml"
    "Gemfile" "go.mod" "shadow-cljs.edn"})

(def cross-repo-path-regex
  "INV-14: detect cross-repo hard-coded paths in staged content.
   Capture 1: sister-repo name. Capture 2: rest-of-path (with leading slash, may be empty)."
  #"(?:/home/joe/code|~/code|\$HOME/code)/([a-z][a-z0-9-]*)((?:/[^\s\"'`)\]<>;]*)?)")

(def cg-binding-ttl-ms
  "INV-1: cg-id bindings expire after 60 minutes. After expiry, the cg-id is
   no longer valid for substantive tool dispatch. Operator can re-emit the
   consent-gate to refresh."
  3600000)

;; =============================================================================
;; Sentinel marker files (operator overrides)
;; =============================================================================

(def sentinel-keep-here       ".sweeper-keep-here")
(def sentinel-defer-this      ".sweeper-defer-this")
(def sentinel-include-this    ".sweeper-include-this")
(def repo-policy-filename     ".sweeper-policy.edn")

;; =============================================================================
;; Defer-reason enum (INV-17 — clustering depends on typed reasons)
;; =============================================================================

(def defer-reasons
  "Closed enum. Reflection (INV-17) clusters by these; threshold ≥3 packets
   in a sweep surfaces a candidate-invariant proposal."
  #{:rejected-secret
    :rejected-excluded
    :rejected-cross-repo-untracked
    :proposed-relocation
    :exceeded-packet-cap
    :security-sensitive-diff
    :new-external-dep
    :intent-marker-in-diff
    :intent-doc-creation
    :operator-marker-defer
    :diff-touches-running-code
    :uncertain-message-derivation})

;; =============================================================================
;; Stage-time invariants — data-driven, modular
;; =============================================================================
;;
;; Each invariant: {:id, :name, :rejects?, :check, :defer-reason}.
;; :rejects? = true → file is REJECTED from staging (hard block).
;; :rejects? = false → file is FLAGGED with proposal but continues to defer queue.
;; :check fn signature: (fn [path-str file-meta] → nil-or-violation-map).
;;
;; Adding a new INV = append a map. The envelope iterates and applies.

(declare ^:private path-ext file-size-bytes)

(def stage-invariants
  [{:id :inv-2 :name "secret-pattern-reject" :rejects? true
    :defer-reason :rejected-secret
    :check (fn [path _]
             (when (some #(re-find % path) secret-patterns)
               {:envelope-rejected true
                :reason :secret-pattern-match
                :path path}))}

   {:id :inv-13 :name "extended-exclusion" :rejects? true
    :defer-reason :rejected-excluded
    :check (fn [path _]
             (when (some #(re-find % path) exclusion-patterns)
               {:envelope-rejected true
                :reason :universal-exclusion
                :path path}))}

   {:id :inv-10 :name "regenerable-artifact-relocation" :rejects? false
    :defer-reason :proposed-relocation
    :check (fn [path {:keys [size repo-policy]}]
             (let [ext (path-ext path)
                   sz (or size 0)
                   basename (or (last (clojure.string/split (str path) #"/")) "")
                   path-str (str path)
                   hand-authored-filename? (contains? hand-authored-filenames basename)
                   ;; INV-26: per-repo policy allowlist (e.g. futon7a's
                   ;; vsatarcs.html as authored-for-static-serving).
                   policy-allowlist (set (:additional-hand-authored-paths repo-policy #{}))
                   policy-authored? (or (contains? policy-allowlist basename)
                                        (contains? policy-allowlist path-str))]
               (when (and (not hand-authored-filename?)
                          (not policy-authored?)
                          (or (contains? generated-extensions ext)
                              (and (> sz regenerable-size-threshold-bytes)
                                   (not (contains? hand-authored-extensions ext)))))
                 {:envelope-proposed-relocation true
                  :reason :regenerable-artifact
                  :path path
                  :size sz
                  :extension ext})))}])

;; =============================================================================
;; Auto-approve criteria — data-driven, modular (INV-15)
;; =============================================================================
;;
;; Each criterion: {:id, :name, :check, :defer-reason-if-fails}.
;; Auto-approve iff ALL criteria pass. Failing criteria → defer with their reason.

(defn- test-context-path? [path]
  (boolean (re-find test-context-path-regex (str path))))

(defn- path-extension-of [path]
  (let [s (str path)
        i (.lastIndexOf s ".")]
    (when (and (pos? i) (< i (dec (count s))))
      (clojure.string/lower-case (subs s (inc i))))))

(defn- common-prefix-segment [paths]
  (let [parts (mapv #(clojure.string/split % #"/") paths)
        n (apply min (map count parts))]
    (loop [i 0 acc []]
      (if (or (>= i n)
              (not (apply = (map #(nth % i) parts))))
        acc
        (recur (inc i) (conj acc (nth (first parts) i)))))))

(defn- uniform-packet?
  "INV-22: every file in `files` shares the same extension AND a non-empty
   common path prefix."
  [files]
  (let [files (mapv str files)]
    (and (seq files)
         (apply = (map path-extension-of files))
         (seq (butlast (common-prefix-segment files))))))

(defn- additive-only?
  "INV-21: every file in `files` is :untracked in file-statuses."
  [files file-statuses]
  (and file-statuses
       (seq files)
       (every? #(= :untracked (get file-statuses (str %))) files)))

(defn all-mission-docs?
  "INV-28 (2026-05-26): every file in `files` lives under `holes/`.
   The convention across futon* repos is that holes/ is the mission /
   handoff / WIP-document subtree. Mission-doc packets get a free pass
   on packet-size cap AND on INV-14 cross-repo dep check, because:
   - mission docs naturally accumulate content (often 1000s of LoC)
   - mission docs naturally reference WIP across repos (the cross-repo
     refs are documentation of in-progress work, not build deps)
   This is the operator-judgement-density override at the file-class
   level, matching Joe's 'mission docs which should get a free pass
   (invariantly)' signal."
  [files]
  (and (seq files)
       (every? #(clojure.string/starts-with? (str %) "holes/") files)))

(defn- all-prose-extensions?
  "INV-23: every file in `files` has an extension in the prose-extensions set."
  [files]
  (and (seq files)
       (every? #(contains? prose-extensions (path-extension-of %)) files)))

(def packet-size-cap-loc-prose
  "INV-23: prose / markdown / pattern packets get a higher LoC cap because
   they accumulate content by design and carry no runtime-execution risk."
  1000)

(defn- effective-loc-cap
  "Compute the LoC cap for a packet.
   Layered modifiers (in evaluation order):
   - INV-23 prose-extension cap (1000 base for .md/.org/.txt/.rst/.flexiarg)
   - INV-21 additive-only cap (500 base when all files untracked)
   - default base (200)
   - INV-29 per-file scaling: max(base, file-count × loc-budget-per-file)
   - INV-22 uniformity bonus (1.5× when same extension + shared prefix)
   - INV-26 per-repo policy multiplier"
  [files file-statuses repo-policy]
  (let [base (cond
               (all-prose-extensions? files)        packet-size-cap-loc-prose
               (additive-only? files file-statuses) packet-size-cap-loc-additive
               :else                                packet-size-cap-loc)
        per-file-scaled (max base (* (count files) loc-budget-per-file))
        uniformity-mult (if (uniform-packet? files) packet-size-cap-loc-uniform-bonus 1.0)
        policy-mult (or (:loc-cap-multiplier repo-policy) 1.0)]
    (long (* per-file-scaled uniformity-mult policy-mult))))

(def auto-approve-checks
  [{:id :inv-15-packet-size :name "within packet-size cap"
    :defer-reason :exceeded-packet-cap
    :check (fn [{:keys [files loc file-statuses repo-policy]}]
             ;; INV-28: mission/handoff doc packets (all files under holes/)
             ;; get a free pass on the packet-size cap.  INV-3 (50-file commit
             ;; cap) still catches mega-packets at commit time.
             (or (all-mission-docs? files)
                 (and (<= (count files) packet-size-cap-files)
                      (<= (or loc 0) (effective-loc-cap files file-statuses repo-policy)))))}

   {:id :inv-15-no-security-sensitive :name "no security-sensitive patterns in diff"
    :defer-reason :security-sensitive-diff
    :check (fn [{:keys [diff-text files]}]
             ;; INV-18: when all files are in test/verify context, skip the
             ;; diff check entirely (verification scripts legitimately mention
             ;; security keywords).
             ;; INV-27 (2026-05-26): same exemption for all-prose packets —
             ;; markdown/pattern files mentioning "authenticate" / "token" /
             ;; "verify" etc. are documentation, not auth code. Symmetric
             ;; with INV-24's intent-marker exemption for prose.
             (let [non-test (remove test-context-path? files)
                   all-test? (every? test-context-path? files)
                   all-prose? (all-prose-extensions? files)]
               (and (not (some #(re-find security-sensitive-regex (str %)) non-test))
                    (or all-test?
                        all-prose?
                        (not (and diff-text (re-find security-sensitive-regex diff-text)))))))}

   {:id :inv-15-no-new-deps :name "no new EXTERNAL dep additions (local-root/git-url OK)"
    :defer-reason :new-external-dep
    :check (fn [{:keys [files diff-text]}]
             ;; INV-25 (2026-05-26): refine the dep-manifest check. Local-root
             ;; / git-url additions don't introduce external deps (no Maven
             ;; coordinate, no supply-chain risk) — auto-approve those.
             ;; Defer only when the diff adds an external coordinate (mvn,
             ;; Leiningen-vector form with version string, npm-package-with-version).
             (let [touches-dep-manifest?
                   (some #(contains? dep-manifest-files
                                     (last (clojure.string/split (str %) #"/")))
                         files)]
               (cond
                 (not touches-dep-manifest?) true
                 (nil? diff-text)            false  ; can't verify safety; defer
                 :else
                 (let [added-lines (->> (clojure.string/split-lines diff-text)
                                        (filter (fn [l]
                                                  (and (clojure.string/starts-with? l "+")
                                                       (not (clojure.string/starts-with? l "+++"))))))
                       ;; External coordinate markers in deps.edn / package.json:
                       has-mvn-coord?       (some #(re-find #":mvn/version" %) added-lines)
                       ;; Leiningen-vector form: [group/artifact "1.2.3"] or [artifact "1.2.3"]
                       has-lein-vec?        (some #(re-find #"\[[a-z][\w./.-]*\s+\"\d" %) added-lines)
                       ;; npm semver string in a JSON-style "pkg": "^1.2.3" or "~1.2.3"
                       has-npm-semver?      (some #(re-find #"\"\s*[\^~]?\d+\.\d+" %) added-lines)]
                   (not (or has-mvn-coord? has-lein-vec? has-npm-semver?)))))) }

   {:id :inv-15-no-intent-markers :name "no TODO/FIXME additions in diff"
    :defer-reason :intent-marker-in-diff
    :check (fn [{:keys [diff-text files]}]
             ;; INV-24: TODO/FIXME in prose/pattern files (.md / .org / .txt /
             ;; .rst / .flexiarg) is deliberate documentation, not code debt.
             ;; Skip the diff-text check entirely when all files are prose.
             ;; (Same structural move as INV-18 for the security regex.)
             (or (all-prose-extensions? files)
                 (nil? diff-text)
                 (not (re-find intent-marker-regex diff-text))))}

   ;; INV-20 (intent-doc-creation defer for new mission/excursion/handoff docs)
   ;; RETIRED 2026-05-26 per operator: "new mission/excursion docs are not
   ;; needing my attention, they can always be committed." The original concern
   ;; (operator-authored intent silently committed by a robotic flow) doesn't
   ;; apply in practice: by the time a mission/excursion doc reaches the
   ;; sweeper, the operator either authored it directly or commissioned it
   ;; via eoi-new / E-prefix excursion handoff — the intent is already
   ;; established. Auto-approving these reduces operator-attention cost
   ;; without weakening any structural invariant. The :intent-doc-creation
   ;; defer-reason keyword remains in the enum (shapes/defer-reasons) for
   ;; backwards compatibility with prior manifests; no new check emits it.
   ])

;; =============================================================================
;; Internal helpers
;; =============================================================================

(defn- path-ext
  "Lowercased file extension without leading dot. nil if none."
  [path]
  (let [i (.lastIndexOf ^String path ".")]
    (when (and (pos? i) (< i (dec (count path))))
      (clojure.string/lower-case (subs path (inc i))))))

(defn- file-size-bytes
  "Size of file at path, 0 if missing."
  [path]
  (try
    (let [f (java.io.File. ^String path)]
      (if (.exists f) (.length f) 0))
    (catch Exception _ 0)))
