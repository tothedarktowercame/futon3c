(ns futon3c.peripheral.mission-control-backend
  "Backend tool implementations for the mission control peripheral.

   Tools:
   - :mc-inventory     — scan mission files across repos, return inventory
   - :mc-devmaps       — read and summarize devmap wiring diagrams
   - :mc-coverage      — compute devmap coverage against mission inventory
   - :mc-mana          — query mana pool stats (if nonstarter.db exists)
   - :mc-review        — produce a full portfolio review
   - :mc-bulletin      — emit a war bulletin as evidence
   - :mc-diff          — compare last two portfolio review snapshots

   Mission focus tools (:mc-focus, :mc-focus-clear, :mc-focus-show)
   are dispatched in mission_control.clj as they only manipulate session state.

   All tools are read-only with respect to external systems.
   Evidence emission happens at the peripheral level, not here."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as cset]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.agents.tickle :as tickle]
            [futon3c.evidence.store :as estore]
            [futon3c.peripheral.mission-backend :as mb]
            [futon3c.peripheral.tools :as tools]))

(def ^:private futon1a-url
  (or (System/getenv "FUTON1A_URL")
      "http://localhost:7071"))

(def ^:private mission-doc-hyperedge-type
  "code/v05/mission-doc")

;; =============================================================================
;; Configuration — repo paths
;; =============================================================================

(defn- normalize-path-separators
  [path]
  (some-> path (str/replace "\\" "/")))

(defn- futon-repo-name?
  [name]
  (boolean (re-matches #"(?i)^futon[0-9a-z].*" (or name ""))))

(defn- parse-repo-pair
  "Parse one FUTON3C_REPOS entry: name=path."
  [entry]
  (let [[repo-name repo-path] (str/split (or entry "") #"=" 2)
        repo-name (some-> repo-name str/trim not-empty)
        repo-path (some-> repo-path str/trim not-empty)]
    (when (and repo-name repo-path)
      [(keyword (str/lower-case repo-name))
       (-> repo-path io/file .getAbsolutePath normalize-path-separators)])))

(defn- parse-repo-roots-env
  "Parse FUTON3C_REPOS/FUTON_REPOS:
   futon3c=/path/to/futon3c,futon4=/path/to/futon4,mfuton=/path/to/mfuton.
   Supports comma or semicolon separators."
  [raw]
  (let [pairs (->> (str/split (or raw "") #"[,;]")
                   (map str/trim)
                   (remove str/blank?)
                   (keep parse-repo-pair)
                   vec)]
    (when (seq pairs)
      (into {} pairs))))

(defn- discover-futon-repo-roots
  "Discover futon* directories directly under BASE."
  [base]
  (let [dir (some-> base io/file .getAbsoluteFile)]
    (if (and dir (.isDirectory dir))
      (->> (.listFiles dir)
           (filter #(.isDirectory ^java.io.File %))
           (keep (fn [^java.io.File f]
                   (let [name (.getName f)]
                     (when (futon-repo-name? name)
                       [(keyword (str/lower-case name))
                        (-> f .getAbsolutePath normalize-path-separators)]))))
           (sort-by (comp str first))
           (into {}))
      {})))

(defn- repo-base-score
  [base]
  (count (discover-futon-repo-roots base)))

(defn- choose-default-repo-roots
  []
  (or (parse-repo-roots-env (System/getenv "FUTON3C_REPOS"))
      (parse-repo-roots-env (System/getenv "FUTON_REPOS"))
      (let [env-base (or (some-> (System/getenv "FUTON3C_REPO_BASE") str/trim not-empty)
                         (some-> (System/getenv "FUTON_REPO_BASE") str/trim not-empty))
            cwd-parent (some-> (System/getProperty "user.dir")
                               io/file
                               .getAbsoluteFile
                               .getParentFile
                               str)
            home-code (str (System/getProperty "user.home") "/code")
            candidates (->> [env-base cwd-parent home-code]
                            (remove str/blank?)
                            distinct)
            scored (->> candidates
                        (map (fn [base]
                               [base (repo-base-score base)]))
                        (sort-by second >))
            best-base (or (some->> scored
                                   (filter (fn [[_ score]] (pos? score)))
                                   ffirst)
                          home-code)]
        (discover-futon-repo-roots best-base))))

(def default-repo-roots
  "Default repo locations.
   Preference order:
   1) FUTON3C_REPOS / FUTON_REPOS (name=path pairs)
   2) auto-discover futon* siblings under FUTON3C_REPO_BASE / FUTON_REPO_BASE
   3) auto-discover under sibling directory of current user.dir
   4) auto-discover under ~/code"
  (choose-default-repo-roots))

;; =============================================================================
;; Mission file parsing
;; =============================================================================

(defn- extract-header
  "Extract a Key: value from markdown text.
   Matches four formats:
   - **Key:** value      (bold key, colon inside)
   - **Key**: value      (bold key, colon outside)
   - Key: value          (plain key)
   - ## Key: value       (heading key)"
  [text key-name]
  (let [quoted (java.util.regex.Pattern/quote key-name)
        ;; Try bold format first: **Key:** value (colon inside bold)
        bold-pat (re-pattern (str "(?mi)^\\*\\*" quoted ":\\*\\*\\s*(.+)$"))
        ;; Also try: **Key**: value (colon outside bold)
        bold-pat2 (re-pattern (str "(?mi)^\\*\\*" quoted "\\*\\*:\\s*(.+)$"))
        ;; Fallback: plain or heading format: Key: value or ## Key: value
        plain-pat (re-pattern (str "(?mi)^(?:#{1,3}\\s+)?" quoted ":\\s*(.+)$"))]
    (or (when-let [m (re-find bold-pat text)]
          (str/trim (second m)))
        (when-let [m (re-find bold-pat2 text)]
          (str/trim (second m)))
        (when-let [m (re-find plain-pat text)]
          (str/trim (second m))))))

(defn classify-status
  "Classify a raw status string into a MissionStatus keyword.

   Recognised statuses:
   - :complete   — done, pass, complete, or derivation phase marked complete
   - :in-progress — active, open, in-progress, or any derivation phase
   - :blocked    — explicitly blocked on a dependency
   - :ready      — ready to start, greenfield
   - :deferred   — scope was right but timing is wrong; not active
   - :nonstarter — gap turned out not to be real or approach is wrong
   - :unknown    — could not classify"
  [raw]
  (when raw
    ;; Strip markdown bold markers, emoji, leading colons, then lowercase
    (let [s (-> raw str/trim
               (str/replace #"\*+" "")
               (str/replace #"[✅❌⚠️🔄]" "")
               str/trim str/lower-case
               (str/replace #"^:" ""))]
      (cond
        (str/starts-with? s "complete")         :complete
        (str/starts-with? s "done")             :complete
        (str/starts-with? s "blocked")          :blocked
        (str/starts-with? s "ready")            :ready
        (str/starts-with? s "testing")          :testing
        (str/starts-with? s "pass")             :complete
        (str/starts-with? s "deferred")         :deferred
        (str/starts-with? s "nonstarter")       :nonstarter
        (str/starts-with? s "proposed")         :ready
        (str/starts-with? s "idea")             :ready
        (str/starts-with? s "draft")            :ready
        (str/starts-with? s "in-progress")      :in-progress
        (str/starts-with? s "in progress")      :in-progress
        (str/starts-with? s "active")           :in-progress
        (str/starts-with? s "open")             :in-progress
        (str/starts-with? s "re-opened")        :in-progress
        (str/starts-with? s "greenfield")       :ready
        (str/starts-with? s "phase 0 done")     :complete
        (str/starts-with? s "superseded")       :complete
        (str/starts-with? s "abandoned")        :nonstarter
        (str/starts-with? s "not implemented")  :ready
        ;; Status-audit additions (2026-05-23, M-weird-modernism follow-up):
        ;; cover the dominant raw-status patterns that previously fell
        ;; through to :unknown.
        (str/starts-with? s "archived")         :archived
        (str/starts-with? s "parked")           :deferred
        (str/starts-with? s "head")             :in-progress
        (str/starts-with? s "not started")      :ready
        (str/starts-with? s "not yet started")  :ready
        (str/starts-with? s "specified")        :ready
        (str/starts-with? s "partial")          :in-progress
        (str/starts-with? s "stage")            :in-progress
        (re-find #"^(?:phase\s+\d+\s+partial|partial[-\s]complete)" s) :in-progress
        ;; "Phase N complete, Phase M pending" → in-progress (partially done)
        (re-find #"^phase\s+\d+\s+complete" s)  :in-progress
        ;; Derivation keywords: check if the *derivation step itself* is marked complete.
        ;; "INSTANTIATE complete" → :complete (the mission finished its last step)
        ;; "INSTANTIATE (complete)" → :complete (parenthetical variant)
        ;; "MAP (landscape survey complete)" → :in-progress (MAP done, mission continues)
        (re-find #"identify|map|derive|argue|verify|instantiate" s)
        (if (re-find #"^(?:identify|map|derive|argue|verify|instantiate)\s+(?:\(?\s*complete)" s)
          :complete
          :in-progress)
        :else :unknown))))

(defn- in-progress-status?
  "Return true when status counts as in-progress/active."
  [status]
  (or (= status :in-progress)
      (= status :testing)))

(defn- count-checkboxes
  "Count checked and total checkboxes in markdown text.
   Returns {:checked N :total N} or nil if no checkboxes found."
  [text]
  (let [checked (count (re-seq #"(?m)^[\s]*- \[x\]" text))
        unchecked (count (re-seq #"(?m)^[\s]*- \[ \]" text))
        total (+ checked unchecked)]
    (when (pos? total)
      {:checked checked :total total})))

(defn- infer-status-from-checkboxes
  "When no explicit Status header, infer from success criteria checkboxes."
  [{:keys [checked total]}]
  (cond
    (= checked total)           :complete
    (zero? checked)             :ready
    (> checked 0)               :in-progress))

;; =============================================================================
;; T-9b: Content enrichment helpers — port of futon3a/missions.clj utilities
;; so substrate-2 hyperedges carry summary, cross-refs, code-paths, and phase
;; directly, removing the need for downstream consumers (futon3a, etc.) to
;; re-parse the source markdown.
;; =============================================================================

(def ^:private mission-ref-pattern
  #"\b(M-[A-Za-z0-9][A-Za-z0-9-]*)\b")

(def ^:private absolute-code-path-pattern
  #"/home/joe/code/[^\s`)\],;]+")

(def ^:private tilde-code-path-pattern
  #"~/code/[^\s`)\],;]+")

(def ^:private repo-relative-path-pattern
  #"\b(?:futon[0-9a-z-]+|npt)/[A-Za-z0-9._/\-]+")

(def ^:private default-code-root
  (str (System/getProperty "user.home") "/code"))

(defn- normalize-spaces [s]
  (-> s (str/replace #"\s+" " ") str/trim))

(defn- normalize-code-path [path]
  (cond
    (str/starts-with? path "~/code/")
    (str/replace path #"^~" (System/getProperty "user.home"))

    (re-matches repo-relative-path-pattern path)
    (str default-code-root "/" path)

    :else path))

(defn- extract-cross-refs
  "Return distinct M-X mission references from BODY, excluding SELF-ID."
  [body self-id]
  (->> (re-seq mission-ref-pattern body)
       (map second)
       (remove #(= % (str "M-" self-id)))
       distinct
       vec))

(defn- strip-trailing-path-punct
  "Strip trailing punctuation that sentences leave on otherwise-valid
   path captures (periods, commas, parens, quotes, etc.). Repeats so
   `wyrd.,` reduces cleanly to `wyrd`."
  [s]
  (str/replace s #"[.,;:!?'\")\]]+\z" ""))

(defn- code-path-has-real-extension?
  "True when PATH's last segment looks like a real file extension —
   a dot followed by 1-8 alphanumeric chars starting with a letter.
   Filters captures like `futon-theory/wyrd` that come from prose
   references rather than actual filesystem paths."
  [path]
  (let [last-segment (last (str/split path #"/"))]
    (boolean (re-find #"\.[A-Za-z][A-Za-z0-9]{0,7}\z" (or last-segment "")))))

(defn- extract-code-paths
  "Return distinct normalized code paths from BODY. Filters out captures
   that don't look like real filesystem paths: trailing sentence punctuation
   is stripped, and the path must have a real-looking extension in its
   last segment."
  [body]
  (->> (concat (re-seq absolute-code-path-pattern body)
               (re-seq tilde-code-path-pattern body)
               (re-seq repo-relative-path-pattern body))
       (map strip-trailing-path-punct)
       (filter code-path-has-real-extension?)
       (map normalize-code-path)
       distinct
       vec))

(def ^:private phase-patterns-ordered
  [[:document    #"(?i)\bDOCUMENT\b"]
   [:instantiate #"(?i)\bINSTANTIATE\b"]
   [:verify      #"(?i)\bVERIFY\b"]
   [:argue       #"(?i)\bARGUE\b"]
   [:derive      #"(?i)\bDERIVE\b"]
   [:map         #"(?i)\bMAP\b"]
   [:identify    #"(?i)\bIDENTIFY\b"]
   [:head        #"(?i)\bHEAD\b"]
   [:complete    #"(?i)\b(COMPLETE|COMPLETED|CLOSED)\b"]])

(defn- match-phase [text]
  (some (fn [[phase re]]
          (when (re-find re (or text ""))
            phase))
        phase-patterns-ordered))

(defn- classify-phase
  "Classify the mission's current phase keyword from its status header
   (preferred) and body (fallback)."
  [raw-status lines]
  (or (match-phase raw-status)
      (some match-phase lines)
      :unknown))

(defn- italic-scaffold-line? [line]
  (boolean
   (or (re-matches #"^\s*\*[^*]+\*\s*$" (or line ""))
       (re-matches #"^\s*_[^_]+_\s*$" (or line "")))))

(def ^:private front-matter-key-pattern
  "Matches markdown bold-key front-matter lines like `**Status:** IDENTIFY`,
   `**Cross-refs:**` (trailing colon, no value on the same line), or
   `**Owner:** Joe`. Used to skip such lines during summary extraction
   so they don't bleed into the captured paragraph. Pragmatic: any line
   starting with `**Word…**` (with optional trailing colon) is treated
   as front-matter, even if it's a legitimate sentence opener — the
   front-matter failure mode is dominant in mission docs."
  #"^\s*\*\*[A-Za-z][^*]*\*\*:?")

(defn- front-matter-line? [line]
  (boolean (re-find front-matter-key-pattern (or line ""))))

(defn- extract-first-paragraph
  "Pull out a single body paragraph from LINES, skipping front-matter
   scaffolding at the start and stopping at the next non-prose element.

   Drop predicate skips: blank lines, `#` headings, HTML comments,
   blockquotes, table rows, bullet items, code fences, lines beginning
   with 2+ spaces (treated as bullet-list continuations), and markdown
   bold-key front-matter (`**Key:**`-style).

   Take predicate stops at: blank lines, headings, comments, tables,
   code fences, bullets, and front-matter — so a paragraph that ends
   when a list begins doesn't slurp the list."
  [lines]
  (->> lines
       (drop-while #(or (str/blank? %)
                        (str/starts-with? % "#")
                        (str/starts-with? % "<!--")
                        (str/starts-with? % ">")
                        (str/starts-with? % "|")
                        (str/starts-with? % "- ")
                        (str/starts-with? % "* ")
                        (str/starts-with? % "  ")
                        (str/starts-with? % "```")
                        (front-matter-line? %)))
       (take-while #(and (not (str/blank? %))
                         (not (str/starts-with? % "#"))
                         (not (str/starts-with? % "<!--"))
                         (not (str/starts-with? % "|"))
                         (not (str/starts-with? % "```"))
                         (not (str/starts-with? % "- "))
                         (not (str/starts-with? % "* "))
                         (not (front-matter-line? %))))
       (str/join " ")
       normalize-spaces))

(defn- subsection-lines [lines heading-re]
  (let [indexed (map-indexed vector lines)
        start (some (fn [[idx line]]
                      (when (re-find heading-re line)
                        idx))
                    indexed)]
    (when start
      (->> (drop (inc start) lines)
           (take-while #(not (re-matches #"^#{1,6}\s+.*$" %)))
           vec))))

(defn- section-paragraph [lines heading-re]
  (some-> (subsection-lines lines heading-re)
          extract-first-paragraph
          not-empty))

(defn- identify-fallback-paragraph [lines]
  (when-let [section (subsection-lines lines #"^##\s+1(?:bis)?\.\s+IDENTIFY")]
    (->> section
         (drop-while #(or (str/blank? %)
                          (re-matches #"^###\s+.*$" %)
                          (str/starts-with? % "<!--")
                          (italic-scaffold-line? %)))
         extract-first-paragraph
         not-empty)))

(defn- post-status-paragraph [lines]
  (when-let [status-idx (some (fn [[idx line]]
                                (when (re-find #"(?i)^\*\*Status:\*\*\s+" line)
                                  idx))
                              (map-indexed vector lines))]
    (->> (drop (inc status-idx) lines)
         extract-first-paragraph
         not-empty)))

(defn- mission-summary
  "Best-effort one-paragraph summary of the mission body, preferring
   explicit summary sections (Plain-language argument, Motivation) over
   IDENTIFY-section fallback, post-status paragraph, then the first body
   paragraph. Returns empty string if nothing usable is found."
  [lines]
  (or (section-paragraph lines #"(?i)^###\s+Plain-language argument")
      (section-paragraph lines #"^###\s+Motivation")
      (section-paragraph lines #"^###\s+Motivation \(re-run")
      (identify-fallback-paragraph lines)
      (post-status-paragraph lines)
      (some-> (drop 1 lines) extract-first-paragraph not-empty)
      ""))

;; ---------------------------------------------------------------------
;; T-9c: PSR / PUR section extraction
;; ---------------------------------------------------------------------

(def ^:private record-heading-pattern
  "Matches any markdown heading of level 2 or deeper whose text begins
   with PSR or PUR (case-insensitive), e.g. `## PSR`, `### PSR/PUR per
   phase`, `#### PSR-1: \\`pattern-name\\``, `### Pattern Selection
   Records (PSRs)`. Level-1 (`#`) is excluded because mission bodies
   rarely use it as a top-level heading and `# PSR-A1:` lines often
   appear inside code fences as in-block titles, not as real document
   headings."
  #"(?i)^#{2,}\s+(?:Pattern\s+(?:Selection|Use)\s+Records?\s*\(?(PSRs?|PURs?)\)?|((?:PSR|PUR)s?)\b)\s*[:\-—/]*\s*(.*)$")

(def ^:private any-heading-pattern #"^#{2,}\s+\S")

(defn- group-bullet-blocks
  "Walk record-body LINES and group each `- Key: value` bullet with its
   indented continuation lines, stopping at the next bullet, the next
   heading, or a blank line. Returns a vec of vecs."
  [lines]
  (loop [remaining lines, groups []]
    (if (empty? remaining)
      groups
      (let [[line & rest-lines] remaining]
        (if (re-find #"^-\s+" line)
          (let [conts (take-while #(and (not (str/blank? %))
                                        (not (re-find any-heading-pattern %))
                                        (not (re-find #"^-\s+" %)))
                                  rest-lines)
                after (drop (count conts) rest-lines)]
            (recur after (conj groups (vec (cons line conts)))))
          (recur rest-lines groups))))))

(defn- parse-bullet-block
  "Convert BLOCK (a vec whose first element is `- Key: value` and the
   rest are continuation lines) into `[key value]` where KEY is the
   lowercased, hyphenated bullet label and VALUE is the joined text.
   Returns nil if the first line isn't bullet-shaped."
  [block]
  (when (seq block)
    (let [first-line (first block)]
      (when-let [m (re-matches #"^-\s+\*{0,2}([^:*][^:*]*?)\*{0,2}:\s*(.*)$" first-line)]
        (let [k (-> (nth m 1) str/trim str/lower-case
                    (str/replace #"\s+" "-"))
              v (str/trim (nth m 2))
              cont-text (->> (rest block)
                             (map str/trim)
                             (remove str/blank?)
                             (str/join " "))
              full-v (if (seq cont-text)
                       (str/trim (str v (when (seq v) " ") cont-text))
                       v)]
          [k full-v])))))

(defn- collect-record-body
  "Collect body lines for a record section, stopping at the next heading."
  [lines]
  (->> lines (take-while #(not (re-find any-heading-pattern %))) vec))

(defn- parse-record
  "Convert a [heading-line body-lines] tuple into a record map
   `{:title <str-or-nil> :fields {key value ...}}`. TYPE is `\"PSR\"` or
   `\"PUR\"` (uppercase) — used to extract the optional title suffix
   after the type marker in the heading."
  [type heading body-lines]
  (let [match (re-find record-heading-pattern heading)
        ;; Group 1 is the bare PSR/PUR(s), group 2 is the parenthesised
        ;; variant from "Pattern Selection Records (PSRs)". The trailing
        ;; capture (group 3) is whatever follows the type marker.
        title-suffix (when match (nth match 3 nil))
        title-suffix (when (and title-suffix (seq (str/trim title-suffix)))
                       (str/trim title-suffix))
        groups (group-bullet-blocks body-lines)
        fields (into {} (keep parse-bullet-block) groups)]
    (cond-> {:fields fields}
      title-suffix (assoc :title title-suffix))))

(defn- extract-records
  "Extract all PSR or PUR records from mission LINES. TYPE-RE is a regex
   matching the section type marker (uppercase). Returns a vec of record
   maps."
  [lines type-re]
  (loop [remaining lines, results []]
    (if (empty? remaining)
      results
      (let [[line & rest-lines] remaining]
        (if (and (re-find any-heading-pattern line)
                 (re-find type-re line))
          (let [body (collect-record-body rest-lines)
                rec (parse-record (str/upper-case (second (re-find type-re line)))
                                  line body)
                after (drop (count body) rest-lines)]
            (recur after (conj results rec)))
          (recur rest-lines results))))))

(defn- extract-psrs
  "Extract all `## PSR`, `### PSR`, `#### PSR-N: title`, `### Pattern
   Selection Records (PSRs)` etc. sections from LINES."
  [lines]
  (extract-records lines #"(?i)\b(PSR)s?\b"))

(defn- extract-purs
  "Mirror of `extract-psrs` for PUR sections."
  [lines]
  (extract-records lines #"(?i)\b(PUR)s?\b"))

;; ---------------------------------------------------------------------
;; End T-9c
;; ---------------------------------------------------------------------

(defn- file-mtime-iso
  "Return PATH's last-modified time as an ISO local-date string
   (yyyy-mm-dd), or nil if the file is missing / cannot be read."
  [^String path]
  (try
    (let [f (io/file path)]
      (when (.exists f)
        (let [instant (java.time.Instant/ofEpochMilli (.lastModified f))
              date    (-> instant
                          (.atZone (java.time.ZoneId/systemDefault))
                          .toLocalDate)]
          (.format date java.time.format.DateTimeFormatter/ISO_LOCAL_DATE))))
    (catch Exception _ nil)))

(defn parse-mission-md
  "Parse a mission .md file into a MissionEntry. T-9b: now extracts the
   content-enrichment fields (`:mission/summary`, `:mission/cross-refs`,
   `:mission/code-paths`, `:mission/phase`) that downstream consumers
   previously re-parsed for, plus the file's last-modified date
   (`:mission/mtime`) for downstream staleness heuristics. These flow
   into the substrate-2 hyperedge props via
   `futon3c.watcher.file-ingest/ingest-mission-doc!`."
  [path repo-name]
  (try
    (let [text (slurp path)
          lines (str/split-lines text)
          ;; Header extraction searches only the first 80 lines to avoid
          ;; picking up subsection **Status**: lines from the body.
          header-text (->> lines (take 80) (str/join "\n"))
          filename (.getName (io/file path))
          mission-id (str/replace filename #"^M-|\.md$" "")
          title (when-let [m (re-find #"(?m)^#\s+(?:Mission:\s*)?(.+)$" header-text)]
                  (str/trim (second m)))
          raw-status (extract-header header-text "Status")
          date (extract-header header-text "Date")
          blocked-by (extract-header header-text "Blocked by")
          owner (extract-header header-text "Owner")
          explicit-status (classify-status raw-status)
          checkboxes (count-checkboxes text)
          inferred-status (when (and (nil? explicit-status) checkboxes)
                            (infer-status-from-checkboxes checkboxes))
          status (or explicit-status inferred-status :unknown)
          ;; T-9b content enrichment
          summary (mission-summary lines)
          cross-refs (extract-cross-refs text mission-id)
          code-paths (extract-code-paths text)
          phase (classify-phase raw-status lines)
          mtime (file-mtime-iso path)
          ;; T-9c: PSR / PUR section extraction (mission-embedded records)
          psrs (extract-psrs lines)
          purs (extract-purs lines)]
      (cond-> {:mission/id mission-id
               :mission/status status
               :mission/source :md-file
               :mission/repo (name repo-name)
               :mission/path (str path)
               :mission/title title
               :mission/date date
               :mission/blocked-by blocked-by
               :mission/owner owner
               :mission/raw-status raw-status
               :mission/summary summary
               :mission/cross-refs cross-refs
               :mission/code-paths code-paths
               :mission/phase phase
               :mission/mtime mtime
               :mission/psrs psrs
               :mission/purs purs}
        checkboxes
        (assoc :mission/gates checkboxes)
        (and (nil? explicit-status) inferred-status)
        (assoc :mission/raw-status
               (str "inferred:" (name inferred-status)
                    " (" (:checked checkboxes) "/" (:total checkboxes) " gates)"))))
    (catch Exception e
      {:mission/id (str path)
       :mission/status :unknown
       :mission/source :md-file
       :mission/repo (name repo-name)
       :mission/path (str path)
       :mission/raw-status (str "parse-error: " (.getMessage e))})))

(defn- path-under-root?
  [root path]
  (let [root (some-> root io/file .getCanonicalPath normalize-path-separators)
        path (some-> path io/file .getCanonicalPath normalize-path-separators)]
    (and root path
         (or (= root path)
             (str/starts-with? path (str root "/"))))))

(defn infer-repo-name-for-path
  "Best-effort repo inference for an absolute path using known repo roots.
   Returns a repo keyword or nil."
  ([path] (infer-repo-name-for-path default-repo-roots path))
  ([repos path]
   (some (fn [[repo-name root]]
           (when (path-under-root? root path)
             repo-name))
         repos)))

(defn parse-mission-path
  "Parse a single mission markdown path with explicit or inferred repo name.
   Returns a MissionEntry or nil when the file does not look like a mission."
  ([path] (parse-mission-path default-repo-roots path nil))
  ([repos path repo-name]
   (let [path-str (str path)
         file-name (.getName (io/file path-str))
         repo-name (or repo-name (infer-repo-name-for-path repos path-str))]
     (when (and repo-name
                (str/starts-with? file-name "M-")
                (str/ends-with? file-name ".md"))
       (parse-mission-md path-str repo-name)))))

(defn- classify-devmap-state
  "Convert an EDN :mission/state keyword to a MissionStatus."
  [state]
  (case state
    :complete   :complete
    :active     :in-progress
    :greenfield :ready
    :composed   :in-progress
    :unknown))

(defn parse-devmap-edn
  "Parse a devmap EDN file into a MissionEntry."
  [path]
  (try
    (let [raw (edn/read-string (slurp path))
          mid (:mission/id raw)
          state (:mission/state raw)]
      (when mid
        {:mission/id (name mid)
         :mission/status (classify-devmap-state state)
         :mission/source :devmap-edn
         :mission/repo "futon5"
         :mission/path (str path)
         :mission/raw-status (when state (name state))
         :mission/devmap-id mid}))
    (catch Exception _e nil)))

;; =============================================================================
;; Inventory scanning
;; =============================================================================

(defn scan-mission-files
  "Scan for mission markdown files in a repo's holes/missions/ tree.

   Canonical format:
   - M-<mission-id>.md (any depth)

   Legacy compatibility:
   - non-M markdown files at top-level holes/missions/ (excluding README.md
     and mission-template.md)."
  [repo-root repo-name]
  (let [dir (io/file repo-root "holes" "missions")]
    (if (.isDirectory dir)
      (->> (file-seq dir)
           (filter (fn [f]
                     (when (.isFile f)
                       (let [name (.getName f)
                             parent-path (some-> (.getParentFile f)
                                                 .getAbsolutePath)
                             root-path (.getAbsolutePath dir)
                             top-level? (= parent-path root-path)
                             canonical? (and (str/starts-with? name "M-")
                                             (str/ends-with? name ".md"))
                             ;; Legacy: only pick up non-M-prefix files that
                             ;; look like missions (M1-, V-, QUEUE-). Exclude
                             ;; random specs, scorecards, and artifact docs.
                             legacy-top-level? (and top-level?
                                                    (str/ends-with? name ".md")
                                                    (not (str/starts-with? name "M-"))
                                                    (not (#{"README.md"
                                                            "mission-template.md"} name))
                                                    (re-find #"^(?:M\d|V-|QUEUE-)" name))]
                         (or canonical? legacy-top-level?)))))
           (sort-by #(.getPath %))
           (mapv #(parse-mission-md (.getPath %) repo-name)))
      [])))

(defn scan-devmap-files
  "Scan for mission EDN files in futon5/data/missions/."
  [futon5-root]
  (let [dir (io/file futon5-root "data" "missions")]
    (if (.isDirectory dir)
      (->> (.listFiles dir)
           (filter #(and (.isFile %)
                         (str/ends-with? (.getName %) ".edn")))
           (keep #(parse-devmap-edn (.getPath %)))
           vec)
      [])))

;; =============================================================================
;; T-9b: Substrate-2 as primary source for mission inventory
;; =============================================================================

(def ^:private stale-in-progress-days
  "When an :in-progress mission's source file hasn't been modified for
   this many days, the read-side overrides its status to
   :stale-in-progress so that downstream UIs (Arxana Browser, WM
   surface) can distinguish actively-worked-on missions from
   indefinitely-pending ones. The raw header status is preserved in
   `:mission/raw-status`."
  7)

(defn- days-since
  "Return whole days between the ISO local-date string MTIME-STR and
   today (system zone). Returns nil if MTIME-STR can't be parsed."
  [mtime-str]
  (when mtime-str
    (try
      (let [mtime-date (java.time.LocalDate/parse mtime-str)
            today (java.time.LocalDate/now)]
        (.between java.time.temporal.ChronoUnit/DAYS mtime-date today))
      (catch Exception _ nil))))

(defn- apply-staleness-override
  "If STATUS is :in-progress and DAYS-STALE exceeds
   `stale-in-progress-days`, return :stale-in-progress. Otherwise return
   STATUS unchanged."
  [status days-stale]
  (if (and (= :in-progress status)
           (some? days-stale)
           (> days-stale stale-in-progress-days))
    :stale-in-progress
    status))

(defn- hyperedge-props->mission-entry
  "Convert a substrate-2 mission-doc hyperedge's :hx/props map back into
   the MissionEntry shape expected by build-inventory and downstream
   consumers. Sets `:mission/source` to `:substrate-2` to distinguish
   from the legacy `:md-file` filesystem-scan source. T-9b: also
   computes `:mission/days-stale` from `:mission/mtime` and overrides
   `:mission/status` to `:stale-in-progress` when an in-progress mission
   has been untouched for more than `stale-in-progress-days`."
  [props]
  (let [g (fn [k] (get props k))
        status-str (g :mission/status)
        base-status (or (some-> status-str classify-status) :unknown)
        phase-raw (g :mission/phase)
        phase (cond
                (nil? phase-raw) nil
                (keyword? phase-raw) phase-raw
                (string? phase-raw) (keyword phase-raw)
                :else nil)
        mtime (g :mission/mtime)
        days-stale (days-since mtime)
        status (apply-staleness-override base-status days-stale)
        gates (g :mission/gates)]
    (cond-> {:mission/id (g :mission/id)
             :mission/status status
             :mission/source :substrate-2
             :mission/repo (g :mission/repo)
             :mission/path (g :source-file)
             :mission/title (g :mission/title)
             :mission/date (g :mission/date)
             :mission/blocked-by (g :mission/blocked-by)
             :mission/owner (g :mission/owner)
             :mission/raw-status (g :mission/raw-status)
             :mission/summary (g :mission/summary)
             :mission/cross-refs (or (g :mission/cross-refs) [])
             :mission/code-paths (or (g :mission/code-paths) [])
             :mission/phase phase
             :mission/mtime mtime
             :mission/days-stale days-stale
             :mission/psrs (or (g :mission/psrs) [])
             :mission/purs (or (g :mission/purs) [])}
      gates (assoc :mission/gates gates))))

(defn- fetch-substrate-2-missions
  "Query futon1a for all current `code/v05/mission-doc` hyperedges and
   return them as MissionEntry-shaped maps. Returns nil if substrate-2
   is unreachable; build-inventory falls back to filesystem scan in
   that case."
  []
  (try
    (let [url (str futon1a-url
                   "/api/alpha/hyperedges?type=" mission-doc-hyperedge-type
                   "&limit=500")
          resp (http/get url {:headers {"Accept" "application/json"}
                              :throw false
                              :timeout 5000})]
      (when (= 200 (:status resp))
        (let [parsed (json/parse-string (:body resp) true)
              hxes (:hyperedges parsed)]
          (->> hxes
               (map :hx/props)
               (filter some?)
               (mapv hyperedge-props->mission-entry)))))
    (catch Exception _ nil)))

(defn build-inventory
  "Build the full cross-repo mission inventory.

   **T-9b**: PRIMARY source is now substrate-2 (futon1a hyperedge
   query). The filesystem walker (`scan-mission-files`) is preserved
   as a fallback only when substrate-2 is unreachable. The previous
   behaviour — three independent parsers competing for the same
   markdown files — is consolidated to one: the watcher's
   `ingest-mission-doc!` writes substrate-2; build-inventory reads it.

   repos: map of {repo-name root-path} (defaults to default-repo-roots).
   In substrate-2 mode the `repos` arg is used only for devmap scanning
   and as filesystem fallback; substrate-2 covers all repos already
   ingested, regardless of the requested subset."
  ([] (build-inventory default-repo-roots))
  ([repos]
   (let [md-missions (or (fetch-substrate-2-missions)
                         ;; Fallback: substrate-2 unreachable. Scan filesystem
                         ;; via the canonical parser so the API stays usable.
                         (into []
                               (mapcat (fn [[repo-name root]]
                                         (scan-mission-files root repo-name)))
                               repos))
         devmap-missions (if-let [f5 (:futon5 repos)]
                           (scan-devmap-files f5)
                           [])]
     (into md-missions devmap-missions))))

;; =============================================================================
;; Mission doc fidelity audit (GF / drift)
;; =============================================================================

(defn- repo-root-for-mission
  [repos mission]
  (let [repo-k (keyword (:mission/repo mission))]
    (or (get repos repo-k)
        (get default-repo-roots repo-k))))

(defn- audit-mission-doc
  "Run mission-doc-audit for markdown missions.
   Returns audit map. Non-markdown sources return :n/a."
  [repos mission]
  (if (not= :md-file (:mission/source mission))
    {:status :n/a
     :reason :non-md-source}
    (let [mission-id (:mission/id mission)
          mission-path (:mission/path mission)
          repo-root (repo-root-for-mission repos mission)
          guide-path (when repo-root (str repo-root "/docs/futonic-missions.md"))
          cwd (or repo-root (System/getProperty "user.dir"))
          backend (mb/make-mission-backend {:cwd cwd} (tools/make-mock-backend))
          opts (cond-> {:mission-doc-path mission-path}
                 guide-path (assoc :guide-path guide-path))
          result (try
                   (tools/execute-tool backend :mission-doc-audit [mission-id opts])
                   (catch Exception e
                     {:ok false :error (.getMessage e)}))]
      (if (:ok result)
        (:result result)
        {:status :error
         :reason :audit-failed
         :error (or (:error result) "mission-doc-audit failed")}))))

(defn- attach-doc-audit
  [repos missions]
  (mapv (fn [m]
          (if (= :md-file (:mission/source m))
            (assoc m :mission/doc-audit (audit-mission-doc repos m))
            m))
        missions))

(defn- summarize-doc-drift
  "Summarize mission docs/code drift across markdown missions."
  [missions]
  (let [audited (->> missions
                     (keep :mission/doc-audit)
                     (filter #(not= :n/a (:status %)))
                     vec)
        ok-count (count (filter #(= :ok (:status %)) audited))
        drift (filter #(= :drift (:status %)) audited)
        error-count (count (filter #(= :error (:status %)) audited))
        open-sections (reduce + (map #(or (:open-section-count %) 0) drift))
        missing-gf (reduce + (map #(count (get-in % [:gf :missing-headings])) drift))
        drifting-ids (mapv :mission-id drift)]
    {:audit/total (count audited)
     :audit/ok ok-count
     :audit/drift (count drift)
     :audit/error error-count
     :audit/open-section-obligations open-sections
     :audit/missing-gf-headings missing-gf
     :audit/drifting-missions drifting-ids}))

;; =============================================================================
;; Devmap reading (minimal reimpl — no futon5 classpath dependency)
;; =============================================================================

(defn read-devmap
  "Read a devmap EDN file and extract structural summary.
   This is a minimal reimpl of futon5.ct.mission/summary that reads
   the EDN directly without depending on the futon5 classpath."
  [path]
  (try
    (let [raw (edn/read-string (slurp path))
          mid (:mission/id raw)
          state (:mission/state raw)
          inputs (get-in raw [:ports :input])
          outputs (get-in raw [:ports :output])
          components (:components raw)
          edges (:edges raw)
          ;; Minimal validation: check structural properties
          input-ids (set (map :id inputs))
          comp-ids (set (map :id components))
          edge-froms (set (map :from edges))
          edge-tos (set (map :to edges))
          ;; Check for orphan inputs (input not connected to anything)
          orphan-inputs (filter #(not (edge-froms %)) input-ids)
          ;; Check for dead components (component not reaching any output)
          ;; Simplified: just check if component appears in any edge
          connected-comps (into (set (filter comp-ids edge-froms))
                                (filter comp-ids edge-tos))
          dead-comps (remove connected-comps comp-ids)
          ;; Check for spec coverage (outputs with :spec-ref)
          unspecified-outputs (filter #(nil? (:spec-ref %)) outputs)
          failed-checks (cond-> []
                          (seq orphan-inputs)
                          (conj :no-orphan-inputs)
                          (seq dead-comps)
                          (conj :coverage)
                          (seq unspecified-outputs)
                          (conj :spec-coverage))]
      {:devmap/id mid
       :devmap/state (or state :unknown)
       :devmap/input-count (count inputs)
       :devmap/output-count (count outputs)
       :devmap/component-count (count components)
       :devmap/edge-count (count edges)
       :devmap/all-valid (empty? failed-checks)
       :devmap/failed-checks (vec failed-checks)
       :devmap/components (mapv (fn [c]
                                  {:component/id (:id c)
                                   :component/name (or (:name c) (name (:id c)))})
                                components)})
    (catch Exception _e
      {:devmap/id (keyword (.getName (io/file path)))
       :devmap/state :error
       :devmap/input-count 0
       :devmap/output-count 0
       :devmap/component-count 0
       :devmap/edge-count 0
       :devmap/all-valid false
       :devmap/failed-checks [:parse-error]
       :devmap/components []})))

(defn- scan-per-mission-wirings
  "Scan holes/missions/*-wiring.edn in each repo for per-mission wiring diagrams."
  [repos]
  (into []
        (mapcat
         (fn [[_repo-name root]]
           (let [dir (io/file root "holes" "missions")]
             (if (.isDirectory dir)
               (->> (.listFiles dir)
                    (filter #(and (.isFile %)
                                  (str/ends-with? (.getName %) "-wiring.edn")))
                    (keep #(read-devmap (.getPath %))))
               []))))
        repos))

(defn read-all-devmaps
  "Read all devmap EDN files from futon5/data/missions/ and per-mission
   wiring diagrams from each repo's holes/missions/*-wiring.edn."
  ([futon5-root] (read-all-devmaps futon5-root nil))
  ([futon5-root repos]
   (let [futon5-devmaps (let [dir (io/file futon5-root "data" "missions")]
                          (if (.isDirectory dir)
                            (->> (.listFiles dir)
                                 (filter #(and (.isFile %)
                                               (str/ends-with? (.getName %) ".edn")
                                               (not (str/includes? (.getName %) "grounding-functor"))))
                                 (mapv #(read-devmap (.getPath %))))
                            []))
         per-mission (scan-per-mission-wirings (or repos default-repo-roots))]
     (into futon5-devmaps per-mission))))

;; =============================================================================
;; Devmap coverage analysis
;; =============================================================================

(def component-coverage-annotations
  "Explicit component → mission-id coverage map.
   The heuristic substring match is a fallback; this map is ground truth
   where provided. Values are sets of lowercase mission-id strings.
   Extend via (mc/audit-coverage-correspondence) to find orphan components."
  {;; social-exotype components
   :S-presence     #{"transport-adapters" "operational-readiness"}
   :S-authenticate #{"agency-refactor"}
   :S-dispatch     #{"dispatch-peripheral-bridge" "agency-refactor"}
   :S-invoke       #{"peripheral-model" "peripheral-behavior"}
   :S-mode         #{"peripheral-model" "peripheral-behavior"}
   :S-validate     #{"proof-peripheral"}
   :S-persist      #{"forum-refactor"}
   ;; coordination-exotype (gate pipeline)
   :G5 #{"peripheral-gauntlet"}
   :G4 #{"agency-refactor"}
   :G3 #{"psr-pur-mesh-peripheral"}
   :G2 #{"dispatch-peripheral-bridge"}
   :G1 #{"proof-peripheral"}
   :G0 #{"futon3-last-mile"}})

(defn compute-coverage
  "Compute coverage of devmap components by missions.

   Three-tier matching:
   1. Parent match: if the devmap's :devmap/id matches an active mission,
      ALL its components are considered covered.
   2. Annotation match: check component-coverage-annotations map for
      explicit component → mission-id correspondence.
   3. Heuristic: substring match on component name ↔ mission name."
  [devmap-summaries missions]
  (let [mission-ids (set (map :mission/id missions))
        mission-id-lower (set (map str/lower-case mission-ids))
        active-ids (set (map (comp str/lower-case :mission/id)
                             (filter #(#{:in-progress :complete} (:mission/status %))
                                     missions)))]
    (mapv (fn [dm]
            (let [components (:devmap/components dm)
                  devmap-mid (when-let [id (:devmap/id dm)]
                               (str/lower-case (name id)))
                  parent-active? (and devmap-mid (contains? active-ids devmap-mid))
                  covered (if parent-active?
                            components
                            (filter (fn [c]
                                      (let [cid (:component/id c)
                                            ;; Tier 2: explicit annotation
                                            annotated (get component-coverage-annotations cid)
                                            annotated? (and annotated
                                                            (some annotated mission-id-lower))
                                            ;; Tier 3: heuristic fallback
                                            cname (str/lower-case (name cid))
                                            heuristic? (some (fn [mid]
                                                               (or (str/includes? mid cname)
                                                                   (str/includes? cname mid)))
                                                             mission-id-lower)]
                                        (or annotated? heuristic?)))
                                    components))
                  uncovered (remove (set (map :component/id covered)) (map :component/id components))
                  total (count components)]
              {:coverage/devmap-id (:devmap/id dm)
               :coverage/total-components total
               :coverage/covered-components (count covered)
               :coverage/uncovered (vec uncovered)
               :coverage/coverage-pct (if (pos? total)
                                        (double (/ (count covered) total))
                                        1.0)}))
          devmap-summaries)))

;; =============================================================================
;; Mana queries
;; =============================================================================

(defn query-mana
  "Query mana pool stats from nonstarter.db (if it exists).
   Returns a ManaSnapshot."
  [futon5-root]
  (let [db-path (str futon5-root "/data/nonstarter.db")
        db-file (io/file db-path)]
    (if (.exists db-file)
      ;; nonstarter.db exists — try to read pool stats
      ;; We avoid requiring nonstarter.db.clj directly to keep repos independent.
      ;; Instead, read via raw JDBC if available, or return :available true with note.
      {:mana/available true
       :mana/pool-balance 0.0
       :mana/total-donated 0.0
       :mana/total-funded 0.0
       :mana/active-proposals 0}
      ;; nonstarter.db doesn't exist yet
      {:mana/available false})))

;; =============================================================================
;; Portfolio review
;; =============================================================================

(defn- summarize-portfolio
  "Generate a human-readable portfolio summary."
  [missions devmap-summaries coverage mana doc-drift]
  (let [total-missions (count missions)
        complete (count (filter #(= :complete (:mission/status %)) missions))
        in-progress (count (filter #(in-progress-status? (:mission/status %)) missions))
        blocked (count (filter #(= :blocked (:mission/status %)) missions))
        ready (count (filter #(= :ready (:mission/status %)) missions))
        total-devmaps (count devmap-summaries)
        valid-devmaps (count (filter :devmap/all-valid devmap-summaries))
        avg-coverage (if (seq coverage)
                       (/ (reduce + (map :coverage/coverage-pct coverage)) (count coverage))
                       0.0)]
    (str total-missions " missions"
         " (" complete " complete"
         ", " in-progress " in-progress"
         ", " blocked " blocked"
         ", " ready " ready)"
         ". " total-devmaps " devmaps"
         " (" valid-devmaps " valid)."
         " Avg coverage: " (format "%.0f%%" (* 100 avg-coverage)) "."
         " Doc drift: " (:audit/drift doc-drift) "/" (:audit/total doc-drift)
         " drift, " (:audit/open-section-obligations doc-drift)
         " open section obligations."
         (when-not (:mana/available mana) " Mana system not yet initialized."))))

(defn- find-gaps
  "Identify gaps: devmap components without missions, blocked missions, etc."
  [missions coverage]
  (let [blocked-missions (filter #(= :blocked (:mission/status %)) missions)
        doc-drift-missions (->> missions
                                (filter (fn [m]
                                          (= :drift (get-in m [:mission/doc-audit :status]))))
                                (map (fn [m]
                                       (let [a (:mission/doc-audit m)]
                                         (str (:mission/id m)
                                              " — doc drift"
                                              " (open sections: "
                                              (or (:open-section-count a) 0)
                                              ", missing GF headings: "
                                              (count (get-in a [:gf :missing-headings]))
                                              ")"))))
                                vec)
        uncovered (mapcat (fn [c]
                            (map (fn [comp-id]
                                   (str (name (:coverage/devmap-id c))
                                        "/" (name comp-id) " — no mission"))
                                 (:coverage/uncovered c)))
                          coverage)]
    (into (into (mapv (fn [m]
                        (str (:mission/id m) " — blocked"
                             (when (:mission/blocked-by m)
                               (str ": " (:mission/blocked-by m)))))
                      blocked-missions)
                doc-drift-missions)
          uncovered)))

(defn audit-coverage-correspondence
  "Audit devmap/mission correspondence. Returns:
   :orphan-components — devmap components with no annotation and no heuristic match
   :orphan-missions   — missions that address no devmap component
   :stale-annotations — annotation entries referencing non-existent missions
   :summary           — counts for quick overview"
  ([] (audit-coverage-correspondence default-repo-roots))
  ([repos]
   (let [missions (build-inventory repos)
         futon5-root (or (:futon5 repos) (:futon5 default-repo-roots))
         devmaps (read-all-devmaps futon5-root repos)
         mission-id-lower (set (map (comp str/lower-case :mission/id) missions))
         all-components (mapcat (fn [dm]
                                  (map (fn [c] {:devmap (:devmap/id dm)
                                                :component (:component/id c)})
                                       (:devmap/components dm)))
                                devmaps)
         annotated-ids (set (keys component-coverage-annotations))
         orphan-components (vec
                            (remove (fn [{:keys [component]}]
                                      (or (contains? annotated-ids component)
                                          (let [cname (str/lower-case (name component))]
                                            (some (fn [mid]
                                                    (or (str/includes? mid cname)
                                                        (str/includes? cname mid)))
                                                  mission-id-lower))))
                                    all-components))
         ;; Missions that appear in no annotation and no devmap parent
         devmap-mids (set (map (comp str/lower-case name :devmap/id) devmaps))
         annotation-mids (reduce into #{} (vals component-coverage-annotations))
         orphan-missions (vec
                          (remove (fn [m]
                                    (let [mid (str/lower-case (:mission/id m))]
                                      (or (contains? devmap-mids mid)
                                          (contains? annotation-mids mid))))
                                  missions))
         stale (reduce-kv (fn [acc comp-id mission-set]
                            (let [missing (remove mission-id-lower mission-set)]
                              (if (seq missing)
                                (conj acc {:component comp-id
                                           :missing-missions (vec missing)})
                                acc)))
                          [] component-coverage-annotations)]
     {:orphan-components orphan-components
      :orphan-missions (mapv #(select-keys % [:mission/id :mission/repo :mission/status])
                             orphan-missions)
      :stale-annotations stale
      :summary {:total-components (count all-components)
                :annotated (count annotated-ids)
                :orphan-components (count orphan-components)
                :total-missions (count missions)
                :orphan-missions (count orphan-missions)
                :stale-annotations (count stale)}})))

(defn- find-actionable
  "Identify actionable missions: ready or in-progress, not blocked."
  [missions]
  (let [actionable (filter #(or (= :ready (:mission/status %))
                                (in-progress-status? (:mission/status %)))
                           missions)]
    (mapv (fn [m]
            (str (:mission/id m)
                 " (" (name (:mission/status m)) ")"
                 (when (:mission/repo m) (str " [" (:mission/repo m) "]"))))
          actionable)))

(defn build-portfolio-review
  "Build a complete portfolio review from scanned data.
   repos: map of {repo-name root-path} (defaults to default-repo-roots)."
  ([] (build-portfolio-review default-repo-roots))
  ([repos]
   (let [missions-raw (build-inventory repos)
         missions (attach-doc-audit repos missions-raw)
         futon5-root (or (:futon5 repos) (:futon5 default-repo-roots))
         devmap-summaries (read-all-devmaps futon5-root repos)
         coverage (compute-coverage devmap-summaries missions)
         mana (query-mana futon5-root)
         doc-drift (summarize-doc-drift missions)
         summary (summarize-portfolio missions devmap-summaries coverage mana doc-drift)
         gaps (find-gaps missions coverage)
         actionable (find-actionable missions)]
     {:portfolio/missions missions
      :portfolio/devmap-summaries devmap-summaries
      :portfolio/coverage coverage
      :portfolio/mana mana
      :portfolio/doc-drift doc-drift
      :portfolio/summary summary
      :portfolio/gaps gaps
      :portfolio/actionable actionable})))

;; =============================================================================
;; Tension export — structured gaps for hyperedge creation
;; =============================================================================

(defn build-tension-export
  "Build structured tension data from portfolio review.
   Returns typed tension entries pre-shaped for Arxana hyperedge creation.
   repos: map of {repo-name root-path} (defaults to default-repo-roots)."
  ([] (build-tension-export default-repo-roots))
  ([repos]
   (let [review (build-portfolio-review repos)
         missions (:portfolio/missions review)
         coverage (:portfolio/coverage review)
         devmaps (:portfolio/devmap-summaries review)
         now (str (java.time.Instant/now))
         ;; Uncovered components: one tension per (devmap, component) pair
         uncovered-tensions
         (into []
               (mapcat (fn [cov]
                         (let [dm-id (:coverage/devmap-id cov)]
                           (map (fn [comp-id]
                                  {:tension/type :uncovered-component
                                   :tension/devmap dm-id
                                   :tension/component comp-id
                                   :tension/coverage-pct (:coverage/coverage-pct cov)
                                   :tension/detected-at now
                                   :tension/summary (str (name dm-id) "/" (name comp-id)
                                                         " — no mission")})
                                (:coverage/uncovered cov)))))
               coverage)
         ;; Blocked missions
         blocked-tensions
         (into []
               (comp (filter #(= :blocked (:mission/status %)))
                     (map (fn [m]
                            {:tension/type :blocked-mission
                             :tension/mission (:mission/id m)
                             :tension/blocked-by (:mission/blocked-by m)
                             :tension/detected-at now
                             :tension/summary (str (:mission/id m) " — blocked"
                                                   (when (:mission/blocked-by m)
                                                     (str ": " (:mission/blocked-by m))))})))
               missions)
         ;; Structural invalidity: devmaps with failed checks
         structural-tensions
         (into []
               (comp (filter #(seq (:devmap/failed-checks %)))
                     (map (fn [dm]
                            {:tension/type :structural-invalid
                             :tension/devmap (:devmap/id dm)
                             :tension/detected-at now
                             :tension/summary (str (name (:devmap/id dm))
                                                   " — failed checks: "
                                                   (str/join ", " (map name (:devmap/failed-checks dm))))})))
               devmaps)
         tensions (into (into uncovered-tensions blocked-tensions) structural-tensions)
         by-type (frequencies (map :tension/type tensions))]
     {:tensions tensions
      :detected-at now
      :summary {:total (count tensions)
                :by-type by-type}})))

;; =============================================================================
;; Tension path tracing — 間 + 関 (gap-reading + gate-traversal)
;; =============================================================================
;;
;; Given a tension, trace the full chain of 関 from devmap to source:
;;   devmap → component → covering missions (or gap) → evidence → var → source
;; Each step is a typed gate that either passes (link found) or blocks (gap).
;; The trace is the replicable form of what tension discovery demonstrated ad hoc.

(defn trace-tension-path
  "Trace a tension through the full chain of 関 (gates/relations).

   Takes a tension entry (from build-tension-export) and the current
   portfolio review. Returns a path map with each gate's result:

   {:tension    — the input tension
    :gates      — ordered vector of gate results
    :complete?  — true if all gates passed (full path traced)
    :blocked-at — keyword of the first blocked gate, or nil}

   Gate types:
   - :devmap     — does the devmap exist and what's its structure?
   - :component  — does the component exist in the devmap?
   - :coverage   — which missions cover this component (may be empty = gap)?
   - :evidence   — what evidence exists for related missions?
   - :reflection — can we resolve a var that implements this?
   - :source     — file + line of the implementing code"
  ([tension] (trace-tension-path tension nil))
  ([tension review]
   (let [review (or review (build-portfolio-review))
         devmaps (:portfolio/devmap-summaries review)
         missions (:portfolio/missions review)
         coverage (:portfolio/coverage review)
         dm-id (:tension/devmap tension)
         comp-id (:tension/component tension)
         ttype (:tension/type tension)

         ;; Gate 1: Devmap
         devmap (first (filter #(= dm-id (:devmap/id %)) devmaps))
         g-devmap {:gate :devmap
                   :status (if devmap :pass :blocked)
                   :data (when devmap
                           {:id (:devmap/id devmap)
                            :state (:devmap/state devmap)
                            :component-count (:devmap/component-count devmap)
                            :edge-count (:devmap/edge-count devmap)})}

         ;; Gate 2: Component
         component (when devmap
                     (first (filter #(= comp-id (:component/id %))
                                    (:devmap/components devmap))))
         g-component {:gate :component
                      :status (if component :pass :blocked)
                      :data (when component
                              {:id (:component/id component)
                               :name (:component/name component)
                               :devmap dm-id})}

         ;; Gate 3: Coverage (this is where uncovered tensions block)
         cov-entry (first (filter #(= dm-id (:coverage/devmap-id %)) coverage))
         covering-missions (when (and cov-entry comp-id)
                             (get (:coverage/by-component cov-entry) comp-id))
         g-coverage {:gate :coverage
                     :status (if (seq covering-missions) :pass :gap)
                     :data {:component comp-id
                            :missions (vec (or covering-missions []))
                            :tension-type ttype}}

         ;; Gate 4: Evidence (for covering missions, or the devmap itself)
         related-mission-ids (if (seq covering-missions)
                               covering-missions
                               ;; No covering missions — look for devmap-level evidence
                               (let [dm-name (when dm-id (str/lower-case (name dm-id)))]
                                 (keep (fn [m]
                                         (when (= dm-name (str/lower-case (:mission/id m)))
                                           (:mission/id m)))
                                       missions)))
         related-missions (filter (fn [m]
                                    (contains? (set related-mission-ids)
                                               (:mission/id m)))
                                  missions)
         g-evidence {:gate :evidence
                     :status (if (seq related-missions) :pass :blocked)
                     :data {:missions (mapv (fn [m]
                                             {:id (:mission/id m)
                                              :status (:mission/status m)
                                              :repo (:mission/repo m)
                                              :path (:mission/path m)})
                                           related-missions)}}

         ;; Gate 5: Reflection (attempt to resolve a related var)
         ;; Heuristic: look for vars in namespaces matching the devmap/component
         reflection-result
         (try
           (let [;; Try to find a namespace related to this devmap
                 dm-name (when dm-id (name dm-id))
                 comp-name (when comp-id (name comp-id))
                 ;; Search loaded namespaces for ones containing the devmap name
                 candidate-nses (when dm-name
                                  (->> (all-ns)
                                       (filter (fn [ns-obj]
                                                 (let [nsn (str (ns-name ns-obj))]
                                                   (or (str/includes? nsn (str/replace dm-name "-" "_"))
                                                       (str/includes? nsn (str/replace dm-name "-" "."))
                                                       (when comp-name
                                                         (str/includes? nsn (str/replace comp-name "-" "_")))))))
                                       (take 3)))
                 ;; Get public vars from first matching namespace
                 first-ns (first candidate-nses)
                 vars-sample (when first-ns
                               (->> (ns-publics first-ns)
                                    (take 5)
                                    (mapv (fn [[sym v]]
                                            (let [m (meta v)]
                                              {:var (str (ns-name first-ns) "/" sym)
                                               :file (:file m)
                                               :line (:line m)
                                               :arglists (str (:arglists m))})))))]
             {:ns (when first-ns (str (ns-name first-ns)))
              :candidate-namespaces (mapv #(str (ns-name %)) candidate-nses)
              :vars vars-sample})
           (catch Exception _ nil))

         g-reflection {:gate :reflection
                       :status (if (:ns reflection-result) :pass :blocked)
                       :data reflection-result}

         ;; Gate 6: Source (file + line from reflection)
         source-file (some :file (:vars reflection-result))
         source-line (some :line (:vars reflection-result))
         g-source {:gate :source
                   :status (if source-file :pass :blocked)
                   :data (when source-file
                           {:file source-file
                            :line source-line})}

         gates [g-devmap g-component g-coverage g-evidence g-reflection g-source]
         first-blocked (first (filter #(= :blocked (:status %)) gates))]

     {:tension tension
      :gates gates
      :complete? (nil? first-blocked)
      :blocked-at (:gate first-blocked)
      :gap-at (when (= :gap (:status g-coverage)) :coverage)})))

(defn trace-all-tensions
  "Trace all current tensions through the gate chain.
   Returns a summary with per-tension paths and aggregate stats."
  ([] (trace-all-tensions default-repo-roots))
  ([repos]
   (let [export (build-tension-export repos)
         review (build-portfolio-review repos)
         tensions (:tensions export)
         paths (mapv #(trace-tension-path % review) tensions)
         complete (filter :complete? paths)
         blocked (remove :complete? paths)
         blocked-at-freq (frequencies (keep :blocked-at blocked))
         gap-count (count (filter :gap-at paths))]
     {:paths paths
      :summary {:total (count paths)
                :complete (count complete)
                :blocked (count blocked)
                :gap-count gap-count
                :blocked-at blocked-at-freq}
      :detected-at (:detected-at export)})))

(defn trace-all-components
  "Trace every component in every devmap through the gate chain.
   Optional DM-FILTER narrows tracing to one devmap id (string or keyword)."
  ([] (trace-all-components nil default-repo-roots))
  ([dm-filter] (trace-all-components dm-filter default-repo-roots))
  ([dm-filter repos]
   (let [review (build-portfolio-review repos)
         devmaps (:devmaps review)
         dm-filter-str (some-> dm-filter str str/trim not-empty)
         selected-devmaps (if dm-filter-str
                            (filterv (fn [dm]
                                       (= dm-filter-str
                                          (some-> dm :devmap/id name)))
                                     devmaps)
                            devmaps)
         component-tensions (vec
                             (mapcat (fn [dm]
                                       (let [dm-id (:devmap/id dm)]
                                         (map (fn [component]
                                                {:tension/type :component-scan
                                                 :tension/source :component-scan
                                                 :tension/devmap-id dm-id
                                                 :tension/component-id (:component/id component)})
                                              (:devmap/components dm))))
                                     selected-devmaps))
         paths (mapv #(trace-tension-path % review) component-tensions)
         complete (filter :complete? paths)
         blocked (remove :complete? paths)
         blocked-at-freq (frequencies (keep :blocked-at blocked))
         gap-count (count (filter :gap-at paths))]
     {:paths paths
      :summary {:total (count paths)
                :complete (count complete)
                :blocked (count blocked)
                :gap-count gap-count
                :blocked-at blocked-at-freq
                :devmap-filter dm-filter-str}
      :detected-at (str (java.time.Instant/now))})))

;; =============================================================================
;; Backfill - legacy missions as evidence (D7)
;; =============================================================================

(defn mission->evidence
  "Convert a MissionEntry into an EvidenceEntry for backfill.
   Produces an observation about a mission's current state."
  [mission]
  (let [mid (:mission/id mission)
        src (name (:mission/source mission))
        now (str (java.time.Instant/now))]
    {:evidence/id (str "e-backfill-" mid "-" (or (:mission/repo mission) "unknown") "-" src)
     :evidence/subject {:ref/type :mission :ref/id mid}
     :evidence/type :coordination
     :evidence/claim-type :observation
     :evidence/author "mission-control/backfill"
     :evidence/at now
     :evidence/body (select-keys mission [:mission/id :mission/status
                                          :mission/source :mission/repo
                                          :mission/path :mission/date
                                          :mission/blocked-by
                                          :mission/raw-status
                                          :mission/devmap-id
                                          :mission/gates])
     :evidence/tags [:mission :backfill :snapshot]}))

(defn- file-sha256
  [path]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        bytes (java.nio.file.Files/readAllBytes (.toPath (io/file path)))]
    (.update md bytes)
    (apply str (map #(format "%02x" %) (.digest md)))))

(defn mission->sync-evidence
  "Convert a MissionEntry into a versioned EvidenceEntry suitable for
   push-based mission sync. Each distinct file content version gets a
   stable evidence id derived from the content hash, so repeated saves
   of unchanged content dedupe while real edits append a new snapshot."
  [mission]
  (let [mid (:mission/id mission)
        repo (or (:mission/repo mission) "unknown")
        src (name (:mission/source mission))
        path (:mission/path mission)
        sha (when (and path (.exists (io/file path)))
              (file-sha256 path))
        sha-short (subs (or sha "missing") 0 (min 12 (count (or sha "missing"))))
        now (str (java.time.Instant/now))]
    {:evidence/id (str "e-mission-sync-" mid "-" repo "-" sha-short)
     :evidence/subject {:ref/type :mission :ref/id mid}
     :evidence/type :coordination
     :evidence/claim-type :observation
     :evidence/author "mission-control/sync"
     :evidence/at now
     :evidence/body (cond-> (select-keys mission [:mission/id :mission/status
                                                  :mission/title
                                                  :mission/source :mission/repo
                                                  :mission/path
                                                  :mission/date
                                                  :mission/blocked-by
                                                  :mission/raw-status
                                                  :mission/devmap-id
                                                  :mission/gates])
                      true (assoc :event :mission-sync-snapshot)
                      sha (assoc :mission/content-sha sha))
     :evidence/tags [:mission :sync :snapshot]}))

(defn backfill-inventory
  "Convert a full mission inventory into backfill evidence entries.
   Returns a vector of EvidenceEntry maps ready for store/append*."
  ([] (backfill-inventory (build-inventory)))
  ([missions]
   (mapv mission->evidence missions)))

;; =============================================================================
;; Tickle — stall detection and agent paging
;; =============================================================================

(defn- stringify-instants
  "Convert Instant values to strings for JSON serialization."
  [activity-map]
  (into {}
        (map (fn [[agent-id info]]
               [agent-id (update info :last-active #(when % (str %)))]))
        activity-map))

(defn tickle-scan
  "Scan for stalled agents. Returns activity map and stalled list.
   Observation only — does not page or escalate."
  [evidence-store opts]
  (let [threshold (long (or (:threshold-seconds opts) 300))
        self-id (or (:self-id opts) "tickle-1")
        registry-snapshot (reg/registered-agents)
        activity (tickle/scan-activity evidence-store registry-snapshot
                                      threshold :self-id self-id)
        stalled (tickle/detect-stalls activity)]
    {:scan/activity (stringify-instants activity)
     :scan/stalled stalled
     :scan/agent-count (count activity)
     :scan/stall-count (count stalled)
     :scan/threshold-seconds threshold}))

(defn tickle-page
  "Page a specific agent via test bell. Returns page result."
  [evidence-store agent-id]
  (tickle/page-agent! (or agent-id "unknown")
                      {:evidence-store evidence-store}))

(defn tickle-cycle
  "Run a full scan → page → escalate cycle. Returns cycle result."
  [evidence-store opts]
  (tickle/run-scan-cycle!
   {:evidence-store evidence-store
    :threshold-seconds (or (:threshold-seconds opts) 300)
    :self-id (or (:self-id opts) "tickle-1")
    :page-config (or (:page-config opts) {})}))

;; =============================================================================
;; Portfolio diff — compare consecutive review snapshots
;; =============================================================================

(defn- compute-portfolio-diff
  "Compute diff between two portfolio snapshots (newest first).
   Each snapshot is an evidence entry with :evidence/body containing
   :portfolio/missions (vec of {:mission/id :mission/status})."
  [new-snapshot old-snapshot]
  (let [new-missions (:portfolio/missions (:evidence/body new-snapshot))
        old-missions (:portfolio/missions (:evidence/body old-snapshot))
        new-by-id (into {} (map (juxt :mission/id identity)) new-missions)
        old-by-id (into {} (map (juxt :mission/id identity)) old-missions)
        new-ids (set (keys new-by-id))
        old-ids (set (keys old-by-id))
        added (vec (for [id (sort (cset/difference new-ids old-ids))]
                     (get new-by-id id)))
        removed (vec (for [id (sort (cset/difference old-ids new-ids))]
                       (get old-by-id id)))
        changed (vec (for [id (sort (cset/intersection new-ids old-ids))
                           :let [new-status (:mission/status (get new-by-id id))
                                 old-status (:mission/status (get old-by-id id))]
                           :when (not= new-status old-status)]
                       {:mission/id id
                        :old-status old-status
                        :new-status new-status}))]
    {:added added
     :removed removed
     :changed changed
     :new-count (count new-missions)
     :old-count (count old-missions)
     :new-summary (:portfolio/summary (:evidence/body new-snapshot))
     :old-summary (:portfolio/summary (:evidence/body old-snapshot))
     :new-coverage (:portfolio/coverage (:evidence/body new-snapshot))
     :old-coverage (:portfolio/coverage (:evidence/body old-snapshot))}))

(defn portfolio-diff
  "Query last two portfolio review snapshots from evidence and compute diff.
   Returns {:ok true :diff {...}} or {:ok true :diff nil :message ...}."
  [evidence-store]
  (if-not evidence-store
    {:ok false :error "Evidence store not available"}
    (let [snapshots (estore/query* evidence-store
                                   {:query/subject {:ref/type :portfolio
                                                    :ref/id "global"}
                                    :query/type :coordination
                                    :query/limit 100})
          snapshots (->> snapshots
                         (filter (fn [e]
                                   (let [tags (set (:evidence/tags e))]
                                     (and (contains? tags :review)
                                          (contains? tags :portfolio-snapshot)))))
                         (take 2)
                         vec)]
      (if (< (count snapshots) 2)
        {:ok true
         :diff nil
         :message "Not enough review history — run mc-review at least twice"}
        {:ok true
         :diff (compute-portfolio-diff (first snapshots) (second snapshots))}))))
