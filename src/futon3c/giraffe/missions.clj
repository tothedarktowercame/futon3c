(ns futon3c.giraffe.missions
  "M-giraffe stage 1, packet 6: the mission-docs source adapter.

  Follows the tickets adapter (packet 2) and the mission-history
  discovery (packet 5).  M-giraffe §\"Stage 1, mission-docs source\"
  (2026-10-28) decided the retrospective cone test cannot run on git
  history (the 2026-09-30 bulk closure sweep), so the test runs
  forward; this adapter is the mission-docs source for it.

  Items: every holes/missions/M-*.md under the given repo roots (the
  canonical checkouts of packet 5; worktree copies excluded by which
  roots the caller passes).  id = file name without .md; if two repos
  have the same id both rows are rejected (:item/duplicate-id) — one
  is never picked.  :repo and :file (workspace-relative, e.g.
  \"futon2/holes/missions/M-x.md\") stay on the item;
  :source-order is repo name then file name.

  Status -> state group: the packet-5 rules plus the mapping fixed in
  M-giraffe §\"Status mapping for mission docs\" (Ready/Blocked
  on/:scoped/... -> unstarted; HEAD/:review/PARTIAL/... -> started;
  DELIVERED/ANSWERED/IMPLEMENTED/ACCEPTED -> completed).  Rules are
  matched against the status text's leading phrase (cut at the first
  em-dash, semicolon, parenthesis or sentence end) so prose after the
  marker — e.g. \"OPEN — ... not done in stage 1 ...\" — cannot flip
  the group.  Anything unmatched stays unmapped: :state-group
  :unstarted plus :status-unmapped, listed, never guessed.

  Edges: the same JSON shape as holes/tickets/giraffe-edges.json, but
  \"file\" is a workspace-relative path so a quote can be checked in
  any repo.  The quote check is the tickets adapter's (reused, not
  copied).  A target that is a mission id (M-...) resolves to an item;
  T- ticket ids and anything else are external needs.

  Missing-column invariant (M-giraffe §DERIVE): an edge whose quoted
  line sits inside a markdown table whose header row contains
  \"Missing\" is rejected (:edge/own-missing-column) — the mission's
  own work, not a dependency.  The table is detected from the cited
  file, never from the edges file's say-so."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.giraffe.tickets :as gt]))

(def default-repos
  "The canonical checkouts scanned in packet 5 (futon1/1a/1b exist but
  hold no mission docs; kept so the list matches the discovery)."
  (vec (keep #(let [d (io/file "/home/joe/code" %)]
               (when (.isDirectory d) (str d)))
             ["futon0" "futon1" "futon1a" "futon1b" "futon2" "futon3"
              "futon3a" "futon3b" "futon3c" "futon4" "futon5" "futon5a"
              "futon6" "futon7"])))

;; ------------------------------------------------------- status mapping

(def ^:private rule-table
  "Ordered [group regex] pairs over the status text's leading phrase.
  SPECIFIED/STUB/NOT-YET-IMPLEMENTED come before IMPLEMENTED because
  \"SPECIFIED, NOT YET IMPLEMENTED\" contains IMPLEMENTED."
  [;; a phrase that BEGINS with a phase name or OPEN is an open mission,
   ;; whatever it says next ("HEAD and IDENTIFY accepted", "INSTANTIATE
   ;; complete for the stash slice") — claude-13 review, 2026-10-28
   [:started #"(?i)^[*_`\s]*(head|identify|map|derive|argue|verify|instantiate|open)\b"]
   [:unstarted #"(?i)not[ -]yet[ -]implemented|specified|stub"]
   [:completed #"(?i)\bclosed\b|\bdone\b|\bcomplete\b|\bdelivered\b|\banswered\b|\bimplemented\b|\baccepted\b"]
   [:cancelled #"(?i)superseded|nonstarter|dropped|abandoned"]
   [:unstarted #"(?i)\bready\b|blocked[ -]on|\bproposed\b|\bdraft\b|not[ -]started|\bplanned\b|\bscoped\b|greenfield|\bdesign\b|\bchartered\b"]
   [:started #"(?i)\bidentify(?:ing)?\b|\bmap(?:ping)?\b|\bderive(?:d|ing)?\b|\bargu(?:e|ing|ment)\b|\bverif(?:y|ying|ication)\b|\binstantiate\b|\bdocument(?:ation|ing)?\b|\bopen\b|\bactive\b|in[ -]progress|\bpartial\b|\bpart\b|\bdispatched\b|\bhead\b|\breview\b|\btesting\b|\bconditional\b|\bslice\b|\bpart\b"]])

(defn- leading-phrase
  "The status text up to its first em-dash, middle dot, semicolon,
  parenthesis or sentence end, with a leading ':' stripped (:scoped, :review)."
  [s]
  (-> s
      (str/trim)
      (str/replace #"^\:+" "")
      (str/replace #"\s+" " ")
      (as-> t (str/replace t #"( — .*|—.*| · .*|·.*|;.*|\(.*|\. .*$|\.$)" ""))
      (str/trim)))

(defn status->group
  "Group for a declared status text, or nil when unmapped.  Public so
  the status-mapping report and tests can see the same rules."
  [^String text]
  (when (and text (not (str/blank? text)))
    (let [phrase (leading-phrase text)]
      (some (fn [[group re]] (when (re-find re phrase) group))
            rule-table))))

;; ------------------------------------------------------------- items

(defn- repo-name [root] (.getName (io/file root)))

(defn- mission-files
  "Sorted M-*.md paths under one repo's holes/missions."
  [root]
  (let [dir (io/file root "holes/missions")]
    (when (.isDirectory dir)
      (->> (file-seq dir)
           (filter #(.isFile ^java.io.File %))
           (map #(str %))
           (filter #(re-find #"/M-[^/]+\.md$" %))
           sort))))

(defn- declared-status
  "Raw text of the first '**Status:**' line, or nil."
  [path]
  (some #(when-let [m (re-matches #"\*\*Status:\*\*(.*)" (str/trim %))]
           (str/trim (second m)))
        (str/split-lines (slurp path))))

(defn- mission-items
  "One row per mission doc across all repos, sorted by repo name then
  file name; :source-order is that order."
  [repos]
  (let [rows (sort-by (juxt :repo :file)
                      (mapcat (fn [root]
                                (let [repo (repo-name root)]
                                  (map (fn [path]
                                         (let [f (.getName (io/file path))]
                                           {:repo repo
                                            :file (str repo "/holes/missions/" f)
                                            :path path
                                            :id (keyword (str/replace f #"\.md$" ""))}))
                                       (mission-files root))))
                              repos))]
    (vec (map-indexed
          (fn [i {:keys [repo file path id]}]
            (let [status (declared-status path)
                  group (status->group status)]
              (cond-> {:id id
                       :state-group (or group :unstarted)
                       :source-order i
                       :repo repo
                       :file file}
                status (assoc :declared-status status)
                (nil? group) (assoc :status-unmapped (or status :status/missing)))))
          rows))))
(defn- repo-roots-by-name [repos]
  (into {} (map (fn [root] [(repo-name root) root]) repos)))

(defn- read-lines-in-repo
  "Lines of a workspace-relative file (\"<repo>/rest/of/path\"), or nil
  when the repo is unknown or the file is absent."
  [repos-by-name file]
  (when (string? file)
    (let [idx (str/index-of file "/")]
      (when (pos? idx)
        (let [root (get repos-by-name (subs file 0 idx))]
          (when root
            (let [f (io/file root (subs file (inc idx)))]
              (when (.isFile f)
                (str/split-lines (slurp f))))))))))

(defn- table-header-at
  "Header row string when the 1-based line sits inside a contiguous
  markdown table block (lines starting with '|'), else nil."
  [lines n]
  (when (and (vector? lines) (pos-int? n) (<= n (count lines)))
    (let [row? (fn [s] (str/starts-with? (str/triml s) "|"))]
      (when (row? (nth lines (dec n)))
        (loop [i (dec n)]
          (if (and (pos? i) (row? (nth lines (dec i))))
            (recur (dec i))
            (nth lines i)))))))

(defn- own-missing-column?
  "The cited line sits in a table whose header row contains 'Missing'."
  [lines line]
  (when-let [header (table-header-at lines line)]
    (str/includes? (str/lower-case header) "missing")))

(defn- mission-id? [to]
  (and (string? to) (str/starts-with? to "M-")))

(defn- edge-class [e]
  (if (contains? e :class)
    (keyword (str/replace (str (:class e)) #"^:" ""))
    :closure))

(defn- edge->pass-edge
  "Convert one JSON edge against the item id set.  Returns {:edge ...}
  or {:reject ...}.  The quote check and its window are the tickets
  adapter's, reused."
  [repos-by-name item-ids e]
  (let [from (keyword (:from e))
        to (:to e)
        file (:file e) line (:line e) quote (:quote e)]
    (cond
      (not= "needs" (:kind e))
      {:reject {:reject :edge/precedes-not-in-stage-1 :edge e}}

      (not (contains? item-ids from))
      {:reject {:reject :edge/dangling-source :edge e}}

      (not (#{:closure :artifact :alternative} (edge-class e)))
      {:reject {:reject :edge/unknown-class :edge e}}

      :else
      (let [lines (read-lines-in-repo repos-by-name file)]
        (cond
          (nil? lines)
          {:reject {:reject :edge/file-not-resolvable :edge e}}

          (not (gt/quote-at-line? lines line quote))
          {:reject {:reject :edge/quote-not-at-line :edge e}}

          (own-missing-column? lines line)
          {:reject {:reject :edge/own-missing-column :edge e}}

          (mission-id? to)
          (let [to-id (keyword to)]
            (if (contains? item-ids to-id)
              {:edge {:from from :to to-id
                      :kind :item :class (edge-class e)
                      :source :prose-reading
                      :quote quote :file file :line line
                      :note (:note e)}}
              {:reject {:reject :edge/dangling-target :edge e}}))

          :else
          {:edge {:from from :to (gt/external-key to)
                  :kind :external :class (edge-class e)
                  :source :prose-reading :met false
                  :external-target (str to)
                  :quote quote :file file :line line
                  :note (:note e)}})))))

;; ------------------------------------------------------------- entry

(defn missions->input
  "Read every mission doc under :repos (default the canonical
  checkouts of packet 5) plus an edges file; return
  {:input <pass input map> :rejected [...] :status-mapping [...]
  :unmapped-statuses [...]} — the same shape as tickets->input.

  :only, when supplied, restricts items to those mission ids (strings
  or keywords, with or without .md); edges into or out of the kept set
  are then dangling."
  [{:keys [repos edges-file only]}]
  (let [repos (or (seq repos) default-repos)
        rows (mission-items repos)
        only-ids (when only
                   (set (map (fn [x] (keyword (str/replace (name (keyword x)) #"\.md$" ""))) only)))
        kept (if only-ids
               (vec (filter #(contains? only-ids (:id %)) rows))
               rows)
        id-counts (frequencies (map :id kept))
        dup-ids (set (for [[id n] id-counts :when (> n 1)] id))
        ;; a doc with no Status line is not counted as an open mission
        ;; (claude-13, 2026-10-28): it is rejected, typed, and listed
        no-status? #(= :status/missing (:status-unmapped %))
        items (vec (map #(dissoc % :path)
                        (remove #(or (contains? dup-ids (:id %)) (no-status? %)) kept)))
        rejects (vec (for [row (sort-by (juxt :repo :file) kept)
                           :when (contains? dup-ids (:id row))]
                       {:reject :item/duplicate-id
                        :id (:id row) :repo (:repo row) :file (:file row)}))
        rejects (into rejects
                      (for [row (sort-by (juxt :repo :file) kept)
                            :when (and (no-status? row)
                                       (not (contains? dup-ids (:id row))))]
                        {:reject :item/no-status-line
                         :id (:id row) :repo (:repo row) :file (:file row)}))
        item-ids (set (map :id items))
        repos-by-name (repo-roots-by-name repos)
        edges-json (json/parse-string (slurp (str edges-file)) keyword)
        converted (map (partial edge->pass-edge repos-by-name item-ids)
                       (:edges edges-json))
        edges (vec (keep :edge converted))
        edge-rejects (vec (keep :reject converted))
        carried (mapv (fn [e] (assoc e :reject :edge/rejected-at-reading))
                      (:rejected edges-json))
        unmapped (vec (for [it items :when (:status-unmapped it)]
                        {:id (:id it) :status-unmapped (:status-unmapped it)}))
        item-statuses (distinct (keep :declared-status items))
        status-mapping (vec (for [s item-statuses]
                              {:status s
                               :group (status->group s)
                               :mapped? (some? (status->group s))}))]
    {:input {:as-of (str (java.time.LocalDate/now))
             :items items
             :edges edges}
     :rejected (vec (concat rejects edge-rejects carried))
     :status-mapping status-mapping
     :unmapped-statuses unmapped}))
