(ns futon3c.giraffe.missions
  "Mission graph adapter. Population, path/status grammar and liveness come
  from mission-registry's shared file reader. Closed/inactive items remain
  graph nodes; their exclusion receipts are separate from the open pass.
  Legacy status->group is used only to rank live work as started/unstarted.
  Cross-type graph admission remains outside this mission-only adapter."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.giraffe.tickets :as gt]
            [futon2.aif.mission-registry :as registry]))

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

(defn- mission-items [repos]
  (let [rows (:missions (registry/load-missions-from-repos repos))
        roots (mapv #(vector (.toPath (.getAbsoluteFile (io/file %)))
                             (repo-name %)) repos)
        rows (map (fn [m]
                    (let [path (.toPath (io/file (:path m)))
                          [root repo] (first (filter #(.startsWith path (first %)) roots))]
                      (assoc m :repo repo :file (str repo "/" (.relativize root path))))) rows)]
    (mapv (fn [i m]
            (let [live? (registry/live-mission? m)
                  rank (status->group (:status-line m))]
              (cond-> {:id (keyword (:id m)) :repo (:repo m) :file (:file m)
                       :source-order i :source-pin (:source m)
                       :registry-status (:status-class m)
                       :state-group (if live?
                                      (if (= :started rank) :started :unstarted)
                                      (if (= :complete (:status-class m)) :completed :cancelled))}
                (:status-line m) (assoc :declared-status (:status-line m))
                (not live?) (assoc :exclusion {:id (:id m) :reason (:status-class m)
                                             :status-line (:status-line m)
                                             :source (:source m)
                                             :operator-gate-lines (:operator-gate-lines m)})
                (= :unknown (:status-class m)) (assoc :status-unmapped (or (:status-line m) :status/missing)))))
          (range) (sort-by (juxt :repo :file) rows))))

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

(defn- task-id? [to]
  (and (string? to) (boolean (re-find #"^[MET]-" to))))

(defn- edge-class [e]
  (if (contains? e :class)
    (keyword (str/replace (str (:class e)) #"^:" ""))
    :closure))

(defn edge->pass-edge
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

          (task-id? to)
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
        items (mapv #(dissoc % :exclusion) kept)
        exclusions (vec (keep :exclusion kept))
        rejects []
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
        status-mapping (vec (distinct (map (fn [it]
                                             {:status (:declared-status it)
                                              :registry-status (:registry-status it)
                                              :group (:state-group it)
                                              :mapped? (not= :unknown (:registry-status it))}) items)))]
    {:input {:as-of (str (java.time.LocalDate/now))
             :items items
             :edges edges}
     :rejected (vec (concat rejects edge-rejects carried))
     :excluded exclusions
     :status-mapping status-mapping
     :unmapped-statuses unmapped}))
