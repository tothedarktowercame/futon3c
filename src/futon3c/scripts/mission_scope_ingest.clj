(ns futon3c.scripts.mission-scope-ingest
  "Ingest mission scope-tree JSON into futon1a as scope entities and hyperedges."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import (java.net URI URLEncoder)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.security MessageDigest)))

(def ^:private default-scope-dir "/home/joe/code/futon6/data/mission-scope-trees")
(def ^:private default-futon1a-url "http://localhost:7071")
(def ^:private default-penholder "api")
(def ^:private code-root "/home/joe/code")
(def ^:private canonical-phases
  #{"head" "identify" "map" "derive" "argue" "verify" "instantiate" "document"})
(def ^:private structural-binders
  ["eightfold-phase" "loose-section" "capability-scope" "map-item"
   "relates-to" "source-material" "mission-scope-in" "mission-scope-out"
   "pattern" "psr" "pur" "plain-argument" "verify-gate" "certificate"])
(def ^:private pattern-library-limit 5000)
(def ^:private !pattern-library-cache (atom nil))

(defn- sha1 [s]
  (let [digest (.digest (MessageDigest/getInstance "SHA-1") (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- slug [s]
  (let [base (-> (str s)
                 str/lower-case
                 (str/replace #"[^a-z0-9]+" "-")
                 (str/replace #"^-+|-+$" ""))]
    (or (not-empty base) (subs (sha1 s) 0 12))))

(defn- url-encode [s]
  (URLEncoder/encode (str s) "UTF-8"))

(defn- http-client [] (HttpClient/newHttpClient))

(defn- http-edn
  ([client method url] (http-edn client method url nil))
  ([client method url body]
   (let [builder (HttpRequest/newBuilder (URI/create url))
         builder (case method
                   :get (.GET builder)
                   :post (.POST builder (HttpRequest$BodyPublishers/ofString (pr-str body))))
         req (-> builder
                 (.header "accept" "application/edn")
                 (.header "content-type" "application/edn")
                 (.build))
         resp (.send client req (HttpResponse$BodyHandlers/ofString))
         body-text (.body resp)]
     {:status (.statusCode resp)
      :body (when (seq body-text) (edn/read-string body-text))})))

(defn- ok!
  [resp context]
  (when-not (<= 200 (:status resp) 299)
    (throw (ex-info "futon1a request failed" (assoc context :response resp))))
  resp)

(defn- get-entity [client base-url id-or-name]
  (let [resp (http-edn client :get
                       (str base-url "/api/alpha/entity/" (url-encode id-or-name)))]
    (when (<= 200 (:status resp) 299)
      (get-in resp [:body :entity]))))

(defn- post-entity! [client base-url penholder payload]
  (-> (http-edn client :post (str base-url "/api/alpha/entity")
                (assoc payload :penholder penholder))
      (ok! {:op :entity :payload payload})
      (get-in [:body :entity])))

(defn- ensure-entity!
  [client base-url penholder {:keys [id name type external-id source props]}]
  (let [existing (get-entity client base-url id)
        merged-props (merge (or (:props existing) {}) (or props {}))]
    (post-entity! client base-url penholder
                  (cond-> {:id id :name name :type type}
                    external-id (assoc :external-id external-id)
                    source (assoc :source source)
                    (seq merged-props) (assoc :props merged-props)))))

(defn- post-hyperedge! [client base-url penholder payload]
  (-> (http-edn client :post (str base-url "/api/alpha/hyperedge")
                (assoc payload :penholder penholder))
      (ok! {:op :hyperedge :payload payload})
      (get-in [:body :hyperedge])))

(defn- write-tx! [client base-url penholder tx-ops claim]
  (-> (http-edn client :post (str base-url "/write")
                {:penholder penholder
                 :model {}
                 :identity nil
                 :tx-ops tx-ops
                 :counter-ratchet {:allow-drop-classes #{:entity}}
                 :claim claim})
      (ok! {:op :write :tx-ops tx-ops})
      :body))

(defn- delete-docs! [client base-url penholder ids]
  (let [ids (->> ids (remove str/blank?) distinct vec)]
    (when (seq ids)
      (write-tx! client base-url penholder
                 (mapv (fn [id] [:xtdb.api/delete id]) ids)
                 {:op :mission-scope/retract-legacy-position-anchors
                  :doc-count (count ids)}))
    {:deleted-count (count ids)}))

(def ^:private !hyperedge-type-cache (atom {}))

(defn- reset-run-caches!
  "Clear the process-global query caches at the start of each ingest run. These
   atoms persist across in-process `-main` invocations in the one serving JVM, so
   without this a reingest serves STALE hyperedge / pattern-library reads from a
   prior run — exactly the data a reingest exists to refresh. Within a single run
   the caches still coalesce repeated reads."
  []
  (reset! !hyperedge-type-cache {})
  (reset! !pattern-library-cache nil))

(defn- hyperedges-by-type [client base-url hx-type]
  (if-let [cached (get @!hyperedge-type-cache hx-type)]
    cached
    (let [hxs (-> (http-edn client :get
                            (str base-url "/api/alpha/hyperedges?type="
                                 (url-encode hx-type)
                                 "&limit=5000"))
                  (ok! {:op :hyperedges-by-type :type hx-type})
                  (get-in [:body :hyperedges])
                  (or []))]
      (swap! !hyperedge-type-cache assoc hx-type hxs)
      hxs)))

(defn- repo-name-from-path [path]
  (or (second (re-find #"/code/([^/]+)/" (str path)))
      (second (re-find #"^([^/]+)/" (str path)))))

(defn- mission-doc-id [mission path]
  (let [repo (or (repo-name-from-path path) "mission")
        repo-key (str repo "-d")
        name (str/replace-first (str mission) #"^M-" "")]
    (str repo-key "/mission/" (slug name))))

(defn- env-name [scope]
  (or (some #(when (= "environment" (:role %)) (:name %)) (:ends scope))
      (some #(when (= "heading" (:role %)) (:title %)) (:ends scope))
      (:scope-id scope)))

(defn- heading-title [scope]
  (or (some #(when (= "heading" (:role %)) (:title %)) (:ends scope))
      (some #(when (= "environment" (:role %)) (:name %)) (:ends scope))
      (:scope-id scope)))

(defn- map-item-title [scope]
  (or (some #(when (= "map-item" (:role %)) (:title %)) (:ends scope))
      (get-in scope [:hx/content :match])
      (heading-title scope)))

(defn- target-mission-ident [scope]
  (:target-mission-ident scope))

(defn- target-source-ref [scope]
  (:target-source-ref scope))

(defn- target-pattern-ident [scope]
  (:target-pattern-ident scope))

(defn- boundary-item-text [scope]
  (:boundary-item-text scope))

(defn- anchor-text [scope]
  (case (:binder-type scope)
    "map-item" (map-item-title scope)
    "relates-to" (or (target-mission-ident scope) (heading-title scope))
    "source-material" (or (target-source-ref scope) (heading-title scope))
    ("pattern" "psr" "pur") (or (target-pattern-ident scope) (heading-title scope))
    "mission-scope-in" (or (boundary-item-text scope) (heading-title scope))
    "mission-scope-out" (or (boundary-item-text scope) (heading-title scope))
    (heading-title scope)))

(defn- env-phase [scope]
  (some #(when (= "environment" (:role %)) (:phase %)) (:ends scope)))

(defn- canonical-phase [scope]
  (let [phase (some-> (env-phase scope) str/lower-case)]
    (if (contains? canonical-phases phase)
      phase
      (let [heading (-> (heading-title scope) str/lower-case)
            hit (some #(when (re-find (re-pattern (str "\\b" % "\\b")) heading) %)
                      canonical-phases)]
        (or hit "head")))))

(defn- end-display [end]
  (or (:term end) (:ident end) (:text end) (:title end) (:ref end) (:name end) (:latex end)))

(defn- long-id [prefix value]
  (str prefix "/" (slug value) "-" (subs (sha1 value) 0 10)))

(defn- slot-entity-spec [mission _path end]
  (let [role (:role end)
        value (end-display end)]
    (case role
      "concept" {:id (str "scope/concept/" (slug value))
                 :name value
                 :type "concept"
                 :props {:scope/role role}}
      "capability" {:id (str "scope/capability/" (slug value))
                    :name value
                    :type "capability"
                    :props {:scope/role role
                            :scope/source (:source end)}}
      "bounded-item" {:id (long-id "scope/bounded-item" value)
                      :name value
                      :type "bounded-item"
                      :props {:scope/role role
                              :mission mission}}
      "map-item" {:id (long-id "scope/map-item" value)
                  :name value
                  :type "map-item"
                  :props {:scope/role role
                          :mission mission}}
      "source" {:id (long-id "scope/source" value)
                :name value
                :type (str "source/" (or (:kind end) "material"))
                :props {:scope/role role
                        :source/kind (:kind end)
                        :source/ref (:ref end)}}
      ("pattern" "psr" "pur") nil
      "mission" nil
      nil)))

(defn- slot-entity-spec* [client base-url mission path end]
  (if (= "mission" (:role end))
    (let [value (end-display end)
          existing (or (get-entity client base-url value)
                       (get-entity client base-url (str "mission|" value)))]
      {:id (or (:id existing) (str "mission-doc/" (slug value)))
       :name (or (:name existing) value)
       :type (or (:type existing) "mission/doc")
       :external-id (:ident end)
       :props {:scope/role (:role end)
               :mission/relation (:relation end)}})
    (slot-entity-spec mission path end)))

(defn- resolve-target-mission [client base-url ident]
  (when (seq (str ident))
    (or (get-entity client base-url ident)
        (get-entity client base-url (str "mission|" ident)))))

(defn- endpoint-exists?
  "True when `end-id` already appears as a hyperedge ENDPOINT in substrate-2,
   even when no entity *document* carries that id. The multi-watcher reflects
   files as edge endpoints (`<repo>-d/file/<path>`), not entity docs, so a file
   can be fully present yet invisible to `get-entity`. BOUNDED point lookup
   (`?end=` + `limit`), never the full-type scan — which can wedge the serving
   JVM (see README-multi-watcher.md)."
  [client base-url end-id]
  (let [resp (http-edn client :get
                       (str base-url "/api/alpha/hyperedges?end="
                            (url-encode end-id) "&limit=1"))]
    (boolean (and (<= 200 (:status resp) 299)
                  (seq (get-in resp [:body :hyperedges]))))))

(defn- resolve-source-file
  "Resolve a source-material file ref to a substrate-2 target. Tries entity
   lookup first (raw ref, then canonical `<repo>-d/file/<path>`); if no entity
   doc exists, falls back to the watcher's ENDPOINT representation so
   endpoint-only files link instead of falsely reporting :detached. Never
   fabricates a node — only returns a target that already exists in the store."
  [client base-url source-ref]
  (when (seq (str source-ref))
    (or (get-entity client base-url source-ref)
        (let [[repo rest-path] (rest (re-matches #"([^/]+)/(.+)" (str source-ref)))]
          (when (and repo rest-path)
            (let [canonical (str repo "-d/file/" rest-path)]
              (or (get-entity client base-url canonical)
                  (when (endpoint-exists? client base-url canonical)
                    {:id canonical
                     :name rest-path
                     :type "source/file"
                     :endpoint-only? true}))))))))

(defn- flexiarg-endpoint [pattern-ref]
  (when-let [[repo rest-path] (rest (re-matches #"([^/]+)/(.+\.flexiarg)" (str pattern-ref)))]
    (str repo "-d/file/" rest-path)))

(defn- pattern-library-key [pattern-ref]
  (when-let [[_ library-path] (re-matches #"[^/]+/library/(.+)\.flexiarg" (str pattern-ref))]
    library-path))

(defn- pattern-library-entities [client base-url]
  (or @!pattern-library-cache
      (let [resp (http-edn client :get
                           (str base-url
                                "/api/alpha/entities/latest?type=pattern%2Flibrary&limit="
                                pattern-library-limit))
            entities (if (<= 200 (:status resp) 299)
                       (or (get-in resp [:body :entities]) [])
                       [])]
        (reset! !pattern-library-cache entities)
        entities)))

(defn- resolve-pattern-library-entity [client base-url pattern-ident pattern-ref]
  (let [key (pattern-library-key pattern-ref)
        candidates (cond-> #{pattern-ident}
                     key (conj key))]
    (some (fn [entity]
            (when (or (contains? candidates (:external-id entity))
                      (contains? candidates (:name entity)))
              entity))
          (pattern-library-entities client base-url))))

(defn- resolve-pattern-node
  "Resolve a cited flexiarg pattern to an existing substrate-2 node. The
   detector passes the literal basename plus its library/*.flexiarg repo path.
   The live store may represent the pattern as a pattern/library entity or as
   a file endpoint, not an entity doc. Never fabricates: endpoint fallback
   succeeds only when `?end=<candidate>&limit=1` finds substrate evidence."
  [client base-url pattern-ident pattern-ref]
  (when (seq (str pattern-ident))
    (let [endpoint (flexiarg-endpoint pattern-ref)]
      (or (get-entity client base-url pattern-ident)
          (resolve-pattern-library-entity client base-url pattern-ident pattern-ref)
          (when (seq (str pattern-ref))
            (get-entity client base-url pattern-ref))
          (when endpoint
            (or (get-entity client base-url endpoint)
                (when (endpoint-exists? client base-url endpoint)
                  {:id endpoint
                   :name pattern-ident
                   :type "pattern/flexiarg"
                   :endpoint-only? true})))))))

(defn- filler-ends [ends]
  (remove #(contains? #{"entity" "environment" "heading"} (:role %)) ends))

(defn- mission-file [mission-path]
  (let [f (io/file mission-path)]
    (if (.isAbsolute f)
      f
      (io/file code-root mission-path))))

(defn- markdown-heading-text [line]
  (some-> line
          (str/replace #"^\s*#+\s*" "")
          (str/replace #"\s*#+\s*$" "")
          str/trim))

(defn- heading-passage [mission-path heading]
  (let [f (mission-file mission-path)]
    (try
      (when (.exists f)
        (let [lines (str/split-lines (slurp f))]
          (or (some (fn [line]
                      (when (= heading (markdown-heading-text line))
                        (str/trim line)))
                    lines)
              (some (fn [line]
                      (when (str/includes? line heading)
                        (str/trim line)))
                    lines))))
      (catch Exception _ nil))))

(defn- list-item-line? [line]
  (boolean (re-find #"^\s*(?:[-*+]|\d+[.)])\s+" line)))

(defn- list-item-passage-info [mission-path item-text]
  (let [f (mission-file mission-path)
        item-text (str/trim (str item-text))]
    (try
      (when (and (.exists f) (seq item-text))
        (let [lines (str/split-lines (slurp f))]
          (or (some (fn [line]
                      (when (and (list-item-line? line)
                                 (str/includes? line item-text))
                        {:passage (str/trim line)
                         :source :list-item}))
                    lines)
              (some (fn [line]
                      (when (str/includes? line item-text)
                        {:passage (str/trim line)
                         :source :contains-line}))
                    lines))))
      (catch Exception _ nil))))

(defn- child-map [scopes]
  (reduce (fn [m scope]
            (if-let [parent (:parent scope)]
              (update m parent (fnil conj []) (:scope-id scope))
              m))
          {}
          scopes))

(defn- descendant-ids [children sid]
  (letfn [(walk [id]
            (cons id (mapcat walk (get children id))))]
    (rest (walk sid))))

(defn- scope-by-id [scopes]
  (into {} (map (juxt :scope-id identity) scopes)))

(defn- bound-concept-set [scopes scope]
  (let [children (child-map scopes)
        by-id (scope-by-id scopes)
        ids (cons (:scope-id scope) (descendant-ids children (:scope-id scope)))]
    (->> ids
         (keep by-id)
         (mapcat :ends)
         filler-ends
         (keep end-display)
         (map str/trim)
         (remove str/blank?)
         distinct
         sort
         vec)))

(defn- content-position-passage
  "Passage from the detector's own `hx/content :position` — the line at that
   byte offset in the mission file. The detector already localised the scope;
   re-deriving the anchor from title text (`heading-passage`) is what mis-anchors
   phases onto the Status line (it names every phase, and first-occurrence wins)."
  [mission-path scope]
  (when-let [pos (get-in scope [:hx/content :position])]
    (let [f (mission-file mission-path)]
      (try
        (when (.exists f)
          (let [text (slurp f)
                pos (max 0 (min (long pos) (dec (count text))))
                bol (inc (long (or (str/last-index-of text "\n" pos) -1)))
                eol (long (or (str/index-of text "\n" pos) (count text)))
                line (str/trim (subs text bol (max bol eol)))]
            (when (seq line)
              {:passage line :source :content-position})))
        (catch Exception _ nil)))))

(defn- anchor-for-scope [mission-path scopes scope]
  (let [heading (anchor-text scope)
        passage-info (case (:binder-type scope)
                       "map-item" (list-item-passage-info mission-path heading)
                       "relates-to" (list-item-passage-info mission-path heading)
                       "source-material" (list-item-passage-info mission-path heading)
                       "mission-scope-in" (list-item-passage-info mission-path heading)
                       "mission-scope-out" (list-item-passage-info mission-path heading)
                       ;; PSR/PUR records anchor on their own record line (the
                       ;; detector's `anchor-line`), not on a heading — the
                       ;; `Pattern chosen:` / `Pattern:` line IS the anchor.
                       ("psr" "pur")
                       (when-let [line (some-> (:anchor-line scope) str/trim not-empty)]
                         {:passage line
                          :source :record-line})
                       (or (content-position-passage mission-path scope)
                           (when-let [passage (heading-passage mission-path heading)]
                             {:passage passage
                              :source :heading})))
        passage (:passage passage-info)
        concepts (bound-concept-set scopes scope)
        basis (pr-str {:heading (or passage heading)
                       :concepts concepts})
        fingerprint (subs (sha1 basis) 0 16)]
    {:state (if passage :anchored :detached)
     :passage (or passage heading)
     :heading heading
     :fingerprint fingerprint
     :concepts concepts
     :source (:source passage-info)
     :resolve-by :verbatim-search
     :detached-reason (when-not passage
                        (case (:binder-type scope)
                          "map-item" :list-item-passage-not-found
                          "relates-to" :reference-passage-not-found
                          "source-material" :source-passage-not-found
                          ("pattern" "psr" "pur") :pattern-citation-passage-not-found
                          "mission-scope-in" :scope-boundary-passage-not-found
                          "mission-scope-out" :scope-boundary-passage-not-found
                          :heading-passage-not-found))}))

(defn- truncated-slug [s]
  (let [base (slug s)]
    (if (> (count base) 72)
      (str (subs base 0 63) "-" (subs (sha1 base) 0 8))
      base)))

(defn- canonical-eightfold-scope-id [mission phase]
  (let [stem (str/replace-first (str mission) #"^M-" "")
        base (str stem "/" phase)]
    base))

(defn- stable-eightfold-scope-id [mission phase heading duplicate?]
  (let [base (canonical-eightfold-scope-id mission phase)]
    (if duplicate?
      (str base "--" (slug heading) "-" (subs (sha1 heading) 0 8))
      base)))

(defn- stable-eightfold-scopes [mission mission-path all-scopes scopes]
  (let [phase-counts (frequencies (map canonical-phase scopes))]
    (mapv (fn [scope]
            (let [phase (canonical-phase scope)
                  heading (heading-title scope)
                  duplicate? (> (get phase-counts phase 0) 1)
                  stable-id (stable-eightfold-scope-id mission phase heading duplicate?)
                  canonical-id (canonical-eightfold-scope-id mission phase)
                  anchor (anchor-for-scope mission-path all-scopes scope)]
              (assoc scope
                     :original-scope-id (:scope-id scope)
                     :scope-id stable-id
                     :stable-scope-id stable-id
                     :canonical-scope-id canonical-id
                     :canonical-phase phase
                     :duplicate-phase? duplicate?
                     :anchor anchor
                     :parent nil)))
          scopes)))

(defn- stable-heading-scope-id [mission heading original-id duplicate?]
  (let [stem (str/replace-first (str mission) #"^M-" "")
        base (str stem "/" (slug heading))]
    (if duplicate?
      (str base "--" (subs (sha1 (str heading "|" original-id)) 0 8))
      base)))

(defn- stable-item-scope-id [mission text original-id duplicate?]
  (let [stem (str/replace-first (str mission) #"^M-" "")
        base (str stem "/" (truncated-slug text))]
    (if duplicate?
      (str base "--" (subs (sha1 (str text "|" original-id)) 0 8))
      base)))

(defn- mission-stem [mission-ident]
  (-> (str mission-ident)
      (str/replace-first #"^M-" "")
      slug))

(defn- target-mission-ends [scope]
  (filter #(and (= "mission" (:role %))
                (seq (:ident %)))
          (:ends scope)))

(defn- target-source-ends [scope]
  (filter #(and (= "source" (:role %))
                (seq (:ref %)))
          (:ends scope)))

(defn- target-pattern-ends [scope]
  (filter #(and (= "pattern" (:role %))
                (seq (:ident %)))
          (:ends scope)))

(defn- bounded-item-ends [scope]
  (filter #(and (= "bounded-item" (:role %))
                (seq (:text %)))
          (:ends scope)))

(defn- stable-relates-to-scope-id [mission target-ident original-id duplicate?]
  (let [stem (str/replace-first (str mission) #"^M-" "")
        target-stem (mission-stem target-ident)
        base (str stem "/relates-to/" target-stem)]
    (if duplicate?
      (str base "--" (subs (sha1 (str target-ident "|" original-id)) 0 8))
      base)))

(defn- stable-source-scope-id [mission source-ref original-id duplicate?]
  (let [stem (str/replace-first (str mission) #"^M-" "")
        source-slug (truncated-slug source-ref)
        base (str stem "/source/" source-slug)]
    (if duplicate?
      (str base "--" (subs (sha1 (str source-ref "|" original-id)) 0 8))
      base)))

(defn- stable-pattern-scope-id [mission binder pattern-ident original-id duplicate?]
  (let [stem (str/replace-first (str mission) #"^M-" "")
        pattern-slug (truncated-slug pattern-ident)
        base (str stem "/" binder "/" pattern-slug)]
    (if duplicate?
      (str base "--" (subs (sha1 (str pattern-ident "|" original-id)) 0 8))
      base)))

(defn- scope-boundary-polarity [scope]
  (case (:binder-type scope)
    "mission-scope-in" :scope/in
    "mission-scope-out" :scope/out
    (let [heading (str/lower-case (heading-title scope))]
      (if (str/includes? heading "out") :scope/out :scope/in))))

(defn- stable-boundary-scope-id [mission polarity item-text original-id duplicate?]
  (let [stem (str/replace-first (str mission) #"^M-" "")
        polarity-slug (if (= :scope/out polarity) "scope-out" "scope-in")
        item-slug (truncated-slug item-text)
        base (str stem "/" polarity-slug "/" item-slug)]
    (if duplicate?
      (str base "--" (subs (sha1 (str item-text "|" original-id)) 0 8))
      base)))

(defn- stable-heading-scopes [mission mission-path all-scopes collision-scopes scopes]
  (let [slug-counts (frequencies (map (comp slug heading-title) collision-scopes))]
    (mapv (fn [scope]
            (let [heading (heading-title scope)
                  heading-slug (slug heading)
                  duplicate? (> (get slug-counts heading-slug 0) 1)
                  stable-id (stable-heading-scope-id mission heading (:scope-id scope) duplicate?)
                  canonical-id (str (str/replace-first (str mission) #"^M-" "") "/" heading-slug)
                  anchor (anchor-for-scope mission-path all-scopes scope)]
              (assoc scope
                     :original-scope-id (:scope-id scope)
                     :scope-id stable-id
                     :stable-scope-id stable-id
                     :canonical-scope-id canonical-id
                     :heading-slug heading-slug
                     :duplicate-heading? duplicate?
                     :anchor anchor
                     :parent nil)))
          scopes)))

(defn- stable-item-scopes [mission mission-path all-scopes collision-scopes scopes]
  (let [slug-counts (frequencies (map (comp truncated-slug anchor-text) collision-scopes))]
    (mapv (fn [scope]
            (let [text (anchor-text scope)
                  item-slug (truncated-slug text)
                  duplicate? (> (get slug-counts item-slug 0) 1)
                  stable-id (stable-item-scope-id mission text (:scope-id scope) duplicate?)
                  canonical-id (str (str/replace-first (str mission) #"^M-" "") "/" item-slug)
                  anchor (anchor-for-scope mission-path all-scopes scope)]
              (assoc scope
                     :original-scope-id (:scope-id scope)
                     :scope-id stable-id
                     :stable-scope-id stable-id
                     :canonical-scope-id canonical-id
                     :heading-slug item-slug
                     :duplicate-heading? duplicate?
                     :anchor anchor
                     :parent nil)))
          scopes)))

(defn- stable-record-scopes
  "PSR/PUR record scopes. Stable ids keyed by binder + pattern ident; the
   anchor is the record's own `Pattern chosen:`/`Pattern:` line (carried by
   the detector as :anchor-line and resolved in `anchor-for-scope`). Without
   this enrichment psr/pur scopes fall through the binder dispatch with no
   :anchor at all and ingest as passage-less rows."
  [mission mission-path all-scopes scopes]
  (let [ident-counts (frequencies (map (juxt :binder-type :pattern-ident) scopes))]
    (mapv (fn [scope]
            (let [pattern-ident (or (:pattern-ident scope) (:scope-id scope))
                  binder (:binder-type scope)
                  duplicate? (> (get ident-counts [binder pattern-ident] 0) 1)
                  stem (str/replace-first (str mission) #"^M-" "")
                  base (str stem "/" binder "/" (truncated-slug pattern-ident))
                  stable-id (if duplicate?
                              (str base "-" (subs (sha1 (str (:scope-id scope))) 0 8))
                              base)
                  scope (assoc scope :target-pattern-ident pattern-ident)
                  anchor (anchor-for-scope mission-path all-scopes scope)]
              (assoc scope
                     :original-scope-id (:scope-id scope)
                     :scope-id stable-id
                     :stable-scope-id stable-id
                     :canonical-scope-id base
                     :heading-slug (str binder "/" (truncated-slug pattern-ident))
                     :duplicate-heading? duplicate?
                     :anchor anchor)))
          scopes)))

(defn- stable-relates-to-scopes [mission mission-path all-scopes scopes]
  (let [expanded (mapcat (fn [scope]
                           (for [target (target-mission-ends scope)]
                             (assoc scope
                                    :target-mission-ident (:ident target)
                                    :target-mission-relation (:relation target))))
                         scopes)
        target-counts (frequencies (map target-mission-ident expanded))]
    (mapv (fn [scope]
            (let [target-ident (target-mission-ident scope)
                  duplicate? (> (get target-counts target-ident 0) 1)
                  stable-id (stable-relates-to-scope-id mission target-ident (:scope-id scope) duplicate?)
                  canonical-id (str (str/replace-first (str mission) #"^M-" "")
                                    "/relates-to/"
                                    (mission-stem target-ident))
                  anchor (anchor-for-scope mission-path all-scopes scope)]
              (assoc scope
                     :original-scope-id (:scope-id scope)
                     :scope-id stable-id
                     :stable-scope-id stable-id
                     :canonical-scope-id canonical-id
                     :heading-slug (str "relates-to/" (mission-stem target-ident))
                     :duplicate-heading? duplicate?
                     :anchor anchor
                     :parent nil)))
          expanded)))

(defn- stable-boundary-scopes [mission mission-path all-scopes scopes]
  (let [expanded (mapcat (fn [scope]
                           (let [items (seq (bounded-item-ends scope))
                                 polarity (scope-boundary-polarity scope)]
                             (if items
                               (for [item items]
                                 (assoc scope
                                        :scope-polarity polarity
                                        :boundary-item-text (:text item)))
                               [(assoc scope
                                       :scope-polarity polarity
                                       :boundary-item-text (heading-title scope))])))
                         scopes)
        key-counts (frequencies (map (fn [scope]
                                        [(:scope-polarity scope)
                                         (truncated-slug (boundary-item-text scope))])
                                      expanded))]
    (mapv (fn [scope]
            (let [polarity (:scope-polarity scope)
                  item-text (boundary-item-text scope)
                  item-slug (truncated-slug item-text)
                  duplicate? (> (get key-counts [polarity item-slug] 0) 1)
                  stable-id (stable-boundary-scope-id mission polarity item-text (:scope-id scope) duplicate?)
                  polarity-slug (if (= :scope/out polarity) "scope-out" "scope-in")
                  canonical-id (str (str/replace-first (str mission) #"^M-" "")
                                    "/" polarity-slug "/" item-slug)
                  anchor (anchor-for-scope mission-path all-scopes scope)]
              (assoc scope
                     :original-scope-id (:scope-id scope)
                     :scope-id stable-id
                     :stable-scope-id stable-id
                     :canonical-scope-id canonical-id
                     :heading-slug (str polarity-slug "/" item-slug)
                     :duplicate-heading? duplicate?
                     :anchor anchor
                     :parent nil)))
          expanded)))

(defn- stable-source-material-scopes [mission mission-path all-scopes scopes]
  (let [expanded (mapcat (fn [scope]
                           (for [source (target-source-ends scope)]
                             (assoc scope
                                    :target-source-ref (:ref source)
                                    :target-source-kind (:kind source))))
                         scopes)
        ref-counts (frequencies (map target-source-ref expanded))]
    (mapv (fn [scope]
            (let [source-ref (target-source-ref scope)
                  duplicate? (> (get ref-counts source-ref 0) 1)
                  stable-id (stable-source-scope-id mission source-ref (:scope-id scope) duplicate?)
                  canonical-id (str (str/replace-first (str mission) #"^M-" "")
                                    "/source/"
                                    (truncated-slug source-ref))
                  anchor (anchor-for-scope mission-path all-scopes scope)]
              (assoc scope
                     :original-scope-id (:scope-id scope)
                     :scope-id stable-id
                     :stable-scope-id stable-id
                     :canonical-scope-id canonical-id
                     :heading-slug (str "source/" (truncated-slug source-ref))
                     :duplicate-heading? duplicate?
                     :anchor anchor
                     :parent nil)))
          expanded)))

(defn- stable-pattern-scopes [mission mission-path all-scopes scopes]
  (let [expanded (mapcat (fn [scope]
                           (for [pattern (target-pattern-ends scope)]
                             (assoc scope
                                    :target-pattern-ident (:ident pattern)
                                    :target-pattern-ref (:ref pattern))))
                         scopes)
        pattern-counts (frequencies (map (juxt :binder-type target-pattern-ident) expanded))]
    (mapv (fn [scope]
            (let [binder (:binder-type scope)
                  pattern-ident (target-pattern-ident scope)
                  duplicate? (> (get pattern-counts [binder pattern-ident] 0) 1)
                  stable-id (stable-pattern-scope-id mission binder pattern-ident (:scope-id scope) duplicate?)
                  canonical-id (str (str/replace-first (str mission) #"^M-" "")
                                    "/" binder "/"
                                    (truncated-slug pattern-ident))
                  anchor (anchor-for-scope mission-path all-scopes scope)]
              (assoc scope
                     :original-scope-id (:scope-id scope)
                     :scope-id stable-id
                     :stable-scope-id stable-id
                     :canonical-scope-id canonical-id
                     :heading-slug (str binder "/" (truncated-slug pattern-ident))
                     :duplicate-heading? duplicate?
                     :anchor anchor
                     :parent nil)))
          expanded)))

(defn- legacy-scope-id? [id]
  (boolean (re-matches #"^M-.+:scope-[0-9]+$" (str id))))

(defn- scope-endpoint-id [h]
  (or (some (fn [end]
              (when (= :environment (:role end))
                (:entity-id end)))
            (:hx/ends h))
      (second (:hx/endpoints h))))

(defn- legacy-position-scope-hyperedge? [binder-filter h]
  (let [scope-id (or (get-in h [:hx/props :scope/id])
                     (scope-endpoint-id h))]
    (and (= (keyword "mission-scope" binder-filter) (:hx/type h))
         (legacy-scope-id? scope-id)
         (contains? (:hx/content h) :position))))

(defn- retract-stale-generation-scopes!
  "True the scope lane against the CURRENT detector tree. Scope ids are
  heading-slug based, so a doc rewrite mints new ids and the old
  generation survives as ghosts — mission-mode then renders EVERY
  generation at once (caught live by Joe on M-first-flights 2026-06-11:
  three '## 3. DERIVE' generations, 46 scopes). For this mission+binder,
  any stored scope whose id is absent from SELECTED-IDS is retracted."
  [client base-url penholder mission-entity binder-filter selected-ids]
  (let [stale-hxs (->> (hyperedges-by-type client base-url (str "mission-scope/" binder-filter))
                       (filter #(some #{(:id mission-entity)} (:hx/endpoints %)))
                       (remove #(let [sid (or (get-in % [:hx/props :scope/id])
                                              (scope-endpoint-id %))]
                                  (or (nil? sid) (contains? selected-ids sid))))
                       vec)
        hx-ids (keep :hx/id stale-hxs)
        scope-ids (->> stale-hxs
                       (keep scope-endpoint-id)
                       (remove #(contains? selected-ids %)))
        ids (distinct (concat hx-ids scope-ids))]
    (delete-docs! client base-url penholder ids)
    {:stale-hyperedge-retract-count (count (distinct hx-ids))
     :stale-entity-retract-count (count (distinct scope-ids))
     :stale-doc-retract-count (count ids)}))

(defn- retract-legacy-position-scopes!
  [client base-url penholder mission-entity binder-filter]
  (let [legacy-hxs (->> (hyperedges-by-type client base-url (str "mission-scope/" binder-filter))
                        (filter #(some #{(:id mission-entity)} (:hx/endpoints %)))
                        (filter #(legacy-position-scope-hyperedge? binder-filter %))
                        vec)
        hx-ids (keep :hx/id legacy-hxs)
        scope-ids (->> legacy-hxs
                       (keep scope-endpoint-id)
                       (filter legacy-scope-id?))
        ids (distinct (concat hx-ids scope-ids))]
    (delete-docs! client base-url penholder ids)
    {:legacy-hyperedge-retract-count (count (distinct hx-ids))
     :legacy-entity-retract-count (count (distinct scope-ids))
     :legacy-doc-retract-count (count ids)}))

(defn- scope-entity-spec [mission path scope]
  {:id (:scope-id scope)
   :name (env-name scope)
   :type (str "scope/" (:binder-type scope))
   :external-id (:scope-id scope)
   :source "mission-scope-tree"
   :props (cond-> {:mission mission
                   :mission/path path
                   :scope/binder-type (:binder-type scope)
                   :scope/parent (:parent scope)}
            (:canonical-phase scope)
            (assoc :scope/canonical-phase (:canonical-phase scope)
                   :scope/original-id (:original-scope-id scope)
                   :scope/stable-id (:stable-scope-id scope)
                   :scope/canonical-id (:canonical-scope-id scope)
                   :scope/duplicate-phase? (boolean (:duplicate-phase? scope)))
            (:anchor scope)
            (assoc :anchor/state (:state (:anchor scope))
                   :anchor/passage (:passage (:anchor scope))
                   :anchor/heading (:heading (:anchor scope))
                   :anchor/fingerprint (:fingerprint (:anchor scope))
                   :anchor/source (:source (:anchor scope))
                   :anchor/resolve-by (:resolve-by (:anchor scope))
                   :anchor/detached-reason (:detached-reason (:anchor scope)))
            (:heading-slug scope)
            (assoc :scope/heading-slug (:heading-slug scope)
                   :scope/original-id (:original-scope-id scope)
                   :scope/stable-id (:stable-scope-id scope)
                   :scope/canonical-id (:canonical-scope-id scope)
                   :scope/duplicate-heading? (boolean (:duplicate-heading? scope)))
            (:target-mission-ident scope)
            (assoc :target/mission-ident (:target-mission-ident scope)
                   :target/relation (:target-mission-relation scope)
                   :target/state (:target-state scope)
                   :target/detached-reason (:target-detached-reason scope))
            (:target-source-ref scope)
            (assoc :source/ref (:target-source-ref scope)
                   :source/kind (:target-source-kind scope)
                   :source/state (:source-state scope)
                   :source/detached-reason (:source-detached-reason scope))
            (:target-pattern-ident scope)
            (assoc :pattern/ident (:target-pattern-ident scope)
                   :pattern/ref (:target-pattern-ref scope)
                   :pattern/state (:pattern-state scope)
                   :pattern/detached-reason (:pattern-detached-reason scope))
            (:scope-polarity scope)
            (assoc :scope/polarity (:scope-polarity scope)
                   :scope/boundary-text (:boundary-item-text scope)))})

(defn- scope-hyperedge [mission-entity scope-entity slot-entities scope]
  {:hx/id (str "hx|mission-scope|" (:scope-id scope))
   :hx/type (str "mission-scope/" (:binder-type scope))
   :hx/endpoints (vec (concat [{:role :entity :entity-id (:id mission-entity)}
                               {:role :environment :entity-id (:id scope-entity)}]
                              (map (fn [{:keys [role entity]}]
                                     {:role (keyword role) :entity-id (:id entity)})
                                   slot-entities)))
   :props {:mission (:external-id mission-entity)
           :scope/id (:scope-id scope)
           :scope/binder-type (:binder-type scope)
           :scope/parent (:parent scope)
           :scope/name (:name scope-entity)
           :scope/original-id (:original-scope-id scope)
           :scope/stable-id (:stable-scope-id scope)
           :scope/canonical-id (:canonical-scope-id scope)
           :scope/canonical-phase (:canonical-phase scope)
           :scope/heading-slug (:heading-slug scope)
           :scope/duplicate-heading? (:duplicate-heading? scope)
           :target/mission-ident (:target-mission-ident scope)
           :target/relation (:target-mission-relation scope)
           :target/state (:target-state scope)
           :target/detached-reason (:target-detached-reason scope)
           :source/ref (:target-source-ref scope)
           :source/kind (:target-source-kind scope)
           :source/state (:source-state scope)
           :source/detached-reason (:source-detached-reason scope)
           :pattern/ident (:target-pattern-ident scope)
           :pattern/ref (:target-pattern-ref scope)
           :pattern/state (:pattern-state scope)
           :pattern/detached-reason (:pattern-detached-reason scope)
           :record (:record scope)
           :record/facets (:facets scope)
           :record/rationale (get-in scope [:facets :rationale])
           :record/candidates (get-in scope [:facets :candidates])
           :record/actions (get-in scope [:facets :actions])
           :record/outcome (get-in scope [:facets :outcome])
           :record/prediction-error (get-in scope [:facets :prediction-error])
           :record/notes (get-in scope [:facets :notes])
           :scope/polarity (:scope-polarity scope)
           :scope/boundary-text (:boundary-item-text scope)
           :anchor/state (:state (:anchor scope))
           :anchor/passage (:passage (:anchor scope))
           :anchor/fingerprint (:fingerprint (:anchor scope))
           :anchor/source (:source (:anchor scope))
           :anchor/resolve-by (:resolve-by (:anchor scope))
           :anchor/detached-reason (:detached-reason (:anchor scope))}
   :hx/content (when-let [anchor (:anchor scope)]
                 {:anchor/kind "verbatim-heading+concept-fingerprint"
                  :anchor/passage (:passage anchor)
                  :anchor/fingerprint (:fingerprint anchor)
                  :anchor/source (:source anchor)
                  :anchor/state (:state anchor)})
   :hx/labels [(str "mission-scope/" (:binder-type scope))]})

(defn- nesting-hyperedge [mission-entity parent-scope child-scope]
  {:hx/id (str "hx|mission-scope-parent|" parent-scope "|" child-scope)
   :hx/type "mission-scope/nesting"
   :hx/endpoints [{:role :entity :entity-id (:id mission-entity)}
                  {:role :parent :entity-id parent-scope}
                  {:role :child :entity-id child-scope}]
   :props {:scope/parent parent-scope
           :scope/child child-scope}
   :hx/labels ["mission-scope/nesting"]})

(declare scope-tree-files)

(defn- hx-props [h]
  (or (:hx/props h) (:props h) {}))

(defn- hx-type-name [h]
  (let [t (:hx/type h)]
    (if (keyword? t)
      (if-let [ns (namespace t)]
        (str ns "/" (name t))
        (name t))
      (str t))))

(defn- canonical-mission-scope-type-name [h]
  (let [t (hx-type-name h)]
    (if (str/starts-with? t "mission-scope/")
      t
      (if-let [binder (get (hx-props h) :scope/binder-type)]
        (str "mission-scope/" binder)
        t))))

(defn- hx-label-names [h]
  [(canonical-mission-scope-type-name h)])

(defn- hx-endpoint-payload [h]
  (if (seq (:hx/ends h))
    (mapv (fn [end]
            {:role (:role end)
             :entity-id (:entity-id end)})
          (:hx/ends h))
    (mapv (fn [end]
            (if (map? end) end {:entity-id end}))
          (:hx/endpoints h))))

(defn- merged-hyperedge-payload [h props]
  (cond-> {:hx/id (:hx/id h)
           :hx/type (canonical-mission-scope-type-name h)
           :hx/endpoints (hx-endpoint-payload h)
           :props (merge (hx-props h) props)}
    (contains? h :hx/content) (assoc :hx/content (:hx/content h))
    (seq (:hx/labels h)) (assoc :hx/labels (hx-label-names h))))

(defn- scope-mission-endpoint [h]
  (or (some (fn [end]
              (when (= :entity (:role end))
                (:entity-id end)))
            (:hx/ends h))
      (first (:hx/endpoints h))))

(defn- scope-id-from-hx [h]
  (or (get (hx-props h) :scope/id)
      (some (fn [end]
              (when (= :environment (:role end))
                (:entity-id end)))
            (:hx/ends h))
      (second (:hx/endpoints h))))

(defn- original-id-from-hx [h]
  (get (hx-props h) :scope/original-id))

(defn- binder-from-hx [h]
  (or (get (hx-props h) :scope/binder-type)
      (str/replace-first (hx-type-name h) #"^mission-scope/" "")))

(defn- original-parent-map [scope-dir selected]
  (let [files (scope-tree-files scope-dir selected)]
    (reduce
     (fn [m file]
       (let [data (json/parse-string (slurp file) true)]
         (reduce
          (fn [acc scope]
            (if-let [parent (:parent scope)]
              (assoc acc (:scope-id scope)
                     {:parent-original-id parent
                      :child-binder (:binder-type scope)
                      :mission (:mission data)
                      :file (.getAbsolutePath file)})
              acc))
          m
          (:scope-hyperedges data))))
     {}
     files)))

(defn- all-structural-scope-hyperedges [client base-url]
  (mapcat (fn [binder]
            (concat (hyperedges-by-type client base-url (str "mission-scope/" binder))
                    ;; Repair path for any scope hxs that were temporarily
                    ;; written with a bare binder type instead of the canonical
                    ;; mission-scope/<binder> type.
                    (hyperedges-by-type client base-url binder)))
          structural-binders))

(defn- correspondence-map [hxs]
  (reduce
   (fn [m h]
     (let [original-id (original-id-from-hx h)
           stable-id (scope-id-from-hx h)]
       (if (and (seq (str original-id)) (seq (str stable-id)))
         (update m original-id (fnil conj #{}) stable-id)
         m)))
   {}
   hxs))

(defn- nesting-hyperedge* [mission-id parent-scope child-scope child-original parent-original]
  (assoc (nesting-hyperedge {:id mission-id} parent-scope child-scope)
         :props {:scope/parent parent-scope
                 :scope/child child-scope
                 :scope/original-id child-original
                 :scope/original-parent parent-original
                 :scope/parent-state :linked
                 :scope/parent-resolve-by :original-id-correspondence}))

(defn- parent-resolution [corr parent-original]
  (let [matches (get corr parent-original)]
    (cond
      (= 1 (count matches)) {:state :linked :parent (first matches)}
      (empty? matches) {:state :detached
                        :reason :parent-original-id-not-found}
      :else {:state :detached
             :reason :parent-original-id-ambiguous
             :matches (sort matches)})))

(defn- update-parent-wiring!
  [{:keys [client base-url penholder scope-dir selected dry-run?]}]
  (let [hxs (vec (all-structural-scope-hyperedges client base-url))
        corr (correspondence-map hxs)
        parent-by-original (original-parent-map scope-dir selected)
        report (atom {})
        bump! (fn [binder k]
                (swap! report update-in [binder k] (fnil inc 0)))]
    (doseq [h hxs
            :let [child-original (original-id-from-hx h)
                  parent-info (get parent-by-original child-original)]
            :when parent-info]
      (let [binder (binder-from-hx h)
            child-scope (scope-id-from-hx h)
            mission-id (scope-mission-endpoint h)
            parent-original (:parent-original-id parent-info)
            resolution (parent-resolution corr parent-original)
            props (cond-> {:scope/original-parent parent-original
                            :scope/parent-state (:state resolution)
                            :scope/parent-resolve-by :original-id-correspondence}
                    (= :linked (:state resolution))
                    (assoc :scope/parent (:parent resolution))
                    (= :detached (:state resolution))
                    (assoc :scope/parent nil
                           :scope/parent-detached-reason (:reason resolution))
                    (:matches resolution)
                    (assoc :scope/parent-candidates (:matches resolution)))]
        (bump! binder :parent-candidate-count)
        (if (= :linked (:state resolution))
          (do
            (bump! binder :parent-resolved-count)
            (when-not dry-run?
              (post-hyperedge! client base-url penholder
                               (merged-hyperedge-payload h props))
              (when-let [entity (get-entity client base-url child-scope)]
                (ensure-entity! client base-url penholder
                                {:id (:id entity)
                                 :name (:name entity)
                                 :type (:type entity)
                                 :external-id (:external-id entity)
                                 :source (:source entity)
                                 :props props}))
              (post-hyperedge! client base-url penholder
                               (nesting-hyperedge* mission-id
                                                   (:parent resolution)
                                                   child-scope
                                                   child-original
                                                   parent-original))))
          (do
            (bump! binder :parent-detached-count)
            (swap! report update-in [binder :detached-reasons (:reason resolution)] (fnil inc 0))
            (when-not dry-run?
              (post-hyperedge! client base-url penholder
                               (merged-hyperedge-payload h props)))))))
    (into (sorted-map)
          (map (fn [[binder counts]]
                 [binder (merge {:parent-candidate-count 0
                                 :parent-resolved-count 0
                                 :parent-detached-count 0}
                                counts)]))
          @report)))

(defn- pxr-hyperedge [mission-id pattern-ident psr-scope pur-scope pxr-scope-id]
  {:hx/id (str "hx|mission-scope|pxr|" pxr-scope-id)
   :hx/type "mission-scope/pxr"
   :hx/endpoints [{:role :entity :entity-id mission-id}
                  {:role :psr :entity-id psr-scope}
                  {:role :pur :entity-id pur-scope}
                  {:role :pattern :entity-id pattern-ident}]
   :props {:scope/id pxr-scope-id
           :scope/binder-type "pxr"
           :pattern/ident pattern-ident
           :pxr/psr-scope psr-scope
           :pxr/pur-scope pur-scope
           :scope/derived-by :psr-pur-correspondence}
   :hx/labels ["mission-scope/pxr"]})

(defn- update-pxr-wiring!
  "Derive (pxr) edges: where a mission has BOTH a psr and a pur scope on the
   SAME pattern, link them into the complete select->use->outcome unit — the
   labelled example the downstream priming layer learns from. Idempotent
   (deterministic pxr id, upsert)."
  [{:keys [client base-url penholder dry-run?]}]
  (let [psrs (hyperedges-by-type client base-url "mission-scope/psr")
        purs (hyperedges-by-type client base-url "mission-scope/pur")
        key-of (fn [h] [(scope-mission-endpoint h) (get (hx-props h) :pattern/ident)])
        psr-by (into {} (map (juxt key-of identity) psrs))
        report (atom {:psr-count (count psrs) :pur-count (count purs) :pxr-written 0})]
    (doseq [pur-h purs
            :let [k (key-of pur-h)
                  psr-h (get psr-by k)]
            :when (and psr-h (second k))]
      (let [[mission-id pattern-ident] k
            psr-scope (scope-id-from-hx psr-h)
            pur-scope (scope-id-from-hx pur-h)
            pxr-scope-id (str/replace (str psr-scope) "/psr/" "/pxr/")]
        (when-not dry-run?
          (post-hyperedge! client base-url penholder
                           (pxr-hyperedge mission-id pattern-ident psr-scope pur-scope pxr-scope-id)))
        (swap! report update :pxr-written inc)))
    @report))

(defn- mission-ident-from-stem [stem]
  (let [s (-> (str stem)
              (str/replace #"\.md$" "")
              (str/replace #"\.json$" ""))]
    (if (str/starts-with? s "M-") s (str "M-" s))))

(defn- find-mission-file [stem]
  (let [mission (mission-ident-from-stem stem)
        filename (str mission ".md")]
    (or (some (fn [f]
                (when (and (.isFile f)
                           (= filename (.getName f))
                           (str/includes? (.getPath f) "/holes/"))
                  f))
              (file-seq (io/file code-root)))
        (throw (ex-info "Mission file not found"
                        {:mission mission
                         :filename filename})))))

(defn- run-detector! [mission-file]
  (let [result (sh/sh "python3"
                      "/home/joe/code/futon6/scripts/mission_scope_detect.py"
                      (.getAbsolutePath (io/file mission-file)))]
    (when-not (zero? (:exit result))
      (throw (ex-info "mission_scope_detect.py failed"
                      {:mission-file (.getAbsolutePath (io/file mission-file))
                       :exit (:exit result)
                       :out (:out result)
                       :err (:err result)})))
    result))

(defn- scope-tree-path-for-mission [mission]
  (io/file default-scope-dir (str mission ".json")))

(defn- stable-scopes-for-binder [mission mission-path raw-scopes binder]
  (let [scopes (filter #(= binder (:binder-type %)) raw-scopes)]
    (case binder
      "eightfold-phase" (stable-eightfold-scopes mission mission-path raw-scopes (vec scopes))
      "loose-section" (stable-heading-scopes mission mission-path raw-scopes (vec scopes) (vec scopes))
      ;; plain-argument: heading-bound like a loose-section (defined ARGUE
      ;; sub-scope, Joe 2026-06-10).
      "plain-argument" (stable-heading-scopes mission mission-path raw-scopes (vec scopes) (vec scopes))
      ;; verify-gate: heading-bound (defined VERIFY sub-scope, the 13th binder)
      "verify-gate" (stable-heading-scopes mission mission-path raw-scopes (vec scopes) (vec scopes))
      ;; certificate: anchors at its verdict line via content-position-passage
      "certificate" (stable-heading-scopes mission mission-path raw-scopes (vec scopes) (vec scopes))
      "capability-scope" (let [collision-scopes (filter #(contains? #{"eightfold-phase"
                                                                       "loose-section"
                                                                       "capability-scope"}
                                                                     (:binder-type %))
                                                        raw-scopes)]
                            (stable-heading-scopes mission mission-path raw-scopes
                                                   (vec collision-scopes)
                                                   (vec scopes)))
      "map-item" (let [collision-scopes (filter #(contains? #{"eightfold-phase"
                                                               "loose-section"
                                                               "capability-scope"
                                                               "map-item"}
                                                             (:binder-type %))
                                                    raw-scopes)]
                    (stable-item-scopes mission mission-path raw-scopes
                                        (vec collision-scopes)
                                        (vec scopes)))
      "relates-to" (stable-relates-to-scopes mission mission-path raw-scopes (vec scopes))
      "source-material" (stable-source-material-scopes mission mission-path raw-scopes (vec scopes))
      "mission-scope-in" (stable-boundary-scopes mission mission-path raw-scopes (vec scopes))
      "mission-scope-out" (stable-boundary-scopes mission mission-path raw-scopes (vec scopes))
      ("pattern" "psr" "pur") (stable-pattern-scopes mission mission-path raw-scopes (vec scopes))
      (vec scopes))))

(defn- stable-scopes-for-tree [data]
  (let [mission (:mission data)
        mission-path (:path data)
        raw-scopes (:scope-hyperedges data)]
    (mapcat #(stable-scopes-for-binder mission mission-path raw-scopes %)
            structural-binders)))

(defn- hyperedges-by-end [client base-url end-id]
  (-> (http-edn client :get
                (str base-url "/api/alpha/hyperedges?end="
                     (url-encode end-id)
                     "&limit=5000"))
      (ok! {:op :hyperedges-by-end :end end-id})
      (get-in [:body :hyperedges])
      (or [])))

(defn- stored-scope-hyperedges-for-mission [client base-url mission]
  (->> structural-binders
       (mapcat (fn [binder]
                 (hyperedges-by-type client base-url (str "mission-scope/" binder))))
       (filter #(= mission (get (hx-props %) :mission)))
       vec))

(defn- retract-absent-binder-scopes!
  "Binder types with stored scopes for MISSION but ZERO presence in the
  current detector tree are wholly stale generations (the per-binder
  ingest loop never visits them, so their ghosts survive rewrites —
  M-first-flights 2026-06-11: 5 map-item + 2 pattern ghosts from a
  superseded draft). Retract every scope of the absent binder types."
  [client base-url penholder mission tree-binders]
  (let [stale-hxs (->> (stored-scope-hyperedges-for-mission client base-url mission)
                       (remove #(contains? tree-binders (name (:hx/type %))))
                       vec)
        hx-ids (keep :hx/id stale-hxs)
        scope-ids (keep scope-endpoint-id stale-hxs)
        ids (distinct (concat hx-ids scope-ids))]
    (delete-docs! client base-url penholder ids)
    {:absent-binder-hyperedge-retract-count (count (distinct hx-ids))
     :absent-binder-entity-retract-count (count (distinct scope-ids))
     :absent-binder-types (->> stale-hxs (map #(name (:hx/type %))) distinct sort vec)}))

(defn- concept-ids-from-hx [h]
  (->> (:hx/ends h)
       (filter #(= :concept (:role %)))
       (map :entity-id)
       set))

(defn- concept-ids-from-scope [mission path scope]
  (->> (:ends scope)
       filler-ends
       (keep #(when-let [spec (slot-entity-spec mission path %)]
                (when (= "concept" (:type spec))
                  (:id spec))))
       set))

(defn- title-token-set [s]
  (->> (str/split (str/lower-case (str s)) #"[^a-z0-9]+")
       (remove #(or (str/blank? %) (< (count %) 3)
                    (#{"section" "scope" "phase" "part" "chapter"} %)))
       set))

(defn- jaccard [a b]
  (let [a (set a)
        b (set b)
        union-count (count (set/union a b))]
    (if (zero? union-count)
      0.0
      (/ (double (count (set/intersection a b))) union-count))))

(defn- indexed-new-scope [mission path scope]
  (assoc scope
         :concept-ids (concept-ids-from-scope mission path scope)
         :title-tokens (title-token-set (get-in scope [:anchor :heading]))
         :match-scope-id (:scope-id scope)
         :match-original-id (:original-scope-id scope)))

(defn- indexed-stored-scope [h]
  (let [props (hx-props h)]
    {:hx h
     :scope-id (scope-id-from-hx h)
     :original-id (:scope/original-id props)
     :reworded-scope-id (:scope/reworded-scope-id props)
     :binder (binder-from-hx h)
     :anchor-passage (:anchor/passage props)
     :anchor-fingerprint (:anchor/fingerprint props)
     :title-tokens (title-token-set (:anchor/passage props))
     :concept-ids (concept-ids-from-hx h)}))

(defn- compatible-reword-match? [stored new-scope]
  (or (= (:scope-id stored) (:match-scope-id new-scope))
      (pos? (jaccard (:title-tokens stored) (:title-tokens new-scope)))))

(defn- best-concept-match [stored new-scopes used-new]
  (let [candidates (->> new-scopes
                        (remove #(contains? used-new (:match-key %)))
                        (filter #(= (:binder stored) (:binder-type %)))
                        (filter #(compatible-reword-match? stored %))
                        (map (fn [scope]
                               [scope (jaccard (:concept-ids stored)
                                               (:concept-ids scope))]))
                        (filter #(>= (second %) 0.8))
                        (sort-by (fn [[scope score]]
                                   [(- score) (:scope-id scope)])))]
    (ffirst candidates)))

(defn- match-scopes [stored-scopes new-scopes]
  (let [new-by-original (group-by :match-original-id new-scopes)
        new-by-scope-id (group-by :match-scope-id new-scopes)]
    (loop [stored stored-scopes
           matches []
           used-new #{}]
      (if-let [s (first stored)]
        (let [original-pick (some-> (get new-by-original (:original-id s)) first)
              pick (or (when (and original-pick
                                   (compatible-reword-match? s original-pick))
                          original-pick)
                       (some-> (get new-by-scope-id (:scope-id s)) first)
                       (best-concept-match s new-scopes used-new))
              key (:match-key pick)]
          (if pick
            (recur (rest stored)
                   (conj matches [s pick])
                   (conj used-new key))
            (recur (rest stored)
                   (conj matches [s nil])
                   used-new)))
        {:matched matches
         :unmatched-new (remove #(contains? used-new (:match-key %)) new-scopes)}))))

(defn- delete-scope-docs! [client base-url penholder scope-id hx-id]
  (let [edge-ids (->> (hyperedges-by-end client base-url scope-id)
                      (keep :hx/id))
        ids (distinct (concat [scope-id hx-id] edge-ids))]
    (delete-docs! client base-url penholder ids)
    {:deleted-count (count ids)
     :deleted-ids ids}))

(defn- anchor-props [scope]
  {:anchor/state (:state (:anchor scope))
   :anchor/passage (:passage (:anchor scope))
   :anchor/heading (:heading (:anchor scope))
   :anchor/fingerprint (:fingerprint (:anchor scope))
   :anchor/source (:source (:anchor scope))
   :anchor/resolve-by (:resolve-by (:anchor scope))
   :anchor/detached-reason (:detached-reason (:anchor scope))})

(defn- reworded? [stored new-scope]
  (or (not= (or (:reworded-scope-id stored) (:scope-id stored))
            (:scope-id new-scope))
      (not= (:anchor-passage stored) (get-in new-scope [:anchor :passage]))))

(defn- update-scope-anchor! [client base-url penholder stored new-scope]
  (let [h (:hx stored)
        old-scope-id (:scope-id stored)
        new-scope-id (:scope-id new-scope)
        props (merge (anchor-props new-scope)
                     {:scope/name (env-name new-scope)
                      :scope/heading-slug (:heading-slug new-scope)
                      :scope/reworded-scope-id (when (not= old-scope-id new-scope-id)
                                                 new-scope-id)
                      :scope/reworded-at (str (java.time.Instant/now))
                      :scope/maintenance-state :re-resolved})]
    (post-hyperedge! client base-url penholder
                     (merged-hyperedge-payload h props))
    (when-let [entity (get-entity client base-url old-scope-id)]
      (ensure-entity! client base-url penholder
                      {:id (:id entity)
                       :name (env-name new-scope)
                       :type (:type entity)
                       :external-id (:external-id entity)
                       :source (:source entity)
                       :props props}))))

(defn- mark-detached! [client base-url penholder stored reason]
  (let [props {:anchor/state :detached
               :anchor/detached-reason reason
               :scope/maintenance-state :detached}]
    (post-hyperedge! client base-url penholder
                     (merged-hyperedge-payload (:hx stored) props))
    (when-let [entity (get-entity client base-url (:scope-id stored))]
      (ensure-entity! client base-url penholder
                      {:id (:id entity)
                       :name (:name entity)
                       :type (:type entity)
                       :external-id (:external-id entity)
                       :source (:source entity)
                       :props props}))))

(defn- upsert-single-scope!
  [client base-url penholder mission mission-path scope]
  (let [mission-entity (ensure-entity! client base-url penholder
                                       {:id (mission-doc-id mission mission-path)
                                        :name (mission-doc-id mission mission-path)
                                        :type "mission/doc"
                                        :external-id mission
                                        :source "mission-scope-tree"
                                        :props {:mission/id mission
                                                :mission/path mission-path}})
        target-entity (when (= "relates-to" (:binder-type scope))
                        (resolve-target-mission client base-url (:target-mission-ident scope)))
        source-entity (when (= "source-material" (:binder-type scope))
                        (resolve-source-file client base-url (:target-source-ref scope)))
        pattern-entity (when (contains? #{"pattern" "psr" "pur"} (:binder-type scope))
                         (resolve-pattern-node client base-url
                                               (:target-pattern-ident scope)
                                               (:target-pattern-ref scope)))
        scope (cond-> scope
                (= "relates-to" (:binder-type scope))
                (assoc :target-state (if target-entity :linked :detached)
                       :target-detached-reason (when-not target-entity :target-mission-not-found))
                (= "source-material" (:binder-type scope))
                (assoc :source-state (if source-entity :linked :detached)
                       :source-detached-reason (when-not source-entity :source-file-not-found))
                (contains? #{"pattern" "psr" "pur"} (:binder-type scope))
                (assoc :pattern-state (if pattern-entity :linked :detached)
                       :pattern-detached-reason (when-not pattern-entity :pattern-node-not-found)))
        scope-entity (ensure-entity! client base-url penholder
                                     (scope-entity-spec mission mission-path scope))
        filler-ends (cond->> (filler-ends (:ends scope))
                      (= "relates-to" (:binder-type scope))
                      (remove #(= "mission" (:role %)))
                      (= "source-material" (:binder-type scope))
                      (remove #(= "source" (:role %)))
                      (= "pattern" (:binder-type scope))
                      (remove #(= "pattern" (:role %))))
        slot-entities (->> filler-ends
                           (keep (fn [end]
                                   (when-let [spec (slot-entity-spec* client base-url mission mission-path end)]
                                     {:role (:role end)
                                      :entity (ensure-entity! client base-url penholder spec)})))
                           (concat (when target-entity
                                     [{:role "target-mission"
                                       :entity target-entity}])
                                   (when source-entity
                                     [{:role "source-file"
                                       :entity source-entity}])
                                   (when pattern-entity
                                     [{:role "target-pattern"
                                       :entity pattern-entity}]))
                           vec)
        hx (post-hyperedge! client base-url penholder
                            (scope-hyperedge mission-entity scope-entity slot-entities scope))]
    hx))

(defn- incremental-mission!
  [{:keys [client base-url penholder mission dry-run?]}]
  (let [mission (mission-ident-from-stem mission)
        mission-file (find-mission-file mission)
        _ (run-detector! mission-file)
        tree-file (scope-tree-path-for-mission mission)
        data (json/parse-string (slurp tree-file) true)
        mission-path (:path data)
        new-scopes (->> (stable-scopes-for-tree data)
                        (map-indexed (fn [idx scope]
                                       (assoc (indexed-new-scope mission mission-path scope)
                                              :match-key idx)))
                        vec)
        stored-scopes (mapv indexed-stored-scope
                            (stored-scope-hyperedges-for-mission client base-url mission))
        {:keys [matched unmatched-new]} (match-scopes stored-scopes new-scopes)
        report (atom {:mission mission
                      :stored-count (count stored-scopes)
                      :detected-count (count new-scopes)
                      :added-count 0
                      :removed-count 0
                      :reworded-count 0
                      :unchanged-count 0
                      :detached-count 0
                      :dry-run? (boolean dry-run?)})]
    (doseq [[stored new-scope] matched]
      (cond
        (nil? new-scope)
        (do
          (swap! report update :removed-count inc)
          (when-not dry-run?
            (delete-scope-docs! client base-url penholder
                                (:scope-id stored)
                                (get-in stored [:hx :hx/id]))))

        (= :detached (get-in new-scope [:anchor :state]))
        (do
          (swap! report update :detached-count inc)
          (when-not dry-run?
            (mark-detached! client base-url penholder stored :verbatim-and-fingerprint-unresolved)))

        (reworded? stored new-scope)
        (do
          (swap! report update :reworded-count inc)
          (when-not dry-run?
            (update-scope-anchor! client base-url penholder stored new-scope)))

        :else
        (swap! report update :unchanged-count inc)))
    (doseq [scope unmatched-new]
      (swap! report update :added-count inc)
      (when-not dry-run?
        (upsert-single-scope! client base-url penholder mission mission-path scope)))
    @report))

(defn ingest-scope-tree!
  [{:keys [client base-url penholder path binder-filter]}]
  (let [data (json/parse-string (slurp path) true)
        mission (:mission data)
        mission-path (:path data)
        raw-scopes (:scope-hyperedges data)
        scopes (cond->> raw-scopes
                 binder-filter (filter #(= binder-filter (:binder-type %))))
        scopes (case binder-filter
                 "eightfold-phase" (stable-eightfold-scopes mission mission-path raw-scopes (vec scopes))
                 "loose-section" (stable-heading-scopes mission mission-path raw-scopes (vec scopes) (vec scopes))
      ;; plain-argument: heading-bound like a loose-section (defined ARGUE
      ;; sub-scope, Joe 2026-06-10).
      "plain-argument" (stable-heading-scopes mission mission-path raw-scopes (vec scopes) (vec scopes))
      ;; verify-gate: heading-bound (defined VERIFY sub-scope, the 13th binder)
      "verify-gate" (stable-heading-scopes mission mission-path raw-scopes (vec scopes) (vec scopes))
      ;; certificate: anchors at its verdict line via content-position-passage
      "certificate" (stable-heading-scopes mission mission-path raw-scopes (vec scopes) (vec scopes))
                 "capability-scope" (let [collision-scopes (filter #(contains? #{"eightfold-phase"
                                                                                   "loose-section"
                                                                                   "capability-scope"}
                                                                                 (:binder-type %))
                                                                  raw-scopes)]
                                      (stable-heading-scopes mission mission-path raw-scopes
                                                             (vec collision-scopes)
                                                             (vec scopes)))
                 "map-item" (let [collision-scopes (filter #(contains? #{"eightfold-phase"
                                                                           "loose-section"
                                                                           "capability-scope"
                                                                           "map-item"}
                                                                         (:binder-type %))
                                                          raw-scopes)]
                              (stable-item-scopes mission mission-path raw-scopes
                                                  (vec collision-scopes)
                                                  (vec scopes)))
                 "relates-to" (stable-relates-to-scopes mission mission-path raw-scopes (vec scopes))
                 "source-material" (stable-source-material-scopes mission mission-path raw-scopes (vec scopes))
                 "mission-scope-in" (stable-boundary-scopes mission mission-path raw-scopes (vec scopes))
                 "mission-scope-out" (stable-boundary-scopes mission mission-path raw-scopes (vec scopes))
                 "pattern" (stable-pattern-scopes mission mission-path raw-scopes (vec scopes))
                 ("psr" "pur") (stable-record-scopes mission mission-path raw-scopes (vec scopes))
                 (vec scopes))
        selected-ids (set (map :scope-id scopes))
        mission-id (mission-doc-id mission mission-path)
        mission-entity (ensure-entity! client base-url penholder
                                       {:id mission-id
                                        :name mission-id
                                        :type "mission/doc"
                                        :external-id mission
                                        :source "mission-scope-tree"
                                        :props {:mission/id mission
                                                :mission/path mission-path}})
        legacy-report (if binder-filter
                        (retract-legacy-position-scopes! client base-url penholder mission-entity binder-filter)
                        {:legacy-hyperedge-retract-count 0
                         :legacy-entity-retract-count 0
                         :legacy-doc-retract-count 0})
        stale-report (if binder-filter
                       (retract-stale-generation-scopes! client base-url penholder
                                                         mission-entity binder-filter
                                                         selected-ids)
                       {:stale-hyperedge-retract-count 0
                        :stale-entity-retract-count 0
                        :stale-doc-retract-count 0})
        entity-ids (atom #{(:id mission-entity)})
        hx-ids (atom #{})
        linked-targets (atom #{})
        dangling-targets (atom #{})
        linked-sources (atom #{})
        dangling-sources (atom #{})
        linked-patterns (atom #{})
        dangling-patterns (atom #{})]
    (doseq [scope scopes]
      (let [scope-entity (ensure-entity! client base-url penholder
                                         (scope-entity-spec mission mission-path scope))
            _ (swap! entity-ids conj (:id scope-entity))
            target-entity (when (= "relates-to" (:binder-type scope))
                            (resolve-target-mission client base-url (:target-mission-ident scope)))
            source-entity (when (= "source-material" (:binder-type scope))
                            (resolve-source-file client base-url (:target-source-ref scope)))
            pattern-entity (when (= "pattern" (:binder-type scope))
                             (resolve-pattern-node client base-url
                                                   (:target-pattern-ident scope)
                                                   (:target-pattern-ref scope)))
            scope (cond-> scope
                    (= "relates-to" (:binder-type scope))
                    (assoc :target-state (if target-entity :linked :detached)
                           :target-detached-reason (when-not target-entity :target-mission-not-found))
                    (= "source-material" (:binder-type scope))
                    (assoc :source-state (if source-entity :linked :detached)
                           :source-detached-reason (when-not source-entity :source-file-not-found))
                    (= "pattern" (:binder-type scope))
                    (assoc :pattern-state (if pattern-entity :linked :detached)
                           :pattern-detached-reason (when-not pattern-entity :pattern-node-not-found)))
            _ (when (= "relates-to" (:binder-type scope))
                (if target-entity
                  (swap! linked-targets conj (:id target-entity))
                  (swap! dangling-targets conj (:target-mission-ident scope))))
            _ (when (= "source-material" (:binder-type scope))
                (if source-entity
                  (swap! linked-sources conj (:id source-entity))
                  (swap! dangling-sources conj (:target-source-ref scope))))
            _ (when (= "pattern" (:binder-type scope))
                (if pattern-entity
                  (swap! linked-patterns conj (:id pattern-entity))
                  (swap! dangling-patterns conj (:target-pattern-ident scope))))
            scope-entity (if (contains? #{"relates-to" "source-material" "pattern"} (:binder-type scope))
                           (ensure-entity! client base-url penholder
                                           (scope-entity-spec mission mission-path scope))
                           scope-entity)
            _ (swap! entity-ids conj (:id scope-entity))
            filler-ends (cond->> (filler-ends (:ends scope))
                          (= "relates-to" (:binder-type scope))
                          (remove #(= "mission" (:role %)))
                          (= "source-material" (:binder-type scope))
                          (remove #(= "source" (:role %)))
                          (= "pattern" (:binder-type scope))
                          (remove #(= "pattern" (:role %)))
                          (contains? #{"mission-scope-in" "mission-scope-out"} (:binder-type scope))
                          (remove #(and (= "bounded-item" (:role %))
                                        (not= (:text %) (:boundary-item-text scope)))))
            slot-entities (->> filler-ends
                               (keep (fn [end]
                                       (when-let [spec (slot-entity-spec* client base-url mission mission-path end)]
                                         {:role (:role end)
                                          :entity (ensure-entity! client base-url penholder spec)})))
                               (concat (when target-entity
                                         [{:role "target-mission"
                                           :entity target-entity}])
                                       (when source-entity
                                         [{:role "source-file"
                                           :entity source-entity}])
                                       (when pattern-entity
                                         [{:role "target-pattern"
                                           :entity pattern-entity}]))
                               vec)]
        (swap! entity-ids into (map (comp :id :entity) slot-entities))
        (let [hx (post-hyperedge! client base-url penholder
                                  (scope-hyperedge mission-entity scope-entity slot-entities scope))]
          (swap! hx-ids conj (:hx/id hx)))
        (when-let [parent (:parent scope)]
          (when (contains? selected-ids parent)
            (let [hx (post-hyperedge! client base-url penholder
                                      (nesting-hyperedge mission-entity parent (:scope-id scope)))]
              (swap! hx-ids conj (:hx/id hx)))))))
    {:mission mission
     :mission-entity (:id mission-entity)
     :entity-count (count @entity-ids)
     :hyperedge-count (count @hx-ids)
     :detached-count (count (filter #(= :detached (get-in % [:anchor :state])) scopes))
     :list-item-anchor-count (count (filter #(= :list-item (get-in % [:anchor :source])) scopes))
     :contains-line-anchor-count (count (filter #(= :contains-line (get-in % [:anchor :source])) scopes))
     :scope-in-count (count (filter #(= :scope/in (:scope-polarity %)) scopes))
     :scope-out-count (count (filter #(= :scope/out (:scope-polarity %)) scopes))
     :duplicate-phase-count (count (filter :duplicate-phase? scopes))
     :duplicate-heading-count (count (filter :duplicate-heading? scopes))
     :legacy-hyperedge-retract-count (:legacy-hyperedge-retract-count legacy-report)
     :legacy-entity-retract-count (:legacy-entity-retract-count legacy-report)
     :legacy-doc-retract-count (:legacy-doc-retract-count legacy-report)
     :stale-hyperedge-retract-count (:stale-hyperedge-retract-count stale-report)
     :stale-entity-retract-count (:stale-entity-retract-count stale-report)
     :stale-doc-retract-count (:stale-doc-retract-count stale-report)
     :linked-target-count (count @linked-targets)
     :dangling-target-count (count @dangling-targets)
     :linked-source-count (count @linked-sources)
     :dangling-source-count (count @dangling-sources)
     :linked-pattern-count (count @linked-patterns)
     :dangling-pattern-count (count @dangling-patterns)}))

(defn- scope-tree-files [dir selected]
  (let [selected (set selected)]
    (->> (file-seq (io/file dir))
         (filter #(.isFile %))
         (filter #(str/ends-with? (.getName %) ".json"))
         (filter (fn [f]
                   (or (empty? selected)
                       (contains? selected (str/replace (.getName f) #"\.json$" "")))))
         (sort-by #(.getName %))
         vec)))

(def ^:private default-capability-graph
  "/home/joe/code/futon0/holes/missions/M-capability-star-map.graph.edn")

(defn- capability-node-spec [cap-id cap]
  (let [nm (name cap-id)]
    {:id (str "scope/capability/" nm)
     :name (or (:title cap) nm)
     :type "scope/capability"
     :external-id (str "scope/capability/" nm)
     :props {:capability/id nm
             :capability/status (:status cap)
             :capability/pre-registered? (boolean (:pre-registered? cap))
             :capability/frontier? (boolean (:frontier cap))
             :capability/region (:region cap)
             :capability/attested? (boolean (:attested cap))
             :capability/claimed? (boolean (seq (:minted-by cap)))
             :capability/minted-by (mapv str (:minted-by cap))}}))

(defn- update-capability-graph!
  "Materialize M-capability-star-map.graph.edn into substrate-2 as the pragmatic-pole
   overlay (D2 keystone). ENRICH each capability node (id scope/capability/<cap-id> —
   the by-construction hinge D1's capability-scopes already endpoint) with the graph's
   pragmatic props (status / pre-registered? / frontier? / claimed?), CREATE nodes for
   unclaimed caps (⭐), add capability->parent ascent edges + mission->capability
   produces edges. Idempotent (deterministic ids + ensure-entity! merges props)."
  [{:keys [client base-url penholder graph-path dry-run?]}]
  (let [g (edn/read-string (slurp (or graph-path default-capability-graph)))
        caps (:capabilities g)
        missions (:missions g)
        report (atom {:capabilities 0 :claimed 0 :unclaimed 0 :ascent-edges 0 :produces-edges 0})]
    (doseq [[cap-id cap] caps]
      (let [nm (name cap-id)]
        (when-not dry-run?
          (ensure-entity! client base-url penholder (capability-node-spec cap-id cap)))
        (swap! report update :capabilities inc)
        (swap! report update (if (seq (:minted-by cap)) :claimed :unclaimed) inc)
        (doseq [parent (:scope cap)]
          (when-not dry-run?
            (post-hyperedge! client base-url penholder
                             {:hx/id (str "hx|capability-ascent|" nm "|" (name parent))
                              :hx/type "capability/ascent"
                              :hx/endpoints [{:role :capability :entity-id (str "scope/capability/" nm)}
                                             {:role :parent :entity-id (str "scope/capability/" (name parent))}]
                              :hx/labels ["capability/ascent"]
                              :props {:capability/id nm :capability/parent (name parent)}}))
          (swap! report update :ascent-edges inc))))
    (doseq [[mstem minfo] missions
            cap (:produces minfo)
            :let [nm (name cap)]]
      (when-not dry-run?
        (post-hyperedge! client base-url penholder
                         {:hx/id (str "hx|capability-produces|" mstem "|" nm)
                          :hx/type "capability/produces"
                          :hx/endpoints [{:role :mission :entity-id (str mstem)}
                                         {:role :capability :entity-id (str "scope/capability/" nm)}]
                          :hx/labels ["capability/produces"]
                          :props {:mission (str mstem) :capability/id nm
                                  :capability/status (get-in caps [(keyword nm) :status])}}))
      (swap! report update :produces-edges inc))
    @report))

(defn maintain-mission!
  "In-process D1.2 entry: re-detect + diff-patch ONE mission's scopes. No child
   JVM — honours I-0; called directly by the watcher's maintenance drainer on its
   own (off-cycle) thread. Returns the `incremental-mission!` report map."
  [stem]
  (incremental-mission! {:client (http-client)
                         :base-url default-futon1a-url
                         :penholder default-penholder
                         :mission stem}))

(defn -main [& args]
  (reset-run-caches!)
  (let [client (http-client)
        [opts missions] (loop [xs args opts {} missions []]
                          (if-let [x (first xs)]
                            (case x
                              "--binder"
                              (recur (nnext xs) (assoc opts :binder-filter (second xs)) missions)

                              "--mission"
                              (recur (nnext xs) (assoc opts :mission (second xs)) missions)

                              "--wire-parents"
                              (recur (next xs) (assoc opts :wire-parents? true) missions)

                              "--wire-pxr"
                              (recur (next xs) (assoc opts :wire-pxr? true) missions)

                              "--wire-capabilities"
                              (recur (next xs) (assoc opts :wire-capabilities? true) missions)

                              "--dry-run"
                              (recur (next xs) (assoc opts :dry-run? true) missions)

                              "--true-up"
                              (recur (next xs) (assoc opts :true-up? true) missions)

                              (recur (next xs) opts (conj missions x)))
                            [opts (remove str/blank? missions)]))
        files (scope-tree-files default-scope-dir missions)
        reports (when-not (or (:wire-parents? opts) (:wire-pxr? opts) (:wire-capabilities? opts) (:mission opts) (:true-up? opts))
                  (mapv #(ingest-scope-tree! {:client client
                                              :base-url default-futon1a-url
                                              :penholder default-penholder
                                              :binder-filter (:binder-filter opts)
                                              :path (.getAbsolutePath %)})
                        files))]
    (cond
      (:true-up? opts)
      (println (pr-str {:true-up
                        (mapv (fn [file]
                                (let [data (json/parse-string (slurp file) true)
                                      mission (:mission data)
                                      tree-binders (->> (keys (:scope-count-by-binder-type data))
                                                        (map name)
                                                        set)]
                                  (assoc (retract-absent-binder-scopes!
                                          client default-futon1a-url default-penholder
                                          mission tree-binders)
                                         :mission mission
                                         :tree-binders (vec (sort tree-binders)))))
                              files)}))

      (:mission opts)
      (println (pr-str {:incremental-mission
                        (incremental-mission! {:client client
                                               :base-url default-futon1a-url
                                               :penholder default-penholder
                                               :mission (:mission opts)
                                               :dry-run? (:dry-run? opts)})}))

      (:wire-parents? opts)
      (println (pr-str {:scope-parent-wiring
                        (update-parent-wiring! {:client client
                                                :base-url default-futon1a-url
                                                :penholder default-penholder
                                                :scope-dir default-scope-dir
                                                :selected missions
                                                :dry-run? (:dry-run? opts)})
                        :dry-run? (boolean (:dry-run? opts))}))

      (:wire-pxr? opts)
      (println (pr-str {:pxr-wiring
                        (update-pxr-wiring! {:client client
                                             :base-url default-futon1a-url
                                             :penholder default-penholder
                                             :dry-run? (:dry-run? opts)})}))

      (:wire-capabilities? opts)
      (println (pr-str {:capability-graph-wiring
                        (update-capability-graph! {:client client
                                                   :base-url default-futon1a-url
                                                   :penholder default-penholder
                                                   :dry-run? (:dry-run? opts)})}))

      :else
      (do
        (doseq [r reports]
          (println (pr-str r)))
        (println (pr-str {:mission-count (count reports)
                          :entity-count (reduce + (map :entity-count reports))
                          :hyperedge-count (reduce + (map :hyperedge-count reports))
                          :detached-count (reduce + (map :detached-count reports))
                          :list-item-anchor-count (reduce + (map :list-item-anchor-count reports))
                          :contains-line-anchor-count (reduce + (map :contains-line-anchor-count reports))
                          :scope-in-count (reduce + (map :scope-in-count reports))
                          :scope-out-count (reduce + (map :scope-out-count reports))
                          :duplicate-phase-count (reduce + (map :duplicate-phase-count reports))
                          :duplicate-heading-count (reduce + (map :duplicate-heading-count reports))
                          :legacy-hyperedge-retract-count (reduce + (map :legacy-hyperedge-retract-count reports))
                          :legacy-entity-retract-count (reduce + (map :legacy-entity-retract-count reports))
                          :legacy-doc-retract-count (reduce + (map :legacy-doc-retract-count reports))
                          :linked-target-count (reduce + (map :linked-target-count reports))
                          :dangling-target-count (reduce + (map :dangling-target-count reports))
                          :linked-source-count (reduce + (map :linked-source-count reports))
                          :dangling-source-count (reduce + (map :dangling-source-count reports))}))))))
