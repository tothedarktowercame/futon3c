(ns futon3c.scripts.mission-scope-ingest
  "Ingest mission scope-tree JSON into futon1a as scope entities and hyperedges."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
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

(defn- anchor-text [scope]
  (case (:binder-type scope)
    "map-item" (map-item-title scope)
    "relates-to" (or (target-mission-ident scope) (heading-title scope))
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

(defn- anchor-for-scope [mission-path scopes scope]
  (let [heading (anchor-text scope)
        passage-info (case (:binder-type scope)
                       "map-item" (list-item-passage-info mission-path heading)
                       "relates-to" (list-item-passage-info mission-path heading)
                       (when-let [passage (heading-passage mission-path heading)]
                         {:passage passage
                          :source :heading}))
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

(defn- stable-relates-to-scope-id [mission target-ident original-id duplicate?]
  (let [stem (str/replace-first (str mission) #"^M-" "")
        target-stem (mission-stem target-ident)
        base (str stem "/relates-to/" target-stem)]
    (if duplicate?
      (str base "--" (subs (sha1 (str target-ident "|" original-id)) 0 8))
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
                   :target/detached-reason (:target-detached-reason scope)))})

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
        entity-ids (atom #{(:id mission-entity)})
        hx-ids (atom #{})
        linked-targets (atom #{})
        dangling-targets (atom #{})]
    (doseq [scope scopes]
      (let [scope-entity (ensure-entity! client base-url penholder
                                         (scope-entity-spec mission mission-path scope))
            _ (swap! entity-ids conj (:id scope-entity))
            target-entity (when (= "relates-to" (:binder-type scope))
                            (resolve-target-mission client base-url (:target-mission-ident scope)))
            scope (cond-> scope
                    (= "relates-to" (:binder-type scope))
                    (assoc :target-state (if target-entity :linked :detached)
                           :target-detached-reason (when-not target-entity :target-mission-not-found)))
            _ (when (= "relates-to" (:binder-type scope))
                (if target-entity
                  (swap! linked-targets conj (:id target-entity))
                  (swap! dangling-targets conj (:target-mission-ident scope))))
            scope-entity (if (= "relates-to" (:binder-type scope))
                           (ensure-entity! client base-url penholder
                                           (scope-entity-spec mission mission-path scope))
                           scope-entity)
            _ (swap! entity-ids conj (:id scope-entity))
            filler-ends (cond->> (filler-ends (:ends scope))
                          (= "relates-to" (:binder-type scope))
                          (remove #(= "mission" (:role %))))
            slot-entities (->> filler-ends
                               (keep (fn [end]
                                       (when-let [spec (slot-entity-spec* client base-url mission mission-path end)]
                                         {:role (:role end)
                                          :entity (ensure-entity! client base-url penholder spec)})))
                               (concat (when target-entity
                                         [{:role "target-mission"
                                           :entity target-entity}]))
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
     :duplicate-phase-count (count (filter :duplicate-phase? scopes))
     :duplicate-heading-count (count (filter :duplicate-heading? scopes))
     :legacy-hyperedge-retract-count (:legacy-hyperedge-retract-count legacy-report)
     :legacy-entity-retract-count (:legacy-entity-retract-count legacy-report)
     :legacy-doc-retract-count (:legacy-doc-retract-count legacy-report)
     :linked-target-count (count @linked-targets)
     :dangling-target-count (count @dangling-targets)}))

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

(defn -main [& args]
  (let [client (http-client)
        [opts missions] (loop [xs args opts {} missions []]
                          (if-let [x (first xs)]
                            (if (= "--binder" x)
                              (recur (nnext xs) (assoc opts :binder-filter (second xs)) missions)
                              (recur (next xs) opts (conj missions x)))
                            [opts (remove str/blank? missions)]))
        files (scope-tree-files default-scope-dir missions)
        reports (mapv #(ingest-scope-tree! {:client client
                                            :base-url default-futon1a-url
                                            :penholder default-penholder
                                            :binder-filter (:binder-filter opts)
                                            :path (.getAbsolutePath %)})
                      files)]
    (doseq [r reports]
      (println (pr-str r)))
    (println (pr-str {:mission-count (count reports)
                      :entity-count (reduce + (map :entity-count reports))
                      :hyperedge-count (reduce + (map :hyperedge-count reports))
                      :detached-count (reduce + (map :detached-count reports))
                      :list-item-anchor-count (reduce + (map :list-item-anchor-count reports))
                      :contains-line-anchor-count (reduce + (map :contains-line-anchor-count reports))
                      :duplicate-phase-count (reduce + (map :duplicate-phase-count reports))
                      :duplicate-heading-count (reduce + (map :duplicate-heading-count reports))
                      :legacy-hyperedge-retract-count (reduce + (map :legacy-hyperedge-retract-count reports))
                      :legacy-entity-retract-count (reduce + (map :legacy-entity-retract-count reports))
                      :legacy-doc-retract-count (reduce + (map :legacy-doc-retract-count reports))
                      :linked-target-count (reduce + (map :linked-target-count reports))
                      :dangling-target-count (reduce + (map :dangling-target-count reports))}))))
