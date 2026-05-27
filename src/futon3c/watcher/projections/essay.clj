(ns futon3c.watcher.projections.essay
  "Project essay-home markdown + sibling annotations.edn into a stable
   essay/section/annotation shape for watcher ingest."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def src-exts #{"md"})

(def ^:private essay-home-rx
  #"(?:^|.*/)futon[^/]+/essays/([^/]+)/([^/]+)\.md$")

(defn- normalize-path
  [path]
  (str/replace (str path) "\\" "/"))

(defn- essay-home-match
  [path]
  (when-let [[_ slug file-slug] (re-matches essay-home-rx (normalize-path path))]
    (when (= slug file-slug)
      {:slug slug
       :file-slug file-slug})))

(defn essay-home-md?
  "True iff PATH is a markdown essay living at
   .../futonN/essays/<slug>/<slug>.md."
  [path]
  (boolean (essay-home-match path)))

(defn- annotations-path
  [path]
  (.getAbsolutePath
   (io/file (.getParentFile (io/file path)) "annotations.edn")))

(defn- read-text
  [path]
  (slurp path))

(defn- read-edn-file
  [path]
  (edn/read-string (slurp path)))

(defn- markdown-title
  [text]
  (some-> (re-find #"(?m)^#\s+(.+?)\s*$" text)
          second
          str/trim))

(defn- section-number
  [index]
  (when (seq index)
    (str/join "." index)))

(defn- section-display-name
  [{:keys [name index]}]
  (if-let [n (section-number index)]
    (str "§" n ". " name)
    name))

(defn- endpoint->projection
  [endpoint]
  (let [entity-id (or (:entity-id endpoint)
                      (:section-id endpoint)
                      (:pattern-name endpoint))]
    (when-not entity-id
      (throw (ex-info "annotation endpoint missing entity reference"
                      {:endpoint endpoint})))
    (assoc endpoint :entity-id entity-id)))

(defn- essay-display-name
  [title annotations]
  (let [generated (:generated annotations)]
    (cond
      (and title generated) (str title " (" generated ")")
      title title
      :else (:essay-id annotations))))

(defn- essay-props
  [source-file annotations-file title annotations sections]
  (cond-> {:label title
           :source-file source-file
           :annotations-file annotations-file
           :paper (:paper annotations)
           :generated (:generated annotations)
           :version (:version annotations)
           :section-count (count sections)
           :annotation-count (count (:annotations annotations))
           :rewrite-pass-status (:rewrite-pass-status annotations)}
    (:survey-pass-by annotations) (assoc :survey-pass-by (:survey-pass-by annotations))
    (:system annotations) (assoc :system (:system annotations))
    (:invariant-audit annotations) (assoc :invariant-audit (:invariant-audit annotations))
    (:rewrite-pass-queue annotations) (assoc :rewrite-pass-queue (:rewrite-pass-queue annotations))))

(defn- section->entity
  [essay-id section]
  {:id (:id section)
   :name (section-display-name section)
   :type "arxana/essay-section"
   :props (merge {:essay-id essay-id
                  :heading-text (:name section)}
                 (dissoc section :id :name))})

(defn- annotation->hyperedge
  [annotation]
  {:id (:id annotation)
   :hx-type (:hx-type annotation)
   :endpoints (mapv endpoint->projection (:endpoints annotation))
   :props (dissoc annotation :id :hx-type :endpoints)})

(defn collect-file
  "Project one essay-home markdown file + sibling annotations.edn.
   Returns {:essay {...} :sections [...] :annotations [...]}.
   Throws when the essay-home shape matches but the sibling EDN is absent."
  [path]
  (when (essay-home-md? path)
    (let [source-file (.getAbsolutePath (io/file path))
          annotations-file (annotations-path source-file)]
      (when-not (.exists (io/file annotations-file))
        (throw (ex-info "essay annotations.edn missing"
                        {:path source-file
                         :annotations-file annotations-file})))
      (let [markdown (read-text source-file)
            annotations (read-edn-file annotations-file)
            title (markdown-title markdown)
            sections (vec (:sections annotations))
            annotation-hyperedges (mapv annotation->hyperedge (:annotations annotations))]
        {:essay {:id (:essay-id annotations)
                 :name (essay-display-name title annotations)
                 :type "arxana/essay"
                 :source-file source-file
                 :props (essay-props source-file annotations-file title annotations sections)}
         :sections (mapv (partial section->entity (:essay-id annotations)) sections)
         :annotations annotation-hyperedges}))))
