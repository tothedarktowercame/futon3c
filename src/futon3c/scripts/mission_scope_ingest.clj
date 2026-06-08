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

(defn- repo-name-from-path [path]
  (second (re-find #"/code/([^/]+)/" (str path))))

(defn- mission-doc-id [mission path]
  (let [repo (or (repo-name-from-path path) "mission")
        repo-key (str repo "-d")
        name (str/replace-first (str mission) #"^M-" "")]
    (str repo-key "/mission/" (slug name))))

(defn- env-name [scope]
  (or (some #(when (= "environment" (:role %)) (:name %)) (:ends scope))
      (some #(when (= "heading" (:role %)) (:title %)) (:ends scope))
      (:scope-id scope)))

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

(defn- filler-ends [ends]
  (remove #(contains? #{"entity" "environment" "heading"} (:role %)) ends))

(defn- scope-entity-spec [mission path scope]
  {:id (:scope-id scope)
   :name (env-name scope)
   :type (str "scope/" (:binder-type scope))
   :external-id (:scope-id scope)
   :source "mission-scope-tree"
   :props {:mission mission
           :mission/path path
           :scope/binder-type (:binder-type scope)
           :scope/parent (:parent scope)}})

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
           :scope/name (:name scope-entity)}
   :hx/content (:hx/content scope)
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
  [{:keys [client base-url penholder path]}]
  (let [data (json/parse-string (slurp path) true)
        mission (:mission data)
        mission-path (:path data)
        mission-id (mission-doc-id mission mission-path)
        mission-entity (ensure-entity! client base-url penholder
                                       {:id mission-id
                                        :name mission-id
                                        :type "mission/doc"
                                        :external-id mission
                                        :source "mission-scope-tree"
                                        :props {:mission/id mission
                                                :mission/path mission-path}})
        entity-ids (atom #{(:id mission-entity)})
        hx-ids (atom #{})]
    (doseq [scope (:scope-hyperedges data)]
      (let [scope-entity (ensure-entity! client base-url penholder
                                         (scope-entity-spec mission mission-path scope))
            _ (swap! entity-ids conj (:id scope-entity))
            slot-entities (->> (filler-ends (:ends scope))
                               (keep (fn [end]
                                       (when-let [spec (slot-entity-spec* client base-url mission mission-path end)]
                                         {:role (:role end)
                                          :entity (ensure-entity! client base-url penholder spec)})))
                               vec)]
        (swap! entity-ids into (map (comp :id :entity) slot-entities))
        (let [hx (post-hyperedge! client base-url penholder
                                  (scope-hyperedge mission-entity scope-entity slot-entities scope))]
          (swap! hx-ids conj (:hx/id hx)))
        (when-let [parent (:parent scope)]
          (let [hx (post-hyperedge! client base-url penholder
                                    (nesting-hyperedge mission-entity parent (:scope-id scope)))]
            (swap! hx-ids conj (:hx/id hx))))))
    {:mission mission
     :mission-entity (:id mission-entity)
     :entity-count (count @entity-ids)
     :hyperedge-count (count @hx-ids)}))

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
        missions (remove str/blank? args)
        files (scope-tree-files default-scope-dir missions)
        reports (mapv #(ingest-scope-tree! {:client client
                                            :base-url default-futon1a-url
                                            :penholder default-penholder
                                            :path (.getAbsolutePath %)})
                      files)]
    (doseq [r reports]
      (println (pr-str r)))
    (println (pr-str {:mission-count (count reports)
                      :entity-count (reduce + (map :entity-count reports))
                      :hyperedge-count (reduce + (map :hyperedge-count reports))}))))
