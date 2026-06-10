(ns futon3c.scripts.mission-scope-view
  "Read-only JSON projection of substrate-2 mission scopes for Emacs."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str])
  (:import (java.net URI URLEncoder)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
           (java.time Instant)))

(def ^:private default-futon1a-url "http://localhost:7071")

(def structural-binders
  ["eightfold-phase" "loose-section" "capability-scope" "map-item"
   "relates-to" "source-material" "mission-scope-in" "mission-scope-out"
   "pattern" "psr" "pur" "plain-argument"])

(defn- url-encode [s]
  (URLEncoder/encode (str s) "UTF-8"))

(defn- http-edn [client url]
  (let [req (-> (HttpRequest/newBuilder (URI/create url))
                (.GET)
                (.header "accept" "application/edn")
                (.build))
        resp (.send client req (HttpResponse$BodyHandlers/ofString))
        body (.body resp)]
    (when-not (<= 200 (.statusCode resp) 299)
      (throw (ex-info "substrate-2 request failed"
                      {:status (.statusCode resp) :url url :body body})))
    (when (seq body)
      (edn/read-string body))))

(defn- hyperedges-by-type [client base-url binder]
  (let [hx-type (str "mission-scope/" binder)
        url (str (str/replace base-url #"/$" "")
                 "/api/alpha/hyperedges?type=" (url-encode hx-type)
                 "&limit=5000")]
    (-> (http-edn client url)
        :hyperedges
        (or []))))

(defn- first-line [s]
  (when (seq (str/trim (str s)))
    (first (str/split-lines (str/trim (str s))))))

(defn- kw-name [x]
  (cond
    (keyword? x) (name x)
    (nil? x) nil
    :else (str x)))

(defn scope-row [h]
  (let [props (:hx/props h)
        binder (or (:scope/binder-type props)
                   (some-> (:hx/type h) name))
        passage (first-line (:anchor/passage props))
        title (or (:scope/name props)
                  (:scope/heading-slug props)
                  (:scope/id props))]
    {:id (:scope/id props)
     :type binder
     :title title
     :mission (:mission props)
     :parent (:scope/parent props)
     :parent_state (kw-name (:scope/parent-state props))
     :anchor_state (kw-name (:anchor/state props))
     :anchor_resolve_by (kw-name (:anchor/resolve-by props))
     :canonical_phase (:scope/canonical-phase props)
     :canonical_id (:scope/canonical-id props)
     :passage passage
     :hx_id (:hx/id h)
     :target_mission (:target/mission props)
     :target_state (kw-name (:target/state props))
     :source_ref (:source/ref props)
     :source_state (kw-name (:source/state props))
     :polarity (kw-name (:scope/polarity props))}))

(defn project-hyperedges [mission hyperedges]
  (let [rows (->> hyperedges
                  (map scope-row)
                  (filter #(or (nil? mission)
                               (= mission (:mission %))))
                  (sort-by (juxt #(or (:mission %) "")
                                 #(.indexOf structural-binders (:type %))
                                 #(or (:parent %) "")
                                 #(or (:id %) "")))
                  vec)]
    {:mission mission
     :generated_at (str (Instant/now))
     :scope_count (count rows)
     :type_counts (->> rows
                       (group-by :type)
                       (map (fn [[type xs]] {:type type :count (count xs)}))
                       (sort-by #(.indexOf structural-binders (:type %)))
                       vec)
     :scopes rows}))

(defn- usage []
  (str "Usage: clojure -M -m futon3c.scripts.mission-scope-view"
       " --mission M-id [--base-url http://localhost:7071]\n"))

(defn- parse-args [args]
  (loop [opts {:base-url default-futon1a-url}
         xs args]
    (case (first xs)
      nil opts
      "--mission" (recur (assoc opts :mission (second xs)) (nnext xs))
      "--base-url" (recur (assoc opts :base-url (second xs)) (nnext xs))
      "--help" (assoc opts :help true)
      "-h" (assoc opts :help true)
      (throw (ex-info "unknown argument" {:arg (first xs) :usage (usage)})))))

(defn -main [& args]
  (let [{:keys [mission base-url help]} (parse-args args)]
    (when help
      (print (usage))
      (System/exit 0))
    (when-not (seq mission)
      (binding [*out* *err*]
        (print (usage)))
      (System/exit 2))
    (let [client (HttpClient/newHttpClient)
          hyperedges (mapcat #(hyperedges-by-type client base-url %) structural-binders)]
      (println (json/generate-string
                (assoc (project-hyperedges mission hyperedges)
                       :base_url base-url))))))
