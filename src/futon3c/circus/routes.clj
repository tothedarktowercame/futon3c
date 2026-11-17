(ns futon3c.circus.routes
  "Read-only HTTP-facing adapter for Circus QA. Maps and keyword values are
  normalized here so transport/http remains a thin Ring adapter."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.circus.qa :as qa]
            [futon3c.giraffe.run :as giraffe]))

(def default-root giraffe/default-root)
(def default-certificate-root
  "/home/joe/code/futon3c/data/circus-certificates")

(defn json-ready [x]
  (cond
    (map? x) (into {} (map (fn [[k v]] [(if (keyword? k) (name k) (str k))
                                         (json-ready v)])) x)
    (keyword? x) (name x)
    (set? x) (mapv json-ready (sort-by pr-str x))
    (sequential? x) (mapv json-ready x)
    :else x))

(defn- run-generated-at [manifest]
  (or (:generated_at manifest) (:generated-at manifest)
      (when-let [[_ stamp] (re-find #"^(\d{8}T\d{6}\.\d{3}Z)-" (str (:run-id manifest)))]
        (try
          (str (java.time.Instant/from
                (.parse (java.time.format.DateTimeFormatter/ofPattern
                         "yyyyMMdd'T'HHmmss.SSSX") stamp)))
          (catch Exception _ stamp)))))

(defn- safe-run-id? [s]
  (boolean (and (string? s) (re-matches #"[A-Za-z0-9._-]+" s))))

(defn- resolve-run [{:keys [root latest-run-fn verify-run-fn] :as _cfg} requested]
  (let [root (or root default-root)
        latest-run-fn (or latest-run-fn giraffe/newest-field-run)
        verify-run-fn (or verify-run-fn giraffe/verify-run)
        run-id (if (or (nil? requested) (= "latest" requested))
                 (:run-id (latest-run-fn root)) requested)
        dir (when (safe-run-id? run-id) (io/file root run-id))]
    (cond
      (or (nil? run-id) (nil? dir) (not (.isDirectory dir)))
      {:error {:status 404 :reason "run-not-found" :run requested}}

      :else
      (let [verification (verify-run-fn dir)]
        (if (:ok? verification)
          {:run-id run-id :run-dir (.getAbsolutePath dir)
           :manifest (edn/read-string (slurp (io/file dir "manifest.edn")))}
          {:error {:status 409 :reason "run-unverified" :run run-id
                   :verification verification}})))))

(defn- certificate [cfg run-id turn-id]
  (let [root (or (:certificate-root cfg) default-certificate-root)
        file (io/file root (str turn-id "-" run-id ".lean"))
        sidecar (io/file (str (.getAbsolutePath file) ".check.json"))]
    (if (.isFile file)
      {:path (.getAbsolutePath file)
       :check (when (.isFile sidecar) (json/parse-string (slurp sidecar)))}
      {:path nil :check nil})))

(defn- attach-certificates [cfg run-id result]
  (letfn [(attach [relation]
            (if-let [turn-id (get-in relation [:turn :id])]
              (assoc relation :certificate (certificate cfg run-id turn-id))
              relation))]
    (cond-> result
      (:items result)
      (update :items
              #(mapv (fn [item]
                       (update item :focus-relations
                               (fn [rows] (mapv attach (or rows []))))) %))
      (:focus-relations result)
      (update :focus-relations #(mapv attach %)))))

(defn- error-response [status reason & [extra]]
  {:status status :body (json-ready (merge {:status :error :reason reason} extra))})

(defn top-items-response [cfg params]
  (try
    (let [resolved (resolve-run cfg (get params "run"))]
      (if-let [error (:error resolved)]
        (error-response (:status error) (:reason error) (dissoc error :status :reason))
        (let [{:keys [run-id run-dir manifest]} resolved
              n (try (Long/parseLong (str (or (get params "n") 10)))
                     (catch Exception _ 10))
              result ((or (:qa-top-fn cfg) qa/top-items)
                      (merge (:qa-opts cfg) {:run-dir run-dir :n n}))]
          {:status 200
           :body (json-ready
                  (assoc (attach-certificates cfg run-id result)
                         :run run-id :generated_at (run-generated-at manifest)))})))
    (catch Throwable t
      (error-response 503 "qa-unavailable"
                      {:message (.getMessage t)
                       :failure (some-> t ex-data :reason)}))))

(defn why-not-response [cfg params]
  (if (str/blank? (get params "item"))
    (error-response 400 "item-required")
    (try
      (let [resolved (resolve-run cfg (get params "run"))]
        (if-let [error (:error resolved)]
          (error-response (:status error) (:reason error) (dissoc error :status :reason))
          (let [{:keys [run-id run-dir manifest]} resolved
                result ((or (:qa-why-fn cfg) qa/why-not)
                        (merge (:qa-opts cfg)
                               {:run-dir run-dir :item (get params "item")}))]
            {:status 200
             :body (json-ready
                    (assoc (attach-certificates cfg run-id result)
                           :run run-id :generated_at (run-generated-at manifest)))})))
      (catch Throwable t
        (error-response 503 "qa-unavailable"
                        {:message (.getMessage t)
                         :failure (some-> t ex-data :reason)})))))
