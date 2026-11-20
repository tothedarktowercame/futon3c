(ns futon3c.arxana.together-structure-http
  "Durable serialised workspace acts, isolated per server and per workspace."
  (:require [cheshire.factory :as json-factory]
            [cheshire.parse :as json-parse]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.arxana.together-structure :as structure])
  (:import [com.fasterxml.jackson.core JsonFactory]
           [java.io PushbackReader StringReader]
           [java.net URLDecoder]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.time Instant]))

(defn- load-hub [file valid-identity?]
  (let [text (if (.exists ^java.io.File file)
               (Files/readString (.toPath ^java.io.File file) StandardCharsets/UTF_8) "")]
    (when (and (seq text) (not (str/ends-with? text "\n")))
      (throw (ex-info "Incomplete structure log line" {})))
    (let [entries (mapv (fn [line]
                          (with-open [r (PushbackReader. (StringReader. line))]
                            (let [entry (edn/read {:eof ::eof} r)]
                              (when-not (= ::eof (edn/read {:eof ::eof} r))
                                (throw (ex-info "Extra data in structure log line" {})))
                              entry)))
                        (if (empty? text) [] (str/split-lines text)))]
      ;; Validate refused claims too: a corrupt refusal must not silently skip an act.
      (reduce (fn [hub entry]
                (when-not (and (map? entry)
                               (contains? #{#{:rev :op :author :at} #{:rev :op :author :at :refused}}
                                          (set (keys entry)))
                               (= (:rev entry) (inc (count (:log hub))))
                               (valid-identity? (:author entry)))
                  (throw (ex-info "Invalid structure log entry/revision" {})))
                (Instant/parse (:at entry))
                (let [result (structure/submit hub (:op entry) (:author entry) (:at entry))]
                  (when-not (= entry (:entry result))
                    (throw (ex-info "Structure log refusal differs from replay" {})))
                  (:hub result)))
              {:state structure/empty-state :log []} entries)
      {:state (structure/rebuild entries) :log entries})))

(defn- workspace! [{:keys [structures dir]} key valid-identity?]
  (locking structures
    (or (get @structures key)
        (let [file (io/file dir "structure" (str key ".log.edn"))
              workspace (atom (try {:hub (load-hub file valid-identity?) :file file}
                                   (catch Exception e
                                     {:file file :error (str "Damaged structure log: " (ex-message e))})))]
          (swap! structures assoc key workspace)
          workspace))))

(defn- parse-op [request]
  (try
    (let [body (with-open [parser (.createParser ^JsonFactory json-factory/json-factory
                                                ^String (slurp (:body request) :encoding "UTF-8"))]
                 (let [value (json-parse/parse-strict parser true nil nil)]
                   (when (.nextToken parser)
                     (throw (ex-info "Trailing JSON value" {})))
                   value))
          raw (:op body)
          op (when (and (map? raw) (string? (:op raw))) (update raw :op keyword))]
      (when (and (map? body) (= #{:op} (set (keys body))) op
                 (not= :invalid-op (structure/check-op structure/empty-state op)))
        op))
    (catch Exception _ nil)))

(defn- after [request]
  (let [params (into {} (map #(let [[k v] (str/split % #"=" 2)]
                               [(URLDecoder/decode k "UTF-8")
                                (URLDecoder/decode (or v "") "UTF-8")])
                            (remove empty? (str/split (or (:query-string request) "") #"&"))))
        raw (get params "after" "0")]
    (when (re-matches #"[0-9]+" raw) (parse-long raw))))

(defn- submit! [workspace op editor response append-entry!]
  (let [result (structure/submit (:hub @workspace) op editor (str (Instant/now)))
        entry (:entry result)]
    (try
      (Files/createDirectories (.toPath (.getParentFile ^java.io.File (:file @workspace)))
                               (make-array java.nio.file.attribute.FileAttribute 0))
      (append-entry! (:file @workspace) entry)
      (swap! workspace assoc :hub (:hub result))
      (response (if (:refused entry) 409 200)
                (cond-> {:rev (:rev entry)} (:refused entry) (assoc :error (name (:refused entry)))))
      (catch Exception e
        (let [error (str "Structure append failed: " (ex-message e))]
          (swap! workspace assoc :error error)
          (response 500 {:error error}))))))

(defn handle
  "Handle a structure request using SERVER's workspace map and shared helpers.
  Helpers are supplied by the listener, avoiding a circular namespace dependency."
  [server request {:keys [response valid-identity? valid-name? append-entry!]}]
  (let [[_ key action] (re-matches #"/together/v1/structure/(.*)/(state|acts|log)" (:uri request))
        editor (get-in request [:headers "x-together-editor"])]
    (cond
      (not (valid-identity? editor)) (response 401 {:error "invalid-editor"})
      (or (nil? action) (not= (:request-method request) (if (= action "acts") :post :get)))
      (response 404 {:error "not-found"})
      (not (valid-name? key 120)) (response 400 {:error "invalid-key"})
      :else
      (let [workspace (workspace! server key valid-identity?)]
        (locking workspace
          (if-let [error (:error @workspace)]
            (response 500 {:error error})
            (let [{:keys [state log]} (:hub @workspace)]
              (case action
                "state" (response 200 (assoc state :key key :rev (count log)
                                            :parents (into {} (keep (fn [[child parents]]
                                                                      (when (seq parents) [child (vec (sort parents))]))
                                                                    (:parents state)))))
                "acts" (if-let [op (parse-op request)]
                         (submit! workspace op editor response append-entry!)
                         (response 400 {:error "invalid-body"}))
                "log" (let [n (try (after request) (catch Exception _ nil))]
                        (if (some? n)
                          (response 200 {:key key :rev (count log) :entries (filterv #(> (:rev %) n) log)})
                          (response 400 {:error "invalid-after"})))))))))))
