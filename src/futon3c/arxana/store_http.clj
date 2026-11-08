(ns futon3c.arxana.store-http
  "HTTP implementation of the narrow D1 store adapter seam.

  Only the authenticated conditional command is implemented in increment 2.
  Other methods refuse honestly until their versioned Arxana routes exist;
  this adapter never falls back to generic writes for reserved records."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.store :as store])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]))

(def http-capabilities
  {:entity-read false
   :entity-write false
   :entity-as-of false
   :entity-compare-and-set true
   :hyperedge-read false
   :hyperedge-write false
   :hyperedge-valid-time false
   :subscription false
   :scope-family-census false})

(defn- unsupported [operation]
  {:error (env/error :unsupported :arxana.store/http-operation-unavailable
                     {:operation operation}
                     nil)})

(defn- category-for-status [status body]
  (or (get-in body [:error :context :category])
      (case status
        400 :invalid
        401 :forbidden
        403 :forbidden
        404 :missing
        409 :conflict
        503 :unavailable
        :internal)))

(defrecord HttpStore [base-url principal ^HttpClient client]
  store/ArxanaGraphStore
  (get-entity [_ entity-id]
    (let [request (-> (HttpRequest/newBuilder
                       (URI/create (str (str/replace base-url #"/$" "")
                                        "/api/alpha/arxana/query")))
                      (.header "Accept" "application/edn")
                      (.header "Content-Type" "application/edn")
                      (.header "x-arxana-principal" principal)
                      (.POST (HttpRequest$BodyPublishers/ofString
                              (pr-str {:query/type :record-by-id
                                       :record-id entity-id})))
                      .build)]
      (try
        (let [response (.send client request (HttpResponse$BodyHandlers/ofString))
              status (.statusCode response) body (edn/read-string (.body response))]
          (cond
            (not (<= 200 status 299))
            {:error (env/error (category-for-status status body)
                               :arxana.store/query-http-error body nil)}
            (= :ok (:status body)) {:ok (:record body)}
            (= :missing (:status body))
            {:error (env/error :missing :arxana.store/record-not-found
                               {:entity/id entity-id} nil)}
            :else {:error (env/error :failed :arxana.store/query-invalid-response
                                     body nil)}))
        (catch Exception e
          {:error (env/error :unavailable :arxana.store/http-unavailable
                             {:message (.getMessage e)} [:retry])}))))
  (find-entities [_ _] (unsupported :entity/query))
  (put-entity! [_ _] (unsupported :entity/put))
  (get-hyperedge [_ _] (unsupported :hyperedge/get))
  (find-hyperedges [_ _] (unsupported :hyperedge/query))
  (put-hyperedges! [_ _] (unsupported :hyperedge/put))
  (retract-hyperedge! [_ _ _] (unsupported :hyperedge/retract))
  (capabilities [_] http-capabilities)
  (conditional-write! [_ command]
    (let [request (-> (HttpRequest/newBuilder
                       (URI/create (str (str/replace base-url #"/$" "")
                                        "/api/alpha/arxana/command")))
                      (.header "Accept" "application/edn")
                      (.header "Content-Type" "application/edn")
                      (.header "x-arxana-principal" principal)
                      (.POST (HttpRequest$BodyPublishers/ofString (pr-str command)))
                      .build)]
      (try
        (let [response (.send client request (HttpResponse$BodyHandlers/ofString))
              status (.statusCode response)
              body (edn/read-string (.body response))]
          (if (<= 200 status 299)
            body
            {:error (env/error (category-for-status status body)
                               (or (get-in body [:error :context :code])
                                   (get-in body [:error :reason])
                                   :arxana.store/http-error)
                               {:http/status status
                                :futon1b/error (:error body)}
                               (get-in body [:error :context :recoverable :choices]))}))
        (catch Exception e
          {:error (env/error :unavailable :arxana.store/http-unavailable
                             {:message (.getMessage e) :base-url base-url}
                             [:retry])}))))
  (bounded-query! [_ query]
    (let [request (-> (HttpRequest/newBuilder
                       (URI/create (str (str/replace base-url #"/$" "")
                                        "/api/alpha/arxana/query")))
                      (.header "Accept" "application/edn")
                      (.header "Content-Type" "application/edn")
                      (.header "x-arxana-principal" principal)
                      (.POST (HttpRequest$BodyPublishers/ofString (pr-str query)))
                      .build)]
      (try
        (let [response (.send client request (HttpResponse$BodyHandlers/ofString))
              status (.statusCode response) body (edn/read-string (.body response))]
          (if (<= 200 status 299) {:ok body}
              {:error (env/error (category-for-status status body)
                                 :arxana.store/query-http-error body nil)}))
        (catch Exception e
          {:error (env/error :unavailable :arxana.store/http-unavailable
                             {:message (.getMessage e)} [:retry])}))))
  )

(defn http-store
  ([base-url principal]
   (http-store base-url principal (HttpClient/newHttpClient)))
  ([base-url principal client]
   (->HttpStore base-url principal client)))
