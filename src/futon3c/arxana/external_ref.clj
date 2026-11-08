(ns futon3c.arxana.external-ref
  "Resolve typed references without turning a cache into source authority."
  (:require [clojure.java.io :as io]
            [futon3c.arxana.digest :as digest])
  (:import [java.net URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]))

(defn- result [status reference & [extra]]
  (merge {:status status :reference/id (:id reference)} extra))

(defn resolve-reference
  ([reference] (resolve-reference reference (HttpClient/newHttpClient)))
  ([reference ^HttpClient client]
   (try
     (let [[bytes schema]
           (case (:ref/kind reference)
             :file
             [(java.nio.file.Files/readAllBytes (.toPath (io/file (:store/endpoint reference))))
              :file/sha256]

             :http-record
             (let [separator (if (.contains ^String (:store/endpoint reference) "?") "&" "?")
                   url (str (:store/endpoint reference) separator "id="
                            (URLEncoder/encode (str (:record/id reference)) StandardCharsets/UTF_8))
                   request (-> (HttpRequest/newBuilder (URI/create url)) .GET .build)
                   response (.send client request (HttpResponse$BodyHandlers/ofByteArray))]
               (if (<= 200 (.statusCode response) 299)
                 [(.body response)
                  (some-> (.firstValue (.headers response) "x-schema-authority")
                          (.orElse nil) keyword)]
                 (throw (ex-info "authority unavailable" {:status (.statusCode response)}))))

             (throw (ex-info "unsupported reference kind" {:kind (:ref/kind reference)})))
           actual (digest/sha256-hex bytes)]
       (cond
         (not= schema (:schema/authority reference))
         (result :schema-drift reference {:expected (:schema/authority reference)
                                          :actual schema})
         (not= actual (:revision/digest reference))
         (result :digest-mismatch reference {:expected (:revision/digest reference)
                                              :actual actual})
         :else (result :ok reference {:bytes bytes :revision/digest actual})))
     (catch Throwable e
       (result :unavailable reference {:reason (.getMessage e)})))))
