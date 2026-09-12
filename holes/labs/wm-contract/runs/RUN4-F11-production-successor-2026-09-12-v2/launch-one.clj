(do
  (require '[cheshire.core :as json]
           '[futon3c.wm.run4-boot :as boot])
  (let [secret-fn (ns-resolve 'futon3c.wm.run4-boot 'production-secret)
        token (secret-fn)
        client (java.net.http.HttpClient/newHttpClient)
        body (json/generate-string
              {:run4-series-ref
               "holes/labs/wm-contract/runs/RUN4-F11-production-successor-2026-09-12-v2/series-pin.edn"})
        request (-> (java.net.http.HttpRequest/newBuilder
                     (java.net.URI/create
                      "http://127.0.0.1:7070/api/alpha/wm/run4/series/step"))
                    (.header "content-type" "application/json")
                    (.header "authorization" (str "Bearer " token))
                    (.POST (java.net.http.HttpRequest$BodyPublishers/ofString body))
                    (.build))
        response (.send client request
                        (java.net.http.HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode response)
     :response (json/parse-string (.body response) true)}))
