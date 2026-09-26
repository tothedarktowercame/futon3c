(ns retry-acceptance
  "Live acceptance for the bounded append retry (Dionysus test site).

  Issues one real evidence append against a futon1b that the operator stops
  and restarts underneath it. Prints timestamped markers so the orchestrating
  shell can assert that the caller never saw :ok while the store was down."
  (:require [futon3c.evidence.backend :as backend]
            [futon3c.evidence.futon1b-backend :as sut]))

(defn -main [& _]
  (let [store (sut/make-futon1b-backend "http://127.0.0.1:7073")
        id (str "e-retry-acceptance-" (System/currentTimeMillis))
        entry {:evidence/id id
               :evidence/type :coordination
               :evidence/claim-type :observation
               :evidence/author "joe"
               :evidence/at (str (java.time.Instant/now))
               :evidence/body {:event "append-retry-live-acceptance"}
               :evidence/tags [:smoketest]}
        t0 (System/currentTimeMillis)]
    (println "APPEND-START" id)
    (flush)
    (let [r (backend/-append store entry)
          dt (- (System/currentTimeMillis) t0)]
      (println "APPEND-RETURNED-AFTER-MS" dt)
      (println "APPEND-OK" (boolean (:ok r)))
      (println "APPEND-ERROR-CODE" (pr-str (:error/code r)))
      (println "APPEND-ATTEMPTS" (pr-str (or (:attempts r) (get-in r [:error/context :attempts]))))
      (println "APPEND-ID" id)
      (flush))))
