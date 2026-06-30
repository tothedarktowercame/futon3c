#!/usr/bin/env bb
;; O4 lander (C-cascade-real, claude-10): POST the canonical cluster entities +
;; cluster-member hyperedges to :7071 (the gated run-write! path), idempotent.
;; Reads o4-land-payloads.edn. penholder=claude-10. NEVER restarts the JVM.
;; Usage: bb futon3c/scripts/o4_land.bb [--smoke]   (--smoke = POST 1 entity + 1 edge only)
(require '[babashka.http-client :as http] '[clojure.edn :as edn])

(def base "http://localhost:7071")
(def penholder "api")   ;; L3 allow-listed; attribution lives in :props (:o4/generated, :o4/by)
(def payloads (edn/read-string (slurp "/home/joe/code/futon3c/holes/excursions/o4-land-payloads.edn")))

(defn post! [path payload]
  (let [resp (try (http/post (str base path)
                             {:headers {"Content-Type" "application/edn" "Accept" "application/edn"}
                              :body (pr-str (assoc payload :penholder penholder))
                              :throw false})
                  (catch Exception e {:status -1 :body (.getMessage e)}))]
    {:ok? (<= 200 (:status resp) 299) :status (:status resp)
     :body (when-not (<= 200 (:status resp) 299) (subs (str (:body resp)) 0 (min 240 (count (str (:body resp))))))}))

(defn land! [path items]
  (reduce (fn [acc p]
            (let [r (post! path p)]
              (when-not (:ok? r) (println "  FAIL" (:status r) (:body r)))
              (-> acc (update (if (:ok? r) :ok :fail) inc))))
          {:ok 0 :fail 0} items))

(let [smoke? (some #{"--smoke"} *command-line-args*)
      ents (:entities payloads) hxs (:hyperedges payloads)]
  (println "O4 land — penholder" penholder "| entities" (count ents) "| hyperedges" (count hxs)
           (if smoke? "| SMOKE (1+1)" ""))
  (if smoke?
    (do (println "entity:" (post! "/api/alpha/entity" (first ents)))
        (println "hyperedge:" (post! "/api/alpha/hyperedge" (first hxs))))
    (let [e (land! "/api/alpha/entity" ents)
          h (land! "/api/alpha/hyperedge" hxs)]
      (println "entities  →" e)
      (println "hyperedges →" h)
      (println (if (and (zero? (:fail e)) (zero? (:fail h))) "LAND OK ✅" "LAND had failures ❌")))))
