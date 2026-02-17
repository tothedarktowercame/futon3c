(ns scripts.encyclopedia-demo
  "First refactor demo: Futon3c encyclopedia endpoints backed by local PlanetMath EDN."
  (:require [futon3c.transport.http :as http]))

(defn- env [k default]
  (or (System/getenv k) default))

(defn- parse-int [s default]
  (try
    (Integer/parseInt (str s))
    (catch Exception _
      default)))

(defn -main [& _args]
  (let [port (parse-int (env "FUTON3C_PORT" "5050") 5050)
        corpus-root (env "FUTON3C_PLANETMATH_ROOT" "/home/joe/code/planetmath")
        handler (http/make-handler {:registry {:agents {}}
                                    :patterns {:patterns/ids []}
                                    :encyclopedia {:corpus-root corpus-root}})
        {:keys [server]} (http/start-server! handler port)]
    (println (str "futon3c encyclopedia demo running at http://localhost:" port))
    (println (str "corpus root: " corpus-root))
    (println (str "try: curl -s http://localhost:" port "/fulab/encyclopedia/corpuses | jq ."))
    (println "press Enter to stop")
    (try
      (read-line)
      (finally
        (server)
        (println "stopped")))))
