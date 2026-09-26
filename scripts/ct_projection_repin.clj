;; Re-pin test/futon3c/diagramprover/fixtures/wm-flight-ct-projection.edn.
;;
;;   clojure -M:test -i scripts/ct_projection_repin.clj -e '(repin! {})'
;;
;; Options: :map-sha (default futon3c HEAD's last commit touching the map),
;; :out (default the fixture path). Reads the map and futon5's
;; src/futon5/ct/mission.clj from git (read-only), computes the value the
;; test compares, and writes it with ct-projection/write-fixture!, which
;; keeps the file's `;;` header and appends one re-pin line.
(require '[clojure.edn :as edn]
         '[clojure.java.shell :as sh]
         '[clojure.string :as str]
         '[futon3c.diagramprover.ct-projection :as proj])

(def fixture-path "test/futon3c/diagramprover/fixtures/wm-flight-ct-projection.edn")
(def map-path "holes/labs/M-wm-wiring/wm-flight-wiring.edn")
(def futon5-sha "aeeee96")

(defn- git [& args]
  (let [{:keys [exit out err]} (apply sh/sh "git" args)]
    (when-not (zero? exit) (throw (ex-info "git failed" {:args args :err err})))
    out))

(defn repin! [{:keys [map-sha out] :or {out fixture-path}}]
  (let [map-sha (or map-sha (str/trim (git "log" "-1" "--format=%h" "--" map-path)))
        projector-sha (str/trim (git "log" "-1" "--format=%h" "--"
                                     "src/futon3c/diagramprover/ct_projection.clj"))
        _ (load-string (git "-C" "../futon5" "show" (str futon5-sha ":src/futon5/ct/mission.clj")))
        diagram (resolve 'futon5.ct.mission/mission-diagram)
        validate (resolve 'futon5.ct.mission/validate)
        m (edn/read-string (git "show" (str map-sha ":" map-path)))
        d (diagram (proj/project m))
        value {:map-path map-path
               :map-sha map-sha
               :futon5-sha futon5-sha
               :counts {:inputs (count (get-in d [:ports :input]))
                        :outputs (count (get-in d [:ports :output]))
                        :components (count (:components d))
                        :edges (count (:edges d))}
               :validate (validate d)
               :i4 (proj/i4-report m {})}]
    (proj/write-fixture! out value {:map-sha map-sha :projector-sha projector-sha
                                    :date (str (java.time.LocalDate/now java.time.ZoneOffset/UTC))})
    {:out out :map-sha map-sha :projector-sha projector-sha}))
