(ns futon3c.diagramprover.wm-wire-target-products-16a
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-driver :as driver]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.observation-checks :as checks]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as r9]))

(def targets ["M-first" "M-second"])
(defn readback [root value]
  (let [file (io/file root "product.edn")]
    (spit file (pr-str value)) (edn/read-string (slurp file))))

(defn products [reader]
  (let [root (w/tmp-dir "target-16a-")
        text "# Mission\nBuild the artifact.\n"
        f (flight/start (driver/resolve-target {:chosen-target (first targets) :draw-seed 42})
                        {:kind :operator-declared :wants [:done] :declared-by "fixture"}
                        {:id "target-16a" :at "fixture"})
        f (cond-> f (= reader :read)
            (assoc :want-source {:kind :a-exits :repo "fixture" :path "mission.md"
                                 :store root :read-text (fn [& _] text)}))
        fs [f (assoc f :target (second targets))]]
    (try
      {:flights fs
       :products
       (mapv
        (fn [f]
          (case reader
            :run
            ;; run!:533 selects locators by target. Observe them with real C3,
            ;; so the changed closure is not supplied by a target-aware stub.
            (let [repo (-> (io/resource "futon2/aif/flight.clj") io/file
                           .getParentFile .getParentFile .getParentFile .getParentFile str)
                  locator {:class :C3 :repo (.getName (io/file repo)) :sha "HEAD"}
                  sources {:locators {"M-first" {:done (assoc locator :path "src/futon2/aif/flight.clj")}
                                      "M-second" {:done (assoc locator :path "absent-16a-file")}}}]
              (readback root
                        (flight/run! f {:sources-fn (constantly sources) :max-clicks 1
                                        :observe-fn (fn [_ locators]
                                                      (with-redefs [checks/repo-root (str (.getParentFile (io/file repo)))]
                                                        (into {} (map (fn [[t l]] [t (:observed (checks/check-path-exists l))])) locators)))
                                        :click-fn (fn [_] {:click-id "click" :unreached-wants []})})))
            :enact
            (let [dir (io/file root "enactments")
                  path (io/file dir "target-16a-click.edn")
                  _ (when (.exists path) (io/delete-file path))
                  r ((fr/enact-fn {:record-dir (str dir) :trace-dir (str (io/file root "trace"))
                                   :repo-root root
                                   :dispatch-step! (fn [_] {:failed {:reason :fixture-no-execution}})})
                     f {:click-id "click" :chosen {:id :action :candidate :C1 :precedence [:p]}})]
              {:path (:record-path r) :record (edn/read-string (slurp (:record-path r)))})
            :read
            (readback root ((fr/read-fn {:store root :read-text (fn [& _] text)
                                         :answer-fn (fn [_] {:state "pending" :seat "fixture"})}) f {}))
            :publication
            ;; No repair-id authority supplied: target is carried on the typed
            ;; absence, not looked up by this reader (flight_runner:736-739).
            (readback root ((fr/observe-publication-fn {}) f {:click-id "click"}))
            :close
            (let [run runner/run-opportunity!
                  r (with-redefs [runner/run-opportunity! (fn [opts] (run (assoc opts :flight f)))]
                      (r9/run-tick (ex-info "cascade decision refused" {:kind :live-c-stale})))]
              ;; run-tick reads the actual persisted run record.
              (get-in r [:record :decision :abstention])))) fs)}
      (finally (doseq [file (reverse (file-seq (io/file root)))] (io/delete-file file true))))))
