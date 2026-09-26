(require '[clojure.edn :as edn] '[clojure.java.io :as io]
         '[cheshire.core :as json]
         '[futon2.aif.c-fold-config :as digest]
         '[futon3c.wm.run4-infrastructure-reconciliation :as r])
(let [pins (json/parse-string (slurp "holes/labs/wm-contract/runs/RUN4-U88-successor-deployment-2026-09-11/TERMINAL-SOURCE-PINS-2026-09-11.json") true)
      _ (doseq [p pins] (assert (= (:sha256 p) (digest/sha256 (slurp (:path p))))))
      path #(first (filter (fn [p] (.endsWith ^String (:path p) %)) pins))
      file #(-> % path :path)
      read! #(edn/read-string (slurp %))
      paths {:started (file "001-started.edn") :reservation (file "reservation.edn")
             :click-result (file "click-result.edn")
             :binding (file "click-run-binding-wm-click-8d1d9141-5e33-4032-aebd-dbadc2d9755f.edn")
             :repair (file "repair-initialization-b076e0f8-dbc2-4368-80a0-073d243951a0-initialization-failed.edn")
             :checkpoint-prefix (mapv file ["001-time-step.edn" "002-selection.edn" "003-construction.edn" "004-dispatch.edn" "005-build.edn" "006-adjudication.edn"])}
      sv (read! (:started paths)) bv (read! (:binding paths)) fv (read! (:repair paths))
      cell (read! (first (:checkpoint-prefix paths)))
      identity {:series-id (:series-id sv) :trial-id (:trial-id sv)
                :controller-attempt-id (:attempt-id sv) :click-id (:click-id sv)
                :cohort-id (:cohort/id cell) :cohort-attempt-id (:attempt/id cell)
                :wrapper-attempt-id (:attempt/id bv) :repair-id (:repair/id fv)
                :pin-sha256 (:pin-sha256 sv) :manifest-sha256 (:manifest-sha256 sv)}
      roots {:series-root "/home/joe/run4/U88-successor/controller"
             :binding-root "/home/joe/run4/U88-successor/bindings"
             :cohort-root "/home/joe/run4/U88-cohort-20260911"
             :repair-root "/home/joe/code/futon2/data/wm-repair-obligations"}
      record (r/construct! roots identity paths)
      out (.toFile (java.nio.file.Files/createTempDirectory "run4-review-publication" (make-array java.nio.file.attribute.FileAttribute 0)))]
  (assert (= record (r/publish! out roots identity paths)))
  (assert (= record (r/publish! out roots identity paths)))
  (doseq [p pins] (assert (= (:sha256 p) (digest/sha256 (slurp (:path p))))))
  (prn (assoc (select-keys record [:schema :task-verdict :redispatch-permitted? :cross-store-association])
              :retained-pins-unchanged (count pins) :temporary-publication-and-replay true)))
