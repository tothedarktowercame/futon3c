(require '[clojure.test :as t] '[clojure.java.io :as io]
         '[futon3c.wm.run4-historical-verification :as v]
         '[futon3c.wm.run4-historical-verification-test :as vt])
(let [captured (atom nil) original v/admit!]
  (with-redefs [v/admit! (fn [opts]
                         (when-not @captured
                           (reset! captured [opts (slurp (:qualification-path opts))]))
                         (original opts))]
    (t/test-vars [#'vt/qualification-to-reviewed-awaiting-validation]))
  (let [[opts text] @captured
        fresh-output (.toFile (java.nio.file.Files/createTempDirectory
                               "hist-review-repro" (make-array java.nio.file.attribute.FileAttribute 0)))
        job ((:review-job-reader opts) (:review-job-id opts))]
    (spit (:qualification-path opts) text)
    (prn {:executed-author-review-admitted
          (:state (original (assoc opts :output-root (.getPath fresh-output)
                                   :review-job-reader
                                   (fn [_] (assoc job :agent-id (:author opts))))))})))
