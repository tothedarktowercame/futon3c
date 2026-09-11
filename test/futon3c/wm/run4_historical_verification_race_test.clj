(ns futon3c.wm.run4-historical-verification-race-test
  (:require [clojure.test :as t] [clojure.edn :as edn]
            [futon3c.wm.run4-historical-verification :as v]
            [futon3c.wm.run4-historical-verification-test :as vt]))

(t/deftest ^:slow publication-refuses-post-check-source-drift
(let [captured (atom nil) original v/admit!]
  (with-redefs [v/admit! (fn [opts]
                         (when-not @captured
                           (reset! captured [opts (slurp (:qualification-path opts))]))
                         (original opts))]
    (t/test-vars [#'vt/qualification-to-reviewed-awaiting-validation]))
  (let [[opts qtext] @captured
        q (edn/read-string qtext)
        source (get-in q [:sources 0 :path])
        original-source (slurp source)
        head-var (ns-resolve 'futon3c.wm.run4-historical-verification 'head)
        original-head @head-var
        out (.toFile (java.nio.file.Files/createTempDirectory
                       "historical-source-race" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (spit (:qualification-path opts) qtext)
    (try
      (with-redefs-fn {head-var (fn [repo]
                                (spit source (str original-source "\n"))
                                (original-head repo))}
        #(t/is (= :source-drift
                  (try (original (assoc opts :output-root (.getPath out)))
                       nil
                       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
      (finally (spit source original-source)))))

)
