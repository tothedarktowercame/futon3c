(ns futon3c.giraffe.preview-refresh-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon3c.giraffe.preview-refresh :as refresh])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest conformance-result-and-crash-both-allow-preview
  (doseq [mode [:fail :crash :unreadable]]
    (let [out (.toFile (Files/createTempDirectory "refresh-test-" (make-array FileAttribute 0)))
          calls (atom [])
          cfg (assoc refresh/default-config :repo-dir (str out) :out-dir (str out)
                     :exec-fn
                     (fn [args]
                       (let [script (.getName (io/file (second args)))]
                         (swap! calls conj script)
                         (cond
                           (= script "giraffe_party.py")
                           (do (spit (io/file out "party.json") "{\"party\":{\"top\":[]}}") {:exit 0})
                           (= script "giraffe_party_conformance.py")
                           (case mode
                             :fail (do (spit (io/file out "conformance.json")
                                             (json/generate-string {:summary {:PASS 0 :FAIL 1 :ABSENT 0}
                                                                    :failing ["toy-check"]})) {:exit 1})
                             :crash {:exit 2}
                             :unreadable (do (spit (io/file out "conformance.json") "{") {:exit 0}))
                           :else {:exit 0}))))
          result (refresh/refresh! cfg "toy-run" ["M-toy"])]
      (is (= ["giraffe_party.py" "giraffe_party_conformance.py" "gcc_preview.py"] @calls))
      (if (= mode :fail)
        (do (is (:ok? result))
            (is (= ["toy-check"] (get-in result [:conformance :failing])))
            (is (= 1 (get-in result [:conformance :summary :FAIL]))))
        (do (is (false? (:ok? result)))
            (is (str/includes? (:error result) "conformance:")))))))
