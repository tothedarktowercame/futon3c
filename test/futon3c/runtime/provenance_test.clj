(ns futon3c.runtime.provenance-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.runtime.provenance :as provenance]))

(deftest summarize-carries-every-ticket-field
  (let [snap (provenance/summarize
              {:checkout "/home/joe/code/futon3c"
               :branch "master"
               :head-sha "c7ffdff8a2bf68c389a2e115564667d229aa3ae9"
               :dirty? true
               :reloaded-from ["/tmp/a.clj" "/tmp/b.clj"]
               :port 7070
               :repo "futon3c"
               :started-at "2026-11-03T09:00:00Z"})]
    (is (= {:schema-version 1
            :checkout "/home/joe/code/futon3c"
            :branch "master"
            :head-sha "c7ffdff8a2bf68c389a2e115564667d229aa3ae9"
            :dirty? true
            :reloaded-from ["/tmp/a.clj" "/tmp/b.clj"]
            :port 7070
            :repo "futon3c"
            :started-at "2026-11-03T09:00:00Z"}
           snap))
    (is (nil? (:reloaded (dissoc snap :reloaded-from))))))

(deftest record-reload-appends-in-order-and-dedupes-paths
  (provenance/reset-for-test!)
  (provenance/record-reload! "/tmp/one.clj" :test)
  (provenance/record-reload! "/tmp/two.clj" :test)
  (is (= ["/tmp/one.clj" "/tmp/two.clj"] (provenance/reloaded-from)))
  (provenance/record-reload! "/tmp/one.clj" :test)
  (is (= ["/tmp/one.clj" "/tmp/two.clj"] (provenance/reloaded-from))
      "a repeated path keeps one entry, first-seen order"))

(deftest summarize-dirty-nil-becomes-false
  (is (false? (:dirty? (provenance/summarize
                        {:checkout "x" :branch "b" :head-sha "s"
                         :dirty? nil :reloaded-from [] :port nil
                         :repo "futon3c" :started-at "t"})))))

(deftest load-file-wrapper-records-the-exact-loaded-path
  ;; The exact bad case the ticket names: load-file leaves no trace in the
  ;; tree. Install the wrapper, load a real file, and require that the
  ;; path of that very file appears in reloaded-from (against the real
  ;; clojure.core/load-file, not a stub).
  (provenance/reset-for-test!)
  (provenance/install-load-recording!)
  (is (true? (provenance/load-recording-installed?)))
  (let [marker (gensym "provenance-loaded-marker")
        file (io/file (str (System/getProperty "java.io.tmpdir")
                           "/" marker ".clj"))]
    (try
      (spit file (str "(def " marker " 42)"))
      (let [value (load-file (.getPath file))]
        (is (var? value) "the wrapper delegates: load still returns its value")
        (is (= 42 (var-get value)) "the file's code really ran"))
      (is (some #(= (.getCanonicalPath file) (.getCanonicalPath (io/as-file %)))
                (provenance/reloaded-from))
          "the loaded path is recorded")
      (finally
        (.delete file)))))

(deftest install-load-recording-is-idempotent
  (provenance/install-load-recording!)
  (let [root-before load-file]
    (provenance/install-load-recording!)
    (is (identical? root-before load-file)
        "second install must not wrap the wrapper")))
