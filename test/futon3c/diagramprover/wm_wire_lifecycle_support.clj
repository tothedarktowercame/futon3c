(ns futon3c.diagramprover.wm-wire-lifecycle-support
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.flight :as flight]
            [futon2.aif.lifecycle-exits :as exits]))

(def definition (slurp "/home/joe/code/futon4/holes/mission-lifecycle.md"))

(def mission
  (str "**Status:** DOCUMENT\n"
       (str/join "\n" (for [p exits/phases]
                         (str "## " (name p) "\n**" (name p) " exit: Met.**")))
       "\n"))

(defn supplied [] (exits/supplied "fixture" mission definition))

(defn flight-exits []
  (exits/flight-exits "fixture" mission definition
                      {:repo "futon3c" :path "fixture.md" :observe (constantly true)}))

(defn source-wants []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "map-2b-exits-" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (flight/source-wants
       {:kind :a-exits :repo "futon3c" :path "fixture.md" :store (str root)
        :read-text (fn [_ repo _] (if (= repo "futon4") definition mission))
        :observe (constantly true)}
       {:target "fixture"} {})
      (finally
        (doseq [f (reverse (file-seq root))] (io/delete-file f true))))))
