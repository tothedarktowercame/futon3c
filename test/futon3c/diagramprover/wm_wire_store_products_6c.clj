(ns futon3c.diagramprover.wm-wire-store-products-6c
  "Temporary publications through the real writers, then the real store readers.
  Criteria filters cues (mission_reading.clj:290-299); coverage filters by
  mission SHA (438-440); locators only projects payloads (301-302)."
  (:require [clojure.java.io :as io]
            [futon2.aif.mission-reading :as reading]
            [futon2.aif.want-interpretation :as wi]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-store-support :as store]))

(defn- publish-filter-input! [root field changed?]
  (let [text (if changed? "Other artifact.\nPublish after building.\n" store/text)
        sha (reading/text-sha text)
        issued (wi/issue! root {:target store/target
                               :want {:token :done :mission-sha sha}
                               :criterion {:stated "Build the artifact."}
                               :known [{:token :done}]})
        response (case field
                   :criteria {:criteria
                              [{:statement "Artifact"
                                :cue {:lines [1 1] :quote (if changed? "Other artifact." "Build the artifact.")}}
                               {:statement "Publication"
                                :cue {:lines [2 2] :quote "Publish after building."}}]}
                   :coverage {:scope-outs [{:statement "Excluded"
                                            :cue {:lines [2 2] :quote "Publish after building."}}]})
        validated ((case field :criteria reading/validate-criteria
                         :coverage reading/validate-coverage) issued response text)]
    (case field
      :criteria (reading/publish-criteria! root issued response validated {:seat "fixture"} sha)
      :coverage (reading/publish-coverage! root issued response validated {:seat "fixture"}))))

(defn products [field]
  (let [root (w/tmp-dir "store-products-6c-")
        read-all #(into {} (for [k [:criteria :locators :coverage]]
                            [k (store/read-back root k)]))
        publish #(if (= field :locators)
                   (store/publish root field (if % "second" "first"))
                   (publish-filter-input! root field %))]
    (try
      (doseq [k [:criteria :locators :coverage]] (store/publish root k "first"))
      (let [v (publish false)
            before (read-all)
            v' (publish true)
            after (read-all)]
        {:written [(store/payload field v) (store/payload field v')]
         :products [(get before field) (get after field)]
         :other-reader-values [(dissoc before field) (dissoc after field)]
         :other-stored-values [(dissoc v field) (dissoc v' field)]})
      (finally
        (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f true))))))
