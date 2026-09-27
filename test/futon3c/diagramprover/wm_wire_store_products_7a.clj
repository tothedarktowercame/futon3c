(ns futon3c.diagramprover.wm-wire-store-products-7a
  "Real publications and read-back. Constraints filter at mission_reading.clj:383;
  declines filter at :457; questions project without inference at :333."
  (:require [clojure.java.io :as io]
            [futon2.aif.mission-reading :as reading]
            [futon2.aif.want-interpretation :as wi]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-store-support :as store]))

(def fields [:criteria :coverage :locators :constraints-read
             :locator-declines :locator-questions])

(defn- publish-filter! [root field changed?]
  (let [text (str store/text (when changed? "Different mission.\n"))
        sha (reading/text-sha text)
        issued (wi/issue! root {:target store/target
                               :want {:token :done :mission-sha sha}
                               :known [{:token :done} {:token :built}]})
        response (if (= field :constraints-read)
                   {:constraints [{:want :done :requires :built :cue store/cue}]}
                   {:reason :no-locator})]
    (if (= field :constraints-read)
      (reading/publish-constraints!
       root issued response (reading/validate-constraints issued response text)
       {:seat "fixture"})
      (reading/record-locator-decline! root issued response {:seat "fixture"} sha))))

(defn products [field]
  (let [root (w/tmp-dir "store-products-7a-")
        read-all #(into {} (for [k fields] [k (store/read-back root k)]))
        publish #(if (= field :locator-questions)
                   (store/publish root field (if % "second" "first"))
                   (publish-filter! root field %))]
    (try
      (doseq [k fields] (store/publish root k "first"))
      (let [v (publish false)
            before (read-all)
            v-prime (publish true)
            after (read-all)]
        {:written [(store/payload field v) (store/payload field v-prime)]
         :products [(get before field) (get after field)]
         :other-reader-values [(dissoc before field) (dissoc after field)]
         :other-stored-values [(dissoc v field) (dissoc v-prime field)]})
      (finally
        (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f true))))))
