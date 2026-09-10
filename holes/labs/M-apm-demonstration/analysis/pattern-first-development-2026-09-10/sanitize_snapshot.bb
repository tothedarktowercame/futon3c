(ns sanitize-snapshot
  (:require [clojure.edn :as edn]
            [cheshire.core :as json]))

;; Only these mathematical/provenance fields cross the offline evidence boundary.
;; Read one committed snapshot on stdin; never open an archived role packet.
(let [snapshot (edn/read-string (slurp *in*))]
  (println
   (json/generate-string
    {:snapshot/id (:snapshot/id snapshot)
     :snapshot/digest (:snapshot/digest snapshot)
     :memories
     (mapv #(select-keys % [:memory-id :name :hook :body :pattern-ids
                           :attachment-status :review-evidence-id :reviewer
                           :depositor :content-digest :provenance :memory-use/kind])
           (:snapshot/memories snapshot))})))
