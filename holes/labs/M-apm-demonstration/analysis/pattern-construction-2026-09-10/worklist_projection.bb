(ns worklist-projection
  (:require [clojure.edn :as edn]
            [cheshire.core :as json]))

(let [worklist (edn/read-string (slurp *in*))]
  (println
   (json/generate-string
    {:campaign (:campaign worklist)
     :items
     (mapv (fn [row]
             (assoc (select-keys row [:id :kind :state :phase :consumer
                                     :other-consumers :depends-on :dag-node
                                     :representation :scope :current-residual
                                     :acceptance])
                    :evidence
                    (assoc (select-keys (:evidence row)
                                        [:commit :author :outcome :placeholder-delta
                                         :changed-paths :residual :finding :axioms])
                           :review
                           (select-keys (get-in row [:evidence :review])
                                        [:outcome :seat :job :finding]))))
           (:items worklist))})))
