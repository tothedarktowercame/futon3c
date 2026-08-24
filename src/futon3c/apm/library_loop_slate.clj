(ns futon3c.apm.library-loop-slate
  "Files-only demonstrator success scoreboard."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.apm.library-loop-runner :as runner])
  (:import (java.io PushbackReader RandomAccessFile)))

(defn read-slate [path]
  (with-open [reader (PushbackReader. (io/reader path))]
    (edn/read {:eof nil} reader)))

(defn apply-status!
  "Applies an ordinary status-recompute result. Only :closed sets success."
  [path problem-id status-result]
  (let [lock-path (str path ".lock")]
    (with-open [file (RandomAccessFile. lock-path "rw")
                _lock (.lock (.getChannel file))]
      (let [slate (read-slate path)
            ruling (:ruling status-result)
            updated (update slate :demonstrators
                            (fn [entries]
                              (mapv (fn [entry]
                                      (if (= problem-id (:problem-id entry))
                                        (cond-> (assoc entry
                                                       :last-ruling ruling
                                                       :status-sha
                                                       (:status-sha status-result))
                                          (= :closed ruling) (assoc :success? true))
                                        entry))
                                    entries)))]
        (when-not (some #(= problem-id (:problem-id %)) (:demonstrators slate))
          (throw (ex-info "demonstrator-not-registered"
                          {:finding :demonstrator-not-registered
                           :problem-id problem-id})))
        (runner/atomic-write-edn! path updated)))))
