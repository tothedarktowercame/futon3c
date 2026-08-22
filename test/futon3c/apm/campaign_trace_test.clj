(ns futon3c.apm.campaign-trace-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-trace :as sut]))

(def valid
  {:campaign-id "qualified-cycle" :manifest-hash "manifest-1"
   :contract-id "apm-complete-frame-cycle-v2"
   :phase-order [:preflight :solve :verify :promote-solver
                 :student-attempt-1 :guide-intervention-1
                 :student-attempt-2 :guide-intervention-2
                 :student-attempt-3 :scribe-reduce :close-frame]
   :steps
   (mapv (fn [ordinal [from to]]
           {:from from :to to
            :ledger-before (str "ledger-" ordinal)
            :ledger-after (str "ledger-" (inc ordinal))
            :receipt-id (str "receipt-" ordinal)
            :prior-receipt-id (when (pos? ordinal)
                                (str "receipt-" (dec ordinal)))})
         (range 11)
         (map vector
              [:registered :preflight :solve :verify :promote-solver
               :student-attempt-1 :guide-intervention-1 :student-attempt-2
               :guide-intervention-2 :student-attempt-3 :scribe-reduce]
              [:preflight :solve :verify :promote-solver :student-attempt-1
               :guide-intervention-1 :student-attempt-2 :guide-intervention-2
               :student-attempt-3 :scribe-reduce :close-frame]))
   :closed true :terminal-ledger-digest "ledger-11"})

(deftest canonical-trace-is-deterministic-and-atomically-published
  (let [directory (.toFile (java.nio.file.Files/createTempDirectory
                            "apm-trace" (make-array java.nio.file.attribute.FileAttribute 0)))
        a (java.io.File. directory "a.json")
        b (java.io.File. directory "b.json")]
    (is (:ok (sut/emit! a valid)))
    (is (:ok (sut/emit! b valid)))
    (is (= (slurp a) (slurp b)))
    (is (.contains (slurp a) "\"promote-solver\""))
    (is (= (json/parse-string
            (slurp "test/resources/apm-traces/valid.json"))
           (json/parse-string (slurp a))))))
