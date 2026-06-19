(ns futon3c.aif.discipline-events-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.aif.discipline-events :as discipline]))

(defn- tmp-path []
  (.getAbsolutePath (java.io.File/createTempFile "discipline-events-" ".edn")))

(def base-event
  {:discipline/event :teleport-refused
   :run-id "run-1"
   :at "2026-06-10T00:00:00Z"
   :action {:type :open-mission :target "M-x"}
   :predicted -5.0
   :note "operator refused teleport"})

(deftest discipline-event-validates-kind
  (is (= base-event (discipline/discipline-event base-event)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"invalid discipline event kind"
                        (discipline/discipline-event
                         (assoc base-event :discipline/event :made-up)))))

(deftest append-read-round-trip
  (let [path (tmp-path)]
    (try
      (is (= base-event (discipline/append-event! base-event path)))
      (is (= [(discipline/discipline-event base-event)]
             (discipline/read-events path)))
      (finally
        (.delete (java.io.File. path))))))
