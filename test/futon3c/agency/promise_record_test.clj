(ns futon3c.agency.promise-record-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [futon3c.agency.promise-record :as promise]
            [futon3c.agency.parked-on :as park]
            [futon3c.agency.followup-queue :as queue]
            [futon3c.transport.http :as http]))

(use-fixtures :each
  (fn [f]
    (let [p (java.io.File/createTempFile "promise-park" ".edn")
          q (java.io.File/createTempFile "promise-followup" ".edn")]
      (with-redefs-fn {#'park/store-path (constantly (str p))}
        #(binding [queue/*path-override* (str q)]
           (try (park/clear!) (queue/clear!) (f)
                (finally (.delete p) (.delete q)
                         (reset! @#'park/!parked nil)
                         (reset! @#'queue/!state nil))))))))

(def metadata-fields
  {:beneficiary "joe" :deadline "2026-10-01T12:00:00Z"
   :fulfilment-criterion {:kind :job-terminal-ok :job-id "test-job"
                         :machine-evaluable? true}})
(def park-request {:agent "test" :session "test" :awaiting ["test-dep"]})
(def followup-request {:agent "test" :session "test" :type :inbox-zero
                       :dedupe-key "test" :prompt "test"})

(deftest durable-metadata-and-old-store
  (doseq [fields [metadata-fields {}]]
    (park/clear!) (queue/clear!)
    (let [id (:id (park/park! (merge park-request fields) {}))]
      (queue/enqueue! (merge followup-request fields))
      (let [before-p (pr-str (park/snapshot))
            before-q (pr-str (queue/snapshot))]
        ;; Force actual EDN disk reads, including legacy records with no fields.
        (reset! @#'park/!parked nil)
        (reset! @#'queue/!state nil)
        (is (= before-p (pr-str (park/snapshot))))
        (is (= before-q (pr-str (queue/snapshot))))
        (is (= fields (select-keys (get-in (park/snapshot) [:records id]) promise/field-keys)))
        (is (= fields (select-keys (first (get-in (queue/snapshot) [:queued ["test" "test"]]))
                                   promise/field-keys)))))))

(deftest invalid-criteria-refuse-without-mutation
  (doseq [criterion [{:kind :unknown} {:kind :prose :text "done" :machine-evaluable? true}
                     "job succeeded" {:kind :job-terminal-ok :job-id ""}]]
    (doseq [[write request snapshot] [[#(park/park! % {}) park-request park/snapshot]
                                     [queue/enqueue! followup-request queue/snapshot]]]
      (let [before (snapshot)]
        (is (= :invalid-fulfilment-criterion
               (try (write (assoc request :fulfilment-criterion criterion)) nil
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
        (is (= before (snapshot)))))))

(deftest typed-forms-and-absolute-deadline
  (is (= {:fulfilment-criterion {:kind :prose :text "review it" :machine-evaluable? false}}
         (promise/fields {:fulfilment-criterion {:kind :prose :text "review it"}})))
  (doseq [[k v reason] [[:deadline "tomorrow" :invalid-deadline]
                        [:deadline "2026-10-01T12:00:00" :invalid-deadline]
                        [:beneficiary "" :invalid-beneficiary]]]
    (is (= reason (try (promise/fields {k v}) nil
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))

(defn request [m] {:body (io/input-stream (.getBytes (json/generate-string m) "UTF-8"))})
(deftest http-writes-readback-and-typed-refusals
  (with-redefs-fn {#'http/parked-on-enabled? (constantly true)
                  #'http/exact-agent-session? (constantly true)
                  #'http/parked-job-lookup (constantly nil)}
    #(do
       (is (= 200 (:status (#'http/handle-park (request (merge park-request metadata-fields)) {}))))
       (is (= 200 (:status (#'http/handle-followup-enqueue
                            (request (merge followup-request metadata-fields))))))
       (let [body (json/parse-string (:body (#'http/handle-parked {} {})) true)
             item (first (:parked body))]
         (is (= "joe" (:beneficiary item)))
         (is (= (:deadline metadata-fields) (:deadline item)))
         (is (= "job-terminal-ok" (get-in item [:fulfilment-criterion :kind]))))
       (doseq [bad [{:kind :unknown} {:kind :prose :text "done" :machine-evaluable? true}]
               response [(#'http/handle-park (request (assoc park-request :fulfilment-criterion bad)) {})
                         (#'http/handle-followup-enqueue
                           (request (assoc followup-request :fulfilment-criterion bad)))]]
         (is (= 400 (:status response)))
         (is (= "invalid-fulfilment-criterion"
                (:reason (json/parse-string (:body response) true))))))))

(deftest promise-deadline-does-not-change-wake-behavior
  (let [resumed (atom [])
        id (:id (park/park! (merge park-request metadata-fields
                                  {:deadline "2000-01-01T00:00:00Z"}) {}))]
    (park/sweep-deadlines! {:resume! #(swap! resumed conj %)})
    (is (contains? (:records (park/snapshot)) id))
    (is (empty? @resumed))
    (park/note-completion! "test-dep" {:ok true} {:resume! #(swap! resumed conj %)})
    (is (= 1 (count @resumed)))
    (is (= "2000-01-01T00:00:00Z" (:deadline (first @resumed))))))
