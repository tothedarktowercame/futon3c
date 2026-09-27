(ns futon3c.diagramprover.wm-wire-c8-products-support
  "C8 records GET diagnostics. Only HTTP transport is replaced; no sockets."
  (:require [babashka.http-client :as http]
            [clojure.test :refer [is]]
            [futon2.aif.observation-checks :as oc]))

(defn observe [reader tamper]
  (let [get-var (ns-resolve 'futon2.aif.observation-checks 'registry-get)
        real-get @get-var written (atom nil) carrier (atom nil) request (atom nil)
        result (binding [oc/*registry-timeout-ms* 50]
                 (with-redefs-fn
                   {#'http/get (fn [url opts]
                                (reset! request [url opts])
                                (throw (java.net.SocketTimeoutException. "registry read timed out")))
                    get-var (fn [url]
                              (let [r (real-get url) v (tamper r)]
                                (reset! written r) (reset! carrier v) v))}
                   #(case reader
                      :entry (oc/fetch-registry-entry "http://registry.invalid" "wire-entry")
                      :latest (oc/fetch-latest-for-namespace "http://registry.invalid" "wire.ns"))))]
    {:written @written :carrier @carrier :request @request :result result}))

(defn assert-record [reader field]
  (let [other (case field :message "different registry diagnostic" :timeout-ms 75)
        a (observe reader identity) b (observe reader #(assoc % field other))
        va (get-in a [:carrier field]) vb (get-in b [:carrier field])]
    (is (= (:written a) (:written b) (:carrier a)))
    (is (= (:carrier a) (assoc (:carrier b) field va)))
    (is (= (:request a) (:request b)))
    (is (= 50 (get-in a [:request 1 :timeout])))
    (is (= va (get-in a [:result :data field])))
    (is (= vb (get-in b [:result :data field])))
    (is (not= va vb))
    (is (= :missing (get-in a [:result :status]) (get-in b [:result :status])))
    (is (= :registry-unreadable (get-in a [:result :kind]) (get-in b [:result :kind])))
    (is (= :unreachable (get-in a [:result :data :status]) (get-in b [:result :data :status])))
    (is (= (:result a) (assoc-in (:result b) [:data field] va)))
    (prn :c8-record {:reader reader :field field :values [va vb]
                    :unchanged-kind (get-in a [:result :kind])})))
