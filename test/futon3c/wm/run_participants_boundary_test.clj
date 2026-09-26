(ns futon3c.wm.run-participants-boundary-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon2.aif.run-participants :as participants]
            [futon2.aif.run-participants-test :as fixture]
            [futon3c.transport.http :as http]
            [futon3c.wm.machinery-execution-cohort :as machinery]
            [futon3c.wm.ordinary-click-budget :as budget]
            [futon3c.wm.runner-service :as service]))

(defn boundary-record [payload drop-provenance?]
  (let [record (atom nil)
        consumed (atom [])
        handler (http/make-handler {})
        persist (fn [opts] (reset! record (fixture/write-record opts)) {:outcome :incomplete})]
    (with-redefs-fn
      {#'machinery/apply-binding identity
       #'service/phase-sink (fn [& _] (constantly nil))
       #'service/close-click! (fn [& _] nil)
       #'service/fail-click! (fn [_ _ e] (throw e))
       #'budget/consume! (fn [& args] (swap! consumed conj args))
       #'service/click!
       (fn [opts]
         ;; Exercise the budget callback only against the stub, then the real
         ;; synchronous worker handoff. Never call click! or start its thread.
         ((:ordinary-click/issue! opts) "test-click" "test-time")
         (#'service/run-click! "test-click"
          (cond-> (dissoc opts :ordinary-click/issue!)
            drop-provenance? (dissoc :issuer-provenance)) (promise))
         {:started true})}
      (fn []
        (binding [service/*resolve-var*
                  (fn [sym]
                    (case sym
                      futon2.aif.full-loop-runner/config identity
                      futon2.aif.full-loop-runner/run-opportunity! persist))]
          (is (= 200 (:status (handler {:request-method :post :uri "/api/alpha/wm/click"
                                       :body (json/generate-string payload)})))))))
    {:record @record :consumed @consumed}))

(deftest issuer-survives-http-worker-writer
  (doseq [[payload expected budget-caller]
          [[{:issuing-caller "commissioner"} "commissioner" "commissioner"]
           [{} :caller-unknown nil]]]
    (let [{:keys [record consumed]} (boundary-record payload false)
          broken (:record (boundary-record payload true))
          role (participants/read-role record :issuing-caller)]
      (is (= {:status :present :identity expected :source :wm-click-http-boundary} role))
      (is (= [(list "test-click" "test-time" budget-caller)] consumed))
      (is (not= record broken))
      (is (not= role (participants/read-role broken :issuing-caller)))
      (is (= {:status :not-observed} (participants/read-role broken :issuing-caller))))))
