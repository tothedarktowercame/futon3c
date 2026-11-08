(ns futon3c.arxana.formal-f4-demo
  "Private Formal Arxana F4 demonstration on loopback 7089."
  (:require [futon3c.arxana.formal-f1-demo :as base]
            [futon3c.arxana.formal-f4 :as f4]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service])
  (:import [java.lang ProcessHandle]))

(def default-port 7089)
(defn req [id op body]
  {:protocol/version "arxana.v1" :message/id id :operation op
   :principal/id "formal-f4-demo" :body body})
(defn call! [cfg id op body] (service/dispatch cfg (req id op body)))
(defn assert! [cfg id record] (call! cfg id :clusion/assert {:record record}))

(defn prepare! [{:keys [cfg prepared]}]
  (let [turn (last (:plans prepared))
        claim (first (:claims turn))
        evidence (first (filter #(= (:subject-clusion-id %) (:id claim))
                                (:attestations turn)))
        seed (call! cfg "f4-demo:seed" :revision/ingest
                    {:artifact/id (:artifact/id turn)
                     :bytes (:bytes turn)
                     :media-type "text/plain; charset=utf-8"
                     :expected/revision-id nil})
        revision (get-in seed [:ok :artifact/revision-id])
        plan (f4/build-plan claim evidence revision)
        writes (f4/materialize! #(service/dispatch cfg %) plan)
        candidate-id (get-in plan [:stages :candidate :id])
        intent-id (get-in plan [:stages :intent :id])
        rejection (records/make-assessment
                   {:subject-clusion-id candidate-id :verdict :rejected :actor "joe"
                    :policy {:id "formal-f4/demo-review" :revision 1}
                    :reason "candidate is intentionally criticised at this stage"
                    :time "2026-11-08T14:00:00.000Z"})
        contradiction (records/make-attestation
                       {:subject-clusion-id intent-id :producer "formal-f4-demo"
                        :basis :human :standing :contradicted :coverage :complete
                        :method {:id "formal-f4/demo-critique" :version 1}
                        :content {:reason "intent is the deliberately contradicted stage"}
                        :source-refs []})
        capability (records/make-clusion
                    {:type-revision [:d6/capability 1]
                     :endpoints [(f4/endpoint :candidate (get-in plan [:stages :candidate]))]
                     :asserted-content {:principal/id "futon4a-demo/emacs"
                                        :dynamics f4/bounded-read-dynamics
                                        :granted-by "formal-f4-demo"
                                        :granted-at "2026-11-08T14:00:01.000Z"}})
        admitted (f4/admitted-decision (get-in plan [:stages :candidate]) (:candidate plan))
        _ (call! cfg "f4-demo:reject" :assessment/append {:record rejection})
        _ (call! cfg "f4-demo:contradict" :evidence/append {:record contradiction})
        _ (assert! cfg "f4-demo:capability" capability)
        _ (assert! cfg "f4-demo:admitted" admitted)
        execute-body {:composite/id (get-in plan [:composite :id])
                      :capability/id (:id capability)
                      :expected/revision-id revision :query/limit 3}
        execute! (fn [mid body]
                   (service/dispatch cfg {:protocol/version "arxana.v1" :message/id mid
                                          :operation :formal-f4/execute
                                          :principal/id "futon4a-demo/emacs" :body body}))
        refused (execute! "f4-demo:refused"
                          (assoc execute-body :decision/id (get-in plan [:stages :decision :id])
                                 :attempted-at "2026-11-08T14:00:02.000Z"))
        success (execute! "f4-demo:success"
                          (assoc execute-body :decision/id (:id admitted)
                                 :attempted-at "2026-11-08T14:00:03.000Z"))]
    (when-not (and (:ok seed) (every? :ok writes) (:ok refused) (:ok success))
      (throw (ex-info "F4 demo preparation failed"
                      {:seed seed :writes writes :refused refused :success success})))
    {:f4a/composite-id (get-in plan [:composite :id])
     :f4b/composite-id (get-in success [:ok :composite/id])
     :refused/composite-id (get-in refused [:ok :composite/id])
     :capability/id (:id capability) :admitted/decision-id (:id admitted)
     :deferred/decision-id (get-in plan [:stages :decision :id])
     :expected/revision-id revision}))

(defn start! ([] (start! default-port)) ([port]
  (let [h (base/start! port)]
    (try (assoc h :f4/prepared (prepare! h))
         (catch Throwable t (base/stop! h) (throw t))))))
(def stop! base/stop!)
(defn -main [& [port-str ready-path]]
  (let [port (if port-str (Long/parseLong port-str) default-port)
        h (start! port) stopped? (atom false)
        stop #(when (compare-and-set! stopped? false true) (stop! h))]
    (.addShutdownHook (Runtime/getRuntime) (Thread. stop "formal-f4-demo-shutdown"))
    (when ready-path
      (spit ready-path (pr-str (merge {:status :ready :pid (.pid (ProcessHandle/current))
                                      :port port} (:f4/prepared h)))))
    (println (pr-str (merge {:status :ready :port port} (:f4/prepared h))))
    (flush) @(promise)))
