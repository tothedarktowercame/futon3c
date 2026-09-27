(ns futon3c.diagramprover.wm-wire-temporal-courier-support
  "Isolated courier witnesses. Execution/check transports are fixture inputs;
   enactment, publication, replay, initialization and consumption are real."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as runner]
            [futon2.aif.temporal-update :as temporal]
            [futon2.aif.token-belief-carry :as carry]
            [futon2.aif.token-belief-predecessor :as predecessor]
            [futon2.aif.token-initialization-policy :as policy]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-token-input-support :as prior]))

(def live-records-read
  (mapv #(assoc % :why "Historical tick records predate the temporal courier; no publication/digest/envelope pair is retained.")
        prior/live-records-read))

(defn live-absent? []
  (every? (fn [{:keys [path sha256]}]
            (and (= sha256 (w/sha256-file path))
                 (not-any? #(and (map? %) (contains? % :temporal-posterior))
                           (tree-seq coll? seq (w/read-record path))))) live-records-read))

(defn isolated [f]
  (let [root (io/file (w/tmp-dir "temporal-courier-wire-"))]
    (try (f root)
         (finally (doseq [p (reverse (file-seq root))] (io/delete-file p true))))))

(defn fixture [root]
  (let [target :courier domain #{[target :done]}
        identity {:A "C3/fixture@declared" :B {:authority 'futon2.aif.cascade-model-manifest/pattern-kernel
                                              :revision :declared-add-only-v1}}
        interpretation {:guard {:needs #{} :forbids #{}} :produces #{:done}
                        :model-identity identity :domain domain}
        inputs [{:target target :declaration {:facts {:done false} :want [:done]
                                             :interpretations {:finish interpretation}}}]
        stage (carry/stage {:value {#{} 1}} inputs nil
                           {:occurrence-id "courier-selection"
                            :observation-initialization {target {:policy policy/disabled}}})
        receipt (predecessor/input-receipt stage (predecessor/inspect-trace nil) prior/admission nil)
        enact (runner/enact-fn
               {:interpretations (constantly {:finish interpretation})
                :record-dir (str (io/file root "records")) :trace-dir (str (io/file root "trace"))
                :publication-observation (fn [& _] {:status :not-observed :reason :isolated-wire-fixture})
                :fetch-run-record (fn [_] {:decision {:selection-certificate
                                                     {:token-belief-input receipt :token-belief-stage stage}}})
                :dispatch-step! (fn [_] {:commit "fixture-revision" :produced :done
                                        :check {:class :C3 :repo "fixture" :path "done" :sha "fixture-revision"}})
                :check-fn (fn [loc] {:check :C3 :observed true :check-mechanism (:A identity)
                                    :evidence (assoc (select-keys loc [:repo :path]) :resolved-sha "fixture-revision")})})
        start (flight/start (flight/choose-target {:requested target})
                            {:kind :operator-declared :wants [:done] :declared-by "fixture"}
                            {:id "temporal-wire"})
        click {:click-id "courier-click" :chosen {:id :finish :precedence [:finish]}}
        enacted (enact start click)
        record (:enactment enacted)]
    (when-not (= :published (get-in enacted [:temporal-receipt :status]))
      (throw (ex-info "Courier fixture did not publish" {:receipt (:temporal-receipt enacted)})))
    {:stage stage :start start :click click :enacted enacted :record record
     :previous (temporal/read-receipt (:temporal-receipt enacted))}))

(defn mutate [mode v]
  (case mode :none v :absent nil :different {:status :absent :reason :wire-intervention}))

(defn run-carrier [start click enacted]
  (flight/run! start {:max-clicks 1 :sources-fn (constantly {})
                      :observe-fn (fn [& _] {:done false})
                      :click-fn (constantly click) :enact-fn (fn [& _] enacted)}))

(defn observe [[writer reader field] mode]
  (isolated
   (fn [root]
     (let [{:keys [stage start click enacted record previous]} (fixture root)
           k (if (vector? field) (first field) field)
           final (@#'temporal/finalize-record (dissoc record :temporal-receipt :temporal-posterior :temporal-cursor)
                                             (get-in record [:temporal-input :previous]) [])
           receipt (:temporal-receipt enacted)
           opts (flight/judge-opts (assoc start :enactments [(select-keys enacted [:record-path :temporal-receipt])]) {})
           inspection (predecessor/inspect-trace nil opts)
           value (case writer
                   :temporal-finalize (get final k)
                   :temporal-write-once (get receipt k)
                   :r0-enact-step (get enacted k)
                   :flight-run (get-in (run-carrier start click enacted) [:enactments 0 k])
                   :flight-judge-opts (get-in opts [:flight k])
                   :temporal-inspect (get inspection k)
                   :token-belief-stage (:initialization stage))
           supplied (mutate mode value)
           [seen product]
           (case reader
             :temporal-envelope
             (let [r (temporal/envelope (assoc final k supplied))]
               [(case k
                  :temporal-receipt (when (= :posterior (:basis r)) {:status :published})
                  :temporal-posterior (:record r)
                  :temporal-cursor (select-keys r [:initial-event-id :consumed-event-ids])) r])
             :temporal-write-once
             (let [r (@#'temporal/write-once! (io/file root "copy.edn") (assoc final k supplied))
                   bytes (edn/read-string (slurp (io/file root "copy.edn")))]
               [(get bytes k) r])
             :temporal-read-receipt
             (let [r (temporal/read-receipt (assoc receipt k supplied))]
               [(get-in r [:publication k]) r])
             :flight-run
             (let [r (run-carrier start click (assoc enacted k supplied))]
               [(get-in r [:enactments 0 k]) r])
             :flight-judge-opts
             (let [r (flight/judge-opts (assoc start :enactments [{k supplied}]) {})]
               [(when (= :posterior (get-in r [:flight :temporal-previous :basis]))
                  (assoc (get-in r [:flight :temporal-previous :publication]) :status :published)) r])
             :temporal-inspect
             (let [r (predecessor/inspect-trace nil (assoc-in opts [:flight k] supplied))]
               [(get r k) r])
             :r1-token-temporal
             (let [r (@#'predecessor/consume-temporal {} stage (assoc inspection k supplied))]
               [(get r k) r])
             :r1-token-input
             (let [r (predecessor/input-receipt stage (assoc inspection k supplied) prior/admission nil)]
               [(get r k) r])
             :r1-token-initialization
             (let [r (@#'predecessor/initialization-input-receipt (assoc stage k supplied)
                       (predecessor/inspect-trace nil) prior/admission nil)]
               [(:initialization r) r])
             :token-initialization-observations
             (let [r (policy/apply-observations (assoc stage k supplied) {} nil)]
               [(when (= (:value supplied) (:continuation-belief r)) supplied) r]))]
       {:writer value :reader seen :product product :previous previous}))))

(defn publication-paths []
  (isolated
   (fn [root]
     (let [{:keys [enacted record]} (fixture root)
           good (temporal/read-receipt (:temporal-receipt enacted))
           bad (temporal/publish! (io/file root "refused.edn") record nil (str (io/file root "trace")))
           persisted (edn/read-string (slurp (io/file root "refused.edn")))
           replay (temporal/read-receipt (:receipt bad))]
       {:published (:basis good) :refused (:reason replay)
        :persisted (:temporal-receipt persisted) :read replay}))))
