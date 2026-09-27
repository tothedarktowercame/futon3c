(ns futon3c.diagramprover.wm-wire-click-record-products-9a
  "record-click stores status/detail in the entry AND needs (flight.clj:
  422, 435-439), and cast in the entry (431). Its stop rule (408-412)
  reads wants/before/after, not these fields."
  (:require [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as runner]))

(def scenarios
  {:closed {:a true :b true}
   :no-progress {:a false :b false}
   :open {:a true :b false}})

(defn products [field after]
  (let [click ((runner/http-click-fn
                {:author "claude-6" :reviewer "claude-13"
                 :today (constantly "fixture")
                 :post! (constantly {:status 409 :body {:error "cast-not-ready"}})
                 :get-status! (fn [] (throw (ex-info "must not poll" {})))})
               {:flight {:flight/id "wire-9a" :target "M" :click 1}})
        path (if (= field :cast) [:cast] [:abstention field])
        replacement (case field
                      :status 503
                      :detail {:error "different-refusal"}
                      :cast (runner/click-cast {:author "claude-13" :reviewer "claude-6"}))
        changed (assoc-in click path replacement)
        f (assoc (flight/start {:target "M"} {:kind :operator-declared}
                               {:id "wire-9a" :at "fixture"})
                 :carried-wants [:earlier])
        record #(flight/record-click f (merge % {:wants [:a :b]
                                                :before {:a false :b false}
                                                :after after
                                                :unreached-wants [{:token :later :reason :fixture}]}))
        before (record click)
        after (record changed)
        entry-path (into [:clicks 0] path)
        strip (fn [r]
                (cond-> (assoc-in r entry-path ::intervened)
                  (not= field :cast) (assoc-in [:needs 0 field] ::intervened)))]
    {:written [(get-in click path) (get-in changed path)]
     :products [(get-in before entry-path) (get-in after entry-path)]
     :needs (when-not (= field :cast)
              [(get-in before [:needs 0 field]) (get-in after [:needs 0 field])])
     :statuses [(:status before) (:status after)]
     :carried [(:carried-wants before) (:carried-wants after)]
     :unchanged [(strip before) (strip after)]}))
