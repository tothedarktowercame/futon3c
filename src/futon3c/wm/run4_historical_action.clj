(ns futon3c.wm.run4-historical-action
  "Server-configured runner ports for a pinned historical verification."
  (:require [futon2.aif.repair-obligation :as repair]))

(defn runner-ports
  [{:keys [repair-root verification-root verification-path verification-sha256]
    :as config}]
  (when-not (= #{:repair-root :verification-root :verification-path
                 :verification-sha256}
               (set (keys config)))
    (throw (ex-info "Historical action configuration invalid" {})))
  (let [evidence {:verification-root verification-root
                  :path verification-path :sha256 verification-sha256}
        read-candidate #(repair/historical-verification-candidate repair-root evidence)]
    {:historical-verification-candidate-fn
     (fn [obligation]
       (let [candidate (read-candidate)]
         (when-not (= (:repair/id obligation) (:repair/id candidate))
           (throw (ex-info "Historical candidate targets another stop-line" {})))
         candidate))
     :historical-verification-execute-fn
     (fn [{:keys [execution-identity obligation candidate]}]
       (let [fresh (read-candidate)]
         (when-not (and (= candidate fresh)
                        (= (:repair/id obligation) (:repair/id fresh)))
           (throw (ex-info "Historical candidate changed before execution" {})))
         (repair/commit-historical-verification!
          repair-root execution-identity evidence)))}))
