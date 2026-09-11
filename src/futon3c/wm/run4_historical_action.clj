(ns futon3c.wm.run4-historical-action
  "Server-configured runner ports for a pinned historical verification."
  (:require [clojure.java.io :as io]
            [futon2.aif.repair-obligation :as repair]))

(defn runner-ports
  [{:keys [repair-root verification-root verification-path verification-sha256]
    :as config}]
  (when-not (= #{:repair-root :verification-root :verification-path
                 :verification-sha256}
               (set (keys config)))
    (throw (ex-info "Historical action configuration invalid" {})))
  (when-not (and (string? repair-root) (string? verification-root)
                 (string? verification-path) (string? verification-sha256)
                 (.isDirectory (io/file repair-root))
                 (.isDirectory (io/file verification-root))
                 (re-matches #"[0-9a-f]{64}" verification-sha256))
    (throw (ex-info "Historical action authority invalid" {})))
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

(defn validate-applicable!
  "Read the actual first open stop-line and require this pinned historical
  action to target it. This is read-only and must run before admission."
  [config]
  (let [ports (runner-ports config)
        obligation (first (filter #(and (= :open (:repair/status %))
                                        (not= :environmental-hold
                                              (:repair/class %)))
                                  (repair/open-obligations
                                   (:repair-root config))))]
    (when-not obligation
      (throw (ex-info "Historical action has no open stop-line" {})))
    ((:historical-verification-candidate-fn ports) obligation)
    {:repair-id (:repair/id obligation)}))
