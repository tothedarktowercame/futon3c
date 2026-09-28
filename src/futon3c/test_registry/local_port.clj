(ns futon3c.test-registry.local-port
  "Composition root for Futon2's read-only test-registry port."
  (:require [clojure.string :as str]
            [futon2.aif.registry-port :as port]
            [futon3c.evidence.backend :as backend]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.currentness :as currentness]
            [futon3c.test-registry.local-store :as local-store]
            [futon3c.test-registry.sqlite-backend :as sqlite]))

(defn registry-path [] (local-store/path))

(def ^:dynamic *repo-roots*
  {"futon2" "/home/joe/code/futon2"
   "futon3c" "/home/joe/code/futon3c"})

(defn- verified-entry [store entry]
  (when entry
    (registry/read-chain! store (:evidence/id entry))
    entry))

(defn- missing-answer [namespace repo reason found-entry-id request]
  {:status :missing
   :kind :no-current-warrant
   :data {:namespace namespace
          :repo repo
          :reason reason
          :request-id (:request-id request)
          :request-state (:state request)
          :run-requested-at (:requested-at request)
          :found-entry-id found-entry-id}})

(defn- current-or-request [store {:keys [namespace repo] :as request}]
  (let [root (get *repo-roots* repo)]
    (if-not (and (string? namespace) (not (str/blank? namespace))
                 (#{"futon2" "futon3c"} repo) root)
      (port/failure :invalid-current-warrant-request
                    {:request request :required {:namespace :nonblank-string
                                                 :repo ["futon2" "futon3c"]}})
      (if-let [entry (sqlite/latest-run-for-namespace store namespace)]
        (let [chain (registry/read-chain! store (:evidence/id entry))
              run (:payload (last chain))
              classification (currentness/classify store entry root)]
          (case (:class classification)
            :current
            {:status :current
             :entry-id (:evidence/id entry)
             :ran-at (:ran-at run)
             :git-head (:git-head run)}

            :not-passing
            {:status :missing :kind :not-passing
             :data {:namespace namespace :repo repo
                    :found-entry-id (:evidence/id entry)
                    :reason :not-a-warrant}}

            :registration-refused
            (let [refusal-reason (:reason classification)
                  queued (sqlite/request-rerun!
                          store {:namespace namespace :repo repo :reason :stale
                                 :detail (str "registration refused: "
                                              (name refusal-reason))})]
              (if (:error/code queued)
                (port/failure :rerun-request-failed
                              {:miss {:namespace namespace :repo repo
                                      :reason :registration-refused
                                      :refusal-reason refusal-reason
                                      :found-entry-id (:evidence/id entry)}
                               :failure queued})
                (assoc-in
                 (missing-answer namespace repo :registration-refused
                                 (:evidence/id entry) queued)
                 [:data :refusal-reason] refusal-reason)))

            :stale
            (let [miss-reason :stale
                  queued (sqlite/request-rerun!
                          store {:namespace namespace :repo repo :reason miss-reason})]
              (if (:error/code queued)
                (port/failure :rerun-request-failed
                              {:miss {:namespace namespace :repo repo
                                      :reason miss-reason
                                      :found-entry-id (:evidence/id entry)}
                               :failure queued})
                (missing-answer namespace repo miss-reason
                                (:evidence/id entry) queued)))

            :unverifiable
            {:status :missing :kind :unverifiable
             :data {:namespace namespace :repo repo
                    :found-entry-id (:evidence/id entry)
                    :reason (:reason classification)}}))
        (let [queued (sqlite/request-rerun!
                      store {:namespace namespace :repo repo :reason :absent})]
          (if (:error/code queued)
            (port/failure :rerun-request-failed
                          {:miss {:namespace namespace :repo repo :reason :absent
                                  :found-entry-id nil}
                           :failure queued})
            (missing-answer namespace repo :absent nil queued)))))))

(defn implementation
  "Construct the complete port over PATH. Every returned record has passed
  the registry's canonical chain verification."
  [path]
  (let [store (sqlite/sqlite-backend path)]
    {:entry (fn [id]
              (verified-entry store (backend/-get store id)))
     :latest-namespace (fn [namespace]
                         (verified-entry store
                           (sqlite/latest-run-for-namespace store namespace)))
     :latest-command (fn [command]
                       (verified-entry store
                         (sqlite/latest-run-for-command store command)))
     :current-or-request (fn [request]
                           (current-or-request store request))
     :runs (fn [{:keys [author since]}]
             (mapv #(verified-entry store %)
                   (backend/-query store
                     (cond-> {:query/tags [:test-registry]}
                       author (assoc :query/author author)
                       since (assoc :query/since since)))))}))

(defn install!
  "Install the local registry implementation. Safe to call again after load."
  ([] (install! (registry-path)))
  ([path] (port/install! (implementation path))))

(install!)
