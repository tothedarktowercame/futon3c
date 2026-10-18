(ns futon3c.wm.cast-registry
  "Install stable War Machine roles over configurable model backends."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.agency.registry :as reg]
            [futon3c.agents.kimi-api :as kimi-api]
            [futon3c.agents.zai-api :as zai-api]))

(def default-specs
  [{:id "wm-author" :role "war-machine-author"
    :backend :codex :model "gpt-5.6-sol"}
   {:id "wm-reviewer" :role "war-machine-reviewer"
    :backend :codex :model "gpt-5.6-sol"}
   {:id "wm-repair-reviewer" :role "war-machine-repair-reviewer"
    :backend :codex :model "gpt-5.6-sol"}])

(def supported-backends #{:codex :claude :zai :kimi})
(def role-identities #{"wm-author" "wm-reviewer" "wm-repair-reviewer"})

(defn validate-specs [specs]
  (let [ids (when (vector? specs) (mapv :id specs))]
    (cond
      (not (vector? specs))
      {:ok false :reason :cast-not-a-vector}
      (not= 3 (count specs))
      {:ok false :reason :cast-role-count :count (count specs)}
      (not= (count ids) (count (set ids)))
      {:ok false :reason :cast-role-identity-duplicated :ids ids}
      (not= role-identities (set ids))
      {:ok false :reason :cast-role-identities-invalid
       :expected role-identities :actual (set ids)}
      (some #(not (and (string? (:id %)) (string? (:role %)))) specs)
      {:ok false :reason :cast-role-malformed}
      (some #(not (supported-backends (:backend %))) specs)
      {:ok false :reason :cast-backend-unsupported
       :supported supported-backends}
      :else {:ok true :specs specs})))

(defn configured-specs []
  (if-let [path (some-> (System/getenv "FUTON3C_WM_CAST_CONFIG") not-empty)]
    (-> path slurp edn/read-string)
    default-specs))

(defn- reset-hook [session-file session-id-atom]
  (fn []
    (reset! session-id-atom nil)
    (let [f (io/file session-file)]
      (when (and (.exists f) (not (.delete f)))
        (throw (ex-info "could not clear WM role session"
                        {:session-file session-file}))))
    {:ok true}))

(defn- evidence-store []
  (some-> (resolve 'futon3c.dev/!evidence-store) deref deref))

(defn- make-invoke-fn
  [{:keys [id backend model cwd] :as spec} session-file session-id-atom]
  (let [common (cond-> {:agent-id id
                        :session-file session-file
                        :session-id-atom session-id-atom
                        :cwd (or cwd "/home/joe/code")}
                 model (assoc :model model))]
    (case backend
      :codex (@(or (resolve 'futon3c.dev/make-codex-invoke-fn)
                   (throw (ex-info "Codex backend unavailable" {:spec spec})))
              common)
      :claude (@(or (resolve 'futon3c.dev/make-claude-invoke-fn)
                    (throw (ex-info "Claude backend unavailable" {:spec spec})))
               common)
      :zai (zai-api/make-invoke-fn
            (assoc common :evidence-store
                   (or (evidence-store)
                       (throw (ex-info "evidence store unavailable"
                                       {:backend backend})))))
      :kimi (kimi-api/make-invoke-fn
             (assoc common :evidence-store
                    (or (evidence-store)
                        (throw (ex-info "evidence store unavailable"
                                        {:backend backend}))))))))

(defn install!
  ([] (install! (configured-specs)))
  ([specs]
   (let [{:keys [ok] :as checked} (validate-specs specs)]
     (when-not ok
       (throw (ex-info "Invalid War Machine cast configuration" checked)))
     (mapv
      (fn [{:keys [id role backend model] :as spec}]
        (let [session-file (str "/tmp/futon-wm-session-" id)
              session-id-atom (atom nil)
              invoke-fn (make-invoke-fn spec session-file session-id-atom)
              session-reset-fn (reset-hook session-file session-id-atom)]
          (reg/register-agent!
           {:agent-id {:id/value id :id/type :continuity}
            :type backend
            :invoke-fn invoke-fn
            :session-reset-fn session-reset-fn
            :capabilities [:explore :edit :test :coordination/execute]
            :metadata {:auto-registered? false
                       :model model :backend backend :role role
                       :freshness {:kind :reset-before-each-click}
                       :cwd (or (:cwd spec) "/home/joe/code")}})
          (reg/update-agent! id
                             :agent/invoke-fn invoke-fn
                             :agent/session-reset-fn session-reset-fn)
          {:id id :backend backend :registered? (reg/agent-registered? id)
           :session-file session-file}))
      specs))))
