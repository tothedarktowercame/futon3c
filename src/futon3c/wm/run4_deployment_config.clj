(ns futon3c.wm.run4-deployment-config
  "Closed conversion from reviewed deployment data to the production service map."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.wm.run4-deployment-preflight :as preflight]))

(def template-keys
  #{:schema :enabled? :activation :credential :casting :authority-root :manifest
    :pin-allowlist :source-allowlist :stores :reserved-unwired :serving :mission})
(def dependency-keys #{:credential :resolve-mission :action-admissible? :enable?})

(defn- refuse [reason] (throw (ex-info "RUN4 deployment refused" {:reason reason})))
(defn- parse [text]
  (try
    (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
      (let [v (edn/read {:eof ::empty} r)]
        (when (or (= ::empty v) (not= ::end (edn/read {:eof ::end} r)))
          (refuse :invalid-deployment-contract))
        v))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Throwable _ (refuse :invalid-deployment-contract))))

(defn materialize
  "Materialize exact service configuration. DEPENDENCIES are server-construction
  values and cannot be supplied by HTTP or the EDN template."
  [template-text dependencies]
  (let [t (parse template-text)]
    (when-not (and (map? t) (= template-keys (set (keys t)))
                   (= dependency-keys (set (keys dependencies)))
                   (= :wm/run4-disabled-deployment-template-v1 (:schema t))
                   (false? (:enabled? t))
                   (= "/api/alpha/wm/run4/series/step" (get-in t [:serving :route]))
                   (= {:author "zai-2" :reviewer "codex-12" :repair-reviewer "codex-17"}
                      (:casting t))
                   (fn? (:credential dependencies))
                   (fn? (:resolve-mission dependencies))
                   (fn? (:action-admissible? dependencies))
                   (boolean? (:enable? dependencies)))
      (refuse :invalid-deployment-contract))
    (let [facts (preflight/inspect template-text)
          token ((:credential dependencies))
          enabled? (:enable? dependencies)
          root (:authority-root t)
          stores (:stores t)]
      (when-not (and (= :current (:sources facts)) (= :supported (:declaration facts)))
        (refuse :deployment-source-invalid))
      (when (and enabled? (not (and (string? token) (re-matches #"[0-9a-f]{64}" token))))
        (refuse :credential-unprovisioned))
      (when (and enabled? (not-every? #(.isDirectory (io/file %)) (vals stores)))
        (refuse :store-unprovisioned))
      {:run4
       (cond-> {:enabled? enabled? :operator "Joe" :casting (:casting t)
                :admission-root (:controller-and-admission stores)
                :pin-root root :pin-allowlist (:pin-allowlist t)
                :source-root root :source-allowlist (:source-allowlist t)
                :resolve-mission (:resolve-mission dependencies)
                :action-admissible? (:action-admissible? dependencies)
                :acceptance (:reserved-unwired t)
                :series {:enabled? enabled? :manifest-root root
                         :manifest-ref (get-in t [:manifest :ref])
                         :manifest-sha256 (get-in t [:manifest :sha256])
                         :manifest-allowlist #{(get-in t [:manifest :ref])}
                         :controller-root (:controller-and-admission stores)
                         :binding-root (:bindings stores)
                         :projection-root (:projections stores)
                         :run-record-root (:run-records stores)
                         :recording-enabled? enabled? :recording-root (:recordings stores)
                         :visibility-enabled? enabled? :visibility-root (:visibility stores)}}
         enabled? (assoc :bearer-token token))})))
