(ns futon3c.wm.run4-codex-fold
  "Authenticated server-owned Agency port for RUN4 construction folds."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.apm.job-port :as jobs])
  (:import [java.nio.file Files LinkOption]
           [java.util UUID]))

(defn- refuse [reason & [data]]
  {:fold/refused true :refusal/class reason
   :why (str "Server-owned Codex fold refused: " (name reason))
   :fold/refusal-evidence data})

(defn- safe-file [root ref]
  (let [base (.getCanonicalFile (io/file root))
        file (.getCanonicalFile (io/file base ref))]
    (when (and (string? ref) (not (.isAbsolute (io/file ref)))
               (.startsWith (.toPath file) (.toPath base))
               (Files/isRegularFile (.toPath file)
                                    (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))
      file)))

(defn make-port
  "Construct a synchronous fold function from server-owned authority. Ports are
  injected for hermetic tests; production defaults to the canonical Agency API."
  [{:keys [root plan-ref plan-sha256 seat agency-base caller]
    :as authority}
   & [{:keys [activate-fn await-fn]
       :or {activate-fn jobs/activate! await-fn jobs/await-terminal!}}]]
  (when-not (and (= :wm/codex-fold-authority-v1 (:schema authority))
                 (= #{:schema :root :plan-ref :plan-sha256 :seat :agency-base :caller}
                    (set (keys authority)))
                 (string? root) (string? plan-ref)
                 (string? plan-sha256) (re-matches #"[0-9a-f]{64}" plan-sha256)
                 (string? seat) (str/starts-with? seat "codex-")
                 (string? agency-base) (not (str/blank? agency-base))
                 (string? caller) (not (str/blank? caller))
                 (fn? activate-fn) (fn? await-fn))
    (throw (ex-info "Codex fold authority refused" {:reason :invalid-fold-authority})))
  (let [plan-file (or (safe-file root plan-ref)
                      (throw (ex-info "Codex fold plan refused"
                                      {:reason :fold-plan-refused})))
        plan-text (slurp plan-file)]
    (when-not (= plan-sha256 (digest/sha256 plan-text))
      (throw (ex-info "Codex fold plan digest mismatch"
                      {:reason :fold-plan-digest-mismatch})))
    (let [plan (edn/read-string plan-text)]
      (when-not (and (= :wm/codex-fold-plan-v1 (:schema plan))
                     (= #{:schema :prompt-prefix} (set (keys plan)))
                     (string? (:prompt-prefix plan))
                     (not (str/blank? (:prompt-prefix plan))))
        (throw (ex-info "Codex fold plan invalid" {:reason :fold-plan-invalid})))
      (fn [construction]
        ;; Reread immediately before dispatch: the captured plan is authority,
        ;; but changed authority must not silently execute.
        (if-not (= plan-text (slurp plan-file))
          (refuse :fold-plan-source-drift {:plan-ref plan-ref})
          (let [job-id (str "run4-fold-" (UUID/randomUUID))
                prompt (str (:prompt-prefix plan) "\n\nConstruction EDN:\n"
                            (pr-str construction)
                            "\n\nReturn exactly one EDN fold-output map and no prose.")
                activation (activate-fn agency-base
                                        {:agent-id seat :prompt prompt
                                         :surface "bell" :caller caller
                                         :job-id job-id})]
            (if-not (and (:ok activation) (:accepted? activation)
                         (= job-id (:job-id activation)))
              (refuse :fold-dispatch-refused {:job-id job-id})
              (let [observed (await-fn agency-base
                                       {:job-id job-id :activation-accepted? true})
                    terminal (get-in observed [:dispatch-observation :terminal])]
                (if-not (and (:ok observed) (= job-id (:job-id terminal))
                             (= seat (:agent-id terminal)) (= :done (:state terminal))
                             (map? (:report terminal)))
                  (refuse :fold-job-invalid
                          {:job-id job-id :state (:state terminal)
                           :agent-id (:agent-id terminal)})
                  (assoc (:report terminal)
                         :fold/execution
                         {:schema :wm/codex-fold-execution-v1
                          :job-id job-id :seat seat
                          :plan-ref plan-ref :plan-sha256 plan-sha256}))))))))))
