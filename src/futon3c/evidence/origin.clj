(ns futon3c.evidence.origin
  "Write-time source provenance, independent of attribution and authorization.
   Never classify by prompt text. Unknown producers remain unknown."
  (:require [clojure.string :as str]
            [futon1b-origin :as contract]))

(def ^:dynamic *input* nil)
(def harness-sources
  #{"auto-bellback" "auto" "system" "cron" "heartbeat" "apm-harvest"
    "claude-loop" "parked-resume" "continuation" "followup" "inbox-zero"
    "apm-store-repair" "kimi-work-target"
    ;; Unregistered scheduler scripts that bell seats via agency_send --from.
    ;; apm-lean: scripts/solve-loop.py, topology-contract/topology-build-loop.sh
    ;; and topology-supervisor.sh.  Without these their turns read Origin: unknown.
    "solve-loop" "topology-build-loop" "topology-supervisor"})
(defn source
  "Classify known routing context at dispatch time. A known harness surface wins
   over a caller named joe. Registered-agent? describes the routing registry,
   not authentication; this stamp never establishes a grant."
  [{:keys [caller surface registered-agent? source-id]}]
  (let [c (some-> caller str str/trim) s (some-> surface str str/trim)
        harness (some harness-sources [s c])
        kind (cond harness :harness
                   (and (#{"joe" "joe-repl"} c)
                        (#{"emacs-repl" "emacs-codex-repl"} s)) :operator
                   registered-agent? :agent
                   :else :unknown)]
    (cond-> {:kind kind :actor (or harness (not-empty c) "unknown")}
      (not-empty s) (assoc :surface s)
      source-id (assoc :source-id (str source-id)))))

(defn stamp
  [entry source writer]
  (let [author (or (:evidence/author entry) (:author entry))
        source (or source {:kind :unknown :actor "unknown"})
        supplied (or (:evidence/origin entry) (:origin entry))
        origin (merge (select-keys source [:kind :actor :source-id :surface])
                      {:writer writer :attributed-author author :authorization :unknown
                       :recorded-at (str (java.time.Instant/now)) :basis :write-time})]
    (when (and supplied (not= :unknown (:kind source))
               (not= (:kind source) (:kind (contract/normalize supplied))))
      (throw (ex-info "Origin claim conflicts with known producer"
                      {:error/code :origin-conflict :expected (:kind source)})))
    (let [chosen (or supplied origin)]
      (when-not (and (contract/valid? chosen)
                     (= author (:attributed-author (contract/normalize chosen))))
        (throw (ex-info "Invalid write-time origin" {:error/code :invalid-origin})))
      (assoc entry (if (some #(= "evidence" (namespace %)) (filter keyword? (keys entry)))
                     :evidence/origin :origin)
             (contract/normalize chosen)))))

(defn harness [actor source-id]
  (cond-> {:kind :harness :actor actor} source-id (assoc :source-id (str source-id))))
