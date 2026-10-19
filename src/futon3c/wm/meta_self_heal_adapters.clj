(ns futon3c.wm.meta-self-heal-adapters
  "Concrete serving adapters for the dedicated META repair transaction.

   The adapters deliberately expose readiness separately from execution.  A
   caller can therefore prove the exact cast/repository/reload configuration
   without dispatching an agent or accepting a click."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.agency.registry :as reg]
            [futon3c.wm.code-identity :as code-identity]))

(def default-cast
  {:author "wm-author" :reviewer "wm-reviewer"
   :repair-reviewer "wm-repair-reviewer"})
(def default-repository "/home/joe/code/futon2")

(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))
(defn- git [repo & args]
  (let [{:keys [exit out err]} (apply shell/sh "git" "-C" repo args)]
    {:ok? (zero? exit) :out (str/trim out) :err (str/trim err)}))

(defn readiness
  "Read-only readiness receipt.  It names every resolved authority and never
   resets a session, dispatches work, reloads code, or queues a click."
  [opts]
  (let [cast (merge default-cast (select-keys opts (keys default-cast)))
        seats (vals cast)
        repo (or (:target-repository opts) default-repository)
        records (into {} (map (fn [seat] [seat (reg/get-agent seat)]) seats))
        tracked (git repo "status" "--porcelain" "--untracked-files=no")
        defects (cond-> []
                  (not= 3 (count (set seats))) (conj :cast-not-distinct)
                  (not-every? #(and % (true? (:agent/invoke-ready? %))
                                    (contains? #{:idle :restored} (:agent/status %)))
                              (vals records))
                  (conj :cast-not-ready)
                  (not (.isDirectory (io/file repo))) (conj :repository-unreadable)
                  (or (not (:ok? tracked)) (seq (:out tracked)))
                  (conj :tracked-worktree-dirty)
                  (not (nonblank? (:meta-injury-ledger-path opts)))
                  (conj :injury-ledger-path)
                  (not (nonblank? (:meta-repair-transaction-path opts)))
                  (conj :transaction-path)
                  (not (ifn? (:queue-retry-fn opts))) (conj :queued-retry-authority))]
    {:schema :wm/meta-self-heal-serving-readiness-v1
     :status (if (seq defects) :refused :ready)
     :defects defects :cast cast :repository repo
     :tracked-head (:out (git repo "rev-parse" "HEAD"))
     :ledger-path (:meta-injury-ledger-path opts)
     :transaction-path (:meta-repair-transaction-path opts)
     :dispatch :futon3c.agency.registry/invoke-agent!
     :session-reset :futon3c.agency.registry/reset-session!
     :reload :futon3c.wm.code-identity/load-file-recorded!
     :retry :queued-after-parent-release}))

(defn reset-cast!
  "Reset exactly the explicit three-seat cast and return the fresh session ids."
  [cast]
  (reduce-kv
   (fn [acc role seat]
     (let [result (reg/reset-session! seat)
           current (reg/get-agent seat)]
       (if-not (:ok result)
         (reduced {:status :refused :reason :cast-reset-failed
                   :role role :seat seat :result result})
         (assoc-in acc [:sessions role] (:agent/session-id current)))))
   {:status :reset :cast cast :sessions {}} cast))

(defn- one-edn-map [text]
  (try
    (let [text (str/trim (str text))
          fenced (re-matches #"(?s)```(?:edn)?\s*(.*?)\s*```" text)
          value (edn/read-string (or (second fenced) text))]
      (when (map? value) value))
    (catch Throwable _ nil)))

(defn agency-invoke!
  "Synchronous Agency port used inside the already-checkpointed coordinator.
   The prompt requires a single EDN result so phase evidence is machine typed."
  [{:keys [seat session-id request]}]
  (let [reset (reg/reset-session! seat)
        prompt (str "Dedicated META self-heal phase. Do not select ordinary work. "
                    "Execute only the sealed request and return exactly one EDN map "
                    "with :status and :payload (no prose).\n\n" (pr-str request))
        result (when (:ok reset)
                 (reg/invoke-agent! seat prompt
                                    {:mode "work" :caller "war-machine"
                                     :session-id session-id}))
        parsed (when (:ok result) (one-edn-map (:result result)))
        job-id (or (get-in result [:invoke-meta :job-id]) (:job-id parsed))]
    (cond
      (not (:ok reset)) {:status :failed :payload {:reason :agency-session-reset-failed
                                                    :reset reset}}
      (not (:ok result)) {:status :failed :payload {:reason :agency-invoke-failed
                                                     :response result}}
      (not (and parsed (map? (:payload parsed))))
      {:status :failed :payload {:reason :agency-response-not-one-edn-map}
       :session-id (:session-id result)}
      (not (#{:succeeded :failed :rejected} (:status parsed)))
      {:status :failed :payload {:reason :agency-response-status-invalid}}
      (not (nonblank? (:session-id result)))
      {:status :failed :payload {:reason :agency-session-missing}}
      (or (not (nonblank? job-id)) (= (:old-session-id reset) (:session-id result)))
      {:status :failed :payload {:reason :agency-session-or-job-not-fresh}}
      :else {:status (:status parsed) :payload (:payload parsed)
             :session-id (:session-id result) :job-id job-id})))

(defn enact!
  "Verify the reviewed patch is canonical HEAD, then reload only its changed
   Clojure sources through the serving JVM's recorded reload authority."
  [{:keys [work-order] :as request}]
  (let [repo (:target-repository work-order)
        patch (get-in request [:transaction :receipts 0 :payload :patch-sha])
        patch (or patch (:patch-sha request))
        head (:out (git repo "rev-parse" "HEAD"))]
    (if-not (= patch head)
      {:status :failed :payload {:reason :patch-not-canonical-head
                                 :patch-sha patch :canonical-head head}}
      (let [parent (:out (git repo "rev-parse" (str patch "^")))
            changed (:out (git repo "diff" "--name-only" parent patch))
            sources (filter #(and (str/starts-with? % "src/")
                                  (str/ends-with? % ".clj"))
                            (str/split-lines changed))
            receipts (mapv #(code-identity/load-file-recorded!
                             (str repo "/" %)) sources)
            retry-click (str "wm-retry-" (:repair-id work-order) "-"
                             (:attempt work-order))]
        {:status :succeeded
         :payload {:patch-sha patch :retry-click retry-click
                   :integration-commit head :reload-receipt receipts}}))))

(defn ports
  "Resolve concrete ports after a successful readiness check. Retry only
   persists/enqueues; the supplied service callback runs it after release."
  [opts]
  (let [ready (readiness opts)]
    (if-not (= :ready (:status ready)) ready
      {:status :ready :readiness ready
       :agency-invoke-fn agency-invoke!
       :repair-enact-fn enact!
       :repair-retry-fn
       (fn [{:keys [work-order] :as request}]
         ((:queue-retry-fn opts)
          {:repair-id (:repair-id work-order)
           :injury-id (:injury-id work-order)
           :attempt (:attempt work-order)
           :patch-sha (get-in request [:transaction :receipts 0 :payload :patch-sha])
           :retry-click (get-in request [:transaction :receipts 2 :payload :retry-click])
           :request request})
         {:status :queued
          :payload {:repair-id (:repair-id work-order)
                    :patch-sha (get-in request [:transaction :receipts 0 :payload :patch-sha])
                    :retry-click (get-in request [:transaction :receipts 2 :payload :retry-click])}})})))
