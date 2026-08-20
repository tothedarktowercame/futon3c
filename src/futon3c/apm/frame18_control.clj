(ns futon3c.apm.frame18-control
  "Concrete operator-stepped controller for the frame-18 qualification run."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-postconditions :as postconditions]
            [futon3c.apm.campaign-qualification :as qualification]
            [futon3c.apm.campaign-stepper :as stepper]
            [futon3c.apm.frame-specification :as frame-specification])
  (:import [java.nio.file Path]
           [java.time Instant]))

(def control-path
  "holes/labs/M-apm-demonstration/frame-18-control.edn")
(def plan-path
  "holes/labs/M-apm-demonstration/frame-18-step-plan.edn")
(def state-directory
  (Path/of "data/apm-campaigns/frame-18-bounded-admission"
           (make-array String 0)))
(def ledger-path (.resolve state-directory "ledger.edn"))
(def certificate-directory (.resolve state-directory "certificates"))
(def projection-directory (.resolve state-directory "projection"))

(defn- fetch-json [url]
  (let [connection ^java.net.HttpURLConnection
        (.openConnection (java.net.URL. url))]
    (.setConnectTimeout connection 2000)
    (.setReadTimeout connection 5000)
    (json/parse-string (slurp (.getInputStream connection)) true)))

(defn- git [& args]
  (let [result (apply shell/sh "git" args)]
    (when (zero? (:exit result)) (str/trim (:out result)))))

(defn- post-json [url payload]
  (let [connection ^java.net.HttpURLConnection
        (.openConnection (java.net.URL. url))]
    (.setRequestMethod connection "POST")
    (.setConnectTimeout connection 2000)
    (.setReadTimeout connection 30000)
    (.setRequestProperty connection "Content-Type" "application/json")
    (.setDoOutput connection true)
    (with-open [writer (java.io.OutputStreamWriter. (.getOutputStream connection))]
      (.write writer (json/generate-string payload)))
    (let [status (.getResponseCode connection)
          stream (if (< status 400) (.getInputStream connection)
                     (.getErrorStream connection))]
      (assoc (json/parse-string (slurp stream) true) :http/status status))))

(defn- frame-runtime-observation [_]
  (try
    (let [jobs-response (fetch-json
                         "http://localhost:7070/api/alpha/invoke/jobs?limit=500")
          frame-jobs (filterv #(str/starts-with? (or (:agent-id %) "") "f18-")
                              (:jobs jobs-response))]
      {:binding-response {:ok true :bound? false}
       :jobs-response {:ok true :jobs frame-jobs}})
    (catch Throwable t
      {:binding-response {:ok false :error :runtime-observation-failed
                          :message (.getMessage t)}
       :jobs-response {:ok false :jobs [] :error :runtime-observation-failed}})))

(defn- qualification-observation [{:keys [obligation]}]
  (let [loaded (ledger/read-ledger ledger-path)
        replayed (when (:ok loaded)
                   (machine/projection (:events loaded)))
        action (:obligation/action obligation)
        active-frame-id (or (:frame-id action)
                            (get-in loaded [:projection :active/frame :frame-id])
                            "f18")
        registration-digest (get-in action [:completion :event/body
                                            :registration-hash])
        control (edn/read-string (slurp control-path))
        head (git "rev-parse" "HEAD")
        branch (git "branch" "--show-current")
        worktree (System/getProperty "user.dir")
        clean? (str/blank? (or (git "status" "--porcelain") "not-clean"))
        harness-hash (get-in action [:completion :event/body :harness-hash])]
    {:specification-check
     (frame-specification/ingest control-path active-frame-id
                                 registration-digest)
     :problem-check
     {:topology? (get-in control [:problem/classification :topology?])
      :classification-source (get-in control [:problem/classification :source])}
     :registration-check
     {:frame-timeout-ms (* 60000 (get-in control [:frame/timeout-policy
                                                   :frame-minutes]))
      :complete? (every? some? [branch head worktree harness-hash])
      :coherent? (and (= branch (:frame/control-branch control))
                      (= head harness-hash))
      :branch branch :commit head :worktree worktree
      :worktree-clean? clean? :head-matches? (= head harness-hash)
      :dedicated-worktree? (not= worktree "/home/joe/code/futon3c")}
     :receipt-check
     {:durable? (and (:ok loaded)
                     (= :valid (get-in loaded [:projection :projection/status])))
      :replayable? (and (:ok loaded)
                        (= (:projection loaded) replayed))}}))

(defn- plan []
  (edn/read-string (slurp plan-path)))

(defn- options []
  {:ledger-path ledger-path
   :certificate-directory certificate-directory
   :projection-directory projection-directory
   :observation-fn frame-runtime-observation
   :now-fn #(Instant/now)
   :project-fn identity
   :gate-provider (qualification/gate-provider
                   (plan) qualification-observation)
   :postcondition-fn postconditions/validate
   :handlers {:open-block
              (fn [action]
                {:ok true
                 :certificate {:gate :durable-replay
                               :block-id (:block-id action)
                               :control (edn/read-string (slurp control-path))}})
              :open-frame
              (fn [action]
                (let [control (edn/read-string (slurp control-path))
                      cast (-> (:frame/cast control) (dissoc :analyst))
                      response (post-json
                                "http://localhost:7070/api/alpha/frames/mint-seats"
                                {:frame-id (:frame-id action) :cast cast})]
                  (if (and (= 200 (:http/status response)) (:ok response))
                    {:ok true :certificate
                     {:effect :frame-seats-minted :response response}}
                    {:ok false :error/code :frame-seat-mint-failed
                     :finding response})))}
   :actor "frame-18-control"})

(defn bootstrap! []
  (let [loaded (ledger/read-ledger ledger-path)]
    (cond
      (not (:ok loaded)) loaded
      (seq (:events loaded))
      {:ok (= "apm-countdown" (get-in loaded [:projection :campaign/id]))
       :status :already-registered :projection (:projection loaded)}
      :else
      (let [control (edn/read-string (slurp control-path))
            body {:series :apm
                  :manifest-hash (machine/ledger-digest [control])
                  :phase-order [:preflight :solve :verify :close-frame]
                  :block-plan
                  [{:block-id "countdown-10" :ordinal 1
                    :units [{:frame-id "f18" :problem-id (:problem/id control)
                             :arm :treatment
                             :registration-hash
                             (machine/ledger-digest [control])
                             :harness-hash (git "rev-parse" "HEAD")}]}]
                  :obligation-plan
                  {:preflight {:kind :preflight :role :proctor}
                   :solve {:kind :solve :role :solver}
                   :verify {:kind :verify :role :proctor}
                   :close-frame {:kind :close-frame :role :guide}}
                  :claims-required? true}
            event-base {:event/seq 0 :event/type :campaign/registered
                        :event/campaign-id "apm-countdown"
                        :event/actor "frame-18-control"
                        :event/at (str (Instant/now))
                        :event/expected-version 0 :event/body body}
            event (assoc event-base :event/id
                         (machine/ledger-digest [event-base]))
            initial (machine/projection [])]
        (ledger/compare-and-append! ledger-path 0
                                    (:ledger/digest initial) event)))))

(defn inspect! []
  (stepper/inspect! (options)))

(defn open-block! []
  (let [bootstrapped (bootstrap!)
        inspection (inspect!)]
    (if-not (and (:ok bootstrapped) (:ok inspection)
                 (= :ready (:stepper/status inspection)))
      {:ok false :status :precondition-failed
       :bootstrap bootstrapped :inspection inspection}
      (let [issued (stepper/issue-permit
                    {:report (:report inspection) :issuer "joe"
                     :issued-at (str (Instant/now))})
            permit (:permit issued)]
        (if-not (:ok issued)
          issued
          (stepper/step!
           (assoc (options) :permit permit
                  :trusted-permit-id (:permit/id permit)
                  :trusted-issuer "joe")))))))

(defn -main [& [command]]
  (let [result (case command
                 "bootstrap" (bootstrap!)
                 "inspect" (inspect!)
                 "open-block" (open-block!)
                 {:ok false :error/code :frame18-command-unknown})]
    (prn result)
    (when-not (:ok result) (System/exit 1))))
