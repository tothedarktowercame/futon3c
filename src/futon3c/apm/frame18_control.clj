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
(def continuation-receipt-path
  "holes/labs/M-apm-demonstration/frame-18-continuation-receipt.edn")
(def preflight-receipt-path
  "holes/labs/M-apm-demonstration/frame-18-preflight-receipt.edn")
(def solve-receipt-path
  "holes/labs/M-apm-demonstration/frame-18-solve-receipt.edn")
(def verify-receipt-path
  "holes/labs/M-apm-demonstration/frame-18-verify-receipt.edn")
(def state-directory
  (Path/of "data/apm-campaigns/frame-18-bounded-admission"
           (make-array String 0)))
(def ledger-path (.resolve state-directory "ledger.edn"))
(def certificate-directory (.resolve state-directory "certificates"))
(def projection-directory (.resolve state-directory "projection"))

(declare valid-solve-receipt?)

(defn- fetch-json [url]
  (let [connection ^java.net.HttpURLConnection
        (.openConnection (java.net.URL. url))]
    (.setConnectTimeout connection 2000)
    (.setReadTimeout connection 5000)
    (json/parse-string (slurp (.getInputStream connection)) true)))

(defn- git [& args]
  (let [result (apply shell/sh "git" args)]
    (when (zero? (:exit result)) (str/trim (:out result)))))

(defn- workspace-command [path command]
  (apply shell/sh (concat command [:dir path])))

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
        continuation-receipt (edn/read-string (slurp continuation-receipt-path))
        agents-response (fetch-json "http://localhost:7070/api/alpha/agents")
        seat-ids (into {} (map (fn [role]
                                 [role (str "f18-" (name role))])
                               qualification/required-roles))
        seat-configs (qualification/seat-configs-from-roster
                      agents-response seat-ids)
        agents (:agents agents-response)
        requested-cast (dissoc (:frame/cast control) :analyst)
        cast-matches?
        (every? (fn [[role {:keys [type]}]]
                  (let [agent (or (get agents (get seat-ids role))
                                  (get agents (keyword (get seat-ids role))))]
                    (and (= (name type) (:type agent))
                         (true? (:invoke-ready? agent)))))
                requested-cast)
        head (git "rev-parse" "HEAD")
        branch (git "branch" "--show-current")
        worktree (System/getProperty "user.dir")
        clean? (str/blank? (or (git "status" "--porcelain") "not-clean"))
        harness-hash (get-in action [:completion :event/body :harness-hash])
        active-frame (get-in loaded [:projection :active/frame])
        problem-repository (:problem/repository control)
        problem-revision (:problem/revision control)
        problem-blob (git "-C" problem-repository "rev-parse"
                          (str problem-revision ":" (:problem/path control)))
        workspaces (:frame/workspaces control)
        solve-receipt (when (.isFile (java.io.File. solve-receipt-path))
                        (edn/read-string (slurp solve-receipt-path)))
        completed-solve? (and (= :solve (:kind action))
                              (valid-solve-receipt? solve-receipt action))
        workspace-observations
        (into {}
              (map (fn [[role {:keys [path branch base-revision
                                      execution-substrate]}]]
                     (let [probe (workspace-command
                                  path (get-in execution-substrate
                                               [:probe :command]))
                           version (workspace-command
                                    path ["lake" "env" "lean" "--version"])
                           manifest (workspace-command
                                     path ["sha256sum" "lake-manifest.json"])
                           lake-path (.getCanonicalPath
                                      (java.io.File. path ".lake"))]
                       [role {:branch (git "-C" path "branch" "--show-current")
                            :head (git "-C" path "rev-parse" "HEAD")
                            :clean? (str/blank?
                                     (or (git "-C" path "status" "--porcelain")
                                         "not-clean"))
                            :expected-branch branch
                            :expected-head base-revision
                            :substrate-ready?
                            (and (= lake-path (:path execution-substrate))
                                 (= (:lean-version execution-substrate)
                                    (str/trim (:out version)))
                                 (str/starts-with?
                                  (:out manifest)
                                  (:lake-manifest-sha256 execution-substrate))
                                 (= (get-in execution-substrate
                                            [:probe :expected-exit])
                                    (:exit probe)))}])))
              workspaces)
        specification-check
        (frame-specification/ingest control-path active-frame-id
                                    registration-digest)
        manifest-matches?
        (= (machine/ledger-digest [control])
           (get-in loaded [:projection :campaign/manifest-hash]))]
    {:specification-check
     specification-check
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
     :seat-configs seat-configs
     :cast-check {:ready? cast-matches? :attributed? cast-matches?}
     :continuation-check
     {:durable? (:receipt/durable? continuation-receipt)
      :wake-tested? (:receipt/wake-tested? continuation-receipt)}
     :projection-check
     {:ledger-derived? (and (:ok loaded) (= (:projection loaded) replayed))
      :frame-matches? (and (= active-frame-id (:frame-id active-frame))
                           (= (:problem/id control) (:problem-id active-frame)))}
     :separation-check
     {:author-reviewer-distinct?
      (true? (get-in control [:frame/separation-policy
                              :author-reviewer-distinct?]))
      :arms-isolated? (true? (get-in control [:frame/separation-policy
                                              :arms-isolated?]))}
     :apparatus-check
     {:unchanged-since-open? (and manifest-matches?
                                  (:valid? specification-check)
                                  (= problem-blob (:problem/blob control)))}
     :workspace-check
     {:ready? (every? (fn [[role observation]]
                        (and (:clean? observation)
                             (:substrate-ready? observation)
                             (= (:branch observation)
                                (:expected-branch observation))
                             (or (= (:head observation)
                                    (:expected-head observation))
                                 (and completed-solve?
                                      (= role :solver)
                                      (= (:head observation)
                                         (:receipt/final-head
                                          solve-receipt))))))
                      workspace-observations)
      :isolated? (= (count workspaces)
                    (count (set (map :path (vals workspaces)))))}
     :receipt-check
     {:durable? (and (:ok loaded)
                     (= :valid (get-in loaded [:projection :projection/status])))
      :replayable? (and (:ok loaded)
                        (= (:projection loaded) replayed))}}))

(defn- plan []
  (edn/read-string (slurp plan-path)))

(defn- valid-preflight-receipt? [receipt action]
  (and (= (:receipt/id receipt)
          (machine/ledger-digest [(dissoc receipt :receipt/id)]))
       (= :frame-preflight (:receipt/type receipt))
       (= (:frame-id action) (:receipt/frame-id receipt))
       (= (:problem-id action) (:receipt/problem-id receipt))
       (= :preflight-passed (:receipt/result receipt))
       (= {:exit 0 :warnings 1 :sorry-warnings 1 :errors 0}
          (:receipt/lean receipt))
       (= :non-topology (:receipt/classification receipt))
       (true? (:receipt/clean-before? receipt))
       (true? (:receipt/clean-after? receipt))))

(defn- valid-solve-receipt? [receipt action]
  (and (map? receipt)
       (= (:receipt/id receipt)
          (machine/ledger-digest [(dissoc receipt :receipt/id)]))
       (= :frame-solve (:receipt/type receipt))
       (= (:frame-id action) (:receipt/frame-id receipt))
       (= (:problem-id action) (:receipt/problem-id receipt))
       (= :solved (:receipt/result receipt))
       (= {:exit 0 :warnings 0 :sorry-warnings 0 :errors 0 :output ""}
          (:receipt/lean receipt))
       (= '[propext Classical.choice Quot.sound] (:receipt/axioms receipt))
       (true? (:receipt/statement-unchanged? receipt))
       (true? (:receipt/clean-after? receipt))))

(defn- valid-verify-receipt? [receipt action]
  (and (map? receipt)
       (= (:receipt/id receipt)
          (machine/ledger-digest [(dissoc receipt :receipt/id)]))
       (= :frame-verify (:receipt/type receipt))
       (= (:frame-id action) (:receipt/frame-id receipt))
       (= (:problem-id action) (:receipt/problem-id receipt))
       (= :verified (:receipt/result receipt))
       (= {:exit 0 :warnings 0 :sorry-warnings 0 :errors 0 :output ""}
          (:receipt/lean receipt))
       (= '[propext Classical.choice Quot.sound] (:receipt/axioms receipt))
       (true? (:receipt/statement-unchanged? receipt))
       (true? (:receipt/mathematical-sound? receipt))
       (true? (:receipt/non-vacuous? receipt))
       (true? (:receipt/clean-before? receipt))
       (true? (:receipt/clean-after? receipt))
       (empty? (:receipt/mutations receipt))))

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
                     :finding response})))
              :preflight
              (fn [action]
                (let [receipt (edn/read-string (slurp preflight-receipt-path))]
                  (if (valid-preflight-receipt? receipt action)
                    {:ok true :certificate receipt}
                    {:ok false :error/code :frame-preflight-receipt-invalid
                     :finding {:receipt/id (:receipt/id receipt)}})))
              :solve
              (fn [action]
                (let [receipt (edn/read-string (slurp solve-receipt-path))]
                  (if (valid-solve-receipt? receipt action)
                    {:ok true :certificate receipt}
                    {:ok false :error/code :frame-solve-receipt-invalid
                     :finding {:receipt/id (:receipt/id receipt)}})))
              :verify
              (fn [action]
                (let [receipt (edn/read-string (slurp verify-receipt-path))]
                  (if (valid-verify-receipt? receipt action)
                    {:ok true :certificate receipt}
                    {:ok false :error/code :frame-verify-receipt-invalid
                     :finding {:receipt/id (:receipt/id receipt)}})))}
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

(defn advance! [expected-kind]
  (let [bootstrapped (bootstrap!)
        inspection (inspect!)]
    (if-not (and (:ok bootstrapped) (:ok inspection)
                 (= :ready (:stepper/status inspection))
                 (= expected-kind
                    (get-in inspection [:obligation :obligation/action :kind])))
      {:ok false :status :precondition-failed
       :expected-kind expected-kind
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

(defn open-block! [] (advance! :open-block))

(defn -main [& [command]]
  (let [result (case command
                 "bootstrap" (bootstrap!)
                 "inspect" (inspect!)
                 "open-block" (open-block!)
                 "preflight" (advance! :preflight)
                 "solve" (advance! :solve)
                 "verify" (advance! :verify)
                 {:ok false :error/code :frame18-command-unknown})]
    (prn result)
    (when-not (:ok result) (System/exit 1))))
