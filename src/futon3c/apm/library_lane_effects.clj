(ns futon3c.apm.library-lane-effects
  "Concrete Agency, Git-worktree, lease, and elaboration effects for the
  library lane. Constructing effects observes authority but never dispatches."
  (:require [clojure.java.io :as io]
            [futon3c.apm.library-lane-phases :as lane-phases]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.library-lane-launch :as launch]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.workspace-lifecycle :as workspace])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(def seat-types {:solver :codex :proctor :codex})

(defn- refusal [code & [finding]]
  (cond-> {:ok false :error/code code}
    finding (assoc :finding finding)))

(defn- regular-directory? [value]
  (and (string? value) (.isDirectory (io/file value))))

(defn- request [http-fn method agency-base path payload]
  (try
    (let [response (http-fn method (str agency-base path) payload)]
      (if (and (map? response) (:ok response)
               (<= 200 (long (or (:http/status response) 0)) 299))
        response
        (refusal :agency-observation-refused response)))
    (catch Throwable t
      (refusal :agency-observation-failed
               {:class (.getName (class t)) :message (.getMessage t)}))))

(defn- agents-response [http-fn agency-base]
  (let [response (request http-fn "GET" agency-base "/api/alpha/agents" nil)]
    (if (and (:ok response) (map? (:agents response)))
      response
      (refusal :agent-roster-unobservable response))))

(defn- frame-id-from-agent
  "Extract the frame id from a seat id like f21-solver.

  Agent ids arrive as KEYWORD map keys from the parsed roster, and (str
  :f21-solver) is \":f21-solver\" -- the leading colon defeats the anchor, so
  a str-based match silently extracted nothing and left the occupied set
  empty. An empty occupied set makes codex-frame-id's collision check vacuous
  without failing, which is the worst shape a guard can take. Accept both
  keywords and strings via name."
  [agent-id]
  (when (or (string? agent-id) (instance? clojure.lang.Named agent-id))
    (second (re-matches #"^(f[0-9]+)-.+$" (name agent-id)))))

(defn- observed-frame-directories [frames-root]
  (try
    (let [root (io/file frames-root)]
      (if-not (.isDirectory root)
        (refusal :frames-root-unobservable {:path (str frames-root)})
        {:ok true
         :frame-ids (->> (.listFiles root)
                         (filter #(.isDirectory %))
                         (map #(.getName %))
                         (filter #(re-matches #"f[0-9]+" %))
                         set)}))
    (catch Throwable t
      (refusal :frames-root-unobservable {:message (.getMessage t)}))))

(defn observe-occupied-frame-ids
  [{:keys [agency-base frames-root http-fn]
    :or {http-fn runtime/http-json}}]
  (let [roster (agents-response http-fn agency-base)
        directories (observed-frame-directories frames-root)]
    (if-not (and (:ok roster) (:ok directories))
      (refusal :occupied-frame-ids-unobservable
               {:roster (when-not (:ok roster) roster)
                :frames-root (when-not (:ok directories) directories)})
      {:ok true
       :occupied-frame-ids
       (into (:frame-ids directories)
             (keep frame-id-from-agent)
             (keys (:agents roster)))})))

(defn- lease-path [frames-root frame-id role]
  (io/file frames-root frame-id (str "workspace-" (name role) ".edn")))

(defn- atomic-edn! [file value]
  (let [target (.toPath (io/file file))
        directory (.getParent target)]
    (Files/createDirectories directory (make-array FileAttribute 0))
    (let [temporary (Files/createTempFile directory ".lease-" ".edn"
                                          (make-array FileAttribute 0))]
      (spit (str temporary) (str (pr-str value) "\n"))
      (Files/move temporary target
                  (into-array java.nio.file.CopyOption
                              [StandardCopyOption/ATOMIC_MOVE
                               StandardCopyOption/REPLACE_EXISTING]))
      (str target))))

(defn- read-lease [frames-root unit role]
  (let [file (lease-path frames-root (:frame/id unit) role)]
    (when (.isFile file)
      (try
        (let [lease (workspace/read-receipt file)]
          (when (and (= (:frame/id unit) (:frame/id lease))
                     (= (:problem/id unit) (:problem/id lease))
                     (= role (:role lease)))
            lease))
        (catch Throwable _ nil)))))

(defn- seat-projection [frame-id role agent]
  {:agent-id (str frame-id "-" (name role))
   :type (some-> (:type agent) name keyword)
   :frame-id frame-id
   :invoke-ready? (:invoke-ready? agent)
   :effective-timeouts (get-in agent [:metadata :effective-timeouts])})

(defn- agent-entry
  "Look up a seat in a parsed roster by id.

  The live agency parses JSON to KEYWORD keys, while fixtures seed strings.
  Accept both: a string-only lookup silently reported every seat missing even
  though the mint had just registered them."
  [agents id]
  (or (get agents (keyword id)) (get agents id)))

(defn- roster [http-fn agency-base frame-id]
  (let [response (agents-response http-fn agency-base)]
    (if-not (:ok response)
      response
      (let [agents (:agents response)
            seats (into {}
                        (map (fn [role]
                               [role (when-let [agent
                                                (agent-entry
                                                 agents
                                                 (str frame-id "-" (name role)))]
                                       (seat-projection frame-id role agent))]))
                        (keys seat-types))
            findings (cond-> []
                       (some nil? (vals seats)) (conj :frame-seat-missing)
                       (some #(not= :codex (:type %)) (vals seats))
                       (conj :frame-seat-type-invalid)
                       (some #(not (true? (:invoke-ready? %))) (vals seats))
                       (conj :frame-seat-not-invoke-ready))]
        (if (seq findings)
          (refusal :frame-roster-refused {:frame-id frame-id
                                          :findings findings})
          seats)))))

(defn- mint [http-fn agency-base corpus-root frame-id requested-types timeouts]
  (if-not (and (string? frame-id) (re-matches #"f[0-9]+" frame-id)
               (= seat-types requested-types) (map? timeouts))
    (refusal :seat-mint-shape-refused
             {:expected seat-types :requested requested-types
              :frame-id frame-id :timeouts timeouts})
    (let [responses
          (mapv (fn [role]
                  (request http-fn "POST" agency-base "/api/alpha/agents/restore"
                           {:agent-id (str frame-id "-" (name role))
                            :type "codex" :cwd corpus-root
                            :metadata {:frame-id frame-id
                                       :effective-timeouts timeouts}}))
                [:solver :proctor])]
      (if (every? :ok responses)
        {:ok true :agent-ids (mapv :agent-id responses)}
        (refusal :seat-restore-failed {:responses responses})))))

(defn- sorry-count [result]
  (count (re-seq #"declaration uses `sorry`"
                 (str (:out result) "\n" (:err result)))))

(defn- clean-verify? [problem-id solve verify]
  (and (map? verify)
       (= :frame-verify (:receipt/type verify))
       (= problem-id (:receipt/problem-id verify))
       (= (:receipt/final-head solve) (:receipt/final-head verify))
       (string? (:receipt/id verify))
       (string? (:receipt/final-head verify))
       (string? (:receipt/frame-id verify))
       (true? (:receipt/mathematical-sound? verify))))

(defn- outcome [run-fn frames-root {:keys [problem-id receipts]}]
  (let [solve (get receipts :solve)
        verify (get receipts :verify)]
    (if-not (clean-verify? problem-id solve verify)
      (refusal :outcome-verify-evidence-missing)
      (let [unit {:frame/id (:receipt/frame-id verify) :problem/id problem-id}
            lease (read-lease frames-root unit :solver)
            workspace-path (:workspace/path lease)
            path (str "problems/" problem-id "/lean/Main.lean")]
        (if-not (and lease (.isDirectory (io/file workspace-path)))
          (refusal :outcome-workspace-unobservable)
          (let [changed (run-fn workspace-path
                                ["git" "diff" "--name-only"
                                 (:base-revision lease)
                                 (:receipt/final-head verify) "--"
                                 "ConstructionTargets"])
                rollup (run-fn workspace-path
                               ["lake" "build" "ConstructionTargets"])
                ;; The workspace and trunk share a substrate. Build the
                ;; certified library source before elaborating its consumer,
                ;; otherwise Main can observe the prior trunk's stale olean.
                problem (when (zero? (:exit rollup))
                          (run-fn workspace-path ["lake" "env" "lean" path]))
                library-produced? (and (zero? (:exit changed))
                                       (some #(and (str/starts-with?
                                                    % "ConstructionTargets/")
                                                   (str/ends-with? % ".lean"))
                                             (str/split-lines (:out changed))))]
        (cond
          (not (zero? (:exit changed)))
          (refusal :outcome-library-diff-failed changed)
          (not (zero? (:exit rollup)))
          (refusal :outcome-library-elaboration-failed rollup)
          (pos? (sorry-count rollup))
          (refusal :outcome-library-carries-sorry
                   {:sorry-warnings (sorry-count rollup)})
          (not (zero? (:exit problem)))
          (refusal :outcome-problem-elaboration-failed problem)
          (zero? (sorry-count problem))
          {:verified-proof? true :remaining-sorries 0}
          (not library-produced?)
          (refusal :outcome-library-production-unverified)
          :else
          {:verified-library? true :library-sorry-warnings 0
           :problem-open? true
           :remaining-sorries (sorry-count problem)
           :boundary "problem remains open after verified sorry-free library work"})))))))

(defn live-effects
  "Observe and construct every effect required by library-lane-launch/launch!.
  Optional http-fn/run-fn are test seams; production uses the existing Agency
  HTTP client and process runner."
  [{:keys [agency-base corpus-root frames-root http-fn run-fn]
    :or {http-fn runtime/http-json
         run-fn (fn [dir argv]
                  (apply shell/sh (concat argv [:dir (str dir)])))}}]
  (cond
    (not (and (string? agency-base) (not (str/blank? agency-base))))
    (refusal :agency-base-unobservable)
    (not (regular-directory? corpus-root))
    (refusal :corpus-root-unobservable {:path corpus-root})
    (not (regular-directory? frames-root))
    (refusal :frames-root-unobservable {:path frames-root})
    :else
    (let [occupied (observe-occupied-frame-ids
                    {:agency-base agency-base :frames-root frames-root
                     :http-fn http-fn})]
      (if-not (:ok occupied)
        occupied
        {:observe-problem-fn launch/observe-problem
         :provision-fn
         (fn [unit role]
           (let [result (workspace/provision!
                         {:unit unit :role role :workspace-root frames-root
                          :substrate-path (str (io/file corpus-root ".lake"))})]
             (if-not (:ok result)
               result
               (try
                 (atomic-edn! (lease-path frames-root (:frame/id unit) role)
                              (:lease result))
                 result
                 (catch Throwable t
                   (refusal :workspace-lease-persist-failed
                            {:message (.getMessage t)}))))))
         ;; allow-advance?: on resume the solver has committed, so its worktree
         ;; head and problem blob legitimately differ from the pins. A fresh
         ;; provision still takes the strict path, because head == base there.
         :validate-workspace-fn lane-phases/validate-workspace
         :workspace-exists?
         (fn [unit role]
           (.isDirectory
            (io/file frames-root
                     (str (:frame/id unit) "-" (:problem/id unit) "-"
                          (name role)))))
         :leases-fn
         (fn [unit]
           (into {}
                 (keep (fn [role]
                         (when-let [lease (read-lease frames-root unit role)]
                           [role lease])))
                 [:solver]))
         :roster-fn (partial roster http-fn agency-base)
         :mint-fn (partial mint http-fn agency-base corpus-root)
         :occupied-frame-ids (:occupied-frame-ids occupied)
         :outcome-fn (partial outcome run-fn frames-root)}))))
