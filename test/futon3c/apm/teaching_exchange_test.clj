(ns futon3c.apm.teaching-exchange-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.apm.teaching-exchange :as sut]
            [futon3c.apm.teaching-plan :as plan]
            [futon3c.apm.typed-role-submission :as submission]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.live-learning-phases :as learning]
            [futon3c.apm.role-memory-search :as memory])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def initial-plan
  {:version 1 :problem-id "development-problem" :revision 0 :parent-digest nil
   :nodes [{:id "root" :parent nil :goal "Construct a continuous extension"
            :definitions ["Continuity quantified at every point"]
            :conditions [{:statement "Domain is dense" :status "open" :argument "Check the statement"}]
            :depends-on [] :warrant {:kind "gap" :explanation "Choose an approximating sequence"}}]
   :responses []})
(defn judgment [p verdict]
  {:plan-digest (plan/digest p) :verdict verdict :reason "Fixture judgment, not a mathematics check"
   :nodes (mapv #(hash-map :node-id (:id %) :verdict (if (= verdict "accept") "suitable" "revise")
                           :diagnosis "applicability" :reason "Make density explicit"
                           :instruction "Explain how to choose the sequence") (:nodes p))})
(defn with-fixture [f]
  (let [root (.toFile (Files/createTempDirectory "v4-teaching-test" (make-array FileAttribute 0)))
        path (.toPath (io/file root "student.edn"))
        sessions (atom {"student" "student-session" "ta" "ta-session"})
        jobs (atom {}) calls (atom []) prepares (atom 0)
        http (fn [method url body]
               (swap! calls conj [method url body])
               (cond
                 (str/includes? url "/agents/")
                 (let [id (last (str/split url #"/"))]
                   {:http/status 200 :ok true :agent-id id :agent {:session-id (@sessions id)}})
                 (= method "GET") {:http/status 200 :job (@jobs (last (str/split url #"/")))}
                 (str/ends-with? url "/announce")
                 (let [id (:job-id body)]
                   (swap! jobs #(if (% id) % (assoc % id {:job-id id :agent-id (:agent-id body)
                                                        :session-id (@sessions (:agent-id body)) :state "queued"})))
                   {:http/status 202 :ok true :accepted true :job-id id :state (get-in @jobs [id :state])})
                 (str/ends-with? url "/activate")
                 (do (when (nil? (@sessions (:agent-id body)))
                       (swap! sessions assoc (:agent-id body) "fresh-student-session"))
                     (swap! jobs update (:job-id body) assoc :state "running"
                            :session-id (@sessions (:agent-id body)))
                     {:http/status 202 :ok true :accepted true})))
        request (submission/prepare-request
                 {:agent-id "student" :phase :student-attempt-1 :role :student
                  :dispatch/type :student-attempt :frame-id "development-frame"
                  :problem-id "development-problem" :dispatch/id "frozen-construction-request"
                  :memory-snapshot {:accessible-memory-ids ["visible"]}
                  :shelf/holdout :same-problem :shelf/withheld-ids ["withheld"]
                  :workspace "/fixture/student" :problem-path "Main.lean" :base-revision "fixture"
                  :v4/teaching-config {:version 1 :ta-agent-id "ta" :max-revisions 1 :job-budget-ms 60000}})
        opts {:request request :phase-path path :http-fn http :agency-base "http://fixture"
              :prepare-fn (fn [_] (swap! prepares inc) (swap! sessions assoc "student" nil) {:ok true})}]
    (try
      (binding [submission/*submission-root* (str (io/file root "submissions"))
                memory/*receipt-root* (str (io/file root "searches"))
                memory/*search-fn* (fn [& _] {:content-matches [] :candidates [] :index-as-of "fixture"})]
        (f {:opts opts :path path :jobs jobs :calls calls :sessions sessions :prepares prepares}))
      (finally (doseq [file (reverse (file-seq root))] (.delete file))))))
(defn current [path] (runtime/read-state (sut/state-path path)))
(defn submit! [path jobs evidence]
  (let [r (:request (current path)) id (:submission/job-id r)
        payload {:command-own-exit 0 :outcome "complete" :failure-account [] :evidence evidence}
        submitted (submission/submit! id (:submission/token r) payload)]
    (is (:ok submitted) (pr-str submitted))
    (swap! jobs assoc-in [id :state] "done")))
(defn dispatch! [opts]
  (is (:ok (sut/step! opts))) ; prepared -> announced
  (is (:ok (sut/step! opts))) ; queued -> activated
  (let [r (:request (current (:phase-path opts)))]
    (when (= :student (:role r))
      (is (:ok (memory/search! (:submission/job-id r) (:submission/token r) "fixture query" 5))))))

(deftest revision-dialogue-survives-every-tick-reload-and-binds-construction
  (with-fixture
    (fn [{:keys [opts path jobs prepares calls]}]
      (is (:ok (sut/step! opts)))
      (dispatch! opts)
      (submit! path jobs {:plan initial-plan :memory-use {:used-ids []}})
      (is (:ok (sut/step! opts)))
      (dispatch! opts)
      (submit! path jobs {:plan-review (judgment initial-plan "revise")})
      (is (:ok (sut/step! opts)))
      (let [revised (assoc initial-plan :revision 1 :parent-digest (plan/digest initial-plan)
                           :responses [{:node-id "root" :action "changed" :response "Use density with radius 1/n"}])]
        (is (= initial-plan (get-in (current path) [:request :v4/teaching :prior-plan])))
        (dispatch! opts)
        (submit! path jobs {:plan revised :memory-use {:used-ids ["visible"]}})
        (is (:ok (sut/step! opts)))
        (dispatch! opts)
        (submit! path jobs {:plan-review (judgment revised "accept")})
        (is (:ok (sut/step! opts)))
        (let [ready (sut/step! opts) before (count @calls)
              construction (sut/construction-request (:request opts) (:receipt ready))]
          (is (= :ready (:status ready)))
          (is (= 4 (count (get-in ready [:receipt :history]))))
          (is (= revised (get-in ready [:receipt :plan])))
          (is (false? (get-in ready [:receipt :mathematics/verified?])))
          (is (not= (:dispatch/id (:request opts)) (:dispatch/id construction)))
          (is (= ready (sut/step! opts)))
          (is (= before (count @calls)))
          (is (= 1 @prepares))
          (is (= #{:plan-use :memory-use} (submission/evidence-required construction)))
          (is (str/includes? (learning/prompt construction) "Construct from the reviewed plan")))))))

(deftest malformed-plan-does-not-consume-slot
  (with-fixture
    (fn [{:keys [opts path jobs]}]
      (sut/step! opts) (dispatch! opts)
      (let [r (:request (current path)) id (:submission/job-id r)
            payload {:command-own-exit 0 :outcome "complete" :failure-account []
                     :evidence {:plan (assoc initial-plan :nodes []) :memory-use {:used-ids []}}}]
        (is (false? (:ok (submission/submit! id (:submission/token r) payload))))
        (is (nil? (submission/submitted id)))
        (submit! path jobs {:plan initial-plan :memory-use {:used-ids []}})))))

(deftest wrong-executed-session-never-advances
  (with-fixture
    (fn [{:keys [opts path jobs]}]
      (sut/step! opts) (dispatch! opts)
      (submit! path jobs {:plan initial-plan :memory-use {:used-ids []}})
      (swap! jobs assoc-in [(get-in (current path) [:request :submission/job-id]) :session-id] "student-session")
      (is (= :teaching-executed-identity-invalid (:error/code (sut/step! opts))))
      (is (empty? (:history (current path)))))))

(deftest refuse-alias-session-and-interrupted-preparation-without-retrying
  (with-fixture
    (fn [{:keys [opts sessions prepares]}]
      (swap! sessions assoc "ta" "student-session")
      (is (= :teaching-review-not-independent (:error/code (sut/step! opts))))
      (is (= :teaching-review-not-independent (:error/code (sut/step! opts))))
      (is (= 0 @prepares)))))

(deftest rejected-plan-exhausts-fixed-budget-and-keeps-history
  (with-fixture
    (fn [{:keys [opts path jobs]}]
      (let [opts (assoc-in opts [:request :v4/teaching-config :max-revisions] 0)]
        (sut/step! opts) (dispatch! opts)
        (submit! path jobs {:plan initial-plan :memory-use {:used-ids []}})
        (sut/step! opts) (dispatch! opts)
        (submit! path jobs {:plan-review (judgment initial-plan "revise")})
        (sut/step! opts)
        (is (= :revision-budget-exhausted (:reason (sut/step! opts))))
        (is (= 2 (count (:history (current path)))))))))

(deftest declared-memory-use-does-not-bypass-holdout
  (with-fixture
    (fn [{:keys [opts path jobs]}]
      (sut/step! opts) (dispatch! opts)
      (let [r (:request (current path)) id (:submission/job-id r)]
        (is (false? (:ok (submission/submit! id (:submission/token r)
                           {:command-own-exit 0 :outcome "complete" :failure-account []
                            :evidence {:plan initial-plan :memory-use {:used-ids ["withheld"]}}}))))
        (is (nil? (submission/submitted id)))
        (submit! path jobs {:plan initial-plan :memory-use {:used-ids []}})))))

(deftest structural-boundaries-and-unknown-use
  (let [auth {:problem-id "development-problem" :v4/teaching {:revision 0}}]
    (is (plan/plan-valid? auth initial-plan))
    (doseq [bad [nil {} (assoc initial-plan :revision 1)
                 (assoc-in initial-plan [:nodes 0 :parent] "root")
                 (assoc-in initial-plan [:nodes 0 :depends-on] ["root"])
                 (assoc-in initial-plan [:nodes 0 :warrant] {:kind "pattern" :id "p" :revision "unpinned"})]]
      (is (false? (plan/plan-valid? auth bad))))
    (let [r {:v4/teaching {:plan initial-plan}}]
      (is (plan/review-valid? r (judgment initial-plan "accept")))
      (is (false? (plan/review-valid? r (assoc (judgment initial-plan "accept") :nodes [])))))
    (let [r {:v4/teaching-receipt {:plan initial-plan}}
          use {:plan-digest (plan/digest initial-plan) :nodes [{:node-id "root" :status "unknown" :reason "No observation"}]}]
      (is (plan/use-valid? r use))
      (is (false? (plan/use-valid? r (assoc-in use [:nodes 0 :status] nil)))))))

(deftest real-learning-entry-gates-construction-and-preserves-v3-route
  (let [called (atom []) base {:dispatch/type :student-attempt :agent-id "student"
                               :frame-id "f" :problem-id "p" :phase :student-attempt-1
                               :v4/teaching-config {:version 1}}
        receipt {:plan initial-plan :receipt/id "fixture-receipt"}]
    (with-redefs [sut/saved-request (constantly base)
                  sut/step! (constantly {:ok true :status :awaiting-terminal})
                  learning/run-construction! #(do (swap! called conj %) {:ok true :status :fixture-construction})]
      (is (= :awaiting-terminal (:status (learning/run-live! {:request base}))))
      (is (empty? @called))
      (is (= :teaching-base-authority-conflict
             (:error/code (learning/run-live! {:request (assoc base :problem-id "wrong")}))))
      (is (= :fixture-construction (:status (learning/run-live! {:request {:phase :student-attempt-1}}))))
      (is (= 1 (count @called))))
    (reset! called [])
    (with-redefs [sut/saved-request (constantly base)
                  sut/step! (constantly {:ok true :status :ready :receipt receipt})
                  learning/run-construction! #(do (swap! called conj %) {:ok true})]
      (is (:ok (learning/run-live! {:request base})))
      (is (= receipt (get-in @called [0 :request :v4/teaching-receipt])))
      (is (= (get-in @called [0 :request]) (get-in @called [0 :fresh-request])))
      (is (= :teaching-construction-authority-conflict
             (:error/code (learning/run-live! {:request (assoc base :v4/teaching-receipt {})})))))))

(deftest plan-use-is-required-before-construction-submission-is-immutable
  (with-fixture
    (fn [{:keys [opts]}]
      (let [r (-> (sut/construction-request (:request opts) {:plan initial-plan}) submission/with-job-authority)
            id (:submission/job-id r)
            payload {:command-own-exit 0 :outcome "complete" :failure-account []
                     :evidence {:memory-use {:used-ids []}}}]
        (is (:ok (submission/register! r {:job-id id})))
        (is (false? (:ok (submission/submit! id (:submission/token r) payload))))
        (is (nil? (submission/submitted id)))
        (is (:ok (submission/submit!
                  id (:submission/token r)
                  (assoc-in payload [:evidence :plan-use]
                            {:plan-digest (plan/digest initial-plan)
                             :nodes [{:node-id "root" :status "unknown" :reason "No observed use"}]}))))))))

(deftest interrupted-initialization-is-explicit-and-does-not-reset-again
  (with-fixture
    (fn [{:keys [opts prepares]}]
      (let [failed (assoc opts :prepare-fn (fn [_] (swap! prepares inc) (throw (ex-info "fixture crash" {}))))]
        (is (= :teaching-io-failed (:error/code (sut/step! failed))))
        (is (= :teaching-initialization-reconciliation-required (:error/code (sut/step! opts))))
        (is (= 1 @prepares))))))
