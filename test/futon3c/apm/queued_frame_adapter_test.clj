(ns futon3c.apm.queued-frame-adapter-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.problem-queue-supervisor :as queue]
            [futon3c.apm.queued-frame-adapter :as sut]))

(def problem {:problem/id "p1" :repository "/repo" :revision "r"
              :path "p1.lean" :blob "b" :classification :non-excluded})
(def frame (:frame (sut/mint {:problem problem :ordinal 0 :queue/id "queue"
                              :frame-number-base 30})))
(def digest (apply str (repeat 64 "a")))

(deftest campaign-paths-use-a-stable-operator-buffer
  (let [paths (sut/campaign-paths {:campaign-root "/tmp/apm-campaigns"}
                                  frame)]
    (is (= "*problem*" (:problem-buffer-name paths)))
    (is (= "/tmp/apm-campaigns/queue-state.edn"
           (:campaign-queue-state-path paths)))
    (is (.endsWith ^String (:problem-buffer-path paths)
                   "problem-buffer.md"))))

(deftest deterministic-mint-and-qualification
  (is (= "f30" (:frame/id frame)))
  (is (sut/valid-mint? frame))
  (is (:ok (sut/qualify {:frame frame :generated-contract-digest digest
                         :qualification-digest digest}))))

(deftest fresh-one-off-manifest-pins-both-scribe-cards
  (let [manifest (sut/one-off-manifest
                  {:frame frame :apparatus-repository "."
                   :apparatus-branch "master" :baseline {}})]
    (is (string? (get-in manifest [:apparatus :artifacts :scribe :blob])))
    (is (string? (get-in manifest [:apparatus :artifacts :zai-scribe :blob])))
    (is (.endsWith (get-in manifest [:apparatus :artifacts :scribe :path])
                   "codex-scribe-v1.md"))
    (is (.endsWith (get-in manifest [:apparatus :artifacts :zai-scribe :path])
                   "zai-scribe-v1.md"))))

(deftest open-precedes-all-resource-effects
  (let [calls (atom [])
        body {:preparation/version 2 :frame/id "f30" :problem/id "p1"}
        preparation (assoc body :preparation/id (machine/ledger-digest [body]))
        result
        (sut/open-and-prepare!
         {:frame frame
          :open-frame-fn (fn [_] (swap! calls conj :open) {:ok true})
          :preparation-observation-fn
          (fn [_] (swap! calls conj :observe)
            {:ok true :version 5 :phase :preflight :claim nil
             :frame-id "f30" :problem-id "p1"})
          :prepare-frame-fn (fn [_ _] (swap! calls conj :prepare)
                              {:ok true :preparation preparation})
          :persist-preparation-fn (fn [_ _] (swap! calls conj :persist)
                                    {:ok true})})]
    (is (:ok result))
    (is (= [:open :observe :prepare :persist] @calls))))

(deftest no-provisioning-before-authoritative-preflight
  (let [calls (atom [])
        result
        (sut/open-and-prepare!
         {:frame frame :open-frame-fn (constantly {:ok true})
          :preparation-observation-fn
          (constantly {:ok true :version 4 :phase :open-frame :claim nil
                       :frame-id "f30" :problem-id "p1"})
          :prepare-frame-fn #(do (swap! calls conj :prepare) {:ok true})
          :persist-preparation-fn #(do (swap! calls conj :persist) {:ok true})})]
    (is (= :queued-frame-preparation-authority-invalid (:error/code result)))
    (is (empty? @calls))))

(defn- lease [frame role]
  (let [body {:workspace/id nil :workspace/path (str "/work/" (:frame/id frame)
                                                        "-" (name role))
              :repository/path "/repo" :branch (str "branch-" (name role))
              :base-revision "rev" :problem/id (:problem/id frame)
              :problem/path "Main.lean" :problem/blob "blob"
              :frame/id (:frame/id frame) :role role :created-at "now"
              :retention/state :provisioned :substrate/path "/lake"}]
    (assoc body :workspace/id
           (machine/ledger-digest [(dissoc body :workspace/id)]))))

(defn- roster [frame-id]
  {:ok true :http/status 200
   :agents
   (into {}
         (map (fn [[role type]]
                [(str frame-id "-" (name role))
                 {:type type :invoke-ready? true
                  :metadata {:effective-timeouts
                             {:request-timeout-ms (if (= type :zai)
                                                    300000 :not-applicable)
                              :turn-timeout-ms (if (= role :student)
                                                 1800000
                                                 3600000)}}}]))
         {:solver :codex :student :zai :guide :claude
          :proctor :codex :promotion-proctor :codex
          :scribe :zai :zai-scribe :zai :analyst :claude})})

(deftest concrete-live-preparation-binds-lifecycle-mint-roster-and-paths
  (let [calls (atom [])
        manifest {:manifest/id digest}
        result (sut/prepare-live!
                {:frame frame
                 :ledger {:version 5 :digest digest :phase :preflight :claim nil}
                 :manifest manifest
                 :role-cards (into {} (map (fn [role]
                                             [role {:path (name role) :blob digest}])
                                           [:solver :student :guide :proctor
                                            :promotion-proctor :scribe
                                            :zai-scribe :analyst]))
                 :workspace-root "/work" :substrate-path "/lake"
                 :provision-fn
                 (fn [{:keys [role]}]
                   (swap! calls conj [:provision role])
                   {:ok true :lease (lease frame role)})
                 :bootstrap-workspace-fn
                 (fn [new-lease]
                   (swap! calls conj [:bootstrap (:role new-lease)])
                   {:ok true})
                 :validate-workspace-fn (constantly {:valid? true})
                 :http-fn
                 (fn [method url & [payload]]
                   (swap! calls conj [method url payload])
                   (if (= method "POST") {:ok true :http/status 200}
                       (roster "f30")))})]
    (is (:ok result) (pr-str result))
    (is (= #{:solver :student}
           (set (keys (get-in result [:preparation :workspaces])))))
    (is (= "f30-student"
           (get-in result [:preparation :seats :student :agent-id])))
    (is (= :zai (get-in result [:preparation :seats :scribe :type])))
    (is (= :zai (get-in result [:preparation :seats :zai-scribe :type])))
    (is (= (:preparation/id (:preparation result))
           (machine/ledger-digest
            [(dissoc (:preparation result) :preparation/id)])))
    (is (= [[:provision :student] [:provision :solver]]
           (filter #(= :provision (first %)) @calls)))
    (is (= [[:bootstrap :student] [:bootstrap :solver]]
           (filter #(= :bootstrap (first %)) @calls)))))

(deftest five-problem-live-effects-never-prepare-a-successor-early
  (let [problems (mapv (fn [n] {:problem/id (str "p" n) :repository "/repo"
                                 :revision "r" :path "Main.lean" :blob "b"
                                 :classification :non-excluded}) (range 5))
        plan (queue/queue-plan problems)
        state (atom nil)
        calls (atom [])
        effects {:mint-frame-fn #(do (swap! calls conj [:mint (:ordinal %)])
                                     (sut/mint (assoc % :frame-number-base 40)))
                 :qualify-frame-fn #(do (swap! calls conj [:qualify (:frame/id %)])
                                        {:ok true})
                 :prepare-frame-fn #(do (swap! calls conj [:prepare (:frame/id %)])
                                        {:ok true :preparation/id digest})
                 :frame-tick-fn #(do (swap! calls conj [:tick (:frame/id %)])
                                     {:ok true :status :parked})
                 :retire-frame-fn #(do (swap! calls conj [:retire %]) {:ok true})
                 :state-provider #(deref state)
                 :persist-state-fn #(do (reset! state %) {:ok true})}]
    (is (= :frame-prepared (:status (queue/tick! (assoc effects :plan plan)))))
    (is (= :parked (:status (queue/tick! (assoc effects :plan plan)))))
    (is (= [[:mint 0] [:qualify "f40"] [:prepare "f40"] [:tick "f40"]]
           @calls))
    (is (= 1 (:next-index @state)))
    (is (= 5 (count (:problems plan))))))

(deftest terminal-evidence-is-derived-from-ledger-not-supervisor-status
  (let [solve {:receipt/type :frame-solve :receipt/id digest
               :receipt/final-head (apply str (repeat 40 "b"))
               :receipt/lean {:sorry-warnings 0}}
        verify {:receipt/type :frame-verify :receipt/id digest
                :receipt/mathematical-sound? true}
        close {:receipt/type :frame-close :receipt/id digest
               :receipt/result :closed}
        result
        (sut/terminal-from-ledger
         {:frame frame
          :ledger {:events (mapv #(hash-map :event/body {:certificate %})
                                 [solve verify close])}
          :preparation {:workspaces
                        {:solver {:branch "exp/f30" :terminal-head
                                  (apply str (repeat 40 "b"))}
                         :student {:terminal-head
                                   (apply str (repeat 40 "c"))}}}})]
    (is (:ok result) (pr-str result))
    (is (= :closed (:frame/result result)))
    (is (= :solved (get-in result [:terminal-receipt :problem/outcome])))
    (is (= "exp/f30" (get-in result [:terminal-receipt :solver :branch])))))

(deftest terminal-replay-uses-a-validated-prior-terminal-after-retirement
  (let [solver-head (apply str (repeat 40 "b"))
        student-head (apply str (repeat 40 "c"))
        solve {:receipt/type :frame-solve :receipt/id digest
               :receipt/final-head solver-head
               :receipt/lean {:sorry-warnings 0}}
        verify {:receipt/type :frame-verify :receipt/id digest
                :receipt/mathematical-sound? true}
        close {:receipt/type :frame-close :receipt/id digest
               :receipt/result :closed}
        ledger {:events (mapv #(hash-map :event/body {:certificate %})
                              [solve verify close])}
        initial (sut/terminal-from-ledger
                 {:frame frame :ledger ledger
                  :preparation {:workspaces
                                {:solver {:branch "exp/f30"
                                          :terminal-head solver-head}
                                 :student {:terminal-head student-head}}}})
        replayed (sut/terminal-from-ledger
                  {:frame frame :ledger ledger
                   :preparation {:workspaces
                                 {:solver {:branch "exp/f30"
                                           :workspace/path "/absent/solver"}
                                  :student {:workspace/path "/absent/student"}}}
                   :prior-terminal (:terminal-receipt initial)})]
    (is (:ok initial) (pr-str initial))
    (is (:ok replayed) (pr-str replayed))
    (is (= {:solver solver-head :student student-head}
           (get-in replayed [:terminal-receipt :workspace/terminal-heads])))))

(deftest apparatus-invalidated-void-derives-terminal-without-verify-or-close
  (let [void {:certificate/type :frame-void :certificate/id digest
              :classification :apparatus-invalidated
              :failed-invariants [:student-snapshot-not-campaign-cumulative]}
        result
        (sut/terminal-from-ledger
         {:frame frame
          :ledger {:events [{:event/body {:certificate void}}]}
          :preparation {:workspaces
                        {:solver {:branch "exp/f30" :terminal-head
                                  (apply str (repeat 40 "b"))}
                         :student {:terminal-head
                                   (apply str (repeat 40 "c"))}}}})]
    (is (:ok result) (pr-str result))
    (is (= :void (:frame/result result)))
    (is (= :invalid (get-in result [:terminal-receipt :problem/outcome])))
    (is (= :skipped (get-in result [:terminal-receipt :learning/outcome])))
    (is (= :apparatus-invalidated
           (get-in result [:terminal-receipt :void/classification])))))
