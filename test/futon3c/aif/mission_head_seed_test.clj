(ns futon3c.aif.mission-head-seed-test
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [futon3c.aif.mission-head :as mh]
            [futon3c.peripheral.mission :as mission]
            [futon3c.peripheral.mission-backend :as mb]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.test-fixtures :as fix]))

(def sample-health
  {:mission "M-test"
   :generated-at "2026-06-10T12:00:00Z"
   :generator "fixture"
   ;; contract-shaped (fable-1 review): healthy sigil carries :xenotype-32
   :sigil {:exotype "00011100" :xenotype-32 "00011100·00011001·10101100·11001000" :per-section {}}
   :health {:bit-confidence 0.72
            :xenotype-completeness 0.41
            :anchor-proximity {:nearest "T2"}
            :reading "usable but thin"}})

(deftest seed-from-health-maps-priors-quality-beliefs
  (let [seed (mh/seed-from-health sample-health)]
    (is (:health/observed? seed))
    (is (false? (:health/degraded? seed)))
    (is (= 0.72 (get-in seed [:beliefs :priors-quality :bit-confidence])))
    (is (= 0.41 (get-in seed [:beliefs :priors-quality :xenotype-completeness])))
    (is (= :observe (get-in seed [:observation :decision-kind])))
    (is (:consumes-health? (:observation seed)))
    (is (= "00011100·00011001·10101100·11001000" (get-in seed [:beliefs :sigil :xenotype-32]))))
  (testing "degraded mode when sigil is null"
    (let [seed (mh/seed-from-health (assoc sample-health :sigil nil))]
      (is (:health/degraded? seed))
      (is (nil? (get-in seed [:beliefs :sigil])))))
  (testing "degraded mode when sigil object present but xenotype null (the producer's REAL degraded shape)"
    (let [seed (mh/seed-from-health (assoc sample-health :sigil {:exotype "00001000" :xenotype-32 nil :per-section nil}))]
      (is (:health/degraded? seed)))))

(deftest mission-head-construction-loads-sibling-health-artifact
  (let [dir (fix/temp-dir!)
        mission-doc (java.io.File. dir "M-test.md")
        absent-doc (java.io.File. dir "M-absent.md")
        health-doc (java.io.File. dir "M-test.health.json")]
    (spit mission-doc "# M-test\n")
    (spit absent-doc "# M-absent\n")
    (spit health-doc (json/generate-string sample-health))
    (let [seeded (mh/mission-head-for-context
                  {:mission-id "M-test"
                   :mission-spec-path (.getPath mission-doc)})
          defaulted (mh/mission-head-for-context
                     {:mission-id "M-absent"
                      :mission-spec-path (.getPath absent-doc)})]
      (is (= "M-test" (get-in seeded [:config :mission-id])))
      (is (= 0.72 (get-in seeded [:config :initial-beliefs
                                  :priors-quality :bit-confidence])))
      (is (nil? (get-in defaulted [:config :health-seed]))))))

(deftest cycle-complete-invokes-mission-head
  (let [calls (atom [])
        backend (tools/make-mock-backend
                 {:cycle-begin {:cycle/id "C1"
                                :cycle/blocker-id "O-b"
                                :cycle/phase :observe
                                :cycle/phases-completed []
                                :cycle/phase-data {}
                                :cycle/started-at "t"
                                :cycle/updated-at "t"}
                  :cycle-advance {:cycle/id "C1"
                                  :cycle/blocker-id "O-b"
                                  :cycle/phase :completed
                                  :cycle/phases-completed [:observe :propose :execute
                                                           :validate :classify :integrate
                                                           :commit :gate-review]
                                  :cycle/phase-data {}
                                  :cycle/result-status :done
                                  :cycle/started-at "t"
                                  :cycle/updated-at "t"}
                  :obligation-query {}
                  :dag-impact []})]
    (with-redefs [mh/mission-observe (fn [head state context]
                                       (swap! calls conj {:head head :state state :context context})
                                       {:channels {:obligation-satisfaction 0.0
                                                   :structural-law-compliance 1.0}})
                  mh/mission-default-mode-fn (fn [_head _state observation]
                                               {:action :advance-phase
                                                :observation observation})]
      (let [p (mission/make-mission backend)
            start (runner/start p {:session-id "sess-aif-complete" :mission-id "M-test"})
            cycle-step (runner/step p (:state start) {:tool :cycle-begin :args ["M-test" "O-b"]})
            advance (runner/step p (:state cycle-step)
                                 {:tool :cycle-advance
                                  :args ["M-test" "C1" {:gates-passed true}]})]
        (is (:ok advance))
        (is (= 1 (count @calls)))
        (is (= :completed (get-in advance [:result :cycle/phase])))))))

(deftest illegal-phase-transition-is-refused-through-check-law
  (let [dir (fix/temp-dir!)
        backend (mb/make-mission-backend
                 {:cwd dir
                  :aif-head (fn [_state _transition]
                              {:ok false
                               :error {:code :structural-law-violation
                                       :law-family :fixture-law
                                       :law-id :illegal-transition
                                       :message "fixture refusal"}})}
                 (tools/make-mock-backend))]
    (mb/init-mission! dir "M-test" "Mission test" ["done"] ["src"] [])
    (tools/execute-tool backend :mission-load ["M-test"])
    (tools/execute-tool backend :obligation-upsert
                        ["M-test" "O-main" {:item/label "Main obligation"
                                            :item/status :open
                                            :item/depends-on #{}
                                            :item/unlocks #{}
                                            :item/artifact-paths []}])
    (let [begin (tools/execute-tool backend :cycle-begin ["M-test" "O-main"])
          cycle-id (get-in begin [:result :cycle/id])
          advance (tools/execute-tool backend :cycle-advance
                                      ["M-test" cycle-id {:blocker-id "O-main"}])]
      (is (not (:ok advance)))
      (is (= :structural-law-violation (get-in advance [:error :code])))
      (is (= :fixture-law (get-in advance [:error :context :law-family]))))))

(deftest health-values-do-not-drive-gate-or-optimize-decisions
  (let [low (mh/make-mission-aif-head
             {:health-seed (mh/seed-from-health
                            (assoc-in sample-health [:health :bit-confidence] 0.01))})
        high (mh/make-mission-aif-head
              {:health-seed (mh/seed-from-health
                             (assoc-in sample-health [:health :bit-confidence] 0.99))})
        state {:tau 1.0}
        context {:tau 1.0
                 :mission-state {}
                 :current-cycle nil
                 :compliance-result {:ok true}
                 :gate-result nil
                 :evidence-counts {}}]
    (is (= :observe (get-in (:health-seed (:config low)) [:observation :decision-kind])))
    (is (= (mh/mission-select-pattern low state context)
           (mh/mission-select-pattern high state context)))))
