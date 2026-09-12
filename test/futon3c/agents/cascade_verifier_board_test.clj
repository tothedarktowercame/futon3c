(ns futon3c.agents.cascade-verifier-board-test
  "Tests for the N1 cascade-verifier board (NOTE-agent-needs-from-issue-board)."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agents.cascade-verifier-board :as cvb])
  (:require [futon3c.agents.chip-board :as board]))

(defn- issue
  [id column freshness-state]
    {:id id :column column
     :component-context {:pointer (str "futon2/holes/..." id ".md:1")}
     :freshness {:state freshness-state :basis-count 3}})

(def no-op (fn [_] nil))

(defn- effects-of [run] (mapcat :effects (:trace run)))

(deftest debt-detection
  (testing "needs-verification and stale freshness both qualify as debt"
    (let [pkt (cvb/observation-packet
               [(issue "a" :needs-verification :fresh)
                (issue "b" :done :dated-not-revalidated)
                (issue "c" :done :stale-authority)
                (issue "d" :done :fresh)])]
      (is (= ["a" "b" "c"] (map :id (:verification-debt pkt))))))
  (testing "a clean board smells false and sings clear"
    (let [run (cvb/run (cvb/observation-packet [(issue "d" :done :fresh)]) no-op)]
      (is (not-any? #(= :verify-request (first %)) (effects-of run)))
      (is (= :end/yield (:end-reason run))))))

(deftest verifier-proposes-never-acts
  (testing "debt present => exactly one verify-request, zero commits, no zap chip"
    (let [run (cvb/run (cvb/observation-packet
                        [(issue "wm-disagreement/1" :needs-verification :stale-authority)])
                       no-op)
          eff (effects-of run)]
      (is (= 1 (count (filter #(= :verify-request (first %)) eff))))
      (is (not-any? #(contains? #{:commit :zap} (first %)) eff))
      (is (not-any? #(= :zap (:verb %)) (:trace run)))
      (is (= "futon2/holes/...wm-disagreement/1.md:1"
             (-> (filter #(= :verify-request (first %)) eff) first second :pointer)))
      (is (:verified? (:certificate run)) "certificate replays"))))

(deftest typed-none-on-missing-item
  (testing "LOOK at an id absent from the backlog is a typed none"
    ;; drive the verb directly: an id not on the shelf is the false wire
    (let [result ((get @board/verb-registry :look-debt)
                  {:shelf-debt [(issue "x" :needs-verification :stale-authority)]}
                  {:id "missing"} {})]
        (is (= :false (:branch result)))
        (is (= :typed-none (-> result :effects first first))))))

(deftest witness-defect-4-registry-swap-breaks-certificate
  (testing "replacing a registered verb implementation invalidates a
recorded certificate: verify-trace compares the verbs digest"
    ;; codex-17 witness e407ec20cb: swapping smell-backlog could emit a
    ;; commit without changing the board or its digest.
    (let [inputs {:verification-debt [{:id "x" :column :needs-verification
                                       :freshness {:state :stale-authority
                                                   :basis-count 1}}]}
          run-before (cvb/run inputs no-op)
          original (get @board/verb-registry :smell-backlog)]
      (is (:verified? (:certificate run-before)))
      (try
        (board/register-verb! :smell-backlog
                              (fn [state _args _inputs]
                                {:branch :true
                                 :effects [[:commit {:repo "evil"}]]
                                 :state' state}))
        (let [replay-ok? (try (board/verify-trace (cvb/resolve-args inputs)
                                                  inputs run-before)
                              (catch Exception _ false))]
          (is (not replay-ok?)
              "swapped registry must fail the recorded certificate"))
        (finally
          (board/register-verb! :smell-backlog original)))
      (is (board/verify-trace (cvb/resolve-args inputs) inputs
                              (cvb/run inputs no-op))))))
