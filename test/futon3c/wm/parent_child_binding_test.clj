(ns futon3c.wm.parent-child-binding-test
  "R11 contract: the real close lifecycle durably binds a queued retry child
  to its parent and writes the child's typed completion back. Requirements are
  intentionally not run here."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.wm.runner-service :as service]
            [futon3c.wm.runner-service-test :as fixture]
            [futon3c.wm.machinery-execution-cohort :as cohort]))

(defn isolated-retry-queue [f]
  (let [queue @#'service/!post-release-retries]
    (reset! queue clojure.lang.PersistentQueue/EMPTY)
    (try (f)
         (finally (reset! queue clojure.lang.PersistentQueue/EMPTY)))))

(use-fixtures :each #'fixture/reset-service! #'isolated-retry-queue
  (fn [f] (with-redefs [cohort/apply-binding identity] (f))))

(def parent-id "wm-click-parent-r11")
(def child-id "wm-retry-repair/r11-1")
(def other-child-id "wm-retry-repair/r11-2")
(def retry {:retry-click child-id :repair-id "repair/r11" :attempt 1
            :patch-sha "0123456789012345678901234567890123456789"})

(defn close-under-fixture!
  ([click-id summary opts click-fn]
   (close-under-fixture! click-id summary opts click-fn nil))
  ([click-id summary opts click-fn while-redefs]
   (with-redefs-fn
     {#'service/result-summary (fn [& _] summary)
      #'service/publish-registry! (fn [& _] nil)
      #'service/launch-route-check-after-close! (fn [_] {:route-check {:status :fixture}})
      #'service/signal-park-deps! (fn [& _] nil)
      #'service/click! click-fn}
     (fn []
       (#'service/close-click! fixture/scratch-agent-id click-id {} opts)
       (when while-redefs (while-redefs))))))

(deftest bind-at-launch-is-durable-before-child-call
  (#'service/enqueue-post-release-retry! retry)
  (let [seen (promise)
        summary {:outcome :unknown :runtime-disposition {:status :awaiting-retry}}]
    (close-under-fixture!
     parent-id summary {:wm-agent-id fixture/scratch-agent-id}
     (fn [opts]
       (deliver seen {:opts opts :parent-state (service/click-status parent-id)})
       {:status :queued :click-id child-id})
     #(is (not= ::timeout (deref seen 5000 ::timeout))))
    (let [observed (deref seen 0 ::timeout)
          {:keys [opts parent-state]} observed]
      (is (not= ::timeout observed))
      (is (= parent-id (:parent-click-id opts)))
      (is (= child-id (:accepted-click-id opts)))
      (is (= child-id (get-in parent-state [:child :click-id])))
      (is (= :pending (get-in parent-state [:child :completion :status])))
      (is (= {:status :not-checked :reason :child-pending}
             (:parent-verdict parent-state))))))

(deftest newer-retry-for-same-transaction-replaces-in-place
  ;; Lean: PostReleaseRetryCoalescing.enqueue and its positive law.
  (let [old-t {:injury-id "injury/T" :repair-id "repair/T" :attempt 2
               :retry-click "wm-retry-T-old"}
        request-u {:injury-id "injury/U" :repair-id "repair/U" :attempt 1
                   :retry-click "wm-retry-U"}
        newer-t {:injury-id "injury/T" :repair-id "repair/T" :attempt 2
                 :retry-click "wm-requeue-T-new"}
        queue @#'service/!post-release-retries]
    (#'service/enqueue-post-release-retry! old-t)
    (#'service/enqueue-post-release-retry! request-u)
    (#'service/enqueue-post-release-retry! newer-t)
    (let [launched (loop [pending @queue
                          result []]
                     (if (empty? pending)
                       result
                       (recur (pop pending)
                              (conj result (:retry-click (peek pending))))))]
      (is (= ["wm-requeue-T-new" "wm-retry-U"] launched))
      (is (not-any? #{"wm-retry-T-old"} launched)))))

(deftest retry-without-complete-transaction-key-remains-fifo
  (let [first-request {:repair-id "repair/unkeyed" :attempt 1
                       :retry-click "wm-retry-unkeyed-1"}
        second-request {:repair-id "repair/unkeyed" :attempt 1
                        :retry-click "wm-retry-unkeyed-2"}
        queue @#'service/!post-release-retries]
    (#'service/enqueue-post-release-retry! first-request)
    (#'service/enqueue-post-release-retry! second-request)
    (is (= ["wm-retry-unkeyed-1" "wm-retry-unkeyed-2"]
           (mapv :retry-click (seq @queue))))))

(deftest child-close-completes-exact-parent-binding
  (#'service/bind-child! parent-id retry)
  (let [summary {:outcome :already-satisfied :run-record "/fixture/child.edn"}]
    (close-under-fixture! child-id summary {:parent-click-id parent-id} (constantly nil))
    (let [parent (service/click-status parent-id)
          child (service/click-status child-id)]
      (is (= {:status :complete
              :outcome :already-satisfied
              :run-record "/fixture/child.edn"
              :verdict {:status :not-checked :reason :requirements-not-run}}
             (dissoc (get-in parent [:child :completion]) :completed-at)))
      (is (= {:status :not-checked :reason :requirements-not-run}
             (:parent-verdict parent)))
      (is (not (contains? child :child)))
      (is (not (contains? child :parent-verdict))))))

(deftest mismatched-child-is-retained-without-overwrite
  (#'service/bind-child! parent-id retry)
  (#'service/complete-child! parent-id other-child-id {:outcome :build-failed})
  (let [state (service/click-status parent-id)]
    (is (= :pending (get-in state [:child :completion :status])))
    (is (= {:child-click-id other-child-id :outcome :build-failed}
           (get-in state [:child :completion-mismatch])))))

(deftest conflicting-bind-never-overwrites-existing-child
  (#'service/bind-child! parent-id retry)
  (#'service/bind-child! parent-id (assoc retry :retry-click other-child-id))
  (let [state (service/click-status parent-id)]
    (is (= child-id (get-in state [:child :click-id])))
    (is (= {:existing child-id :attempted other-child-id}
           (:child-conflict state)))))

(deftest parent-verdict-is-exact-function-of-child-completion
  (testing "pending cannot inherit a successful runtime outcome"
    (is (= {:status :not-checked :reason :child-pending}
           (service/parent-verdict
            {:result {:outcome :success} :child {:completion {:status :pending}}}))))
  (doseq [verdict [{:status :passed} {:status :failed}]]
    (is (= verdict
           (service/parent-verdict
            {:child {:completion {:status :complete :verdict verdict}}}))))
  (is (= {:status :not-applicable :reason :no-child}
         (service/parent-verdict {:status :completed}))))

(deftest ordinary-click-shape-is-unchanged
  (let [state {:schema :wm/click-state-v1 :click-id "ordinary" :status :completed}]
    (is (= {:status :not-applicable :reason :no-child}
           (service/parent-verdict state)))
    (is (not (contains? state :child)))))

(deftest real-child-click-reaches-the-parent-binding
  ;; The seam the other tests stub past: a child launched with
  ;; retry-child-opts keeps :parent-click-id through configured-runner-opts
  ;; (apply-binding, configure-self-heal-ports, full-loop-runner/config) so
  ;; the real close-click! writes its completion to the parent.
  (#'service/bind-child! parent-id retry)
  (let [run! (fn [_] {:outcome :grounded-change})
        child-opts (#'service/retry-child-opts
                    {:wm-agent-id fixture/scratch-agent-id} retry parent-id)]
    (binding [service/*resolve-var* (#'fixture/resolver run! (constantly {}))]
      (let [result (service/click! child-opts)]
        (is (= :queued (:status result)))
        (is (= child-id (:click-id result)))
        (service/await-click! (:click-id result))))
    (let [parent (service/click-status parent-id)]
      (is (= :complete (get-in parent [:child :completion :status])))
      (is (= :grounded-change (get-in parent [:child :completion :outcome])))
      (is (= {:status :not-checked :reason :requirements-not-run}
             (:parent-verdict parent)))
      (is (not (contains? (get parent :child) :completion-mismatch))))))

(deftest unreadable-child-completion-is-typed-not-nil
  (is (= {:status :not-checked :reason :child-completion-unreadable
          :completion-status :garbage}
         (service/parent-verdict {:child {:completion {:status :garbage}}})))
  (is (= {:status :not-checked :reason :child-verdict-absent}
         (service/parent-verdict {:child {:completion {:status :complete}}}))))
