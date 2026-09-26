(ns futon3c.apm.library-lane-queue
  "Sequential, bounded execution of the live-derived library lane."
  (:require [futon3c.apm.library-lane :as lane]
            [futon3c.apm.library-lane-runner :as runner]))

(def landing-rulings #{:closed :partial-banked})

(defn- problem-report [problem-id result bank-request dry-run?]
  (cond-> {:problem-id problem-id
           :keying-targets (or (:keying-targets result)
                               (:targets result)
                               (:targets bank-request))
           :ruling (:ruling result)}
    (:seam result) (assoc :seam (:seam result))
    ;; Carry the finding: a :blocked ruling whose cause is dropped is
    ;; undiagnosable, which is fatal for a loop meant to run unattended.
    (:finding result) (assoc :finding (:finding result))
    (get-in result [:receipt :receipt/id])
    (assoc :bank-receipt-id (get-in result [:receipt :receipt/id]))
    (and dry-run? bank-request)
    (assoc :would-ruling (:ruling bank-request)
           :bank-request bank-request)))

(defn- dry-bank [captured request]
  (reset! captured request)
  {:ok true :dry-run? true
   :receipt {:receipt/ruling (:ruling request)}})

(defn run-queue!
  "Run a bounded sequential slice of the current :library lane.

  The lane queue is re-read before every attempt. `attempted` excludes
  non-landing problems whose status remains unchanged, while landed problems
  naturally disappear after bank rewrites status.json. Dry-run replaces the
  bank function at its boundary; the bank driver never receives the request."
  [{:keys [corpus-root problem-id area max-problems
           max-consecutive-non-landings dry-run? run-one-fn]
    :or {max-problems 1 max-consecutive-non-landings 2
         dry-run? false run-one-fn runner/run-one!}
    :as options}]
  (cond
    (not (pos-int? max-problems))
    {:ok false :error/code :max-problems-invalid
     :stop-condition :configuration :reports []}

    (not (pos-int? max-consecutive-non-landings))
    {:ok false :error/code :max-consecutive-non-landings-invalid
     :stop-condition :configuration :reports []}

    :else
    (loop [attempted #{} reports [] consecutive 0]
      (let [derived (lane/queue corpus-root :library area)
            ;; A launch mints one frame for one problem. When that identity is
            ;; supplied, the queue must not silently substitute its current
            ;; head: the resulting phase authority belongs to another unit.
            current (if problem-id
                      (if (some #{problem-id} derived) [problem-id] [])
                      derived)
            problem-id (first (remove attempted current))]
        (cond
          (>= (count reports) max-problems)
          {:ok true :stop-condition :max-problems :reports reports}

          (>= consecutive max-consecutive-non-landings)
          {:ok true :stop-condition :consecutive-non-landings :reports reports}

          (nil? problem-id)
          {:ok true :stop-condition :queue-empty-after-exclusions
           :reports reports}

          :else
          (let [captured (atom nil)
                run-options (cond-> (assoc options :problem-id problem-id)
                              dry-run? (assoc :bank-fn (partial dry-bank captured)))
                result (try
                         (run-one-fn (dissoc run-options :run-one-fn))
                         (catch Throwable t
                           {:ok false :ruling :blocked :seam :exception
                            :finding {:class (.getName (class t))
                                      :message (.getMessage t)}}))
                bank-request @captured
                report (problem-report problem-id result bank-request dry-run?)
                landed? (contains? landing-rulings (:ruling result))]
            (recur (conj attempted problem-id)
                   (conj reports report)
                   (if landed? 0 (inc consecutive)))))))))
