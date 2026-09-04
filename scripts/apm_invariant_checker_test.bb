#!/usr/bin/env bb
;; Unit tests for scripts/apm_invariant_checker.bb — pure functions only.
;; Run: bb scripts/apm_invariant_checker_test.bb
;; Loads the checker with no CLI args, so its main guard does not fire.

(load-file "scripts/apm_invariant_checker.bb")
(in-ns 'apm-invariant-checker)

(def t-cfg {:churn-window-ms (* 24 3600 1000)
            :churn-span-window-ms (* 7 24 3600 1000)
            :min-mean-span-ms (* 2 3600 1000)
            :min-span-count 3})

(def t-now 1000000000000) ; fixed epoch for determinism

(def hour 3600000)

(let [failures (atom [])
      check (fn [name actual expected]
              (when (not= actual expected)
                (swap! failures conj {:test name :expected expected :actual actual})))
      run! (fn [name f] (f name) (println (str "  ok " name)))

      ;; enabled-spans pairs each enable with the next disable.
      _ (run! "enabled-spans-pairs"
           (fn [n]
             (check n
               (enabled-spans [{:transition/timestamp-ms 0 :enabled/new true}
                               {:transition/timestamp-ms 10 :enabled/new false}
                               {:transition/timestamp-ms 20 :enabled/new true}])
               [[0 10]])))

      ;; A trailing enable without a disable is an open span, not counted.
      _ (run! "enabled-spans-open-tail"
           (fn [n]
             (check n
               (enabled-spans [{:transition/timestamp-ms 0 :enabled/new true}
                               {:transition/timestamp-ms 10 :enabled/new false}
                               {:transition/timestamp-ms 20 :enabled/new true}
                               {:transition/timestamp-ms 30 :enabled/new false}])
               [[0 10] [20 30]])))

      ;; Churn: 4 spans of 45 min inside the 7d window -> violated, numbers reported.
      _ (run! "churn-violated-below-floor"
           (fn [n]
             (let [h (mapcat (fn [i]
                               [{:transition/timestamp-ms (- t-now (* 12 i hour) hour) :enabled/new true}
                                {:transition/timestamp-ms (- t-now (* 12 i hour)) :enabled/new false}])
                             (range 4))
                   r (churn-check (vec h) t-cfg t-now)]
               (check (str n ":verdict") (:invariant/verdict r) :violated)
               (check (str n ":stops24h") (get-in r [:invariant/observed :churn/stops-24h]) 2)
               (check (str n ":spans") (get-in r [:invariant/observed :churn/completed-spans-7d]) 4)
               (check (str n ":mean") (get-in r [:invariant/observed :churn/mean-enabled-span-minutes]) 60.0))))

      ;; Churn: few spans -> unknown, never a silent pass.
      _ (run! "churn-unknown-few-spans"
           (fn [n]
             (let [r (churn-check [{:transition/timestamp-ms (- t-now (* 24 hour)) :enabled/new true}
                                   {:transition/timestamp-ms (- t-now (* 22 hour)) :enabled/new false}]
                                 t-cfg t-now)]
               (check (str n ":verdict") (:invariant/verdict r) :unknown))))

      ;; Churn: empty history -> unknown.
      _ (run! "churn-unknown-empty"
           (fn [n]
             (check n (:invariant/verdict (churn-check [] t-cfg t-now)) :unknown)))

      ;; Churn: healthy 6h spans -> pass.
      _ (run! "churn-pass-healthy"
           (fn [n]
             (let [h (mapcat (fn [i]
                               [{:transition/timestamp-ms (- t-now (* 12 i hour)) :enabled/new true}
                                {:transition/timestamp-ms (- t-now (* 12 i hour) (* 6 hour)) :enabled/new false}])
                             (range 4))
                   r (churn-check (vec h) t-cfg t-now)]
               (check (str n ":verdict") (:invariant/verdict r) :pass))))

      ;; Substrate-fault stop with disabled coordinator -> loud violation.
      _ (run! "health-substrate-stop-violated"
           (fn [n]
             (let [entry {:coordinator/id "x:v1"
                          :coordinator/enabled? false
                          :coordinator/enabled-history
                          [{:transition/timestamp-ms 1 :enabled/new true}
                           {:transition/timestamp-ms 2 :enabled/new false
                            :stop/cause {:stop-cause/type :fault
                                         :stop-cause/fault-class :substrate
                                         :stop-cause/reason-code :x/y}}]}
                   r (coordinator-health-check entry)]
               (check (str n ":verdict") (:invariant/verdict r) :violated))))

      ;; Legacy stop without :stop/cause -> pass (cause reported as unknown-type, not invented).
      _ (run! "health-legacy-stop-passes"
           (fn [n]
             (let [entry {:coordinator/id "x:v1"
                          :coordinator/enabled? false
                          :coordinator/enabled-history
                          [{:transition/timestamp-ms 1 :enabled/new true}
                           {:transition/timestamp-ms 2 :enabled/new false}]}
                   r (coordinator-health-check entry)]
               (check (str n ":verdict") (:invariant/verdict r) :pass)
               (check (str n ":cause") (get-in r [:invariant/observed :stop/cause :stop-cause/type]) :unknown))))

      ;; Missing :coordinator/enabled? -> unknown, not silent pass.
      _ (run! "health-missing-enabled-unknown"
           (fn [n]
             (check n (:invariant/verdict (coordinator-health-check {:coordinator/id "x"})) :unknown)))

      ;; JVM count: 5 procs vs 2 -> violated with process details.
      _ (run! "jvm-count-violated"
           (fn [n]
             (let [r (jvm-check 2 (repeat 5 {:pid 1 :age-days 4.0 :rss-gb 5.0 :cmd "a"}))]
               (check (str n ":verdict") (:invariant/verdict r) :violated))))

      _ (run! "jvm-count-zero-unknown"
           (fn [n]
             (check n (:invariant/verdict (jvm-check 2 [])) :unknown)))

      ;; parse-jps handles etime with day prefix and plain minutes.
      _ (run! "parse-jps-etime"
           (fn [n]
             (let [procs (parse-jps ["  123 11-04:00:00 500000 /usr/bin/java -Xfoo"]
                                    )]
               (check (str n ":pid") (:pid (first procs)) 123)
               (check (str n ":age") (:age-days (first procs)) 11.0)
               (check (str n ":rss") (:rss-gb (first procs)) (/ 500000.0 1048576.0)))))

      ;; frame-advanced-at picks the max ISO timestamp.
      _ (run! "frame-advanced-latest"
           (fn [n]
             (check n
               (frame-advanced-at [{:event/type :frame/advanced :event/at "2026-08-01T00:00:00Z"}
                                   {:event/type :frame/advanced :event/at "2026-08-02T00:00:00Z"}
                                   {:event/type :block/opened :event/at "2026-08-03T00:00:00Z"}])
               "2026-08-02T00:00:00Z")))

      ;; Exit codes: violated -> 1, unknown-only -> 2, all pass -> 0.
      _ (run! "exit-codes"
           (fn [n]
             (check (str n ":violated") (exit-code {:invariants [{:invariant/verdict :violated}]}) 1)
             (check (str n ":unknown") (exit-code {:invariants [{:invariant/verdict :unknown}]}) 2)
             (check (str n ":pass") (exit-code {:invariants [{:invariant/verdict :pass}]}) 0)))

      ;; Degrade-to-unknown: unreadable registry input.
      _ (run! "read-edn-missing-nil"
           (fn [n]
             (check n (read-edn-file "/nonexistent/nope.edn") nil)))

      ;; ISO parsing sanity.
      _ (run! "iso-parse"
           (fn [n]
             (check n (iso-to-ms "2026-08-27T08:26:24.254718248Z")
                    (.toEpochMilli (java.time.Instant/parse "2026-08-27T08:26:24.254718248Z")))))]

  (if (seq @failures)
    (do (println "FAILURES:")
        (doseq [f @failures] (prn f))
        (System/exit 1))
    (do (println "all invariant-checker unit tests passed")
        (System/exit 0))))
