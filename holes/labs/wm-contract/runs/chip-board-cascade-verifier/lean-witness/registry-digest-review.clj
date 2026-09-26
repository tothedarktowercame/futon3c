(require '[futon3c.agents.chip-board :as b]
         '[futon3c.agents.cascade-verifier-board :as c])
(with-redefs [b/verb-registry (atom @b/verb-registry)]
  (let [inputs {:verification-debt []}
        board (c/resolve-args inputs)
        original (get @b/verb-registry :smell-backlog)
        recorded (c/run inputs (fn [_] nil))]
    ;; Same output, different fn: isolates digest comparison from trace equality.
    (b/register-verb! :smell-backlog (fn [s a i] (original s a i)))
    (let [wrapped (c/run inputs (fn [_] nil))]
      (assert (= (:trace recorded) (:trace wrapped)))
      (assert (not= (:verbs/digest recorded) (:verbs/digest wrapped)))
      (assert (false? (b/verify-trace board inputs recorded))))
    (b/register-verb! :smell-backlog original)
    (assert (true? (b/verify-trace board inputs recorded)))
    ;; A new certificate under unsafe semantics can still self-replay correctly.
    (b/register-verb! :smell-backlog
                     (fn [s _ _] {:branch :false
                                  :effects [[:commit {:repo "counterexample-only"}]]
                                  :state' s}))
    (let [bad (c/run inputs (fn [_] nil))]
      (assert (not-any? #(= :zap (:verb %)) (:chips board)))
      (assert (= :commit (ffirst (get-in bad [:trace 0 :effects]))))
      (assert (true? (get-in bad [:certificate :verified?])))
      (prn {:same-trace-different-registry-refuses true
            :restored-registry-reverifies-original true
            :new-unsafe-registry-still-verifies true
            :no-zap-board-can-still-propose-commit true
            :digest-on-run (contains? bad :verbs/digest)
            :digest-in-certificate (contains? (:certificate bad) :verbs/digest)
            :effects-executed false}))))
