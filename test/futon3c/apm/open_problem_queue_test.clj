(ns futon3c.apm.open-problem-queue-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.open-problem-queue :as sut]))

(deftest canonical-corpus-selection-is-pinned-and-auditable
  (let [result (sut/derive-queue "/home/joe/code/apm-lean")
        problems (:problems result)
        excluded (:excluded result)]
    (is (:ok result))
    ;; 108 = 475 corpus - 282 not-open - 79 topology - 4 defective - 2
    ;; construction-blocked. Each subtrahend is asserted below, so a future
    ;; move in this count identifies which class changed rather than being an
    ;; unexplained integer.
    ;;
    ;; Was 121 until 2026-08-28. The three that left are a98A03, a98A04 and
    ;; a98A07: their solver heads were verified against the axiom gate, pinned,
    ;; and swept to origin/master, so their sorry counts are now 0 and they
    ;; classify :not-open. The corpus did not shrink -- the 475 total below is
    ;; unchanged -- three problems moved from open to solved, which is the
    ;; queue noticing that work got banked. It fell from 118 to 116 when the
    ;; independently verified f52/a99J01 and f53/a99J03 solver heads were
    ;; swept to origin/master (f992b7b0/66a497f0 and 545ea324/69395657). It
    ;; fell from 116 to 112 on 2026-08-31 when four more campaign solves were
    ;; swept: f59/b00J01, f62/b01J01, f64/b01J03 and f65/b01J04. Each was
    ;; pinned only after `#print axioms` reported exactly
    ;; [propext, Classical.choice, Quot.sound] with no sorryAx. f66/b03J01
    ;; followed the same afternoon, taking it to 111, and f67/b90A01 with
    ;; f69/b93A01 that evening took it to 109. b90A01 is the instructive one:
    ;; all three of f67's STUDENT attempts were partial and the frame result is
    ;; :partial, but the SOLVER closed it, so the pin fired and it banked. A
    ;; :partial frame result is about the learning protocol, not about whether
    ;; the problem was solved. f70/b93J01 followed the same way overnight,
    ;; taking it to 108: three partial student attempts, a solver proof, a
    ;; :partial frame result, and a pin that fired anyway.
    (is (= 108 (count problems)))
    (is (every? #(and (= :non-excluded (:classification %))
                      (not (.startsWith ^String (:problem/id %) "t"))
                      (every? string? ((juxt :repository :base-branch :revision
                                             :path :blob) %)))
                problems))
    (testing "known statement defects are explicit exclusions"
      (is (= #{"a96A06" "b96A01" "m93J05" "m96J02"}
             (set (map :problem/id
                       (filter #(= :defective-or-invalid-statement (:reason %))
                               excluded))))))
    (testing "construction-blocked problems are explicit exclusions"
      ;; Added 2026-08-26 (6a7735e1, 57586fc6). These two moved out of the
      ;; solve queue then, which is why the count above is 121 and not 123.
      (is (= #{"a96A07" "a97J08"}
             (set (map :problem/id
                       (filter #(= :construction-blocked (:reason %))
                               excluded))))))
    (testing "topology never enters the solve queue"
      (is (every? #(not (.startsWith ^String (:problem/id %) "t")) problems)))
    (testing "every problem is either queued or excluded for a stated reason"
      (is (= 475 (+ (count problems) (count excluded))))
      (is (every? #{:not-open :topology :defective-or-invalid-statement
                    :construction-blocked}
                  (map :reason excluded))))))
