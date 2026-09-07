(ns futon3c.apm.open-problem-queue-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.open-problem-queue :as sut]))

(deftest canonical-corpus-selection-is-pinned-and-auditable
  (let [result (sut/derive-queue "/home/joe/code/apm-lean")
        problems (:problems result)
        excluded (:excluded result)]
    (is (:ok result))
    ;; 69 = 501 corpus - 360 not-open - 66 topology - 4 defective - 2
    ;; construction-blocked. Note that only the defective and
    ;; construction-blocked subtrahends are actually asserted below; not-open
    ;; and topology are not, so a move in this count still needs the class
    ;; breakdown read by hand.
    ;;
    ;; Was 101 over a 475 corpus until 2026-09-07. Both numbers moved on the
    ;; same day for two independent reasons, which is why the total is no
    ;; longer the stable denominator the older notes below assume:
    ;;
    ;;   -32 open   problems banked as solves landed through the day
    ;;   +26 corpus new statements encoded into problems/ (2a20e5eb..f3ce843e,
    ;;                "Encode ... statements", 15:23 onward)
    ;;   -13 topology  t-problems that were solved; :not-open is decided before
    ;;                :topology, so a solved t-problem leaves the topology
    ;;                class rather than staying in it
    ;;
    ;; Those close the +71 in not-open exactly (-32 + 26 + 13), and 501 is the
    ;; count of directories under apm-lean problems/, so no problem was lost.
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
    ;; :partial frame result, and a pin that fired anyway. f71/b93J03 was
    ;; the third of that shape, taking it to 107, and f74/b94J01 the fourth,
    ;; taking it to 106. f74 is the instructive one: all three student
    ;; attempts ran with a FAILED memory cascade (futon1b 503
    ;; :expensive-read-busy) and reported :failed, and the solver's proof
    ;; banked anyway. A degraded student protocol does not withhold the pin;
    ;; only a park does. f75/b94J03 followed at 105 -- three sorries closed,
    ;; and the first frame to run end to end with the cascade retry live.
    ;; f76/b95J02 took it to 104 and closed outright rather than partial --
    ;; the first :closed frame result since f66. f77/b95J04 took it to 103,
    ;; the twentieth banked and the second three-sorry problem closed.
    ;; f79/b96A03 took it to 102, and f81/b96J03 to 101 -- the third frame
    ;; to close outright rather than partial, after f66 and f76.
    (is (= 69 (count problems)))
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
      (is (= 501 (+ (count problems) (count excluded))))
      (is (every? #{:not-open :topology :defective-or-invalid-statement
                    :construction-blocked}
                  (map :reason excluded))))))
