(ns futon3c.apm.memory-access-gate-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.memory-access-gate :as sut]))

(deftest cascade-offers-can-be-gated-on-their-own-evidence-record
  ;; Measured 2026-09-06: 54 of 56 student attempt-1 dispatches received ZERO
  ;; cascade offers, every candidate refused :unverifiable-depositor-provenance.
  ;; Cause: the cascade offers only NON-shelf memories (expand-memory-cascade
  ;; excludes its own seeds, and the seeds are the shelf), while provenance was
  ;; only ever campaign-assembled FOR shelf entries. The two sets are disjoint
  ;; by construction, so no cascade offer could ever be served at attempt 1 --
  ;; the one moment cross-problem memory would matter. At attempt 2+ the gate
  ;; short-circuits on (nil? holdout), which is why offers appeared there and
  ;; all witnessed transfer has been within-frame.
  (let [authority {:problem-id "b97J03" :frame-id "f83"
                   :shelf/holdout :same-problem}
        serve #(sut/may-serve? authority % :cascade :memory-cascade)
        ;; the shape the fixed cascade now builds, from :evidence/author and
        ;; :evidence/subject on the memory's own substrate record
        cross {:memory-id "e-apm-promotion-62d4bd2f8ad1da80e73f402ba72039be"
               :depositor "f57-scribe"
               :provenance {:problem-id "b95J04" :frame-id "f57"
                            :provenance/source :evidence-record}}]
    (is (true? (:allowed? (serve cross)))
        "a memory mined from a DIFFERENT problem must reach the student")
    (is (= :different-problem-depositor (:reason (serve cross))))
    ;; everything the gate was protecting must still hold
    (is (false? (:allowed? (serve (assoc-in cross [:provenance :problem-id]
                                            "b97J03"))))
        "the attempt-1 same-problem holdout must still refuse")
    (is (= :same-problem-depositor
           (:reason (serve (assoc-in cross [:provenance :problem-id] "b97J03")))))
    (is (false? (:allowed? (serve (assoc-in cross [:provenance :frame-id] "f99"))))
        "a record whose author and frame disagree must still be refused")
    (is (false? (:allowed? (serve {:memory-id "e-x" :depositor "f57-scribe"})))
        "no provenance at all must still be refused -- not a blanket accept")
    ;; and the campaign-assembled shelf path is untouched
    (is (true? (:allowed? (serve {:memory-id "e-y" :depositor "f108-scribe"
                                  :provenance {:campaign-id "jit-all-open-v2"
                                               :frame-id "f108"
                                               :problem-id "m02J01"}})))
        "shelf provenance must keep working exactly as before")))
