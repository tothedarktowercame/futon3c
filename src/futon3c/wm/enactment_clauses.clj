(ns futon3c.wm.enactment-clauses
  "Operational clauses for the compile-phase holes of cascade
  af6294ee687ca69fac2cb0ddd7e75c92cccf38e716a5082f3a3333d41e218f91
  (requisition f0ba279aab94dd4656a995a4d854484b93bc8ac4e5ebf2b6b4566b2752b5ed87).

  Each clause states, operationally and checkably, how its pattern is
  enacted in this cascade. Clause text is grounded in the pinned
  library source (futon3/library, rev b784e44e2473b468b63b3a2e47ef720cee4130b5);
  the :source-pin sha256 in each decision cites the exact file body.")


(def ^:private read-what-exists-first-clause
  "Before any pattern is enacted, read and record four sources with their
  citations: (1) the requirement text and decisions against it, (2) the
  implementation located by search (callers, timers, services), not assumed,
  (3) the live state now (logs, journals, gate runs, evidence store), and
  (4) prior attempts and their findings. Done when each source is read or
  recorded unavailable and every disagreement between sources is listed;
  the disagreements decide which patterns this cascade still needs.")

(def ^:private stops-on-schedule-clause
  "The halt is on the schedule, not in an operator's judgment: at each wave
  boundary stop cleanly, re-read the defect register, and convert the top
  'guarantee: none' rows into mechanisms -- recurrence requires a named
  mechanism to fail -- before the next wave starts. The next wave's start
  checks this precondition mechanically; register stewards drain between
  waves so no raw queue reaches the operator.")

(def ^:private evidence-over-assertion-clause
  "Every substantive claim emitted by this cascade's phases carries an
  evidence anchor (file path, trace event, test result, or quotation).
  A claim with no available evidence is emitted as provisional with an
  expiry and a verification path. Claims missing anchors are recorded as
  detectable violations, not accepted as style.")

(def ^:private process-conduct-is-unassured-clause
  "Problem node, not a mechanism: this hole is the landing point for @why
  edges from patterns that answer process-conduct assurance. Solving it is
  scoped by the record's solved-field -- Lean process spec checked at every
  transition, typed handoff contracts, author-never-closes-own-handoff,
  stepper boundary, T5 reflexivity -- and no mechanism is claimed at
  problem grain here.")

(def ^:private r10-scheduled-entrypoint-clause
  "Problem node R10 (scheduled entrypoint): unattended liveness means
  recurring execution without operator intervention plus a queryable trace,
  sharpened to observable state change -- not a scheduler firing or painting
  green no-op rows. Any enactment claiming to answer R10 is measured against
  observable state change and a queryable trace; the equation registry gives
  R10 no equation and classifies it as plumbing.")

(def ^:private promote-at-turn-end-clause
  "At turn end, promote exactly the files the turn touched: scope to
  session-attributed files, exempt files with another seat's in-flight
  activity inside the window (recorded with reason, re-checked at execution),
  gate on compile and lint, commit with attribution, then push per
  inbox-zero/push-the-declared. Held promotions are loud in the operator's
  agent view; harnesses without a turn-end hook are reported as uncovered,
  not assumed covered; exemptions older than 24h are reported as failures
  of the exemption.")

(defn- decision
  [clause source-path source-sha]
  {:resolution :operational-clause
   :clause clause
   :source {:path source-path :sha256 source-sha
            :revision "b784e44e2473b468b63b3a2e47ef720cee4130b5"}})

(defn enact-clauses
  "Patch var for requisition f0ba279aab94dd4656a995a4d854484b93bc8ac4e5ebf2b6b4566b2752b5ed87.
  Takes one context map {:cascade-digest :phase} and returns the decision
  map keyed by hole id. Refuses (throws ex-info) any other cascade or phase."
  [{:keys [cascade-digest phase] :as _ctx}]
  (when (or (nil? cascade-digest) (nil? phase))
    (throw (ex-info "patch context requires :cascade-digest and :phase"
                    {:schema :wm/patch-context-missing-keys})))
  (when (not= cascade-digest "af6294ee687ca69fac2cb0ddd7e75c92cccf38e716a5082f3a3333d41e218f91")
    (throw (ex-info "patch bound to a different cascade"
                    {:schema :wm/patch-cascade-mismatch
                     :cascade-digest cascade-digest})))
  (when (not= phase :compile)
    (throw (ex-info "patch bound to phase :compile"
                    {:schema :wm/patch-phase-mismatch :phase phase})))
  {["cascade-construction/read-what-exists-first" 0]
   (decision read-what-exists-first-clause
             "/home/joe/code/futon3/library/cascade-construction/read-what-exists-first.flexiarg"
             "04b442f40edd33be35382de2355944a0135c50dbefce4a7ec5967378ec21d81e")
   ["apparatus/the-system-stops-on-schedule" 1]
   (decision stops-on-schedule-clause
             "/home/joe/code/futon3/library/apparatus/the-system-stops-on-schedule.flexiarg"
             "afb63a67df2f5323003c984b67b2db79795ba94d5f4528589892342ad28ef11f")
   ["agent/evidence-over-assertion" 2]
   (decision evidence-over-assertion-clause
             "/home/joe/code/futon3/library/agent/evidence-over-assertion.flexiarg"
             "28425a05c4a87cbfe3147c1998e83701b8519b42c1796f1d0f25b4b039271248")
   ["problems/process-conduct-is-unassured" 4]
   (decision process-conduct-is-unassured-clause
             "/home/joe/code/futon3/library/problems/process-conduct-is-unassured.flexiarg"
             "11294171bd2e049cf21a741abe44bcd4a3e59d04c5cdf267bdf2ddd5aa88ea2")
   ["apparatus/the-system-stops-on-schedule" 5]
   (decision stops-on-schedule-clause
             "/home/joe/code/futon3/library/apparatus/the-system-stops-on-schedule.flexiarg"
             "afb63a67df2f5323003c984b67b2db79795ba94d5f4528589892342ad28ef11f")
   ["problems/r10-scheduled-entrypoint" 6]
   (decision r10-scheduled-entrypoint-clause
             "/home/joe/code/futon3/library/problems/r10-scheduled-entrypoint.flexiarg"
             "2ff7e4e388e7dc0c8f335dc4f89f56ae577124017c425f5a44b9c18b6f48f2bd")
   ["problems/process-conduct-is-unassured" 8]
   (decision process-conduct-is-unassured-clause
             "/home/joe/code/futon3/library/problems/process-conduct-is-unassured.flexiarg"
             "11294171bd2e049cf21a741abe44bcd4a3e59d04c5cdf267bdf2ddd5aa88ea2")
   ["inbox-zero/promote-at-turn-end" 9]
   (decision promote-at-turn-end-clause
             "/home/joe/code/futon3/library/inbox-zero/promote-at-turn-end.flexiarg"
             "8dde76d6103403d1561a5efec9e10736f7f3800201fb3058c278fd08f520a403")
   ["apparatus/the-system-stops-on-schedule" 10]
   (decision stops-on-schedule-clause
             "/home/joe/code/futon3/library/apparatus/the-system-stops-on-schedule.flexiarg"
             "afb63a67df2f5323003c984b67b2db79795ba94d5f4528589892342ad28ef11f")})
