(ns futon3c.wm.ordinary-click-budget
  "Issue-time accounting for Joe's five ordinary clicks. Specialized RUN4/R10
   requests retain their own authority; only the plain HTTP branch calls this."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [java.nio ByteBuffer]
           [java.nio.channels FileChannel]
           [java.nio.file StandardOpenOption]))

(def authorization
  ;; Eighth grant, Joe 2026-09-24, heard directly by claude-5 in the operator
  ;; buffer: "I award 5 more clicks".
  ;; The seventh (AUTH-ordinary-click-budget-renewal-6-2026-09-23.md @
  ;; 2996eb6b) was fully consumed, 7 of 7, so nothing carries forward and this
  ;; allocates five. The seven earlier grants allocated five each bar that one;
  ;; consumption counts ledger entries whose :authorization equals THIS map, so
  ;; the thirty-five already spent are neither double-counted nor erased --
  ;; each remains in the ledger citing the authority in force when it was
  ;; spent.
  ;;
  ;; The grant does not restart the machine. The grounding misreport that ran
  ;; from 2026-09-23 17:44 (entity :props read back as a string, so :resolved?
  ;; was false about a commit the entity does name) is repaired and loaded.
  ;; What holds the clicks now is Joe's stop-line rule, settled 2026-09-24:
  ;; "if there is a stop-line, in my vocabulary that means the system should be
  ;; repaired from outside." An open stop-line stops the line -- it is not
  ;; per-defect -- so no click issues while the board is dirty. 35 open
  ;; :machine-failure findings stand. The authorization document carries the
  ;; board and the repair route.
  ;;
  ;; 2026-09-24, later: Joe to claude-8, heard directly in claude-8's
  ;; operator buffer: "I would award 10 clicks for you to use overnight as
  ;; you see fit." Recorded in the same document's section "Joe's grant to
  ;; claude-8, 2026-09-24" (futon2 326826ac) per that document's own rule
  ;; that a draw raises `allocated` here rather than minting a new file.
  ;; 5 (claude-5's lane, banked) + 10 (claude-8's lane) = 15. The ledger's
  ;; :caller says which lane spent which. No click-path gate reads the
  ;; board; the stop-line discipline is the lane's, taken from the store.
  {:path "futon2/holes/labs/wm-contract/AUTH-ordinary-click-budget-renewal-7-2026-09-24.md"
   :sha "326826ac"})
(def allocated
  "Five of the eighth grant (unspent, claude-5) plus ten granted to claude-8
   on 2026-09-24 for overnight use at claude-8's discretion, plus twenty
   granted to claude-1 on 2026-09-29 (\"OK let's run 20 ticks, you can do
   repairs between them\"), plus ten granted to codex-10 on 2026-10-01 for
   the new registered-run series (\"I'll grant a block of 10 clicks\"; repairs
   may land between clicks), plus one granted to codex-10 on 2026-10-04
   after the repaired read-only META preview (\"1 is authorised\"), plus one
   granted to codex-10 on 2026-10-05 for the R20 certificate evaluation, plus
   one granted to claude-12 on 2026-10-09 for a debugger-attached click with
   Lean, token and timing assessment, plus one more granted to claude-12 on
   2026-10-09 for the post-repair verification click, watched phase by phase
   under the debugger, plus one more granted to claude-12 on 2026-10-09 for the
   click that scores the first report-card preregistrations, and one on
   2026-10-10 after the close-overflow fix, and one more on 2026-10-10 for the
  click codex-68 signed off (PRELAUNCH-DEFECTS.md), plus one granted directly
  by Joe to codex-68 on 2026-10-12 for a debugger-attached click after the
  giant-checkpoint repair, plus one granted directly by Joe to codex-68 on
  2026-10-12 after the live outer-selector repair (`please run 1 click and use
  the debugger`), plus one successor click granted directly by Joe to
  codex-68 on 2026-10-12 to verify selection through actual work and a valid
  runtime trace. All grants are recorded in the operator thread or the
  authorization document named above, plus one further successor click granted
  directly by Joe on 2026-10-12 after the reviewed-mission pin repair, plus one
  granted directly by Joe to codex-68 on 2026-10-14 after the bounded CI and
  build repairs (`run one click ... turn on the debugger and watch the run`),
  plus one granted directly by Joe to codex-68 on 2026-10-14 after that run's
  debugger postmortem (`ok, let's run another click under this system`), plus
  one replacement demonstration click granted directly by Joe on 2026-10-14
  after the post-selection debugger-boundary repair (`do any needed repairs
  and get another live run going`), plus two clicks granted directly by Joe to
  codex-68 on 2026-10-14 to validate the new learning certificates under the
  debugger (`run 1 or 2 clicks as needed`), plus one granted directly by Joe
  to codex-68 on 2026-10-15 after the debugger readiness repair (`OK please
  try 1 click now, we'll see whether it can go end-to-end`), plus five ordinary
  clicks authorized directly by Joe to codex-68 on 2026-10-16 for the live
  post-Q7--Q10 run-and-repair sequence, plus one ordinary click authorized
  directly by Joe to codex-68 on 2026-10-16 for the live selected-target
  author-contact proof, plus one further debugger-attached contact-proof click
  authorized directly by Joe after the live-judgement predicate repair. Joe additionally required a designated,
  freshness-checked WM cast rather than opportunistic roster reuse, plus one
  post-closure-fix verification click authorized directly by Joe, plus one
  handoff-repair verification click authorized directly by Joe with the
  debugger attached, plus one post-contract-audit debugger click and one
  consolidated-handoff verification click authorized directly by Joe, plus
  one explicit repair-loop click authorized directly by Joe after the live
  excerpt-boundary repair, plus one post-loop-repair debugger click authorized
  directly by Joe, plus one post-handoff-format-audit debugger click authorized
  directly by Joe on 2026-10-16, plus one post-handoff-index debugger click
  authorized directly by Joe on 2026-10-16, plus one post-boundary-repair
  debugger click authorized directly by Joe on 2026-10-16
  (\"OK, let's run one click now\"), plus one debugger-attached live-readiness
  click authorized directly by Joe on 2026-10-17
  (\"OK please run 1 click with the debugger attached to catch any error\"), plus
  one timing-instrumented debugger click authorized directly by Joe on
  2026-10-17 after the inner-cascade-reconstruction replay
  (\"OK, it sounds like one more click is warranted\"), plus one further
  debugger-attached click authorized directly by Joe on 2026-10-17 to test
  live execution and the premise that the debugger does not introduce an
  unnecessary hang, plus five readiness-campaign clicks authorized directly
  by Joe on 2026-10-17, to be spent one at a time with inspection and logged
  repairs between them. These are followed by five sequel-readiness clicks authorized directly by
  Joe on 2026-10-18, run one at a time with debugger repairs and recorded in
  TN-war-machine-readiness-oct-18-2026.md, plus three observation-only clicks
  authorized directly by Joe on 2026-10-18 to test whether META selects and
  runs the self-heal Algorithm without outside intervention, plus one
  replacement for the first trial run after the observation harness itself
  violated that condition by dispatching an external repair agent, plus five
  back-to-back clicks authorized directly by Joe to codex-68 on 2026-10-19 to
  exhibit META self-repair, stopping for an outside repair if self-repair does
  not select or act, plus one replacement for charged click
  wm-click-fd0b2f60-ad05-4995-b272-db3bb793951b, which has no durable
  click/run binding or terminal run record because it was admitted during the
  campaign stop race. This replacement restores the unspent fifth campaign
  click; it is not a new grant, plus five further back-to-back clicks
  authorized directly by Joe to codex-68 on 2026-10-19 to prove the repaired
  META self-heal route, with an outside repair only if the Algorithm still
  cannot execute, plus one click authorized directly by Joe to codex-1 on
  2026-10-19: \"ok run 1 click to see how far it gets\", following the
  outer-policy provider and equal-minimum selection repairs, plus one click
  in Joe's measured-frontier repair packet relayed by claude-5 on 2026-10-19
  (Agency invoke-1792396784770-17217-0962b0d7), after a serving-JVM probe,
  plus five back-to-back debugger-attached clicks authorized directly by Joe
  to codex-1 on 2026-10-19: \"we can run 5 back to back now. please use the
  debugger to catch any errors that dont immediately lead to the self-heal
  algorithm running.\", plus five clicks authorized directly by Joe to claude-5
  on 2026-10-19 at 17:23Z for the working loop (one click, one bounded repair
  per class of stop, independent review, then the next): \"ok I authorize up
  to 5 clicks to proceed in the way you described, fixing each class of error
  that is revealed\", plus five clicks authorized directly by Joe to claude-5
  on 2026-10-19 at about 20:30Z, after click wm-click-c78d0f27's masked
  prepare refusal was found and fixed (futon2 df2209a33, futon3c ddb3fcb79):
  \"we can do 5 more as long as progress is being made. if you found the
  fix, claude-2 could help formalise that as a proof\", plus ten clicks
  authorized directly by Joe to claude-5 on 2026-10-20 at about 12:35Z for the
  self-heal retry route's first live run (enact, retry, re-queues to the typed
  abandonment), item 1 of DECISIONS-FOR-JOE-2026-10-20.md, after M10b-4
  (futon2 b5dd9269c) and Lean rev 11 landed: \"click budget of 10 allocated\",
  plus ten clicks authorized directly by Joe to claude-5 on 2026-10-20 at
  about 18:39Z, after M23L (darktower 87784b4534, Lean rev 24) was accepted
  and the alignment regenerated (futon2 38419663c): \"10 more clicks granted\""
  ;; Joe to codex-11, 2026-10-22: ten clicks for frozen-section development.
  ;; Record successful boundaries; resume without a click whenever possible.
  ;; Authority: renewal-7 document, frozen-section project grant section.
  ;; Joe to claude-7, 2026-10-23 ~03:38Z, after the E3 rerun (M2 F1-F3) was
  ;; refused at 158/158: "10 clicks granted". Lane: E3 rerun, resumes it
  ;; needs, and the failure-detail injury via the self-heal route.
  168)
(def ^:dynamic *ledger-path*
  "/home/joe/code/futon2/data/wm-ordinary-clicks/consumption.jsonl")
(defonce ^:private issue-lock (Object.))

(defn- charged-click-ids [entries]
  (let [consumed (into #{} (keep #(when (and (= authorization (:authorization %))
                                             (nil? (:event %)))
                                    (:click-id %))) entries)
        refunded (into #{} (keep #(when (and (= authorization (:authorization %))
                                             (= "refund" (:event %)))
                                    (:click-id %))) entries)]
    (set/difference consumed refunded)))

(defn- parse-ledger [text]
  (mapv #(json/parse-string % true)
        (remove str/blank? (str/split-lines text))))

(defn- ledger-entries [file]
  (if (.exists file) (parse-ledger (slurp file)) []))

(defn availability
  "Return a non-consuming, source-pinned snapshot of the ordinary-click ration.
   The same lock as `consume!` makes the ledger bytes and count one observation."
  []
  (locking issue-lock
    (let [file (io/file *ledger-path*)
          bytes (if (.exists file)
                  (with-open [channel (FileChannel/open
                                       (.toPath file)
                                       (into-array StandardOpenOption
                                                   [StandardOpenOption/READ]))
                              _file-lock (.lock channel 0 Long/MAX_VALUE true)]
                    (java.nio.file.Files/readAllBytes (.toPath file)))
                  (byte-array 0))
          entries (parse-ledger (String. bytes "UTF-8"))
          consumed (count (charged-click-ids entries))]
      {:schema :wm/ordinary-click-availability-v1
       :authorization authorization
       :allocated allocated
       :consumed consumed
       :available (max 0 (- allocated consumed))
       :unit :ordinary-click
       :ledger-source
       {:path *ledger-path*
        :sha256 (let [digest (.digest (java.security.MessageDigest/getInstance "SHA-256") bytes)]
                   (apply str (map #(format "%02x" (bit-and % 0xff)) digest)))}})))

(defn consume!
  "Append and force consumption before the worker starts. Serialize the count
   and append across threads and processes; failed runs never refund a grant."
  [click-id issued-at caller]
  (locking issue-lock
    (let [file (io/file *ledger-path*)]
      (io/make-parents file)
      (with-open [channel (FileChannel/open
                          (.toPath file)
                          (into-array StandardOpenOption
                                      [StandardOpenOption/CREATE StandardOpenOption/READ
                                       StandardOpenOption/WRITE]))
                  _file-lock (.lock channel)]
        (let [entries (ledger-entries file)
              consumed (count (charged-click-ids entries))]
          (when (>= consumed allocated)
            (throw (ex-info
                    (str "Ordinary click budget exhausted. Return to Joe for renewal. Authority: "
                         (:path authorization) " @ " (:sha authorization))
                    {:status 409 :error :ordinary-click-budget-exhausted
                     :authorization authorization :allocated allocated
                     :consumed consumed :renewal "Joe"})))
          (let [entry {:click-id click-id :issued-at issued-at
                       :authorization authorization
                       :caller (or caller :caller-unknown)}
                buffer (ByteBuffer/wrap
                        (.getBytes (str (json/generate-string entry) "\n") "UTF-8"))]
            (.position channel (.size channel))
            (while (.hasRemaining buffer) (.write channel buffer))
            (.force channel true)
            ;; Force the ledger directory and its entry in the existing data
            ;; directory, including the first issue that creates this store.
            (doseq [dir [(.getParentFile file) (.getParentFile (.getParentFile file))]]
              (with-open [directory (FileChannel/open
                                     (.toPath dir)
                                     (into-array StandardOpenOption [StandardOpenOption/READ]))]
                (.force directory true)))
            entry))))))

(defn refund!
  "Append a compensating event for one previously consumed ordinary click.
   This preserves the original charge and refuses unknown or already-refunded
   click IDs. REASON names the operator-authorized reason for restoration."
  [click-id refunded-at caller reason]
  (locking issue-lock
    (let [file (io/file *ledger-path*)]
      (when-not (.exists file)
        (throw (ex-info "ordinary click ledger is absent"
                        {:error :ordinary-click-refund-unknown :click-id click-id})))
      (with-open [channel (FileChannel/open
                          (.toPath file)
                          (into-array StandardOpenOption
                                      [StandardOpenOption/READ StandardOpenOption/WRITE]))
                  _file-lock (.lock channel)]
        (let [entries (ledger-entries file)
              charge (some #(when (and (= authorization (:authorization %))
                                       (= click-id (:click-id %))
                                       (nil? (:event %))) %) entries)
              prior-refund (some #(when (and (= authorization (:authorization %))
                                             (= click-id (:click-id %))
                                             (= "refund" (:event %))) %) entries)]
          (when-not charge
            (throw (ex-info "ordinary click was not charged under this authority"
                            {:error :ordinary-click-refund-unknown :click-id click-id})))
          (when prior-refund
            (throw (ex-info "ordinary click was already refunded"
                            {:error :ordinary-click-already-refunded :click-id click-id})))
          (let [entry {:event "refund" :click-id click-id :refunded-at refunded-at
                       :authorization authorization :caller (or caller :caller-unknown)
                       :reason reason}
                buffer (ByteBuffer/wrap
                        (.getBytes (str (json/generate-string entry) "\n") "UTF-8"))]
            (.position channel (.size channel))
            (while (.hasRemaining buffer) (.write channel buffer))
            (.force channel true)
            entry))))))
