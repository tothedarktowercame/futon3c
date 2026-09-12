(ns futon3c.agents.chip-board
  "Chip-board runtime v0 — SPEC-chip-boards-v0.md (futon2/holes/labs/wm-contract).

  Boards are finite, versioned EDN: a vector of chips, each an observation
  verb with exactly :true/:false wires (two-wire semantics — no maybe), or an
  action/flow verb. Chips are pure functions of (state, args, inputs); all
  I/O leaves the board as effects the executor performs through the effect
  handler. Every fired chip appends a trace record carrying the board
  version, so 'which board governed this decision' is always answerable.

  Hazards: :zap refuses as a typed refusal unless the state carries a
  feel-certified adjacency for the target (the sweeper-vs-edit lesson, C446:
  refusals are reported, never silent; U59: never act on a target whose
  liveness you have not felt for).

  Determinism: byte-identical board + identical inputs => identical trace,
  given the same effect handler. Verification is replay (verify-trace)."
  (:require [clojure.string :as str])
  (:import [java.security MessageDigest]))

;; ---------------------------------------------------------------- schema

(def observation-verbs
  "Verbs that branch. Every one of these requires both wires."
  #{:feel :look :smell :keypress :compare-move})

(def terminals #{:yield :done})

(defn- sha256 [s]
  (let [d (.digest (MessageDigest/getInstance "SHA-256")
                   (.getBytes (pr-str s) "UTF-8"))]
    (str "sha256:" (apply str (map #(format "%02x" (bit-and % 0xff)) d)))))

(defn board-digest
  "Content digest of a board (chips + constants), for trace provenance."
  [board]
  (sha256 (select-keys board [:chips :constants])))

(defn validate-board
  "Return nil when BOARD conforms, else a reason string. Checks: entry
  exists; chips finite and id-unique; observation verbs carry both wires;
  every wire resolves to a chip id or a terminal; constants are declared
  numbers."
  [board]
  (let [chips (vec (:chips board))
        ids (set (map :chip/id chips))
        dupes (str/join "," (keep #(when (> (val %) 1) (key %))
                                  (frequencies (map :chip/id chips))))]
    (cond
      (not (seq chips)) "board has no chips"
      (seq dupes) (str "duplicate chip ids: " dupes)
      (not (contains? ids (:entry board))) "entry chip missing"
      (not (map? (:constants board))) "constants must be a map"
      (some #(and (number? %) (not (== % (double %)))) (vals (:constants board)))
      "constants must be numeric"
      :else
      (first
       (for [chip chips
             :when (contains? observation-verbs (:verb chip))
             :let [wires (:wires chip)]
             :when (not (and (contains? wires :true) (contains? wires :false)))]
         (str "chip " (:chip/id chip) " (" (:verb chip) ") lacks two wires"))))))

(defn- wire-resolves? [w ids]
  (or (contains? terminals w) (contains? ids w)))

(defn validate-wiring
  "Second pass: every wire target resolves."
  [board]
  (let [ids (set (map :chip/id (:chips board)))]
    (first (for [chip (:chips board)
                 [_wire target] (:wires chip)
                 :when (not (wire-resolves? target ids))]
             (str "chip " (:chip/id chip) " wire to unknown target " target)))))

;; ------------------------------------------------------------- verb fns
;;
;; Each: (fn [state args inputs]) => {:branch :true|:false :effects [..]
;;                                     :state' state}
;; Pure. Inputs is the observation packet supplied by the executor caller.

(defn- branch [b] {:branch b :effects []})

(defn verbs-branch
  "Public branch constructor for registered verbs (the two-wire result)."
  [b] (branch b))

(def base-verbs
  {:keypress
   (fn [state _args inputs]
     (assoc (branch (if (:operator-pending? inputs) :true :false))
            :state' state))
   :smell
   (fn [state _args inputs]
     (let [sweep (vec (:sweep inputs))
           flagged (filterv (complement :clean?) sweep)]
       (assoc (branch (if (seq flagged) :true :false))
              :effects [[:observe {:channel :inbox-zero/sweep
                                   :repos-flagged (mapv :repo flagged)}]]
              :state' (assoc state :shelf-sweep sweep))))

   :look
   (fn [state args _inputs]
     (let [repo (:repo args)
           row (some #(when (= repo (:repo %)) %) (:shelf-sweep state))]
       (if row
         ;; evidential distance v0: one hop through a declared channel,
         ;; precision 1.0 by construction => distance = clause count.
         (let [distance (count (:clauses-failed row))]
           (assoc (branch (if (pos? distance) :true :false))
                  :effects [[:observe {:channel :inbox-zero/clauses
                                       :repo repo
                                       :clauses-failed (:clauses-failed row)}]]
                  :state' (assoc state :range-finder distance)))
         ;; typed none, not an error: the false wire is the wired path.
         (assoc (branch :false)
                :effects [[:typed-none {:channel :inbox-zero/clauses
                                        :repo repo
                                        :reason :repo-absent-from-sweep}]]
                :state' state))))

   :feel
   (fn [state args inputs]
     (let [repo (:repo args)
           live (set (:in-flight-repos inputs))]
       (assoc (branch (if (contains? live repo) :true :false))
              :state' (if (contains? live repo)
                        (update state :feel-blocked (fnil conj #{}) repo)
                        (assoc state :feel-certified repo)))))

   :zap
   (fn [state args _inputs]
     (if (and (:repo args) (= (:feel-certified state) (:repo args)))
       {:branch :true
        :effects [[:commit {:repo (:repo args) :mode (:mode args :commit)}]]
        :state' state}
       ;; The hazard gate: acting on an unfelt target is a typed refusal.
       ;; A nil/missing repository is never feel-certified (witness defect 1).
       {:branch :false
        :effects [[:refusal {:record/type :inbox-zero/refusal
                             :reason :zap-without-feel
                             :repo (:repo args)}]]
        :state' state}))

   :compare-move
   ;; Two-wire: compare the top of the move stack to the argued move.
   ;; A nil argument is the empty-square test: true iff the stack is empty.
   (fn [state args _inputs]
     (let [stack (vec (:move-stack state))
           arg (:move args)
           match? (if (nil? arg)
                    (empty? stack)
                    (and (seq stack) (= (peek stack) arg)))]
       (assoc (branch (if match? :true :false))
              :state' state)))

   :sing
   ;; Meters only, never prose (SPEC: SING is not evidence; singing a
   ;; meter's value is a typed read).
   (fn [state _args _inputs]
     {:branch :true
      :effects [[:report {:meters (select-keys state [:range-finder :fuel :damage])}]]
      :state' state})

   :yield
   (fn [state _args _inputs] {:branch :true :effects [[:yield-turn {}]] :state' state})})

(def verb-registry
  "The verb library. Registration is code-level (load time), never a
  run-time transition: boards reference registered verbs; a board naming
  an unregistered verb fails validation-by-execution as a type error."
  (atom base-verbs))

(defn register-verb! [verb f] (swap! verb-registry assoc verb f) verb)

;; -------------------------------------------------------------- executor

(defn- inputs-for [_verb _chip inputs] inputs)

(defn run-board
  "Run BOARD from :entry against INPUTS until a terminal, fuel exhaustion,
  or the step cap (cycle guard). EFFECT-HANDLER is (fn [effect] result);
  it performs I/O. Returns {:trace [...] :end-reason ... :final-state ...}.

  Fuel is charged per fired chip; exhaustion is a typed end, not a crash."
  ([board inputs effect-handler]
   (run-board board inputs effect-handler 64))
  ([board inputs effect-handler max-steps]
   (let [v (validate-board board)]
     (when v (throw (ex-info (str "invalid board: " v) {:board board})))
     (let [w (validate-wiring board)]
       (when w (throw (ex-info (str "invalid wiring: " w) {:board board}))))
     (let [chips (into {} (map (juxt :chip/id identity) (:chips board)))
           digest (board-digest board)
           fuel0 (long (get (:constants board) :fuel-budget 32))]
       (loop [id (:entry board)
              state {:fuel fuel0 :damage 0 :range-finder nil}
              trace []]
         (let [step (count trace)]
           (cond
             (contains? terminals id)
             {:trace (conj trace {:chip id :verb :terminal
                                  :board/digest digest :branch :true})
              :end-reason (keyword "end" (name id))
              :final-state state}

             (>= step max-steps)
             {:trace trace :end-reason :end/step-cap :final-state state}

             (zero? (long (:fuel state)))
             {:trace trace :end-reason :end/fuel-exhausted :final-state state}

             :else
             (let [chip (get chips id)
                   verb (:verb chip)
                   terminal? (contains? terminals verb)
                   f (or (get @verb-registry verb)
                         (throw (ex-info (str "unknown verb " verb) {:chip id})))
                   result (f (update state :fuel dec)
                             (:args chip)
                             (inputs-for verb chip inputs))
                   {:keys [branch effects state']} result
                   _ (run! effect-handler effects)
                   rec {:chip id :verb verb :branch branch
                        :board/digest digest
                        :effects (mapv vec effects)}]
               (if terminal?
                 {:trace (conj trace rec)
                  :end-reason (keyword "end" (name verb))
                  :final-state state'}
                 (let [wire-target (get-in chip [:wires branch])]
                   (when (nil? wire-target)
                     (throw (ex-info (str "unwired branch :" branch " on chip " id)
                                     {:chip id :branch branch})))
                   (recur wire-target state' (conj trace rec))))))))))))

(defn verify-trace
  "Replay BOARD against INPUTS with a no-op effect handler and compare the
  full trace (chip/verb/branch/digest AND effects) plus end-reason to the
  recorded run. Effects are pure functions of (state, args, inputs), so an
  altered payload in the recorded trace must fail the comparison — the
  certificate certifies what was done, not just the route (witness
  defect 2)."
  [board inputs recorded-run]
  (let [replayed (run-board board inputs (fn [_] nil))]
    (and (= (:trace replayed) (:trace recorded-run))
         (= (:end-reason replayed) (:end-reason recorded-run)))))
