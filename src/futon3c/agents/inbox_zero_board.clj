(ns futon3c.agents.inbox-zero-board
  "Inbox-zero as the first chip-board (SPEC-chip-boards-v0.md §4 task 0;
  NOTE-inbox-zero-aif.md). The board is data; the observation packet is
  built from existing futon3c state (sweeper outputs, agency registry) by
  the caller — this namespace stays pure.

  v0 policy, received from the README's rulings (each clause cites an
  incident — the R17 clause-accretion discipline):
  - operator pending (keypress true) => yield immediately;
  - sweep clean => sing the meters and yield (report, don't touch);
  - repo flagged => LOOK its failed clauses, FEEL for in-flight turns,
    ZAP only if felt-idle (commit); a live turn forces the safe arm
    (report the flag, never commit under an edit — U59);
  - every refusal is a typed :inbox-zero/refusal record (C446)."
  (:require [futon3c.agents.chip-board :as board]))

(def board-v0
  "The board. Wires in comments are the authored policy above."
  {:board/id "b-inbox-zero-0"
   :board/version 1
   :entry :ck/keypress
   :constants {:fuel-budget 16}
   :provenance {:author "joe+zai-7" :spec "SPEC-chip-boards-v0.md"}
   :chips
   [{:chip/id :ck/keypress :verb :keypress
     :wires {:true :ck/yield :false :ck/smell}}
    {:chip/id :ck/smell :verb :smell
     :wires {:true :ck/look-first :false :ck/sing-clean}}
    {:chip/id :ck/look-first :verb :look
     :args {:repo :first-flagged}
     :wires {:true :ck/feel-target :false :ck/yield}}
    {:chip/id :ck/feel-target :verb :feel
     :args {:repo :first-flagged}
     :wires {:true :ck/sing-flag-live :false :ck/zap-commit}}
    {:chip/id :ck/zap-commit :verb :zap
     :args {:repo :first-flagged :mode :commit}
     :wires {:true :ck/yield :false :ck/yield}}
    {:chip/id :ck/sing-clean :verb :sing
     :wires {:true :ck/yield}}
    {:chip/id :ck/sing-flag-live :verb :sing
     :wires {:true :ck/yield}}
    {:chip/id :ck/yield :verb :yield}]})

(defn- first-flagged-repo
  [inputs]
  (->> (:sweep inputs) (filter (complement :clean?)) first :repo))

(defn resolve-args
  "Resolve :first-flagged placeholders against INPUTS when the board runs."
  [inputs]
  (let [target (first-flagged-repo inputs)]
    (update board-v0 :chips
            (fn [chips]
              (mapv #(if (= :first-flagged (get-in % [:args :repo]))
                       (assoc-in % [:args :repo] target)
                       %)
                    chips)))))

(defn observation-packet
  "Build the board's R2 packet from a sweep summary and an in-flight set.
  Pure; callers extract these from futon3c.inbox-zero state and the agency
  registry (the explicit in-flight-turn channel — NOTE-inbox-zero-aif
  prediction 3)."
  [sweep in-flight-repos operator-pending?]
  {:sweep (mapv #(-> (select-keys % [:repo :clean? :clauses-failed])) sweep)
   :in-flight-repos (vec in-flight-repos)
   :operator-pending? (boolean operator-pending?)})

(defn run
  "Run one sweep cycle under the board. EFFECT-HANDLER performs the
  effects (commit/report/refusal are the caller's I/O). Returns the run map
  plus a runtime certificate: the claim (trace + end-reason), the board
  digest, and the inputs digest, with :lean/status :pending — validation in
  DarkTower/WarMachine is consumed when a witness module exists, never
  asserted before it does."
  ([inputs effect-handler]
   (let [b (resolve-args inputs)
         run (board/run-board b inputs effect-handler)
         cert {:cert/type :chip-board/run-v0
               :board/id (:board/id b)
               :board/digest (board/board-digest b)
               :inputs/digest (board/board-digest {:chips [inputs]})
               :claim {:trace (:trace run) :end-reason (:end-reason run)}
               :verified? (board/verify-trace b inputs run)
               :lean/status :pending}]
     (assoc run :certificate cert))))
