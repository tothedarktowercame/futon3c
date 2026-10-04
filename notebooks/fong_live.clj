;; # Open causal theories, live per turn: a first working sketch
;;
;; Joseph Corneli, with Claude (claude-17). Futon stack, 4 October 2026.
;; Everything below is evaluated when this page is built; the outputs are
;; what the code returned, not illustrations.
;;
;; The setting is a working environment in which a human operator (Joe) and
;; several AI agents hand work to one another in a chat-like REPL. Two kinds
;; of reading happen after every turn. The operator's turn is read by 小象
;; ("little elephant", a classical classifier, about 0.1 s) and then by 象, a
;; language model that confirms or corrects it. The agent's turn is read by
;; 大象 ("big elephant"), which is entirely classical: agents write each
;; paragraph of a reply under a mark that declares its intent (🈸 asks the
;; operator to decide something, ㊢ reports, ㊭ proposes, and so on), so
;; there is nothing to infer.
;;
;; The question this notebook works on: can the resulting stream of acts be
;; given a logic that says, after each turn, which obligations closed and
;; which are still open, and can design patterns be composed over that
;; stream in the way Brendan Fong's open causal theories suggest?

(require '[clojure.data.json :as json]
         '[clojure.java.io :as io]
         '[futon3c.xiang.turn-record :as tr]
         '[futon3c.xiang.turn-acts :as ta]
         '[futon3c.diagramprover.causal.open-theory :as ot]
         '[futon3c.diagramprover.causal.scm :as scm]
         '[futon3c.diagramprover.causal.dag :as dag])

;; ## 1. Four real turns
;;
;; These are four consecutive turns from 1 October 2026, copied unchanged
;; from the store. In the first, the agent asks Joe whether it should look
;; into something. In the second it offers him two numbered options. Joe
;; answers "1" in the third, and the agent's commit in that turn carries
;; option 1 out. The first question is never answered.

(def dir "test/futon3c/xiang/turn_acts_fixtures")

(defn read-json [name] (json/read-str (slurp (io/file dir name)) :key-fn keyword))

(def turns
  (vec (for [base ["turn-455grB" "turn-xJ4TAP" "turn-I7n9AZ" "turn-HUylGP"]]
         (let [record (read-json (str base ".json"))]
           {:record record
            :reading (read-json (str base ".json.analysis.json"))
            :reply (slurp (io/file dir (str base ".reply.txt")))
            :commits (ta/happened-commits (:happened_summary record))}))))

(for [{:keys [record]} turns]
  [(:turn_id record) (subs (:source_text record) 0 (min 70 (count (:source_text record))))])

;; ## 2. 大象 reads the agent's turn
;;
;; No model is involved. A reply is split into paragraphs, and each marked
;; paragraph is an act whose kind is fixed by its mark. Here is the agent's
;; reply in the second turn, the one with the numbered offer.

(for [{:keys [mark intent text]} (tr/reply-marks (:reply (nth turns 1)))]
  [mark intent (subs text 0 (min 60 (count text)))])

;; It is fast enough to run on every turn without anyone noticing.

(let [reply (:reply (nth turns 1)) t0 (System/nanoTime)]
  (dotimes [_ 100] (tr/reply-marks reply))
  (format "%.2f ms per reply" (/ (- (System/nanoTime) t0) 1e6 100)))

;; ## 3. The session as acts
;;
;; The operator's turns contribute the fragments 象 read (approve, ask,
;; report, ...); the agent's turns contribute 大象's marked paragraphs; the
;; commits listed in each turn's record contribute commit acts. Two links
;; are drawn only where there is explicit evidence: Joe's approval in turn
;; 393 accepts the offer of turn 392, and the commit in 393 carries that
;; accepted offer out. Anything subtler is left to the kernel.

(def acts (ta/session-acts turns))

(frequencies (map :kind acts))

(for [a acts :when (#{:offer :accept :commit} (:kind a))]
  (select-keys a [:id :kind :target :option :carries-out]))

;; ## 4. The marble run: what closed, what is still open
;;
;; The kernel (`futon3c.logic.xiang`, a small relational program in the
;; spirit of McCarthy's Elephant 2000) treats some acts as creating a
;; standing (an offer, a request, a reported problem) and others as closing
;; one. A standing that nothing has closed is an open port. After each turn:

(for [t ["claude-17-turn-391" "claude-17-turn-392" "claude-17-turn-393" "claude-17-turn-394"]]
  (let [{:keys [closed-this-turn still-open]} (ta/turn-ports acts t)]
    {:turn t
     :closed closed-this-turn
     :open (mapv (juxt :act :kind) still-open)}))

;; The offer of 392 closes in 393, when Joe accepts and the commit carries
;; it out. The agent's question of 391 (`…-391-r-3`, "shall I read how P11's
;; records get written?") is still open at the end. Nobody dropped it on
;; purpose; nothing kept it in view either. In this environment that is the
;; usual way work stalls, and it is the case we most want the logic to
;; catch.

;; ## 5. Design patterns as open causal theories
;;
;; Following Fong, each pattern is a small causal theory: variables, and a
;; mechanism (a Boolean structural equation) for each variable the pattern
;; owns. It is made open by an interface, the variables it is willing to
;; share. Theories compose by gluing along shared names. A name may be
;; shared only if every theory mentioning it lists it in its interface, and
;; only one theory may own it; otherwise gluing refuses, as data.
;;
;; Three small patterns from our own practice:
;;
;; *Answer your offers.* If the agent asks and nobody answers, the question
;; is stranded.
;;
;; *The token machine.* If a question is stranded and the reminder machine
;; is on, the holder is nudged.
;;
;; *The operator's burden.* If a question is stranded and nobody was nudged,
;; the operator ends up chasing it by hand ("tickling").

(def answer-your-offers
  (ot/theory :answer-your-offers
             {"unanswered" "not answered"
              "stranded" "asked and unanswered"}
             :inputs ["asked" "answered"]
             :interface [:stranded]))

(def token-machine
  (ot/theory :token-machine
             {"nudged" "stranded and machine-on"}
             :inputs ["machine-on"]
             :interface [:stranded :nudged]))

(def operator-burden
  (ot/theory :operator-burden
             {"left-alone" "not nudged"
              "joe-tickles" "stranded and left-alone"}
             :interface [:stranded :nudged]))

(def glued (ot/glue [answer-your-offers token-machine operator-burden]))

(select-keys glued [:glued? :owners :inputs])

;; The composite is an ordinary closed causal model. Its arrows:

(sort (map (juxt :from :to) (get-in glued [:dag :edges] (get-in glued [:dag :arrows]))))

;; Gluing refuses when a pattern tries to own a variable another pattern
;; already owns, here a careless pattern that calls a question stranded
;; whenever somebody was nudged:

(select-keys (ot/glue [answer-your-offers token-machine operator-burden
                       (ot/theory :careless {"stranded" "nudged"}
                                  :interface [:stranded :nudged])])
             [:glued? :reason :details])

;; And when a pattern reads a variable that another keeps private, here
;; `asked`, which answer-your-offers does not offer in its interface:

(:reason (ot/glue [answer-your-offers
                   (ot/theory :eager {"chased" "asked"} :inputs ["asked"]
                              :interface [:asked])]))

;; ## 6. Per turn: the acts supply the evidence
;;
;; The bridge between the two layers is a pair of observables computed from
;; the kernel after each turn: `asked` holds when the agent opened a
;; question earlier in the session, and `answered` when that question is no
;; longer open. For the question of turn 391, read after turn 394:

(defn observe [acts act-id turn-id]
  (let [open (set (map :act (:still-open (ta/turn-ports acts turn-id))))]
    {:asked true :answered (not (contains? open act-id))}))

(def seen (observe acts "claude-17-turn-391-r-3" "claude-17-turn-394"))

seen

;; What happened that night: the reminder machine did not exist yet, and
;; Joe had to come back to the question himself. Given that, ask the
;; counterfactual: had the machine been on, would he still have had to?

(scm/counterfactual (:dag glued)
                    {:evidence (merge seen {:machine-on false :joe-tickles true})
                     :intervention {:machine-on true}
                     :outcome :joe-tickles})

;; And for the offer of turn 392, which was answered, no pattern fires:

(let [seen-392 (observe acts "claude-17-turn-392-r-4" "claude-17-turn-394")]
  [seen-392
   (:answer (scm/counterfactual (:dag glued)
                                {:evidence (merge seen-392 {:machine-on false :joe-tickles false})
                                 :intervention {:machine-on true}
                                 :outcome :nudged}))])

;; ## 7. What this is, and is not, yet
;;
;; It runs: the readers, the kernel and the gluing above are the code that
;; now runs in the environment after every turn (the reminder machine went
;; live on 4 October and has already nudged agents). The patterns in §5
;; were written by hand for this page; the library holds about 1,200 more,
;; each an IF/HOWEVER/THEN/BECAUSE argument, and the next step is to compile
;; each into an open theory whose inputs are observables of the act stream,
;; so that the pattern selected for the current turn is checked as the turn
;; lands.
;;
;; It is finite and Boolean. Variables are identified by name, gluing is by
;; shared names (a pushout in the simplest sense), and mechanisms are
;; deterministic. Two things we would most like to get right with you:
;; mechanisms in a Markov category, so that 小象's uncertain readings enter
;; as distributions rather than guesses; and promises as combs, a process
;; with a hole the debtor fills later, which is what an open port in §4
;; already behaves like.
