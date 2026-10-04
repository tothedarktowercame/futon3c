;; # 大象 live: reading a working conversation as acts, per turn
;;
;; Joseph Corneli, with Claude (claude-17). Futon stack, work in progress,
;; begun 4 October 2026. Everything below is evaluated when this page is
;; built; the outputs are what the code returned, not illustrations. The
;; source is `futon3c/notebooks/daxiang_live.clj`.
;;
;; The setting is a working environment in which a human operator (Joe) and
;; several AI agents hand work to one another in a chat-like REPL. Two kinds
;; of reading happen after every turn. The operator's turn is read by 小象
;; ("little elephant", a classical classifier, about 0.1 s) and then by 象, a
;; language model that confirms or corrects it. The agent's turn is read by
;; 大象 ("big elephant"), which is entirely classical: agents write each
;; paragraph of a reply under a mark that declares its intent (🈸 asks the
;; operator to decide something, ㊢ reports, ㊭ proposes, and so on), so
;; there is nothing to infer. The family name is a nod to McCarthy's
;; Elephant 2000 [1], a programming language whose primitives are speech
;; acts and the obligations they create; this is an attempt to reimplement
;; some of its ideas over a live conversation.
;;
;; No turn is read alone. 大象 itself looks only at one reply: its marked
;; paragraphs and the commits made during the turn (§2). What a reply
;; means for the work depends on what surrounds it, and that is joined on
;; when the session's acts are assembled (§4): the operator's turn it
;; answers, as 象 read it; up to 50 earlier turns of the same session, with
;; the offers and questions they left open; and two links between turns
;; that are stated only on explicit evidence (an approval accepts the offer
;; just made, a commit carries an accepted offer out). When the operator
;; opens a paragraph with `<mark>:`, the server also works out which of the
;; agent's recent paragraphs is being answered. Two pieces of context are
;; written down on every turn and not yet read: the bracketed target an
;; agent puts after each mark, and the design pattern retrieved for the
;; turn. §9 says what we will do with them.
;;
;; The question worked on here: can the stream of acts be given a logic
;; that says, after each turn, which obligations closed and which are still
;; open, and can design patterns be composed over that stream as open
;; causal models [2, 4]? Mistakes in the use of that work are ours.

(require '[clojure.data.json :as json]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[futon3c.xiang.turn-record :as tr]
         '[futon3c.xiang.turn-acts :as ta]
         '[futon3c.diagramprover.causal.open-theory :as ot]
         '[futon3c.diagramprover.causal.scm :as scm]
         '[futon3c.notebook.svg :as svg])

;; ## 1. How intents flow
;;
;; Each operator turn and each agent turn becomes acts; the kernel turns the
;; act history into open ports; the reminder machine (live since 4 October)
;; nudges whoever holds an open port and has gone idle. Everything settled
;; is written to the evidence store, futon1b.

{:notebook/html
 (svg/dag [[:op "operator turn"] [:xx "小象 (classical)"] [:x "象 (LLM)"]
           [:ag "agent turn"] [:dx "大象 (classical)"]
           [:acts "acts"] [:kernel "kernel"] [:ports "open ports"]
           [:machine "reminder machine"] [:store "futon1b"]]
          [[:op :xx] [:xx :x] [:x :acts] [:ag :dx] [:dx :acts]
           [:acts :kernel] [:kernel :ports] [:ports :machine] [:acts :store]
           [:machine :ag]]
          :fill {:x "#f6e3e3" :dx "#e3eef6" :xx "#e3eef6"})}

;; Only 象 is a language model, and it only reads the operator's words. The
;; agents' words are read by 大象, by rule.

;; ## 2. 大象
;;
;; This is the reader itself. The block below is tangled into
;; `src/futon3c/xiang/daxiang.clj`, and that file is what the running system
;; loads: the notebook is its source, and a test checks the two agree.
;; Of its two functions, the server calls `asks-operator?` after every
;; agent turn. `read-agent-turn` is called only on this page; it wraps the
;; adapter (`turn-acts`) that the server uses when it computes open ports.
;;
;; tangle: src/futon3c/xiang/daxiang.clj
(ns futon3c.xiang.daxiang
  "大象 (big elephant): the classical reader of agent turns.

   Tangled from notebooks/daxiang_live.clj; edit it there and run
   `-m futon3c.notebook.render tangle notebooks/daxiang_live.clj`.

   An agent writes each paragraph of a reply under a proforma mark that
   declares its intent, so reading it needs no model: the marks give the
   acts, `<mark>:` paragraphs point at what they answer, and a 🈸 or 🈯
   paragraph (not a pointer) means the turn ends waiting on the operator."
  (:require [futon3c.xiang.reply-target :as rt]
            [futon3c.xiang.turn-acts :as ta]
            [futon3c.xiang.turn-record :as tr]))

(defn asks-operator?
  "Whether reply TEXT ends its turn waiting on the operator: a paragraph
   opening 🈸 (ask-action) or 🈯 (clarify, including a question) that is
   not a `<mark>:` pointer answering the operator's own paragraph."
  [text]
  (boolean (and (string? text)
                (re-find #"(?m)^\s*(?:🈸|🈯)(?!\s*:)" text))))

(defn read-agent-turn
  "One agent turn, read classically. TURN is {:turn-id :agent-id
   :session-id :at :text :commits}. Returns {:acts [...] :skipped n
   :marks [{:mark :intent}] :answers [{:mark :index}] :asks-operator? b}:
   the kernel acts of its marked paragraphs and commits, every mark in
   order, the paragraphs that answer the operator's marked paragraphs,
   and whether it ends waiting on the operator."
  [{:keys [turn-id agent-id session-id at text commits]}]
  (let [record {:turn_id turn-id :agent_id agent-id :session_id session-id :created_at at}
        acts (ta/turn->acts record nil text (or commits []))]
    {:acts acts
     :skipped (:skipped (meta acts))
     :marks (mapv #(select-keys % [:mark :intent]) (tr/reply-marks text))
     :answers (mapv #(select-keys % [:mark :index]) (filter :pointer? (rt/operator-marks text)))
     :asks-operator? (asks-operator? text)}))

;; ## 3. Four real turns
;;
;; Four consecutive turns from 1 October 2026, copied unchanged from the
;; store. In the first, the agent asks Joe whether it should look into
;; something. In the second it offers him two numbered options. Joe answers
;; "1" in the third, and the agent's commit in that turn carries option 1
;; out. The first question was answered too ("🈸:yes" in the second turn),
;; but, as §4 shows, the record holds no act answering it.

(require '[futon3c.xiang.daxiang :as dx])

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

;; 大象 on the agent's reply in the second turn, the one with the numbered
;; offer:

(let [{:keys [record reply commits]} (nth turns 1)]
  (-> (dx/read-agent-turn {:turn-id (:turn_id record) :agent-id (:agent_id record)
                           :at (:created_at record) :text reply :commits commits})
      (update :acts #(mapv (fn [a] (select-keys a [:id :kind :option])) %))))

;; It is fast enough to run on every turn without anyone noticing:

(let [reply (:reply (nth turns 1)) t0 (System/nanoTime)]
  (dotimes [_ 100] (dx/read-agent-turn {:turn-id "t" :text reply}))
  (format "%.2f ms per reply" (/ (- (System/nanoTime) t0) 1e6 100)))

;; ## 4. The session as acts, and the marble run
;;
;; The operator's turns contribute the fragments 象 read; the agent's turns
;; contribute 大象's acts. Two links are drawn only where the evidence is
;; explicit: Joe's approval in turn 393 accepts the offer of turn 392, and
;; the commit in 393 carries that offer out. A third kind of link comes
;; from the operator's own pointers: when a paragraph uses `<mark>:` to
;; answer an agent paragraph, the server stores which one on the turn's
;; record (`:reply_to`), and `session-acts` reads that stored pointer as an
;; explicit answer. None of these four turns has one (see below).

(def acts (ta/session-acts turns))

(for [a acts :when (#{:offer :accept :commit} (:kind a))]
  (select-keys a [:id :kind :target :option :carries-out]))

;; The kernel (`futon3c.logic.xiang`, a small relational program) treats
;; some acts as creating a standing (an offer, a request, a reported
;; problem) and others as closing one. A standing nothing has closed is an
;; open port. After each turn:

(for [t ["claude-17-turn-391" "claude-17-turn-392" "claude-17-turn-393" "claude-17-turn-394"]]
  (let [{:keys [closed-this-turn still-open]} (ta/turn-ports acts t)]
    {:turn t :closed closed-this-turn :open (mapv (juxt :act :kind) still-open)}))

;; The offer of 392 closes in 393. The agent's question of 391
;; (`…-391-r-3`, "shall I read how P11's records get written?") is still
;; open at the end, but that is the record's fault, not the conversation's:
;; Joe answered "🈸:yes" in 392 and the agent did what it proposed. The
;; parser of the time did not accept that answer form (fixed in 393), and
;; 象 read the fragment as a report. The resolver now reads a pointer
;; written inside a sentence, and `session-acts` reads stored pointers,
;; but turn 392's record was written on 2026-10-01, three days before the
;; server began storing pointers, and records are not backfilled. So the
;; port stays open in this stored session. This is a fourth outcome a proposal can have besides accepted,
;; declined and unanswered: considered and accepted, but not recorded. A
;; kernel that reads only the record cannot tell it from neglect.

;; What the resolver returns for turn 392's text today, with turn 391's
;; reply as the only candidate. This is the resolver's output, not the
;; stored session: it is what the server would have stored had it been
;; running then.

(futon3c.xiang.reply-target/resolve-targets (:source_text (:record (nth turns 1)))
                                            [{:turn-id "391-reply" :origin "operator" :text (:reply (nth turns 0))}])

;; ## 5. Patterns as open causal models
;;
;; Following Fong [2], a pattern's causal structure is a small DAG, which
;; presents a causal theory, the free copy-discard category on its
;; variables and mechanisms; the pattern's Boolean structural equations are
;; a model of that theory in Set [2, §4.2]. Fong's causal theories are
;; closed. For open ones, in which some variables are inputs with no
;; mechanism, we follow Lorenz and Tull's open causal models [4, §5], which
;; they relate to decorated [3] and structured [5] cospans [4, Rem. 59].
;;
;; Our `glue` is not the binary pushout composition of [3]. It glues any
;; number of patterns at once along shared names, and it is partial: it
;; refuses when a shared name is missing from a sharer's interface, when a
;; variable would get two mechanisms, or when the result has a cycle. The
;; last two are the monogamy and acyclicity conditions that single out
;; copy-discard string diagrams among hypergraph diagrams [6, Defs 3.5–3.6].
;; Three small patterns from our own practice:

(def answer-your-offers
  (ot/theory :answer-your-offers
             {"unanswered" "not answered" "stranded" "asked and unanswered"}
             :inputs ["asked" "answered"] :interface [:stranded]))

(def token-machine
  (ot/theory :token-machine {"nudged" "stranded and machine-on"}
             :inputs ["machine-on"] :interface [:stranded :nudged]))

(def operator-burden
  (ot/theory :operator-burden
             {"left-alone" "not nudged" "joe-tickles" "stranded and left-alone"}
             :interface [:stranded :nudged]))

(def glued (ot/glue [answer-your-offers token-machine operator-burden]))

(select-keys glued [:glued? :owners :inputs])

(let [owners (:owners glued)
      colour {:answer-your-offers "#f4f2ea" :token-machine "#e3eef6" :operator-burden "#f6e3e3"}
      vars (sort (keys (get-in glued [:dag :variables])))]
  {:notebook/html
   (svg/dag vars (map (juxt :from :to) (get-in glued [:dag :arrows]))
            :col-width 140
            :fill (into {} (for [v vars] [v (colour (owners v) "#ffffff")]))
            :title "glued: answer-your-offers (beige), token-machine (blue), operator-burden (pink); white = input")})

;; Gluing refuses when two patterns would own one variable, or when a
;; theory reads a shared variable without listing it in its own interface:

[(select-keys (ot/glue [answer-your-offers token-machine operator-burden
                        (ot/theory :careless {"stranded" "nudged"} :interface [:stranded :nudged])])
              [:reason :details])
 (:reason (ot/glue [answer-your-offers
                    (ot/theory :eager {"chased" "asked"} :inputs ["asked"] :interface [:asked])]))]

;; ## 6. Per turn: the acts supply the evidence
;;
;; The bridge between the two layers is a pair of observables computed from
;; the kernel after each turn: `asked` holds when the agent opened a
;; question earlier, `answered` when that question is no longer open. Here
;; `asked` is set by hand, because we pass in the act id of a question we
;; already know was opened; only `answered` is read from the kernel.

(defn observe [acts act-id turn-id]
  (let [open (set (map :act (:still-open (ta/turn-ports acts turn-id))))]
    {:asked true :answered (not (contains? open act-id))}))

(def seen (observe acts "claude-17-turn-391-r-3" "claude-17-turn-394"))

seen

;; Take the record at face value: the port stayed open, with no reminder
;; machine, and suppose Joe had to come back to the question himself (§4
;; shows the open port is an artefact of the reading, so this premise is
;; illustrative). Had the machine been on, would he still have had to?

(select-keys (scm/counterfactual (:dag glued)
                                 {:evidence (merge seen {:machine-on false :joe-tickles true})
                                  :intervention {:machine-on true}
                                  :outcome :joe-tickles})
             [:method :answer])

;; For the offer that was answered, nothing fires. A caution on both
;; queries: the outcome does not depend on the exogenous noise once the
;; evidence is fixed, so abduction does no work and these are answers to
;; interventional questions (rung 2), not genuine counterfactuals.

(let [seen-392 (observe acts "claude-17-turn-392-r-4" "claude-17-turn-394")]
  [seen-392 (:answer (scm/counterfactual (:dag glued)
                                         {:evidence (merge seen-392 {:machine-on false :joe-tickles false})
                                          :intervention {:machine-on true}
                                          :outcome :nudged}))])

;; ## 7. A pattern cascade, compiled from the library
;;
;; The patterns in §5 were written for this page. The library holds about
;; 1,400 more, each an IF/HOWEVER/THEN/BECAUSE argument. They are linked in
;; two directions, and the two are not inverses of each other. A pattern's
;; `@why` points toward the general: the problems it answers, or the more
;; general pattern it rests on. That is its rationale, and the pattern's
;; own author writes it. A pattern's `@how` points toward the specific: the
;; named methods by which it is carried out. That is its practical side,
;; added later by an editor for the methods worth naming, and it is the
;; closer of the two to Alexander's smaller patterns that complete a
;; larger one. Counted from the library files:

(def library "/home/joe/code/futon3/library")

(def pattern-ids
  (set (for [f (file-seq (io/file library))
             :let [p (str f)]
             :when (str/ends-with? p ".flexiarg")]
         (subs p (inc (count library)) (- (count p) (count ".flexiarg"))))))

(defn directive-lines [directive]
  (for [id pattern-ids
        line (str/split-lines (slurp (str library "/" id ".flexiarg")))
        :when (str/starts-with? line (str "@" directive " "))
        :let [targets (remove str/blank?
                              (str/split (str/replace (subs line (+ 2 (count directive))) #"[\[\]]" "")
                                         #"\s+"))]]
    {:from id :targets targets :links? (every? pattern-ids targets)}))

(into {}
      (for [d ["why" "how"]
            :let [ls (directive-lines d)]]
        [d {:patterns-with-links (count (filter :links? ls))
            :links (reduce + (map (comp count :targets) (filter :links? ls)))
            :other-lines (count (remove :links? ls))}]))

;; So `@why` is the well-populated direction. Most `@how` lines are not
;; links yet: they hold a sentence of practical instruction (the
;; `:other-lines` above). The compilation below therefore uses `@why`
;; only. It is generic, the same for every pattern: a pattern is *unmet*
;; when its IF holds and its THEN does not, and a problem counts as
;; addressed when some pattern answering it has its THEN hold. Read
;; upward, the cascade says why an unmet pattern matters: which problems
;; above it go unanswered. `@how` would add the downward reading, what to
;; do about it (§9). Here is the cone above the card that has been active
;; in Joe's REPL this week, WR-26, read from the library files.

(defn flexiarg [id]
  (let [text (slurp (str library "/" id ".flexiarg"))
        section (fn [k] (some-> (re-find (re-pattern (str "(?s)\\+ " k ":\\s*\\n(.*?)\\n\\s*\\n")) text)
                                second str/trim (str/replace #"\s+" " ")))]
    {:id id
     :key (second (re-find #"/([a-z]+-?\d+)" id))
     :why (vec (some-> (re-find #"(?m)^@why \[?([^\]\n]*)" text) second (str/split #"\s+")))
     :if (section "IF") :then (section "THEN")}))

(defn cascade [root]
  (loop [todo [root] seen {}]
    (if-let [id (first todo)]
      (if (seen id) (recur (rest todo) seen)
          (let [p (flexiarg id)] (recur (concat (rest todo) (:why p)) (assoc seen id p))))
      seen)))

(def cone (cascade "war-room/wr-26-a-capability-switched-off-carries-its-re-arm-condition-in-writing-at-the-switch"))

(for [{:keys [key why if then]} (vals cone)]
  {:pattern key :answers (mapv #(second (re-find #"/([a-z]+-?\d+)" %)) why)
   :if (subs if 0 (min 90 (count if))) :then (subs then 0 (min 90 (count then)))})

;; Compiling: each pattern owns `k-unmet = k-if and not k-then`; a problem's
;; `k-then` is owned by a link theory saying that one of the patterns
;; answering it has its THEN hold. Leaves take their THEN from observation.

(defn pattern-theory [{:keys [key]} answered-by]
  (ot/theory (keyword key)
             {(str key "-open") (str "not " key "-then")
              (str key "-unmet") (str key "-if and " key "-open")}
             :inputs (cond-> [(str key "-if")] (empty? answered-by) (conj (str key "-then")))
             :interface [(keyword (str key "-then")) (keyword (str key "-unmet"))]))

;; The equation grammar has only two-input `or`, so a problem answered by
;; more than two patterns gets a chain of private intermediate variables.

(defn link-theory [{:keys [key]} children]
  (let [[t & more] (map #(str (:key %) "-then") children)
        mids (map #(str key "-then-or-" %) (range 1 (count more)))
        lhs (concat mids [(str key "-then")])]
    (ot/theory (keyword (str key "-answered-by"))
               (if (empty? more)
                 {(str key "-then") t}
                 (into {} (map (fn [v prev x] [v (str prev " or " x)])
                               lhs (cons t mids) more)))
               :interface (map keyword (cons (str key "-then") (cons t more))))))

(def answered-by
  (reduce (fn [m p] (reduce #(update %1 %2 (fnil conj []) p) m (:why p))) {} (vals cone)))

(def cascade-theory
  (ot/glue (concat (for [p (vals cone)] (pattern-theory p (answered-by (:id p))))
                   (for [[parent kids] answered-by] (link-theory (cone parent) kids)))))

(select-keys cascade-theory [:glued? :inputs])

{:notebook/html (svg/dag (sort (keys (get-in cascade-theory [:dag :variables])))
                         (map (juxt :from :to) (get-in cascade-theory [:dag :arrows]))
                         :col-width 130 :title "the WR-26 cone, compiled")}

;; ## 8. The cascade on a real event
;;
;; On 3 October Joe switched 象 off (it was switched back on at 22:55 UTC
;; the same day). The switch requires a written re-arm condition, and the
;; log holds it:

(def switch-off
  (first (filter #(re-find #" off " %)
                 (str/split-lines (slurp (str (System/getProperty "user.home")
                                              "/.emacs-graph/session-turn-analysis/xiang-off.log"))))))

switch-off

;; WR-26's THEN asks for more than a written condition: a countable one ("a
;; specified number of runs…"). A classical test for that, and the
;; observables for this event: a capability was switched off; the problems
;; above it are standing context.

(defn countable? [s] (boolean (re-find #"\b\d+\s+(?:runs?|turns?|times|cohorts?|days?|clicks?)\b" s)))

(def event {:wr-26-if true :wr-26-then (countable? switch-off)
            :wr-8-if true :r20-if true :wr-0-if true})

event

(let [d (:dag cascade-theory)]
  {:unmet-as-it-happened
   (into (sorted-map)
         (for [k ["wr-26" "wr-8" "r20" "wr-0"]]
           [k (:answer (scm/counterfactual d {:evidence event :intervention {}
                                              :outcome (keyword (str k "-unmet"))}))]))
   :had-the-condition-been-countable
   (into (sorted-map)
         (for [k ["wr-26" "wr-8" "r20" "wr-0"]]
           [k (:answer (scm/counterfactual d {:evidence event :intervention {:wr-26-then true}
                                              :outcome (keyword (str k "-unmet"))}))]))})

;; The first column has no intervention, so it is a reading of the record
;; rather than a counterfactual. As it happened, the condition was written
;; but not countable ("Joe asks
;; for 象 back"), so WR-26 is unmet and so, within this cone, is every
;; problem it answers. The counterfactual shows what a countable condition
;; would have changed. Two cautions: the problems above WR-26 have other
;; patterns answering them, outside this cone, which would change the
;; answer for them; and `countable?` is a first classical test, not a
;; verdict.

;; ## 9. What this is, and is not, yet
;;
;; The readers, the kernel and the reminder machine run in the environment
;; after every turn; `asks-operator?` in §2 is the running code. The
;; compilation in §7 is new and runs only here so far.
;;
;; Next steps, each with the check that will say whether it worked:
;;
;; 1. Use the operator's pointers as links. Done, 2026-10-04. The resolver
;; (`futon3c.xiang.reply-target`) now reads a `<mark>:` pointer inside a
;; sentence as well as at the start of a paragraph ("…and 🈸:yes"), though
;; not one in backticks or quotation marks. `session-acts` reads the
;; pointer the server stored on the record and never recomputes it. A
;; stored pointer names the agent turn by an id session records do not
;; carry, so it is matched to a reply paragraph by its mark and stored
;; excerpt. Check: the turn-acts tests close `…-391-r-3` in turn 392 when
;; the pointer is supplied, and keep it open in the stored session, whose
;; record predates the writer. Known limit: an unquoted sentence about the
;; notation ("the legend says 🈸: ask-action") also reads as a pointer.
;; A second form, `<mark> (target): …`, is also a pointer: there the mark
;; is the operator's own intent and the bracket text names the paragraph
;; answered, so it is matched by finding the bracket text in exactly one
;; paragraph of the agent's newest turn that has it.
;;
;; 2. Read the bracketed target. Agents write, after each mark, what the
;; paragraph is about; today that text is kept but not parsed. Resolve it
;; against the operator's fragments in the same way. Check: over a week of
;; replies, the share of marked paragraphs whose target resolves, and how
;; many open ports those links close.
;;
;; 3. Count the fourth outcome of §4. With 1 and 2 in place, re-run the
;; kernel over the stored sessions. Ports that close were answered but not
;; recorded; ports that stay open are the reminder machine's real work.
;; Check: the two counts, per week.
;;
;; 4. Run the cascade per turn. Each operator turn already arrives with a
;; retrieved pattern. Compile the cone above it as in §7 and evaluate it
;; against that turn's acts, starting with the three patterns of §5, whose
;; observables (`asked`, `answered`) the kernel already supplies. Check:
;; for each turn, the unmet patterns shown beside its open ports.
;;
;; 5. Use `@how` on the way down. When a pattern is unmet, show its `@how`:
;; the linked methods where they exist, the instruction sentence
;; otherwise. Check: an unmet pattern in step 4 comes with something to do.
;;
;; It is finite and Boolean. Variables are identified by name, gluing is by
;; shared names, and mechanisms are deterministic. Two places we would most
;; like to go further: mechanisms in a Markov category, so that 小象's
;; uncertain readings enter as distributions rather than guesses; and
;; promises treated as processes with a hole the debtor fills later. That
;; reading fits the kernel's open ports, but not its fuller notion of a
;; standing being in force, which has cancellation, reversal and two time
;; axes and is closer to a fluent of the event calculus.
;;
;; ## References
;;
;; [1] John McCarthy, "Elephant 2000: A Programming Language Based on
;; Speech Acts", Stanford draft, 1989 (revised 1993; HTML 1998),
;; http://www-formal.stanford.edu/jmc/elephant/elephant.html; abstract in
;; OOPSLA '07 Companion, pp. 723–724.
;;
;; [2] Brendan Fong, "Causal Theories: A Categorical Perspective on Bayesian
;; Networks", MSc thesis, University of Oxford, 2012; arXiv:1301.6201.
;;
;; [3] Brendan Fong, "Decorated Cospans", Theory and Applications of
;; Categories 30(33), 2015; arXiv:1502.00872.
;;
;; [4] Robin Lorenz and Sean Tull, "Causal models in string diagrams",
;; arXiv:2304.07638.
;;
;; [5] John C. Baez and Kenny Courser, "Structured cospans",
;; arXiv:1911.04630.
;;
;; [6] Tobias Fritz and Wendong Liang, "Free gs-monoidal categories and
;; free Markov categories", arXiv:2204.02284.
