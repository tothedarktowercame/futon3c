# Excursion: E-typed-bells — type the bell channel; make ArSE the Q&A substrate

**Date:** 2026-06-11
**Status:** PROMOTED → `holes/missions/M-typed-bells.md` (2026-06-11). This file remains
the origin design sketch + full prior-art reading; the mission carries it through the
lifecycle (slices, gates, ARGUE tensions). IDENTIFY (design + prior-art grounding; not
built). Thinking captured at Joe's request, building on E-crossed-bells / the bell router.
**Repo:** futon3c — the bell path (`transport/http.clj` handle-bell), the conversation
graph (`agency/bell_router.clj`), and ArSE (`peripheral/arse.clj`, `agents/arse_work_queue.clj`,
`README-arse.md`).
**Spawned from:** the bell router (E-crossed-bells) gave us a *threaded* conversation
graph for free. Joe: bells are basically an **untyped** channel — *"rather than 'bell me'
we could say 'here is a pointer to my question, help me resolve it.'"* Typing the bells
makes the graph semantic and makes ArSE populate by construction.

## HEAD (one line)
**Type the bells.** A bell is currently one untyped imperative ("invoke B with text");
typing it recovers the **illocutionary force** (ask / answer / assert / challenge / …),
turning the conversation graph from *structural* (in-reply-to) into *semantic*. The
`:query`/`:answer` types back directly onto ArSE — so the live agent mesh becomes a
running instance of the **IATC** model from Corneli et al. 2017, with agents as the
conversants Turing imagined "convers[ing] with each other to sharpen their wits."

## Prior art — Corneli, Martin, Murray-Rust & Pease 2017 (the backbone)
*"Towards mathematical AI via a model of the content and process of mathematical Q&A
dialogues"* (`metameso.org/~joe/papers/corneli2017towards.pdf`). The paper's thesis is
exactly our lever: **the semi-structured Q&A format already exposes knowledge in a form
close to machine-reasonable** — Q&A is *both* a source **of** reasoning *and* a modality
**for** developing reasoning. It gives us four things we can lift almost directly:

1. **The mechanism — IAT (Inference Anchoring Theory).** Dialogue is modelled by
   anchoring each locution to its **illocutionary performative**. *That is "typing the
   bell."* The paper's IATC framework (IAT + Content) layers: **inferential structure**
   (`implies`), **reasoning tactics** (`goal`, `auxiliary`), and **heuristics**, with
   content nodes connected by performatives.
2. **The type vocabulary — for free.** IATC's performatives are exactly a bell-type
   taxonomy: **assert, query, challenge, agree, define, retract, suggest** (Fig. legend).
   No need to invent one — start here.
3. **Sub-typologies for `:query` and `:answer`.** Question types (Martin & Pease):
   *Conjecture 36% ("is it true that"), What-is-this 28%, Example 14%, Formula,
   Different-proof, Reference, Perplexed, Motivation*. Collaborative acts
   (Tausczik & Kittur): *provide-information, clarify-question, critique-answer,
   revise-answer, extend-answer*. These are the refinements `:query`/`:answer` can carry.
4. **The conversation graph IS IATC.** The paper's Figs 1–5 diagram a Q&A dialogue as a
   graph of typed performative nodes (Assert / Challenge / Question / Reform / Suggest /
   Agree) over content, with dialectical edges (`analogy`, `Reform`,
   `necessary-for-the-implication-to-hold`). Our `bell_router/graph` is the same object,
   one layer up: type the bells and the mesh *is* an IATC dialogue graph.

**The live twist.** In 2017 the paper modelled *existing human traces* (Mathoverflow).
The agent mesh now **generates the dialogues live** — so the futon stack is a place to
*instantiate* IATC on a running social machine, not just analyse a frozen corpus. This is
the Turing line the paper quotes (ref 41) made operational.

## Why typing beats the "promotion" model (from the previous excursion thinking)
Ambient-capture → mine/promote-to-ArSE is lossy and after-the-fact. **Typing makes the
Q&A case lossless and at-birth:** a `:query` bell with a `:ref` to an ArSE question *is*
that question routed to a helper; the `:answer` reply *is* its ArSE answer. No mining —
the agent *declared* the type. And **typing is the filter**: only `:query`/`:answer`
bells back onto ArSE; `:request`/`:fyi`/untyped stay ambient. So ArSE captures the Q&A
slice of normal coordination traffic, structured, because the bells say what they are.

## ArSE as the substrate it was built for
`README-arse.md`: ArSE is Q&A that becomes first-class evidence; unanswered questions are
**legible gaps**. ArSE already has a **work-queue** (agents pick up questions to answer).
A *directed* `:query` bell (→ B) is a *directed ArSE assignment*; a broadcast `:query` is
the open queue. So the bell mesh and ArSE's queue are one machine seen from two ends —
*"here's a pointer to my question, help me resolve it"* = `bell B {:type :query :ref Q-123}`,
framed to B as "help resolve ArSE Q-123: …", answer feeding straight back.

## Design shape (and the restraint the paper itself warns about)
- Bell payload gains `:type` (+ optional `:ref`), exactly as `in-reply-to` was added.
  **Backward-compatible:** untyped → `:request` (today's behavior).
- The `bell_router` edge carries `:type` → a *typed* conversation graph; the surface
  header shows it ("Type: query — help resolve ArSE Q-123").
- **Start with the IAT seven** (assert/query/challenge/agree/define/retract/suggest);
  add the question/answer sub-typologies only when the traffic asks for them. The paper's
  own §4 caution: argument-relation structure is hard and "may have to be laboriously
  hand-coded" — so let the live traffic reveal which types/relations earn their keep
  (trust-the-method) rather than pre-building IATC in full.

## Evaluation idea (also from the paper)
**SEMATCH** (paper §4): present a system a sample of questions and answers, *don't* say
which matches which, and have it recover the pairing (Q→A / A→Q). With a typed-bell
corpus we'd have ground-truthed pairs (`:bellback-of`) for free — a ready benchmark for
"does the mesh understand its own coordination?"

## Relations
- **E-crossed-bells** / `bell_router.clj` (parent) — threading gave the *graph*; typing
  gives the *semantics*. Same object, +illocutionary force.
- **ArSE** (`README-arse.md`) — the `:query`/`:answer` substrate; the work-queue unifies
  with directed `:query` bells.
- **Corneli et al. 2017** (`metameso.org/~joe/papers/corneli2017towards.pdf`) — the IATC
  model; key refs: IAT (Budzynska et al. ref 5), the Lakatos game / Lakatos-style
  collaborative argumentation (Pease et al. ref 32), the Mathoverflow question typology
  (Martin & Pease ref 26), social machines (Berners-Lee ref 4).
- **Surface contracts** (futon3c CLAUDE.md) — a bell-type is a richer surface contract
  (illocutionary force is accurate environment info, not capability restriction).
- The ambient-corpus + promotion idea (prior turn) — survives for the *non*-Q&A types;
  typing makes the Q&A slice first-class instead of mined.
