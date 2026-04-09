# MathDojo — Choose-Your-Own-Adventure Prelim Tutor

## Concept

The 489 APM proof files in `data/apm-informal-proofs/` are the knowledge
base. MathDojo is a navigation layer that lets you study them efficiently:

- **No LLM needed for browsing.** The topic index, technique graph, and
  difficulty ratings are pre-computed from the existing files.
- **LLM only for questions.** When you want clarification ("why does
  Cauchy-Schwarz work here but not there?"), you ask — and the question
  is answered in the context of the specific proof you're looking at.
- **Choose-your-own-adventure.** You pick the topic, the depth, and the
  path through related problems. The system doesn't quiz you — you drive.

## Architecture (token-efficient)

```
Pre-computed (zero tokens):
  proof files → topic index → technique graph → difficulty map

Interactive (tokens only when you ask):
  you pick a problem → read the proof (free) → ask a question (tokens)
  → answer appears in context of that proof → follow a connection (free)
```

## The Three Views

### 1. Topic Map (pre-computed)
Group problems by mathematical technique:
- Cauchy-Schwarz applications (a92J01, a93A03, a00J01, ...)
- Contour integration (a92J05, a92J06, a96J06, ...)
- Measure theory / DCT / Fubini (a93J03, a94A02, a03J04, ...)
- Schwarz lemma / max modulus (a92J04, a92J07, ...)
- Series convergence (a93J02, a95J03, ...)

### 2. Technique Graph (pre-computed)
Which techniques connect to each other:
- Cauchy-Schwarz → Hölder → Lp embedding → interpolation
- Schwarz lemma → max modulus → Liouville → entire functions
- Fubini → layer cake → distribution functions → weak Lp

### 3. Difficulty Ladder (pre-computed)
Within each technique cluster, order by difficulty:
- Entry: "apply Cauchy-Schwarz directly" (a92J01)
- Intermediate: "Cauchy-Schwarz + level sets + squeeze" (a93J04)
- Advanced: "Cauchy-Schwarz in Hilbert space + weak convergence" (a93A03)

## Usage in Emacs

```
M-x mathdojo-browse          ;; open topic map
M-x mathdojo-problem RET     ;; open a specific problem
M-x mathdojo-related          ;; show related problems from technique graph
M-x mathdojo-ask "why..."    ;; ask a question about the current problem
M-x mathdojo-next             ;; next problem on the difficulty ladder
M-x mathdojo-random           ;; surprise me
```

## Token Budget

- Browsing: 0 tokens (pre-computed index)
- Reading a proof: 0 tokens (local file)
- Asking a question: ~500-2000 tokens (one LLM call with proof as context)
- Following a connection: 0 tokens (pre-computed graph)

At 5 questions per day, ~5000-10000 tokens/day. That's <£1/day.
Compare: regenerating proofs burns ~£1/problem.

## Pilot

Pre-compute the topic index for the ~103 analysis problems we have.
Build the Emacs commands. Test with 3 study sessions.
