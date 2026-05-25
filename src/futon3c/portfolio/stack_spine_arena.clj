(ns futon3c.portfolio.stack-spine-arena
  "Stack-spine arena for Portfolio Inference — the leaf-registry-driven
   candidate-action source that PI consumes when observation-source is
   `the-stack` (per WM agenda wm.close-s6.v1, per claude-1 inhabitation
   cycle on cg-fb78973a-d2a4-496c-b8ae-5df5a6180676).

   Verbatim from the WM Close-S6 recommendation prose
   (`/api/alpha/aif-stack/live` :reading :next-move :feeding-input):

     'PI's sensory channels read from futon1a. The 16 .aif.edn files are
      EDN — they can be ingested directly. For first-step input: feed the
      leaf-registry's :next-move column to PI as candidate actions, with
      leaf-load-bearing-conflict-weight (C1=7, C2=4, C3=4, C4=3) as
      candidate utility weighting.'

   This file is v0: it lays the integration shape; the leaf-registry
   ingestion + per-conflict utility weighting are TODOs to be implemented
   in subsequent cycles (or via codex pickup of this branch).

   When complete, `stack-spine-actions` will return a richer candidate set
   for `:portfolio.policy/expected-free-energy` to evaluate, so that PI's
   ranked-actions can name concrete moves like
   'step-PI-with-leaf-input', 'land-candidate-queue-upsampling',
   'build-falsifiability-monitor', etc. — instead of the generic
   :wait / :review / :consolidate / :upvote / :work-on set."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def conflict-weights
  "Per-conflict utility weights from THE-STACK.aif.edn :stack-conflicts.
   Higher weight = more load-bearing conflict; actions that dis-bite a
   higher-weight conflict get higher utility in EFE computation."
  {:C1 7
   :C2 4
   :C3 4
   :C4 3})

(def the-stack-aif-edn-path
  "Cached structural prior; source of the spine-node + leaf-registry
   topology."
  "/home/joe/code/futon5a/holes/stories/THE-STACK.aif.edn")

(defn- read-the-stack
  "Read the cached prior; safe across missing-file / unreadable cases."
  []
  (try
    (-> the-stack-aif-edn-path slurp edn/read-string)
    (catch Throwable _ nil)))

(defn stack-spine-actions
  "Return the leaf-registry's :next-move candidates as PI action records.

   v0 stub: returns an empty seq + an `:incomplete? true` flag.  Future
   cycles will:
   - parse the 16 leaf .aif.edn files
   - extract :next-move + :conflict-id from each leaf
   - compute per-action utility = conflict-weights[:conflict-id]
   - return action records matching the portfolio.policy expected shape."
  []
  (when-let [stack (read-the-stack)]
    {:actions []
     :source the-stack-aif-edn-path
     :stack-loaded? true
     :incomplete? true
     :todo "v0 stub; subsequent cycle parses leaf-registry into typed actions"
     :conflict-weights conflict-weights}))

(defn applies?
  "True when PI's observation-source argument selects the stack-spine arena.

   Called by `:portfolio.policy` at policy-selection time to choose
   between the default `portfolio-arena` (generic action set) and the
   stack-spine extension."
  [observation-source]
  (= "the-stack" (str observation-source)))
