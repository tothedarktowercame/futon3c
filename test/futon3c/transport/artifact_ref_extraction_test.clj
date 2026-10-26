(ns futon3c.transport.artifact-ref-extraction-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.transport.http :as http]))

;; Retained source sha256:
;; af4cc4f5ba4d2f901edaef96d0559b9b6749207f20f8328c4e87aaba409ee413
(def ^:private witness-reply
  "Inspection result: the selected construction's twelve acceptance criteria are all mechanically satisfied at the base head — they are quoted spans of the mission record's title/status/owner/cross-ref block, every one present verbatim in `holes/missions/M-recommendation-bindings.md` at HEAD `09a7cb8d` (grep-verified all 12), and the operational produces were already landed by that very commit (`:binds-to` populated: 14 occurrences in `THE-STACK.aif.edn`, 5 in `leaf-invariants.aif.edn`; `scripts/check_bindings.clj` added). Working tree is clean, HEAD equals the stated base head, mission sha256 matches the mission record (`4aae9e60…`). Per requirement 7: no cosmetic commit, no re-presentation of base HEAD as a new DONE artifact.

FULL_LOOP_FEATURE_CARD: {:built \"all 12 cascade completion checks already satisfied at base head 09a7cb8d\" :want-coverage \"each check names a mission-record span present verbatim at HEAD; :binds-to landed (14+5)\" :matches-intent? true :things-to-try [\"grep -c :binds-to holes/stories/THE-STACK.aif.edn -> 14\"] :proof-ref \"09a7cb8d3468031ed3fa2beb6db21c602617a572\"}

Validations performed:
- `git rev-parse HEAD` = `09a7cb8d3468031ed3fa2beb6db21c602617a572` (equals target base head)
- `sha256sum holes/missions/M-recommendation-bindings.md` = `4aae9e60e48a25cd5134250c6fb99580038fc8cca83a8c1e290a13cf6111ff80` (matches mission record)
- All 12 completion-check quotings grep-verified present in the mission record at HEAD (title, status, owner, and each of the nine cross-ref lines)
- `grep -c :binds-to` → `THE-STACK.aif.edn` 14, `leaf-invariants.aif.edn` 5; `wc -l` leaf-invariants 224 lines
- `git status --short` clean (no unrelated dirty files; no commit made)

Patterns enacted (already in force at base head, not newly by this attempt): `contracts/resolve-pointers-at-head` (steps 0/5/7), `futon-theory/mission-lifecycle` (1), `agent/handoff-preserves-context` (2), `agent/evidence-over-assertion` (3/4/6/8/12), `measurement/warrant-travels-with-the-number` (10/11) — the compiled cascade f1bd2242…/13c0068d… whose clauses I authored under requisition 1899a1d3.

This reports only the selected construction's existing evidence; the mission itself remains open (7 retained open holes: `:serves-spine` annotations, web-surface live counts, the \"Close S6\" demo, criteria 8–11 checks, and three DERIVE-lifecycle holes) — the runtime `:not-executed` effect and those holes are not claimed closed.

FULL_LOOP_AUTHOR: REFUSE :already-satisfied-at-base-head")

(def ^:private full-sha "09a7cb8d3468031ed3fa2beb6db21c602617a572")
(def ^:private artifact-ref #'http/first-artifact-ref)

(deftest artifact-reference-priority-is-independent-of-path-and-sha-order
  (testing "retained R24 witness prefers its full commit sha over earlier paths and short shas"
    (is (= full-sha (artifact-ref witness-reply))))
  (testing "sha-first input retains the same answer"
    (is (= full-sha
           (artifact-ref (str "Proof " full-sha
                              " is recorded in holes/missions/M-recommendation-bindings.md.")))))
  (testing "moving the proof-ref sentence before the path sentence does not decide the result"
    (let [moved (str "The full proof ref is " full-sha ".\n" witness-reply)]
      (is (= (artifact-ref witness-reply) (artifact-ref moved))))))

(deftest artifact-reference-kinds-retain-their-own-behaviour
  (testing "path-only and no-reference replies"
    (is (= "/missions/M-recommendation-bindings.md"
           (artifact-ref "See /missions/M-recommendation-bindings.md.")))
    (is (nil? (artifact-ref "No artifact was produced."))))
  (testing "a PR reference still takes priority over a sha"
    (is (= "https://github.com/example/project/pull/17"
           (artifact-ref
            (str "Review https://github.com/example/project/pull/17 before " full-sha ".")))))
  (testing "a full sha beats earlier short shas, while short-only input keeps its first sha"
    (is (= full-sha
           (artifact-ref (str "Started at 09a7cb8d and finished at " full-sha "."))))
    (is (= "09a7cb8d"
           (artifact-ref "Compare 09a7cb8d with 1234567.")))))

(deftest frontiermath-local-reference-remains-the-pre-filter
  (is (= "mfuton/data/frontiermath-local/FM-543/answer.md"
         (artifact-ref
          (str "Commit " full-sha
               " records mfuton/data/frontiermath-local/FM-543/answer.md.")))))
