(ns futon3c.social.coordination-ledger-model-test
  "Logic-model for mesh-edge coverage before implementation.

  Invariant: each invoke-with-edge! call records exactly one :invoke edge and
  exactly one :invoke-result edge for the same from/to pair. Missing callers are
  normalized to unknown, never dropped."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn- normalize-from
  [from]
  (let [s (some-> from str str/trim)]
    (if (str/blank? s) "unknown" s)))

(defn- model-run
  [calls]
  (mapcat (fn [{:keys [from to surface ok?]}]
            (let [from* (normalize-from from)]
              [{:edge/kind :invoke
                :edge/from from*
                :edge/to to
                :edge/surface surface}
               {:edge/kind :invoke-result
                :edge/from from*
                :edge/to to
                :edge/surface surface
                :edge/ok? (boolean ok?)}]))
          calls))

(defn- edge-count
  [edges kind from to]
  (count (filter #(and (= kind (:edge/kind %))
                       (= from (:edge/from %))
                       (= to (:edge/to %)))
                 edges)))

(deftest every-call-produces-one-invoke-and-one-result-edge
  (let [calls [{:from "claude-6" :to "codex-1" :surface "dispatch" :ok? true}
               {:from "irc-joe" :to "claude-1" :surface "irc" :ok? false}]
        edges (vec (model-run calls))]
    (doseq [{:keys [from to]} calls]
      (is (= 1 (edge-count edges :invoke from to)))
      (is (= 1 (edge-count edges :invoke-result from to))))))

(deftest nil-or-blank-caller-is-unknown-not-dropped
  (let [calls [{:from nil :to "codex-1" :surface "whistle" :ok? true}
               {:from "   " :to "claude-1" :surface "dispatch" :ok? true}]
        edges (vec (model-run calls))]
    (is (= 4 (count edges)))
    (is (= 1 (edge-count edges :invoke "unknown" "codex-1")))
    (is (= 1 (edge-count edges :invoke-result "unknown" "codex-1")))
    (is (= 1 (edge-count edges :invoke "unknown" "claude-1")))
    (is (= 1 (edge-count edges :invoke-result "unknown" "claude-1")))))
