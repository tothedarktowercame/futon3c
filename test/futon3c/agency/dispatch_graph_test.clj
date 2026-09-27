(ns futon3c.agency.dispatch-graph-test
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.dispatch-graph :as graph]))
(def fixture (json/read-str (slurp "holes/labs/M-象-2000/P3-2-dispatch-fixture.json") :key-fn keyword))
(def target "invoke-1790546585343-25693-c50aba00")
(def parent "invoke-1790539003417-25635-c9c4175e")
(def es (graph/edges fixture))
(defn job [id] (first (filter #(= id (:job-id %)) (:jobs fixture))))
(defn moved [at jobs]
  (with-meta (mapv #(if (= target (:job-id %)) (assoc % :at at) %) es)
    (assoc (meta es) :jobs jobs)))

(deftest real-dispatch-direction-and-unattributed-origin
  (let [answer (graph/upstream es target) first-edge (first (:path answer))]
    (is (= :bounded-reconstruction (:basis answer)))
    (is (= ["claude-17" "codex-5"] ((juxt :from :to) first-edge)))
    (is (= :unattributed (:status answer)))
    (is (= :no-running-caller-job (:reason answer))))
  (let [from "2026-09-27T19:00:00Z" to (:as-of fixture)]
    (is (= #{parent} (set (map :job-id (graph/dispatched-to es "claude-17" from to)))))
    (is (= 6 (count (graph/dispatched-by es "claude-17" from to))))))

(deftest one-containing-job-then-shifted-outside
  ;; Positive case is an unmodified real codex-4 -> p5-hash-probe edge.
  (let [child-id "invoke-1790539177774-25638-6b49179b"
        cause-id "invoke-1790538588909-25630-83088148"
        p (job cause-id)
        before (str (.minusNanos (java.time.Instant/parse (:started-at p)) 1))
        valid (graph/upstream es child-id)
        changed (with-meta (mapv #(if (= child-id (:job-id %)) (assoc % :at before) %) es) (meta es))
        invalid (graph/upstream changed child-id)]
    (is (= cause-id (:cause-job-id (first (:path valid)))))
    (is (= 2 (count (:path valid))))
    (is (= :unattributed (:status invalid)))
    (is (nil? (:cause-job-id (first (:path invalid)))))
    (is (= 1 (count (:path invalid))))))

(deftest overlapping-jobs-are-ambiguous
  (let [p (job parent) copy (assoc p :job-id "synthetic-overlap")
        answer (graph/upstream (moved (:started-at p) [p copy]) target)]
    (is (= :ambiguous (:status answer)))
    (is (= #{parent "synthetic-overlap"} (set (:candidates answer))))))

(deftest blank-caller-survives-normalization
  (let [rows (mapv (fn [r]
                     (let [raw (:evidence/body r) b (if (string? raw) (edn/read-string raw) raw)]
                       (if (= target (:edge/id b))
                         (assoc r :evidence/body (assoc b :edge/from "  ")) r))) (:evidence fixture))
        normalized (graph/edges (assoc fixture :evidence rows))
        answer (graph/upstream normalized target)]
    (is (= (count es) (count normalized)))
    (is (= "unknown" (:from (first (:path answer)))))
    (is (= :unknown-caller (:reason answer)))))

(deftest cycle-guard
  (let [edge (first (filter #(= target (:job-id %)) es))
        synthetic (assoc (job target) :agent-id "claude-17"
                         :started-at (:at edge) :finished-at (:at edge))
        answer (graph/upstream (with-meta es (assoc (meta es) :jobs [synthetic])) target)]
    (is (= :cycle (:status answer)))
    (is (= 1 (count (:path answer))))))

(deftest missing-metadata-and-missing-terminal-time-do-not-invent-causes
  (is (= :unattributed (:status (graph/upstream (with-meta es nil) target))))
  (let [p (assoc (job parent) :finished-at nil :state "done")]
    (is (= :unattributed (:status (graph/upstream (moved (:started-at p) [p]) target))))))

(deftest result-is-not-a-second-dispatch-and-false-is-preserved
  (let [r (first (:evidence fixture)) raw (:evidence/body r)
        b (if (string? raw) (edn/read-string raw) raw)
        result (assoc r :evidence/id "synthetic-result"
                        :evidence/body (assoc b :edge/kind :invoke-result :edge/ok? false))
        normalized (graph/edges (update fixture :evidence conj result))]
    (is (= (count es) (count normalized)))
    (is (false? (:ok? (first (filter #(= (:edge/id b) (:job-id %)) normalized)))))))
