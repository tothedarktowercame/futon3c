;; Authoritative offline verifier for the first-repair historical
;; revalidation of repair-ea1-78cd8a42...--attempt-001-pinned-selection-refused.
;; Fetches the REAL zai-1 audit job from Agency live and runs
;; futon3c.wm.run4-historical-verification/admit! with author codex-17,
;; reviewer zai-1 (historical authorship; no relabeling). Source HEAD read
;; fresh at invocation (expected bf10b3d9, descendant of first-commit
;; 0d1e203c). Writes only under this packet directory.
(ns run4-first-repair-verifier
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-historical-verification :as v]))

(def packet "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-pinned-selection-admission-2026-09-11")
(def verification-id "repair-ea1-78cd8a42-revalidation-20260911-v3")
(def review-job-id "invoke-1789156072050-20265-61d62304")
(def qualification-sha256 "629ae20a3530ce4cacd340b960e9fdd91e0d434f6cc1315e556da40dc67c289f")

(defn fetch-job [id]
  (let [raw (slurp (str "http://localhost:7070/api/alpha/invoke/jobs/" id))
        j (get (json/parse-string raw true) :job)]
    (when-not (and (= id (:job-id j)) (= "done" (:state j)))
      (throw (ex-info "Audit job not terminal" {:state (:state j)})))
    {:job-id (:job-id j) :state (:state j) :agent-id (:agent-id j)
     :result (:result j) :result-summary (:result-summary j)
     :terminal-message (:terminal-message j)
     :events (mapv #(update-keys % keyword) (:events j))
     :execution (update-keys (or (:execution j) {}) keyword)}))

(defn -main [& _]
  (let [head (str/trim (:out (clojure.java.shell/sh
                              "git" "-C" "/home/joe/code/futon2" "rev-parse" "HEAD")))
        _ (when-not (= "2bcefdd84c6036f0773874ab3dc12ba4881b8044" head)
            (throw (ex-info "Source HEAD moved; refresh input explicitly" {:head head})))
        record (v/admit! {:finding-root "/home/joe/code/futon2/data/wm-repair-obligations"
                          :qualification-root (str packet "/offline-evidence")
                          :qualification-source-root "/home/joe/code"
                          :output-root (str packet "/offline-verification")
                          :source-repo "/home/joe/code/futon2"
                          :finding-path "/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-ea1-78cd8a42e23392a5ce1412e49d181c3e7c3ac95aaa40d4f54c5c9d8ef3ae08eb--attempt-001-pinned-selection-refused.edn"
                          :finding-sha256 "07129f197c22d25731d15e07befa0593d1c25665c7e09f7a2ca9371de9681267"
                          :qualification-path (str packet "/offline-evidence/" verification-id ".qualification.edn")
                          :qualification-sha256 qualification-sha256
                          :expected-check-ids [:authenticated-run4-pin-enters-normal-gated-runner-path
                                               :pinned-mission-identity-retains-real-proposer-action]
                          :first-commit "0d1e203cdcf8528614b50c0a176df006a84fb329"
                          :last-commit head
                          :source-head head
                          :verification-id verification-id
                          :author "codex-17" :reviewer "zai-1"
                          :review-job-id review-job-id
                          :review-job-reader fetch-job})
        vf (io/file packet "offline-verification" (str verification-id ".verification.edn"))]
    (println {:admitted true
              :actors (:actors record)
              :review (select-keys (:review record) [:job-id :verdict])
              :verification-path (.getPath vf)
              :verification-sha256 (digest/sha256 (slurp vf))})))
