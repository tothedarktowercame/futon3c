;; Authoritative offline verifier for the initialization-close historical
;; revalidation of repair-initialization-8c13130f...-initialization-failed
;; (invalid close outcome / unknown-outcome; fix 0d1e203c, first-commit base
;; 810be2a9). Fetches the REAL zai-1 audit job from Agency live and runs
;; futon3c.wm.run4-historical-verification/admit! with author codex-17,
;; reviewer zai-1 (historical actors; no relabeling). Source HEAD read fresh
;; at invocation; must be a descendant of the fix and byte-identical pinned
;; sources. Writes only under this packet directory.
(ns run4-init-close-verifier
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-historical-verification :as v]))

(def packet "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-initialization-close-admission-2026-09-11")
(def verification-id "repair-initialization-8c13130f-revalidation-20260911-v1")
(def review-job-id "invoke-1789157036919-20272-b3acad58")
(def qualification-sha256 "48b522d5ada20351f5f6fdc95dfab39a86ed44cfccb5b51272884f763d0b4377")

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
                          :finding-path "/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-initialization-8c13130f-bed1-4eac-b2f3-261243c119dc-initialization-failed.edn"
                          :finding-sha256 "2e269db3cecd467bcdbf62c850896fc67cb0f7251ec80697822a98e1b2fbb588"
                          :qualification-path (str packet "/offline-evidence/" verification-id ".qualification.edn")
                          :qualification-sha256 qualification-sha256
                          :expected-check-ids [:authenticated-run4-pin-enters-normal-gated-runner-path]
                          :first-commit "810be2a9a19d70b054d9ef7ceb43a2349b7a923d"
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
