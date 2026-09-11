;; Authoritative offline verifier for the Zai-staffed successor-v2-selection
;; historical revalidation. Fetches the REAL zai-1 Agency review job
;; (invoke-1789149846493-20228-9dfa3dda) and runs
;; futon3c.wm.run4-historical-verification/admit! with author zai-2,
;; reviewer zai-1. Source HEAD read fresh at invocation (810be2a9 verified
;; descendant of first-commit 8788443d). Writes only the new verification
;; artifact under this packet's offline-verification root.
(ns run4-zai-verifier
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [cheshire.core :as json]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-historical-verification :as v]))

(def packet "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-successor-v2-selection-zai-admission-2026-09-11")
(def verification-id "repair-successor-v2-selection-revalidation-zai-20260911-v1")
(def review-job-id "invoke-1789149846493-20228-9dfa3dda")
(def qualification-sha256 "655a867699f6fdd6ca789c903a4c7281e06066c3b20bcd387d45abc20703ea17")

(defn fetch-job [id]
  (let [raw (slurp (str "http://localhost:7070/api/alpha/invoke/jobs/" id))
        j (get (json/parse-string raw true) :job)]
    (when-not (and (= id (:job-id j)) (= "done" (:state j)))
      (throw (ex-info "Review job not terminal" {:state (:state j)})))
    ;; Keywordize only the fields the consumer validators read; events kept
    ;; as delivered so execution evidence derives from the real ledger.
    {:job-id (:job-id j) :state (:state j) :agent-id (:agent-id j)
     :result (:result j)
     :result-summary (:result-summary j)
     :terminal-message (:terminal-message j)
     :events (mapv #(update-keys % keyword) (:events j))
     :execution (update-keys (or (:execution j) {}) keyword)}))

(defn -main [& _]
  (let [head (str/trim (:out (clojure.java.shell/sh
                              "git" "-C" "/home/joe/code/futon2" "rev-parse" "HEAD")))
        _ (when-not (= "810be2a9a19d70b054d9ef7ceb43a2349b7a923d" head)
            (throw (ex-info "Source HEAD moved; refresh input explicitly" {:head head})))
        plan-path (str packet "/qualification-plan-zai.disabled.edn")
        record (v/admit! {:finding-root "/home/joe/code/futon2/data/wm-repair-obligations"
                          :qualification-root (str packet "/offline-evidence")
                          :qualification-source-root "/home/joe/code"
                          :output-root (str packet "/offline-verification")
                          :source-repo "/home/joe/code/futon2"
                          :finding-path "/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-run4-u88-production-successor-20260911-v2--attempt-001-untyped-failure.edn"
                          :finding-sha256 "b888bcdd80d014c072f60faaa28d912908e4dc7674edce2f50d1ca6b45fc6068"
                          :qualification-path (str packet "/offline-evidence/" verification-id ".qualification.edn")
                          :qualification-sha256 qualification-sha256
                          :expected-check-ids [:actual-stop-line-selection-controls
                                               :new-admission-and-replay-controls
                                               :exact-target-inspection-and-lock-controls]
                          :first-commit "8788443d7cf0c806261933e2c68009d84f57819d"
                          :last-commit head
                          :source-head head
                          :verification-id verification-id
                          :author "zai-2" :reviewer "zai-1"
                          :review-job-id review-job-id
                          :review-job-reader fetch-job})
        vf (io/file packet "offline-verification" (str verification-id ".verification.edn"))]
    (println {:admitted true
              :actors (:actors record)
              :review (select-keys (:review record) [:job-id :verdict])
              :verification-path (.getPath vf)
              :verification-sha256 (digest/sha256 (slurp vf))})))
