;; First-repair qualification producer v3 for
;; repair-ea1-78cd8a42...--attempt-001-pinned-selection-refused.
;; v1 used a cognitect -v pattern that selected the wrong population; v2 used
;; a nonexistent summarize-results var. v3 selects the two focused vars via
;; clojure.test/test-vars with an explicit report counter (public API only).
;; Both earlier outputs are retained untouched as receipts. Sources pinned at
;; current bytes (futon2 HEAD bf10b3d9). produce! runs ONCE per id.
(ns run4-first-repair-qualification-producer
  (:require [clojure.java.io :as io]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-historical-qualification :as qualification]))

(def sources
  ["/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-ea1-78cd8a42e23392a5ce1412e49d181c3e7c3ac95aaa40d4f54c5c9d8ef3ae08eb--attempt-001-pinned-selection-refused.edn"
   "/home/joe/code/futon2/src/futon2/aif/full_loop_runner.clj"
   "/home/joe/code/futon2/test/futon2/aif/full_loop_runner_test.clj"])

(def verification-id "repair-ea1-78cd8a42-revalidation-20260911-v3")
(def packet "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-repair-pinned-selection-admission-2026-09-11")

(def var-runner
  (str "cd /home/joe/code/futon2 && clojure -M:test -e \""
       "(require '[clojure.test :as ct]"
       " '[futon2.aif.full-loop-runner-test :as t])"
       " (let [summary (atom {:pass 0 :fail 0 :error 0})]"
       " (with-redefs [ct/report (fn [m] (case (:type m)"
       " :pass (swap! summary update :pass inc)"
       " :fail (swap! summary update :fail inc)"
       " :error (swap! summary update :error inc) nil))]"
       " (ct/test-vars"
       " [#'t/authenticated-run4-pin-enters-normal-gated-runner-path"
       " #'t/pinned-mission-identity-retains-real-proposer-action]))"
       " (println :summary @summary)"
       " (System/exit (if (pos? (+ (:fail @summary) (:error @summary))) 1 0)))\""))

(defn audit []
  (into {} (map (juxt identity #(digest/sha256 (slurp %))) sources)))

(defn -main [& _]
  (let [before (audit)
        plan-file (io/file packet "qualification-plan-v3.disabled.edn")
        plan {:schema :wm/historical-qualification-plan-v1
              :verification-id verification-id
              :repair-id "repair-ea1-78cd8a42e23392a5ce1412e49d181c3e7c3ac95aaa40d4f54c5c9d8ef3ae08eb--attempt-001-pinned-selection-refused"
              :sources (mapv (fn [p] {:path p :sha256 (before p)}) sources)
              :checks [{:id :authenticated-run4-pin-enters-normal-gated-runner-path
                        :argv ["bash" "-lc" var-runner]
                        :timeout-ms 300000}
                       {:id :pinned-mission-identity-retains-real-proposer-action
                        :argv ["bash" "-lc" var-runner]
                        :timeout-ms 300000}]}
        _ (spit plan-file (str (pr-str plan) "\n"))
        _ (qualification/produce! {:source-root "/home/joe/code"
                                   :output-root (str packet "/offline-evidence")
                                   :manifest-path (.getPath plan-file)
                                   :manifest-sha256 (digest/sha256 (slurp plan-file))})
        after (audit)
        drift (filter (fn [[p h]] (not= h (after p))) before)
        qfile (io/file packet "offline-evidence" (str verification-id ".qualification.edn"))]
    (println {:verification-id verification-id
              :source-drift (vec (map first drift))
              :qualification-path (.getPath qfile)
              :qualification-sha256 (digest/sha256 (slurp qfile))})))
