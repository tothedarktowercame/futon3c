;; Initialization-close qualification producer for
;; repair-initialization-8c13130f...-initialization-failed
;; (invalid close outcome / unknown-outcome; fixed by 0d1e203c, zai-1 review
;; 7cea2e60). Single check: the exact closure-regression var
;; authenticated-run4-pin-enters-normal-gated-runner-path, selected via
;; clojure.test/test-vars (v3 mechanics; no full namespace/suite).
;; Sources pinned at current bytes (futon2 HEAD 2bcefdd8; fix 0d1e203c is an
;; ancestor). produce! runs ONCE; before/after source audit.
(ns run4-init-close-qualification-producer
  (:require [clojure.java.io :as io]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-historical-qualification :as qualification]))

(def sources
  ["/home/joe/code/futon2/data/wm-repair-obligations/findings/repair-initialization-8c13130f-bed1-4eac-b2f3-261243c119dc-initialization-failed.edn"
   "/home/joe/code/futon2/src/futon2/aif/full_loop_runner.clj"
   "/home/joe/code/futon2/test/futon2/aif/full_loop_runner_test.clj"])

(def verification-id "repair-initialization-8c13130f-revalidation-20260911-v1")
(def packet "/home/joe/code/futon3c/holes/labs/wm-contract/runs/RUN4-initialization-close-admission-2026-09-11")

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
       " [#'t/authenticated-run4-pin-enters-normal-gated-runner-path]))"
       " (println :summary @summary)"
       " (System/exit (if (pos? (+ (:fail @summary) (:error @summary))) 1 0)))\""))

(defn audit []
  (into {} (map (juxt identity #(digest/sha256 (slurp %))) sources)))

(defn -main [& _]
  (let [before (audit)
        plan-file (io/file packet "qualification-plan.disabled.edn")
        plan {:schema :wm/historical-qualification-plan-v1
              :verification-id verification-id
              :repair-id "repair-initialization-8c13130f-bed1-4eac-b2f3-261243c119dc-initialization-failed"
              :sources (mapv (fn [p] {:path p :sha256 (before p)}) sources)
              :checks [{:id :authenticated-run4-pin-enters-normal-gated-runner-path
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
