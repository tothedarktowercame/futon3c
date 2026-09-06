(ns futon3c.apm.live-preflight-runtime-test
  (:require [clojure.java.shell :as shell]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.live-preflight-runtime :as sut]))

(deftest terminal-job-and-fenced-edn-are-normalized
  (let [report {:command-own-exit 0 :mutations []}
        delivery {:terminal-job-id "j1" :delivery-status "delivery-failed"
                  :inbox-file-created? false
                  :registered-push-performed? false
                  :polling-available? true}
        terminal (sut/job->terminal
                  {:job {:job-id "j1" :agent-id "f19-proctor" :state "failed"
                         :terminal-code "invoke-error"
                         :terminal-message "wall-clock-budget"
                         :invocation/model "gpt-5.6-sol"
                         :trace/delivery-observation delivery
                         :result (str "```edn\n" (pr-str report) "\n```")}})]
    (is (= :failed (:state terminal)))
    (is (= :invoke-error (:terminal-code terminal)))
    (is (= "wall-clock-budget" (:terminal-message terminal)))
    (is (= report (:report terminal)))
    (is (= "gpt-5.6-sol" (:invocation/model terminal)))
    (is (= delivery (:trace/delivery-observation terminal)))))

(deftest non-edn-result-is-not-evidence
  (is (nil? (sut/parse-report "I think it passed"))))

(deftest invalid-edn-retains-reader-diagnostic
  (let [result (sut/parse-report-diagnostic
                "{:lane \"challenge\" :ran-empty :memory-ids []}")]
    (is (false? (:ok result)))
    (is (= :report-edn-lint-failed (:error/code result)))
    (is (= 3 (:linter/exit result)))
    (is (re-find #"missing value for key" (:error/message result)))
    (is (re-find #"1:" (:error/message result)))))

(deftest parseable-report-does-not-need-the-linter
  ;; The normalization ladder (2026-09-06) accepts by parsing, not by
  ;; linting; clj-kondo is now diagnostics-only for total failures.
  (with-redefs [shell/sh
                (fn [& _] (throw (java.io.IOException. "missing clj-kondo")))]
    (let [result (sut/parse-report-diagnostic "{:ok true}")]
      (is (true? (:ok result)))
      (is (= {:ok true} (:report result))))))

(deftest linter-unavailability-on-unparseable-text-fails-closed
  (with-redefs [shell/sh
                (fn [& _] (throw (java.io.IOException. "missing clj-kondo")))]
    (let [result (sut/parse-report-diagnostic "I think it passed")]
      (is (false? (:ok result)))
      (is (= :report-edn-linter-unavailable (:error/code result))))))

(deftest prose-wrapped-edn-report-is-normalized
  ;; The f172 shape (2026-09-06): markdown narrative around the map. The
  ;; frame voided on :report-edn-lint-failed with a finished proof inside.
  (let [result (sut/parse-report-diagnostic
                (str "**Status:** complete\n\n"
                     "Workspace: /home/joe/code/apm-frames/f172-b00J02-student\n\n"
                     "{:command-own-exit 0 :outcome :proved :mutations []}\n\n"
                     "All checks passed."))]
    (is (true? (:ok result)))
    (is (= {:command-own-exit 0 :outcome :proved :mutations []}
           (:report result)))
    (is (= {:route :embedded-map :syntax :edn}
           (:report/normalization result)))))

(deftest json-fenced-report-is-converted-to-edn-shape
  (let [result (sut/parse-report-diagnostic
                "```json\n{\"command-own-exit\": 0, \"mutations\": []}\n```")]
    (is (true? (:ok result)))
    (is (= {:command-own-exit 0 :mutations []} (:report result)))
    (is (= {:route :json-fence :syntax :json}
           (:report/normalization result)))))

(deftest report-fence-wins-over-other-fenced-blocks
  ;; Two fences used to make the parser return nil (submission-missing).
  (let [result (sut/parse-report-diagnostic
                (str "The proof:\n```lean\ntheorem t : True := trivial\n```\n"
                     "Report:\n```edn\n{:command-own-exit 0}\n```"))]
    (is (true? (:ok result)))
    (is (= {:command-own-exit 0} (:report result)))
    (is (= :edn-fence (get-in result [:report/normalization :route])))))

(deftest whole-text-json-report-is-accepted
  (let [result (sut/parse-report-diagnostic
                "{\"outcome\": \"proved\", \"lean\": {\"exit\": 0}}")]
    (is (true? (:ok result)))
    (is (= {:outcome "proved" :lean {:exit 0}} (:report result)))
    (is (= :json (get-in result [:report/normalization :syntax])))))

(deftest lean-brace-debris-is-not-mistaken-for-a-report
  ;; Prose quoting Lean anonymous-constructor/set braces parses as an EDN
  ;; map of symbols; a report must carry keyword keys.
  (let [result (sut/parse-report-diagnostic
                "The bijection {toSub fromSub} closes the goal; no report was produced.")]
    (is (false? (:ok result)))))

(deftest last-embedded-map-wins-when-several-parse
  (let [result (sut/parse-report-diagnostic
                "attempt one {:draft 1} superseded by {:command-own-exit 0}")]
    (is (true? (:ok result)))
    (is (= {:command-own-exit 0} (:report result)))))

(deftest generic-dispatch-state-rehydrates-without-rewriting-authority
  (let [request {:dispatch/id "dispatch" :problem-id "m94A03"}
        ticket {:job-id "job" :dispatch/id "dispatch"}
        state {:state/type :live-job-dispatched
               :request request :ticket ticket
               :activation/accepted? true
               :terminal-collection {:submission/id "observed"}}
        normalized (sut/normalize-preflight-state state)]
    (is (= :preflight-dispatched (:state/type normalized)))
    (is (= request (:request normalized)))
    (is (= ticket (:ticket normalized)))
    (is (= normalized (sut/normalize-preflight-state normalized)))))
