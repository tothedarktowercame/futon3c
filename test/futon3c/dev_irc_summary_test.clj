(ns futon3c.dev-irc-summary-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [futon3c.dev :as dev]))

(deftest summarize-irc-result-keeps-artifact-refs
  (testing "long replies are shortened and keep concrete refs"
    (let [input (str "Implemented the fix for relay routing and added tests across the stack. "
                     "Updated /home/joe/code/futon3c/src/futon3c/transport/irc.clj and "
                     "opened https://github.com/tothedarktowercame/futon3c/issues/42 with "
                     "follow-up notes. Commit 29d18a9 contains the exact change set.")
          out (#'futon3c.dev/summarize-irc-result input)]
      (is (<= (count out) 320))
      (is (.contains out "refs:"))
      (is (.contains out "29d18a9")))))

(deftest summarize-irc-result-avoids-json-dumps
  (testing "structured payloads are reduced to a short status line"
    (let [input "{\"thread_id\":\"019cc01c\",\"result\":\"very long JSON payload\"}"
          out (#'futon3c.dev/summarize-irc-result input)]
      (is (= "Structured output generated. (no artifact reference yet)" out))
      (is (not (.contains out "thread_id"))))))

(deftest summarize-irc-result-flags-missing-artifacts
  (testing "non-planning text without refs is explicitly marked"
    (let [out (#'futon3c.dev/summarize-irc-result
               "Ran checks locally and prepared a candidate fix for review.")]
      (is (.contains out "no artifact reference yet")))))

(deftest invoke-trace-response-block-persists-full-payload
  (testing "invoke trace shows only metadata and writes full payload to disk"
    (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                            "f3c-invoke-trace-test"
                            (make-array java.nio.file.attribute.FileAttribute 0)))
          payload "{\"thread_id\":\"synth-p2-s3a-000\",\"title\":\"long structured payload\"}"
          trace-id "invoke-1234"
          block (with-redefs [futon3c.dev/env (fn [k & [default]]
                                                (if (= k "FUTON3C_INVOKE_ARTIFACT_DIR")
                                                  (.getAbsolutePath tmp-dir)
                                                  default))]
                  (#'futon3c.dev/invoke-trace-response-block "codex-1" "019cc01c-b049-7ce1" trace-id payload))
          artifact-path (some->> (re-find #"Artifact: (.+)" block) second)]
      (is (.contains block "--- response trace (metadata only) ---"))
      (is (.contains block "Result: kind=structured"))
      (is (.contains block ", chars="))
      (is (.contains block "sha256="))
      (is (.contains block (str "Delivery: pending (trace-id " trace-id ")")))
      (is (.contains block "Delivery guarantee: caller must record where reply was sent."))
      (is artifact-path)
      (is (not (.contains block "thread_id")))
      (is (.exists (java.io.File. artifact-path)))
      (is (= payload (slurp artifact-path))))))

(deftest format-delivery-receipt-line-includes-surface-and-destination
  (let [line (#'futon3c.dev/format-delivery-receipt-line
              "invoke-42"
              {:surface "irc"
               :destination "#futon as <codex>"
               :delivered? true
               :note "dispatch-relay"})]
    (is (.contains line "Delivery: delivered via irc -> #futon as <codex>"))
    (is (.contains line "(trace-id invoke-42)"))
    (is (.contains line "[dispatch-relay]"))))

(deftest record-invoke-delivery-uses-agent-emacs-socket
  (testing "delivery updates target the same emacs socket used by the agent invoke buffer"
    (let [calls (atom [])]
      (with-redefs [futon3c.agency.registry/get-agent
                    (fn [_agent-id] {:agent/metadata {:emacs-socket "workspace1"}})
                    futon3c.blackboard/blackboard-eval!
                    (fn [elisp opts]
                      (swap! calls conj {:elisp elisp :opts opts})
                      {:ok true :output "ok"})]
        (is (true? (futon3c.dev/record-invoke-delivery!
                    "claude-1"
                    "invoke-xyz"
                    {:surface "whistle"
                     :destination "caller joe"
                     :delivered? true
                     :note "whistle-response"})))
        (is (= 1 (count @calls)))
        (is (= "workspace1" (get-in (first @calls) [:opts :emacs-socket])))))))

(deftest invoke-response->irc-reply-covers-success-and-failure
  (testing "successful invoke with text preserves refs in summary"
    (let [out (#'futon3c.dev/invoke-response->irc-reply
               {:ok true
                :result "Updated docs and committed 29d18a9 in /home/joe/code/futon3c/README.md"})]
      (is (.contains out "refs:"))
      (is (.contains out "29d18a9"))))
  (testing "successful invoke with nil result still returns a non-blank fallback"
    (let [out (#'futon3c.dev/invoke-response->irc-reply {:ok true :result nil})]
      (is (string? out))
      (is (not (str/blank? out)))
      (is (.contains out "invoke completed"))))
  (testing "failed invoke always returns a visible failure marker"
    (let [out (#'futon3c.dev/invoke-response->irc-reply
               {:ok false :error {:error/message "timeout waiting for response"}})]
      (is (.contains out "[invoke failed]"))
      (is (.contains out "timeout")))))
