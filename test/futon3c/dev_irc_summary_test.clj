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
  (testing "invoke trace shows a short summary and writes full payload to disk"
    (let [tmp-dir (.toFile (java.nio.file.Files/createTempDirectory
                            "f3c-invoke-trace-test"
                            (make-array java.nio.file.attribute.FileAttribute 0)))
          payload "{\"thread_id\":\"synth-p2-s3a-000\",\"title\":\"long structured payload\"}"
          block (with-redefs [futon3c.dev/env (fn [k & [default]]
                                                (if (= k "FUTON3C_INVOKE_ARTIFACT_DIR")
                                                  (.getAbsolutePath tmp-dir)
                                                  default))]
                  (#'futon3c.dev/invoke-trace-response-block "codex-1" "019cc01c-b049-7ce1" payload))
          artifact-path (some->> (re-find #"Artifact: (.+)" block) second)]
      (is (.contains block "--- response summary (trace only) ---"))
      (is (.contains block "Summary: Structured output generated."))
      (is artifact-path)
      (is (not (.contains block "thread_id")))
      (is (.exists (java.io.File. artifact-path)))
      (is (= payload (slurp artifact-path))))))

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
