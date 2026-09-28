(ns futon3c.transport.prompt-line-header-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.transport.http :as http]))

(def observed-at "2026-09-27T23:40:51Z")

(defn- segment [extra]
  (merge {:segment/id :pattern
          :segment/provider "test/pattern"
          :segment/observed-at observed-at
          :segment/basis {:evidence-ref "e-pattern" :scope {}}}
         extra))

(use-fixtures :each (fn [f] (prompt-line/reset-registry!) (f)))

(deftest current-turn-header-renders-pattern-fact-before-reply-contract
  (prompt-line/register-provider!
   {:segment/id :pattern :provider "test/pattern" :budget-ms 100
    :fn (fn [_]
          (segment {:segment/value "~peripherals/hot-reload-as-default-fix-path"
                    :segment/header
                    "retrieved peripherals/hot-reload-as-default-fix-path 0.4426; also a, b"}))})
  (let [header (with-redefs-fn
                 {#'futon3c.transport.http/reply-auto-routes? (constantly true)}
                 #(#'http/wrap-surface-header
                   "body" "bell" "claude-17" "codex-5"
                   {:bell-id "job-1"} "session-1"))
        prompt-line-text
        "Prompt: pattern ~peripherals/hot-reload-as-default-fix-path (retrieved, score 0.4426; also a, b)\n"]
    (is (str/includes? header prompt-line-text))
    (is (< (.indexOf header prompt-line-text)
           (.indexOf header "Reply delivery:")))))

(deftest no-segments-preserves-header-byte-for-byte
  (let [before (#'http/wrap-surface-header
                "body" "emacs-repl" "joe" "codex-5" nil)
        after (#'http/wrap-surface-header
               "body" "emacs-repl" "joe" "codex-5" nil "session-1")]
    (is (= before after))))

(deftest failed-providers-never-fail-or-annotate-the-turn
  (doseq [[provider f budget]
          [["test/throw" (fn [_] (throw (ex-info "boom" {}))) 100]
           ["test/slow" (fn [_] (Thread/sleep 2000)) 20]]]
    (prompt-line/reset-registry!)
    (prompt-line/register-provider!
     {:segment/id :pattern :provider provider :budget-ms budget :fn f})
    (let [started (System/nanoTime)
          header (#'http/wrap-surface-header
                  "body" "emacs-repl" "joe" "codex-5" nil "session-1")
          elapsed-ms (/ (- (System/nanoTime) started) 1000000.0)]
      (is (not (str/includes? header "Prompt:")))
      (is (str/ends-with? header "---\n\nbody"))
      (is (< elapsed-ms 250.0)))))

(deftest analysis-seats-get-no-prompt-line
  (prompt-line/register-provider!
   {:segment/id :pattern :provider "test/pattern" :budget-ms 100
    :fn (fn [_] (segment {:segment/value "~x/y"
                          :segment/header "retrieved x/y 0.5"}))})
  (doseq [seat ["象" "象-sonnet" "象-kimi"]]
    (is (not (str/includes? (#'http/wrap-surface-header
                             "body" "bell" "turn-capture" seat nil "s1")
                            "Prompt:"))
        seat))
  (is (str/includes? (#'http/wrap-surface-header
                      "body" "bell" "joe" "claude-17" nil "s1")
                     "Prompt: pattern ~x/y"))
  (is (not (prompt-line/analysis-seat? "claude-象"))))
