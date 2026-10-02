(ns futon3c.xiang.turn-record-test
  "The pure half of the 象 frontend, checked against the seam's own example
   and against a turn with an emoji and CJK, where UTF-16 and codepoints part."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.xiang.turn-record :as tr]))

(def example
  "The seam's one real record; found from the repo root, or FUTON3C_ROOT
   when the runner's working directory is elsewhere."
  (delay (let [rel "packages/turn-seam/example/turn-example.json"
               f (first (filter #(.exists ^java.io.File %)
                                [(io/file rel) (io/file (or (System/getenv "FUTON3C_ROOT") ".") rel)]))]
           (json/parse-string (slurp f) true))))

(defn- spans [record]
  (mapv #(select-keys % [:start :end :text]) (:sentences record)))

(deftest structure-reproduces-the-seam-example
  (let [ex @example
        st (tr/structure-turn (:source_text ex))]
    (is (= (spans ex) (spans st)))
    (is (= ["s1" "s2" "s3" "s4"] (:unmatched st)))
    (is (= tr/offset-unit (:offset_unit st)))))

(deftest offsets-are-codepoints-not-utf16-units
  (let [text "I agree 🗣 with 象 here.  Let's ask 象-2 next?\n\nQUOTE"
        st (tr/structure-turn text)]
    (is (= 3 (count (:sentences st))))
    (doseq [s (:sentences st)]
      (is (= (:text s) (tr/cp-subs text (:start s) (:end s))) (:id s))
      (doseq [c (:cues s)]
        (is (= (:text c) (tr/cp-subs text (:start c) (:end c))))))
    (is (= [[0 7 "approve" "I agree"]]
           (mapv (juxt :start :end :label :text) (:cues (first (:sentences st))))))
    ;; The second sentence starts after the emoji: 24 codepoints, 25 UTF-16 units.
    (is (= 24 (:start (second (:sentences st)))))
    (is (= 25 (tr/cp->utf16 text 24)))))

(deftest sentence-ends-need-a-blank-or-a-line-end
  (is (= ["v1.2 is out." "Use it."] (map :text (tr/sentence-spans "v1.2 is out. Use it."))))
  (is (= ["a line\nwith no terminator"] (map :text (tr/sentence-spans "a line\nwith no terminator"))))
  (is (= ["first paragraph" "second"] (map :text (tr/sentence-spans "first paragraph\n\nsecond"))))
  (is (= ["Really?!" "Yes."] (map :text (tr/sentence-spans "Really?! Yes.")))))

(deftest cue-matching-is-bounded-and-apostrophe-tolerant
  (is (= [] (tr/turn-matches "I agreed with that")))
  (is (= [[0 7 "approve" "I agree"]] (tr/turn-matches "I agree.")))
  (is (= ["constrain"] (map #(nth % 2) (tr/turn-matches "don’t do that"))))
  (is (= ["constrain"] (map #(nth % 2) (tr/turn-matches "DON'T DO THAT"))))
  (testing "a learned vocabulary replaces the default"
    (is (= [[0 5 "greet" "hello"]] (tr/turn-matches "hello world" [["greet" ["hello"]]])))))

(deftest vocabulary-from-json-accepts-the-emacs-shape
  (is (= [["approve" ["I agree" "looks good"]] ["defer" ["for now"]]]
         (tr/vocabulary-from-json {"version" 3 "rules" [["approve" "I agree" "looks good"] ["defer" "for now"]]})))
  (is (nil? (tr/vocabulary-from-json {"rules" [["bad tag!" "x"]]})))
  (is (nil? (tr/vocabulary-from-json {"rules" "nope"}))))

(deftest quotes-are-elided-to-one-token
  (is (= {:text "Look at this\nQUOTE\nand my words" :quotes ["quoted stuff\nmore quoted"]}
         (tr/elide-quotes "Look at this\n>>> quoted stuff\nmore quoted\n>>>\nand my words")))
  (is (= {:text "Look at this\nQUOTE" :quotes ["Here's a thing\nstill quoted"]}
         (tr/elide-quotes "Look at this\n>>> Here's a thing\nstill quoted")))
  (is (= {:text "no fence here" :quotes []} (tr/elide-quotes "no fence here")))
  (testing "a quoted secret is still a secret"
    (is (= ["aws-access-key"] (:kinds (tr/redact-secrets ">>> AKIAABCDEFGHIJKLMNOP"))))))

(deftest secrets-are-redacted-by-kind
  (let [{:keys [text kinds]} (tr/redact-secrets
                              (str "key AKIAABCDEFGHIJKLMNOP, gh ghp_abcdefghijklmnopqrstuvwxyz0123,"
                                   " password: hunter2secret, Authorization: Bearer abc123def456ghi"))]
    (is (= ["aws-access-key" "github-token" "keyword-value" "bearer"] kinds))
    (is (not (str/includes? text "hunter2secret")))
    (is (not (str/includes? text "AKIAABCDEFGHIJKLMNOP")))
    (is (str/includes? text "[REDACTED:keyword-value]")))
  (testing "ordinary identifiers survive"
    (let [plain "sha 5146606d at /home/joe/code, uuid cb1be3dd-5b2d-4986-911c-74c178f84408, max_token=4096, {:token :foo/bar}"]
      (is (= {:text plain :kinds []} (tr/redact-secrets plain)))))
  (testing "a url password"
    (is (= ["url-credentials"] (:kinds (tr/redact-secrets "https://joe:s3cr3t@host/x"))))))

(deftest surface-marker-only-counts-when-leading
  (is (= {:surface "dictated" :text "hello"} (tr/split-surface-marker "🗣 hello")))
  (is (= {:surface nil :text "say 🗣 hello"} (tr/split-surface-marker "say 🗣 hello"))))

(deftest make-record-carries-the-emacs-metadata
  (let [{:keys [record redacted]}
        (tr/make-record {:text "🗣 I agree. token=ghp_abcdefghijklmnopqrstuvwxyz0123\n>>> shown\n>>>\nDone?"
                         :agent-id "claude-1" :session-id "s" :turn-id "t1"
                         :evidence-id "ev1" :now-ms 0})]
    (is (= ["github-token"] redacted (:secrets_redacted record)))
    (is (= "dictated" (:surface record)))
    (is (= "requested" (:analysis_status record)))
    (is (= "operator" (:origin record)))
    (is (= ["shown"] (:quotes record)))
    (is (= "1970-01-01T00:00:00Z" (:created_at record)))
    (is (= "ev1" (:evidence_id record)))
    (is (= 1 (:version record)))
    (is (str/includes? (:source_text record) "QUOTE"))
    (is (not (str/includes? (:source_text record) "🗣")))
    (doseq [s (:sentences record)]
      (is (= (:text s) (tr/cp-subs (:source_text record) (:start s) (:end s))))))
  (testing "tagging failed keeps the original and requests analysis"
    (let [{:keys [record]} (tr/make-record {:text "words" :original-text "!x words" :failed? true
                                            :agent-id "a" :session-id "s" :turn-id "t"
                                            :analysis-requested? (constantly false)})]
      (is (true? (:tagging_failed record)))
      (is (= "!x words" (:original_text record)))
      (is (= "requested" (:analysis_status record)))))
  (testing "a policy can decline analysis"
    (is (= "not-requested"
           (:analysis_status (:record (tr/make-record {:text "hi" :agent-id "a" :session-id "s" :turn-id "t"
                                                       :analysis-requested? (constantly false)})))))))

(deftest the-brief-names-the-record-and-the-requisition
  (let [brief (tr/analysis-brief "turn-abc123" "/x/turn-abc123.json" {:requisition "M-futon-seams"})]
    (is (str/starts-with? brief "Requisition: M-futon-seams — interpret operator turn turn-abc123\n"))
    (is (str/includes? brief "unresolved sentences: /x/turn-abc123.json\n"))
    (is (str/includes? brief "interpretation version 3"))
    (is (str/includes? brief "You did NOT receive this turn"))
    (is (str/includes? brief "Suggested intents: approve, disagree"))))

;; ---------------------------------------------------------------------------
;; validate-analysis

(def record-1
  (merge (tr/structure-turn "Please continue with the port. I withdraw that pattern.")
         {:interpretation_version 3 :vocabulary_version 3 :evidence_id "ev-1"}))

(defn- fragment [m]
  (merge {:relations ["action"] :rationale "because" :display_cues []
          :no_surface_cue "implicit"} m))

(def good-analysis
  {:labeller "象-1"
   :sentences [{:id "s1"
                :fragments [(fragment {:start 0 :end 30 :text "Please continue with the port."
                                       :intent "continue" :target "the port"
                                       :display_cues [{:start 0 :end 15 :text "Please continue"}]
                                       :pattern_refs [{:id "social/keep-going" :rationale "fits"}]})]}
               {:id "s2"
                :fragments [(fragment {:start 31 :end 55 :text "I withdraw that pattern."
                                       :intent "withdraw" :target nil})]}]})

(deftest a-good-analysis-is-canonicalised
  (let [out (tr/validate-analysis record-1 good-analysis
                                  {:pattern-source {"social/keep-going" "@flexiarg social/keep-going\n..."}
                                   :now-ms 0})]
    (is (= "analyzed" (:status out)))
    (is (= 2 (:version out)))
    (is (= "象-1" (:labeller out)))
    (is (= "resolved" (:pattern_check out)))
    (is (= "ev-1" (:evidence_id out)))
    (is (= 3 (:interpretation_version out)))
    (is (= (tr/sha256 (:source_text record-1)) (:source_sha256 out)))
    (let [frag (first (:fragments (first (:sentences out))))]
      (is (= "candidate" (:status (first (:pattern_refs frag)))))
      (is (string? (:source_sha256 (first (:pattern_refs frag)))))
      (is (= [{:start 0 :end 15 :text "Please continue"}] (:display_cues frag))))
    (is (nil? (:target (first (:fragments (second (:sentences out)))))))
    (is (= "" (:unresolved_reason (first (:sentences out)))))))

(defn- invalid-reason [record analysis opts]
  (try (tr/validate-analysis record analysis opts) nil
       (catch clojure.lang.ExceptionInfo e (.getMessage e))))

(deftest invalid-analyses-say-what-is-wrong
  (is (re-find #"one analysis per source sentence"
               (invalid-reason record-1 (update good-analysis :sentences pop) {})))
  (is (re-find #"offsets/text must match"
               (invalid-reason record-1 (assoc-in good-analysis [:sentences 0 :fragments 0 :end] 29) {})))
  (is (re-find #"unknown canonical pattern"
               (invalid-reason record-1 good-analysis {:pattern-source (constantly nil)})))
  (is (re-find #"declaration does not match"
               (invalid-reason record-1 good-analysis {:pattern-source (constantly "@flexiarg other/id\n")})))
  (is (re-find #"unresolved_reason"
               (invalid-reason record-1 (assoc-in good-analysis [:sentences 1 :fragments] []) {})))
  (is (re-find #"no_surface_cue"
               (invalid-reason record-1 (assoc-in good-analysis [:sentences 1 :fragments 0 :no_surface_cue] "") {})))
  (is (re-find #"target is required"
               (invalid-reason record-1 (assoc-in good-analysis [:sentences 0 :fragments 0 :target] nil) {})))
  (testing "the cue budget names the sentence and the overshoot"
    (let [rec (tr/structure-turn "one two three four five six seven eight nine ten.")
          an {:labeller "x"
              :sentences [{:id "s1"
                           :fragments [(fragment {:start 0 :end 49 :text (:source_text rec)
                                                  :intent "report-problem" :target "t"
                                                  :display_cues [{:start 0 :end 13 :text "one two three"}
                                                                 {:start 14 :end 27 :text "four five six"}
                                                                 {:start 28 :end 44 :text "seven eight nine"}]})]}]}]
      (is (re-find #"s1 marks 9 of 10 words \(90%\); the budget here is 5 words, so drop about 4"
                   (invalid-reason rec an {})))))
  (testing "a long cue"
    (let [rec (tr/structure-turn "a b c d e f g h i j k l m.")
          an {:labeller "x"
              :sentences [{:id "s1"
                           :fragments [(fragment {:start 0 :end 26 :text (:source_text rec)
                                                  :intent "x" :target "t"
                                                  :display_cues [{:start 0 :end 17 :text "a b c d e f g h i"}]})]}]}]
      (is (re-find #"short keyword phrases" (invalid-reason rec an {}))))))

(deftest rnode-fields-pass-only-with-a-contract
  (let [an (assoc-in good-analysis [:sentences 0 :fragments 0 :rnode]
                     {:node "n1" :operation "count" :quantity "turns" :justification "because"})]
    (is (= {:accepted_fragments 0 :accepted_cues 0}
           (-> (tr/validate-analysis record-1 an {}) :rnode_validation (select-keys [:accepted_fragments :accepted_cues]))))
    (is (= 1 (get-in (tr/validate-analysis record-1 an {}) [:rnode_validation :dropped :no_contract])))
    (let [out (tr/validate-analysis record-1 an {:rnode-contract {"n1" {:operations #{"count"} :label "N1" :stage "s"}}})]
      (is (= 1 (get-in out [:rnode_validation :accepted_fragments])))
      (is (= "N1" (get-in out [:sentences 0 :fragments 0 :rnode :label]))))))

;; ---------------------------------------------------------------------------
;; Reaping and withdrawals

(deftest job-outcomes-follow-the-reaper
  (is (= :unreachable (:outcome (tr/job-outcome nil))))
  (is (= :unreachable (:outcome (tr/job-outcome {:unreachable "connection refused"}))))
  (is (= :running (:outcome (tr/job-outcome {:state "running"}))))
  (is (= :running (:outcome (tr/job-outcome {:state "queued"}))))
  (is (= :running (:outcome (tr/job-outcome {:state "done" :execution {:executed true}}))))
  (is (= {:outcome :refused :state "done" :reason "refusal: no requisition"}
         (tr/job-outcome {:state "done" :execution {:executed false}
                          :events [{:type "start"} {:type "refusal" :text "no requisition"}]})))
  (is (= :failed (:outcome (tr/job-outcome {:state "error" :events []}))))
  (is (= :refused (:outcome (tr/job-outcome {:state "cancelled"}))))
  (is (= "terminal: boom" (:reason (tr/job-outcome {:state "failed" :events [{:type "terminal" :error "boom"}]}))))
  (is (tr/quota-failure? "refusal: usage limit reached"))
  (is (tr/quota-failure? "HTTP 429"))
  (is (not (tr/quota-failure? "no requisition")))
  (is (tr/store-busy-failure? "Turn not started: futon1b busy"))
  (is (not (tr/store-busy-failure? "usage limit"))))

(deftest withdraw-fragments-and-notices
  (let [analysis {:sentences [{:id "s1" :fragments [{:intent "continue"} {:intent "withdraw" :target nil}]}
                              {:id "s2" :fragments [{:intent "withdraw" :target "act:1"}]}]}]
    (is (= [["s1:1" {:intent "withdraw" :target nil}] ["s2:0" {:intent "withdraw" :target "act:1"}]]
           (tr/withdrawal-fragments analysis))))
  (is (= {:kind "effect" :effect_id "act:9" :text "withdraw inferred: effect act:9 (undo to reverse)"}
         (tr/withdrawal-notice {:status 200 :effect_id "act:9"})))
  (is (= "no-grant" (:kind (tr/withdrawal-notice {:status 403 :reason "no-grant"}))))
  (is (= "unresolved" (:kind (tr/withdrawal-notice {:status 422 :reason "target-unresolved"}))))
  (is (nil? (tr/withdrawal-notice {:status 500 :reason "store-failure"}))))

;; ---------------------------------------------------------------------------
;; Agent turns: proforma marks read, not inferred

(deftest reply-marks-are-read-off-marked-paragraphs
  (let [reply (str "㊥ Fixed the three faults.\n\n"
                   "Some plain prose\nover two lines.\n\n"
                   "🈸: May we adopt a transparent provisional numeric prior?\n\n"
                   "  🈡 I withdraw the earlier 象 suggestion.")
        marks (tr/reply-marks reply)]
    (is (= ["gist" "ask-action" "withdraw"] (map :intent marks)))
    (is (= ["annotator" "act" "select"] (map :stage marks)))
    (is (= "May we adopt a transparent provisional numeric prior?" (:text (second marks))))
    (doseq [m marks]
      (is (str/starts-with? (tr/cp-subs reply (:start m) (:end m)) (:mark m)) (:mark m)))
    (is (= "🈡 I withdraw the earlier 象 suggestion." (tr/cp-subs reply (:start (nth marks 2)) (:end (nth marks 2))))))
  (is (= [] (tr/reply-marks "no marks here\n\nnone")))
  (is (= [] (tr/reply-marks nil))))

(deftest an-agent-turn-carries-its-marks-and-author
  (let [{:keys [record]} (tr/make-record {:text "㊥ Done.\n\n🈸 Shall I continue?" :origin "agent"
                                          :agent-id "claude-17" :session-id "s" :turn-id "t-reply"
                                          :now-ms 0})]
    (is (= "agent" (:origin record)))
    (is (= "claude-17" (:author record)))
    (is (= ["gist" "ask-action"] (map :intent (:proforma_marks record))))
    (is (nil? (:operator_id record))))
  (testing "an operator turn names who typed it"
    (let [{:keys [record]} (tr/make-record {:text "yes" :agent-id "claude-17" :session-id "s" :turn-id "t"
                                            :operator-id "@joe:matrix.paragogy.net"})]
      (is (= "operator" (:origin record)))
      (is (= "@joe:matrix.paragogy.net" (:operator_id record) (:author record)))
      (is (nil? (:proforma_marks record))))))

(deftest the-agent-brief-is-for-a-reply
  (let [brief (tr/agent-brief "turn-x" "/p/turn-x.json" {})]
    (is (str/starts-with? brief "Requisition: M-futon-seams — interpret agent turn turn-x\n"))
    (is (str/includes? brief "Do NOT label withdraw on an agent turn"))
    (is (str/includes? brief "🈸"))
    (is (str/includes? brief "/p/turn-x.json"))))
