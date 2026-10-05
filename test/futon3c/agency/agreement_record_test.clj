(ns futon3c.agency.agreement-record-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.agreement-record :as agreement]
            [futon3c.agency.offer-record :as offer-record]))

(def harness {:kind :none :basis :producer-context :source-ref "test:p11-3"})
(def offer-stamp {:executor "agent-a" :signer "agent-a"
                  :authority {:grant "act:offer-grant"}
                  :executor-basis :declared})
(def seat {:agent "agent-a" :session "session-a"})
(def offer
  {:id "act:offer-a" :kind :offer/record :author "agent-a" :addressee "joe"
   :seat seat :at "2026-09-28T14:00:00Z" :until "2026-09-28T15:00:00Z"
   :options [{:option/id "1" :option/label "one"
              :option/scope {:description "one"}}
             {:option/id "2" :option/label "two"
              :option/scope {:description "two"}}]
   :act/stamp offer-stamp :act/harness harness})
(def agreement-record
  {:id "act:agreement-a" :kind :agreement/record
   :agreement/offer "act:offer-a"
   :agreement/acceptance-evidence "emacs:joe-turn"
   :agreement/option-id "2"
   :agreement/scope {:description "two"}
   :agreement/offeror "agent-a" :agreement/acceptor "joe"
   :agreement/at "2026-09-28T14:30:00Z"
   :act/stamp agreement/operator-stamp :act/harness harness})

(defn reason [f]
  (try (f) nil
       (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest acceptance-grammar-is-classical
  (doseq [[input expected]
          [["yes" {:offer-id nil :option-id nil}]
           [" YES. " {:offer-id nil :option-id nil}]
           ["yes 2" {:offer-id nil :option-id "2"}]
           ["yes 2!" {:offer-id nil :option-id "2"}]
           ["yes act:offer-a" {:offer-id "act:offer-a" :option-id nil}]
           ["Yes act:offer-a 2." {:offer-id "act:offer-a" :option-id "2"}]
           ["yes please" nil]
           ["yes, but" nil]
           ["yes 2 3" nil]
           ["not yes" nil]
           ["yes?" nil]
           ["🈸:yes" {:offer-id nil :option-id nil}]
           ["🈸: yes 2." {:offer-id nil :option-id "2"}]
           ["🈸:yes act:offer-a 2" {:offer-id "act:offer-a" :option-id "2"}]
           ["🈸:" nil]
           ["🈸:yes please" nil]
           ["㊭:yes" nil]
           ["x 🈸:yes" nil]]]
    (is (= expected (agreement/parse-acceptance input)) input)))

(deftest resolver-does-not-break-ties-by-recency
  (testing "one offer and one option"
    (let [single (assoc offer :options [(first (:options offer))])]
      (is (= {:accept {:offer single :option (first (:options single))}}
             (agreement/resolve-acceptance {:offers [single]}
                                           (agreement/parse-acceptance "yes"))))))
  (testing "bare yes leaves two options ambiguous"
    (is (= #{{:offer-id "act:offer-a" :option-id "1"}
             {:offer-id "act:offer-a" :option-id "2"}}
           (set (get-in (agreement/resolve-acceptance
                         {:offers [offer]} (agreement/parse-acceptance "yes"))
                        [:ambiguous :candidates])))))
  (testing "option id shared by two offers is ambiguous"
    (let [other (assoc offer :id "act:offer-b" :at "2026-09-28T14:01:00Z")]
      (is (= #{"act:offer-a" "act:offer-b"}
             (set (map :offer-id
                       (get-in (agreement/resolve-acceptance
                                {:offers [offer other]}
                                (agreement/parse-acceptance "yes 2"))
                               [:ambiguous :candidates])))))))
  (testing "yes <option> with two offers is ambiguous even if one lacks the option"
    (let [other (assoc offer :id "act:offer-b"
                       :options [(first (:options offer))])]
      (is (= #{"act:offer-a" "act:offer-b"}
             (set (map :offer-id
                       (get-in (agreement/resolve-acceptance
                                {:offers [offer other]}
                                (agreement/parse-acceptance "yes 2"))
                               [:ambiguous :candidates]))))))))

(deftest resolver-refuses-invisible-offer-and-unknown-option
  (let [withdrawal {:id "act:withdraw" :kind :act/withdrawal :author "agent-a"
                    :target (:id offer) :status :effective :basis {:kind :self}
                    :at "2026-09-28T14:10:00Z"}
        visible (offer-record/active-offers-as-of
                 [offer withdrawal] seat "2026-09-28T14:20:00Z")]
    (is (= {:refused {:reason :unknown-offer}}
           (agreement/resolve-acceptance
            visible (agreement/parse-acceptance "yes act:offer-a")))))
  (is (= {:refused {:reason :unknown-option}}
         (agreement/resolve-acceptance
          {:offers [offer]} (agreement/parse-acceptance "yes act:offer-a 3")))))

(deftest agreement-is-checked-against-exact-offer-choice
  (is (= agreement-record
         (agreement/validate-against-offer! agreement-record offer)))
  (is (= :scope-mismatch
         (reason #(agreement/validate-against-offer!
                   (assoc-in agreement-record [:agreement/scope :extra] true)
                   offer))))
  (is (= :invalid-operator-stamp
         (reason #(agreement/validate!
                   (assoc agreement-record :act/stamp
                          {:executor "joe" :signer "joe"
                           :authority {:grant "act:grant"}
                           :executor-basis :session-bound})))))
  (is (= :offer-not-visible
         (reason #(agreement/validate-against-offer!
                   (assoc agreement-record :agreement/at (:until offer)) offer))))
  (is (= :offer-not-visible
         (reason #(agreement/validate-against-offer!
                   (assoc agreement-record :agreement/at "2026-09-28T15:00:01Z")
                   offer)))))

(deftest agreement-shape-is-closed
  (is (= :unexpected-key
         (reason #(agreement/validate! (assoc agreement-record :extra true)))))
  (is (= :missing-field
         (reason #(agreement/validate! (dissoc agreement-record
                                               :agreement/acceptance-evidence)))))
  (is (= :acceptor-not-operator
         (reason #(agreement/validate! (assoc agreement-record
                                              :agreement/acceptor "agent-a"))))))

(deftest agreement-hyperedge-round-trips-losslessly
  (let [edge (agreement/record->hyperedge agreement-record)]
    (is (= :agreement/record (:hx/type edge)))
    (is (= ["act:offer-a" "agent:agent-a" "agent:joe" "emacs:joe-turn"]
           (:hx/endpoints edge)))
    (is (= 1 (get-in edge [:hx/props :agreement/schema])))
    (is (= agreement-record (agreement/hyperedge->record edge)))
    (is (= agreement-record
           (agreement/hyperedge->record (dissoc edge :hx/valid-time))))))

(def reply-evidence
  {:evidence/id "emacs-reply" :evidence/at "2026-10-02T04:10:27Z"
   :evidence/body {:event "chat-turn" :role "assistant"
                   :text "㊥ (gist) Done.\n\n🈸 (one decision) Shall I build it?\n"}})

(deftest a-single-ask-in-a-reply-is-accepted-by-yes
  ;; Joe, 2026-10-02: "if you ask me a yes-no 'Shall I...' question, then
  ;; 🈸:yes does have an obvious interpretation".
  (let [offer (assoc (agreement/reply-offer-record reply-evidence "claude-17" "s1")
                     :id "act:reply-offer")
        resolution (agreement/resolve-acceptance
                    {:offers [offer]} (agreement/parse-acceptance "🈸:yes"))]
    (is (= ["🈸 (one decision) Shall I build it?"]
           (agreement/reply-asks (get-in reply-evidence [:evidence/body :text]))))
    (is (= "1" (get-in resolution [:accept :option :option/id])))
    (is (= "🈸 (one decision) Shall I build it?"
           (get-in resolution [:accept :option :option/label])))
    (is (= {:reply-evidence "emacs-reply"}
           (get-in resolution [:accept :option :option/scope])))
    (is (= "2026-10-02T04:10:27Z" (:at offer)))))

(deftest several-asks-need-a-number
  (let [reply (assoc-in reply-evidence [:evidence/body :text]
                        "🈸 (a) Shall I do A?\n\n㊢ (b) did B\n\n🈸 (c) Shall I do C?")
        offer (assoc (agreement/reply-offer-record reply "claude-17" "s1")
                     :id "act:reply-offer")]
    (is (= ["1" "2"] (mapv :option/id (:options offer))))
    (is (:ambiguous (agreement/resolve-acceptance
                     {:offers [offer]} (agreement/parse-acceptance "🈸:yes"))))
    (is (= "🈸 (c) Shall I do C?"
           (get-in (agreement/resolve-acceptance
                    {:offers [offer]} (agreement/parse-acceptance "yes 2"))
                   [:accept :option :option/label])))))

(deftest a-reply-without-an-ask-makes-no-offer
  (is (nil? (agreement/reply-offer-record
             (assoc-in reply-evidence [:evidence/body :text] "㊢ (report) Done.\n\nNo question here.")
             "claude-17" "s1")))
  (is (= [] (agreement/reply-asks
             (str "㊢ (\"shall I go on?\") Ran `ok? ` and `done?` then fetched https://a.b/c?d=e.\n\n"
                  "㊟ (why stop? a quote) Joe asked “is it done?” and it is.\n\n"
                  "```\nwhy? \n\nreally?\n```"))))
  ;; 🈸 inside a paragraph is not an ask paragraph
  (is (= [] (agreement/reply-asks "㊢ I would mark it 🈸 if asking.")))
  (is (= [] (agreement/reply-asks nil))))

(deftest a-reply-offer-is-a-valid-offer
  (let [offer (assoc (agreement/reply-offer-record reply-evidence "claude-17" "s1")
                     :id "act:reply-offer"
                     :act/stamp {:executor "claude-17" :signer "claude-17"
                                 :authority {:grant "act:grant"}
                                 :executor-basis :declared})]
    (is (= offer (offer-record/validate! offer)))))

(deftest a-question-under-any-mark-is-an-ask
  (testing "claude-4, 2026-10-05: the decision was asked under 🈳, and Joe's
            yes found no visible offer"
    (let [text (str "㊢ (watch) The watcher is `bg-…748`.\n\n"
                    "🈳 (waiting on you) May I push `8c5a3146` to Rob's main? "
                    "It matches Lean on all 20 rows. And should stop C be the first fix?")
          offer (assoc (agreement/reply-offer-record
                        (assoc-in reply-evidence [:evidence/body :text] text) "claude-4" "s1")
                       :id "act:reply-offer")
          resolution (agreement/resolve-acceptance {:offers [offer]} (agreement/parse-acceptance "yes"))]
      (is (= 1 (count (:options offer))) "one asking paragraph, one option")
      (is (str/starts-with? (get-in resolution [:accept :option :option/label]) "🈳 (waiting on you) May I push"))))
  (testing "a question inside a gist paragraph, as claude-4 first asked about stop C"
    (is (= 1 (count (agreement/reply-asks
                     "㊥ (results) Stop C accounts for 6 of 14. Shall I authorize it, or would you rather review first? No fixes are out."))))))
