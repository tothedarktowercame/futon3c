(ns futon3c.xiang.turn-acts-test
  "The turn->acts adapter against four real consecutive settled turns of
   session 564c8e50-c240-46fc-81ad-55afafbc63ee (claude-17 turns 391-394,
   2026-10-01T13:49-14:03Z), copied unmodified into turn_acts_fixtures/
   with claude-17's replies as text.

   The stretch: in turn 392's reply claude-17 posted the 🈸 offer
   \"Reply 'yes 1' or 'yes 2'\"; Joe accepted option 1 in turn 393
   (\"1\", read as approve) and commit d9d30ead in turn 393's happened
   carried it out. The 🈸 ask-actions of turns 391 (\"read how P11's
   records get written\") and 394 (\"do 1 now and look into 2\") Joe never
   answered."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.logic.xiang :as lx]
            [futon3c.xiang.reply-target]
            [futon3c.xiang.turn-record]
            [futon3c.xiang.turn-acts :as ta]))

(def ^:private fixture-dir "test/futon3c/xiang/turn_acts_fixtures")

(def ^:private turn-files
  ["turn-455grB" "turn-xJ4TAP" "turn-I7n9AZ" "turn-HUylGP"])

(defn- read-json [name]
  (json/read-str (slurp (io/file fixture-dir name)) :key-fn keyword))

(defn- fixture-turn [base]
  {:record (read-json (str base ".json"))
   :reading (read-json (str base ".json.analysis.json"))
   :reply (slurp (io/file fixture-dir (str base ".reply.txt")))})

(defn- fixture-turns []
  (mapv (fn [base]
          (let [{:keys [record] :as turn} (fixture-turn base)]
            (assoc turn :commits (ta/happened-commits (:happened_summary record)))))
        turn-files))

(def turn-ids ["claude-17-turn-391" "claude-17-turn-392"
               "claude-17-turn-393" "claude-17-turn-394"])

;; ---------------------------------------------------------------------------

(deftest happened-commits-parse
  (let [t391 (:record (fixture-turn "turn-455grB"))
        t392 (:record (fixture-turn "turn-xJ4TAP"))]
    (is (= [{:repo "futon3c" :sha "1ee5fea7"
             :subject "xiaoxiang-preview: bind keys in REPL buffers running on their own copy of the mode map (+12 -0 over 1 files)"}]
           (ta/happened-commits (:happened_summary t391))))
    (is (= [] (ta/happened-commits (:happened_summary t392))))
    (is (= [] (ta/happened-commits nil)))))

(deftest turn-acts-kinds-and-ids
  (let [{:keys [record reading reply commits]} (first (fixture-turns))
        acts (ta/turn->acts record reading reply commits)
        by-id (into {} (map (juxt :id identity) acts))]
    (testing "operator fragments keep the reading's intents, with stable ids"
      (is (= :redirect (:kind (by-id "claude-17-turn-391-f-s1-0"))))
      (is (= :report-problem (:kind (by-id "claude-17-turn-391-f-s2-0"))))
      (is (= "operator" (:author (by-id "claude-17-turn-391-f-s1-0")))))
    (testing "reply paragraphs take the kind their mark declares"
      (let [kinds (mapv :kind (filter #(str/includes? (:id %) "-r-") acts))]
        (is (= [:report-problem :approve :ask-action] kinds)
            "㊩ ㊣ and 🈸; the ㊥ gist has no kind and is skipped"))
      (is (= 1 (:skipped (meta acts))) "the gist paragraph alone"))
    (testing "a 🈸 without options is :ask-action, not :offer"
      (is (some #(= :ask-action (:kind %)) acts))
      (is (not-any? #(= :offer (:kind %)) acts)))
    (testing "the happened commit is a :commit act"
      (is (= :commit (:kind (by-id "claude-17-turn-391-c-1ee5fea7")))))
    (testing "every kind is from the kernel's table"
      (is (every? #(contains? lx/kinds (:kind %)) acts)))))

(deftest offer-options-are-detected
  (let [{:keys [record reading reply commits]} (second (fixture-turns))
        acts (ta/turn->acts record reading reply commits)
        offer (first (filter #(= :offer (:kind %)) acts))]
    (is (some? offer) "the 🈸 \"Reply 'yes 1' or 'yes 2'\" paragraph is an offer")
    (is (= [1 2] (:option offer)))
    (is (= "claude-17-turn-392" (:turn offer)))))

(deftest no-kind-intents-are-skipped-and-counted
  (let [record (:record (fixture-turn "turn-455grB"))
        reading {:sentences [{:id "s1"
                              :fragments [{:intent "frobnicate" :text "x"}
                                          {:intent "gist" :text "y"}
                                          {:intent "report" :text "z"}]}]}
        acts (ta/turn->acts record reading "" [])]
    (is (= 1 (count acts)) "only the report fragment has a kernel kind")
    (is (= :report (:kind (first acts))))
    (is (= 2 (:skipped (meta acts))) "the two no-kind intents are counted, not invented")))

(deftest session-acts-acceptance-and-carry-out
  (let [acts (ta/session-acts (fixture-turns))
        by-id (into {} (map (juxt :id identity) acts))
        offer (first (filter #(= :offer (:kind %)) acts))
        offer-id (:id offer)]
    (testing "turns are ordered by created_at"
      (is (= turn-ids (distinct (map :turn acts)))))
    (testing "Joe's approve in 393 became an :accept targeting the offer"
      (let [accept (first (filter #(= :accept (:kind %)) acts))]
        (is (= "claude-17-turn-393" (:turn accept)))
        (is (= offer-id (:target accept)))
        (is (= 1 (:option accept)))))
    (testing "the commit in 393 carries the accepted offer out"
      (is (= offer-id (:carries-out (by-id "claude-17-turn-393-c-d9d30ead")))))
    (testing "391's commit, before any acceptance, carries nothing out"
      (is (nil? (:carries-out (by-id "claude-17-turn-391-c-1ee5fea7")))))
    (testing "the kernel closes the offer in turn 393"
      (let [ports (ta/turn-ports acts "claude-17-turn-393")]
        (is (some #{offer-id} (:closed-this-turn ports)))))
    (testing "the unanswered 🈸 ask-actions are still open at the end"
      (let [ports (ta/turn-ports acts "claude-17-turn-394")
            open (set (map :act (:still-open ports)))]
        (is (contains? open "claude-17-turn-391-r-3")
            "turn 391's \"shall I read how P11's records get written\"")
        (is (some (fn [{:keys [act kind]}]
                    (and (= "claude-17-turn-394" (subs act 0 (count "claude-17-turn-394")))
                         (= :ask-action kind)))
                  (:still-open ports))
            "turn 394's \"shall I do 1 now and look into 2 afterwards\"")))
    (testing "still-open entries carry act, kind and since"
      (let [ports (ta/turn-ports acts "claude-17-turn-394")]
        (is (every? #(and (:act %) (:kind %) (:since %)) (:still-open ports)))))))

;; ---------------------------------------------------------------------------
;; Stored pointers (:reply_to)
;;
;; turn-ArDfbO and turn-xm9Otx are claude-17 turns 27-28 of the same
;; session (2026-10-04T02:34/02:48Z), copied unmodified from the live store
;; with turn 27's reply from evidence emacs-06125058b144b04aa089afd28febd38a.
;; Turn 28 opens "㊟: So it should go into futon1b"; the turn service stored
;; a :reply_to naming turn 27's ㊟ paragraph (rule mark-match). That
;; paragraph is a qualify, which opens no port, so this pair pins the link,
;; not a closure.

(defn- pointer-session []
  [(fixture-turn "turn-ArDfbO")
   {:record (read-json "turn-xm9Otx.json")
    :reading (read-json "turn-xm9Otx.json.analysis.json")
    :reply ""}])

(deftest a-stored-pointer-links-the-paragraph-it-names
  (let [acts (ta/session-acts (pointer-session))
        linked (filter #(= "claude-17-turn-27-r-2" (:target %)) acts)]
    (is (= {:linked 1 :unlinked 0} (:pointers (meta acts))))
    (is (seq linked))
    (is (every? #(and (= "operator" (:author %)) (= "claude-17-turn-28" (:turn %))) linked)
        "only the operator's fragments in the pointer paragraph")
    (is (not-any? #(re-find #"小象" (:text % "")) linked)
        "fragments of the later ㊭ paragraph are not linked")
    (is (some #{["claude-17-turn-27-r-2"]} (map (comp vec rest) (lx/explicit-answers acts)))
        "the kernel sees an explicit answer")))

(deftest a-stored-pointer-closes-an-open-port
  (testing "turn 392 with the pointer the 1a resolver gives for its text
            (constructed in memory; the fixture predates the writer)"
    (let [[t391 t392 & more] (fixture-turns)
          stored (futon3c.xiang.reply-target/resolve-targets
                  (get-in t392 [:record :source_text])
                  [{:turn-id "stream-id" :origin "operator" :text (:reply t391)}])
          turns (into [t391 (assoc-in t392 [:record :reply_to] stored)] more)
          acts (ta/session-acts turns)]
      (is (= {:linked 1 :unlinked 0} (:pointers (meta acts))))
      (is (some #{"claude-17-turn-391-r-3"}
                (:closed-this-turn (ta/turn-ports acts "claude-17-turn-392")))))))

(deftest stored-pointers-that-link-nothing
  (testing "missing reply_to: 391's question stays open at 392"
    (let [acts (ta/session-acts (fixture-turns))]
      (is (= {:linked 0 :unlinked 0} (:pointers (meta acts))))
      (is (some #{"claude-17-turn-391-r-3"}
                (map :act (:still-open (ta/turn-ports acts "claude-17-turn-392")))))))
  (testing "empty :replies"
    (let [[a b] (pointer-session)
          acts (ta/session-acts [a (assoc-in b [:record :reply_to :replies] [])])]
      (is (= {:linked 0 :unlinked 0} (:pointers (meta acts))))
      (is (not-any? #(= "claude-17-turn-27-r-2" (:target %)) acts))))
  (testing "a reply naming a turn outside the session"
    (let [acts (ta/session-acts [(second (pointer-session))])]
      (is (= {:linked 0 :unlinked 1} (:pointers (meta acts))))
      (is (not-any? :target acts))))
  (testing "a stored fallback (rule newest) is not a link"
    (let [[a b] (pointer-session)
          acts (ta/session-acts [a (assoc-in b [:record :reply_to :replies 0 :rule] "newest")])]
      (is (= {:linked 0 :unlinked 1} (:pointers (meta acts)))))))

(deftest a-pointer-on-an-accepting-paragraph-keeps-the-agreement
  (testing "turn 393 (\"1\") accepts turn 392's offer; a stored pointer at
            that same offer (constructed in memory) leaves the acceptance
            and the obligation it makes as they were"
    (let [turns (fixture-turns)
          offer "claude-17-turn-392-r-4"
          [mark text] (some (fn [[idx {:keys [mark text]}]]
                              (when (= 4 idx) [mark text]))
                            (map-indexed vector (futon3c.xiang.turn-record/reply-marks (:reply (turns 1)))))
          pointed (assoc-in turns [2 :record :reply_to]
                            {:replies [{:index 0 :mark mark :rule "mark-match"
                                        :paragraph (futon3c.xiang.reply-target/excerpt text)}]})
          accept-target (fn [acts]
                          (some #(when (and (= "claude-17-turn-393" (:turn %)) (= :accept (:kind %)))
                                   (:target %))
                                acts))
          before (ta/session-acts turns)
          after (ta/session-acts pointed)]
      (is (= {:linked 1 :unlinked 0} (:pointers (meta after))))
      (is (= offer (accept-target before) (accept-target after)))
      (is (= [{:debtor "claude-17" :creditor "operator" :source "claude-17-turn-393-f-s1-0"}]
             (:obligations (ta/turn-ports before "claude-17-turn-393"))
             (:obligations (ta/turn-ports after "claude-17-turn-393")))))))

;; turn-uJNPcf and turn-WdLaYK are claude-17 turns 42-43 of the same
;; session (2026-10-04T05:51/05:56Z), copied unmodified; turn 42's reply is
;; evidence emacs-0b3b74bca37997e6f56b4b4357a62c5d. Turn 43 opens "🈯 (no
;; requisition line): …"; its stored :replies are empty (it predates the
;; bracket form), so the pointer is supplied in memory from the resolver.

(deftest a-bracket-pointer-joins-on-the-agent-paragraph-mark
  (let [t42 (fixture-turn "turn-uJNPcf")
        t43 {:record (read-json "turn-WdLaYK.json")
             :reading (read-json "turn-WdLaYK.json.analysis.json")
             :reply ""}
        bracket (->> (futon3c.xiang.reply-target/resolve-targets
                      (get-in t43 [:record :source_text])
                      [{:turn-id "stream-id" :origin "operator" :text (:reply t42)}])
                     :replies (filter :bracket) vec)
        acts (ta/session-acts [t42 (assoc-in t43 [:record :reply_to :replies] bracket)])]
    (is (= ["🈯" "㊟"] ((juxt :mark :paragraph-mark) (first bracket))))
    (is (= {:linked 1 :unlinked 0} (:pointers (meta acts))))
    (is (some #(= "claude-17-turn-42-r-2" (:target %)) acts)
        "the ㊟ paragraph, joined on its own mark, not the operator's 🈯")))

;; ---------------------------------------------------------------------------
;; Fragments assigned to pointers by position (1d)

(deftest two-pointers-in-one-paragraph-are-assigned-by-position
  (testing "turn 43 with both of its pointers, as the resolver gives them:
            the 🈯 bracket pointer at codepoint 0 and the inline 🈸 at 160
            (161 as a Java string index, after the 🈯 surrogate pair)"
    (let [t42 (fixture-turn "turn-uJNPcf")
          t43 {:record (read-json "turn-WdLaYK.json")
               :reading (read-json "turn-WdLaYK.json.analysis.json")
               :reply ""}
          stored (futon3c.xiang.reply-target/resolve-targets
                  (get-in t43 [:record :source_text])
                  [{:turn-id "stream-id" :origin "operator" :text (:reply t42)}])
          acts (ta/session-acts [t42 (assoc-in t43 [:record :reply_to] stored)])
          by-start (into (sorted-map)
                         (for [a acts :when (= "claude-17-turn-43" (:turn a))]
                           [(:start a) (:target a)]))]
      (is (= [0 160] (map :offset (:replies stored))))
      (is (= {:linked 2 :unlinked 0} (:pointers (meta acts))))
      (is (= {0 "claude-17-turn-42-r-2"
              160 "claude-17-turn-42-r-4" 247 "claude-17-turn-42-r-4" 280 "claude-17-turn-42-r-4"}
             by-start)
          "the clarify before the 🈸 answers the ㊟; the rest answer the 🈸"))))

(deftest a-short-fragment-of-a-later-paragraph-is-not-linked
  (testing "\"yes\" in paragraph 2 also occurs in the pointer paragraph 1;
            by text it would take paragraph 1's link, by position it does not"
    (let [asked {:record {:turn_id "t-1" :created_at "2026-10-04T10:00:00Z" :agent_id "a"
                          :session_id "s" :source_text "start"}
                 :reading {:sentences []}
                 :reply "🈸 (next) Shall I do X?"}
          src "🈸: yes, do it\n\nyes"
          answer {:record {:turn_id "t-2" :created_at "2026-10-04T10:01:00Z" :agent_id "a"
                           :session_id "s" :source_text src
                           :reply_to (futon3c.xiang.reply-target/resolve-targets
                                      src [{:turn-id "x" :origin "operator" :text "🈸 (next) Shall I do X?"}])}
                  :reading {:sentences [{:id "s1" :fragments [{:intent "qualify" :text "🈸: yes, do it" :start 0 :end 13}]}
                                        {:id "s2" :fragments [{:intent "qualify" :text "yes" :start 15 :end 18}]}]}
                  :reply ""}
          acts (ta/session-acts [asked answer])
          target-of (into {} (map (juxt :id :target)) acts)]
      (is (= "t-1-r-0" (target-of "t-2-f-s1-0")))
      (is (nil? (target-of "t-2-f-s2-0"))))))
