(ns futon3c.xiang.ma-reading-test
  "The 馬 alignment reading: classical quote pass, bounded model pass whose
  verdicts are accepted only with a verbatim quote, the seat-absent fallback,
  and bell turns left unread. Run:
  FUTON3C_INVOKE_JOBS_FILE=$(mktemp -d)/i.edn clojure -M:test -n futon3c.xiang.ma-reading-test"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.arxana.xiang-interpretations :as interpretations]
            [futon3c.xiang.ma-reading :as ma]
            [futon3c.xiang.turn-record :as turn-record]
            [futon3c.xiang.turn-service :as svc]
            [futon3c.xiang.turn-store :as ts]))

(use-fixtures :each
  (fn [f]
    (with-redefs [interpretations/read-acts
                  (fn [_ record] {:acts (filterv #(= "operator" (:author %)) (:acts record))})]
      (f))))

(defn- temp-store []
  (ts/store (str (java.nio.file.Files/createTempDirectory
                  "ma-reading-" (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- record!
  "Write a crafted operator record; returns [store id]."
  ([] (record! {}))
  ([over]
  (let [store (temp-store)
        base {:origin "operator" :agent_id "claude-3" :session_id "sess-1"
              :turn_id "claude-3-turn-9" :created_at "2026-10-19T00:00:00Z"
              :reply_text "㊢ did the thing exactly as asked.\n\n🈝 the reasons are deferred to tomorrow."
              :reply_paragraphs [{:id "claude-3-turn-9-r-0" :mark "㊢"
                                  :text "did the thing exactly as asked."}
                                 {:id "claude-3-turn-9-r-1" :mark "🈝"
                                  :text "the reasons are deferred to tomorrow."}]
              :ports_status {:state "computed"}
              :acts [{:id "claude-3-turn-9-f-s1-0" :kind "delegate" :author "operator"
                      :text "do the thing"}
                     {:id "claude-3-turn-9-f-s1-1" :kind "explain" :author "operator"
                      :text "why it matters"}
                     {:id "claude-3-turn-9-r-0" :kind "report" :author "claude-3"
                      :mark "㊢"
                      :text "did the thing exactly as asked."}
                     {:id "claude-3-turn-9-r-1" :kind "defer" :author "claude-3"
                      :mark "🈝"
                      :text "the reasons are deferred to tomorrow."}]}
        {:keys [id]} (ts/write-record! store (merge base over))]
    [store id])))

(defn- alignment [store id] (:alignment (ts/read-record store id)))

(defn- by-act [store id act]
  (some #(when (= act (:act %)) %) (:acts (alignment store id))))

(deftest gcc-interpretations-supply-operator-acts-in-offset-order
  (let [[store id] (record! {:acts [{:id "claude-3-turn-9-r-0" :author "agent"}]})
        read (fn [_ _] {:acts [{:id "claude-3-turn-9-f-s1-0" :kind "report" :author "operator"
                                :start 0 :end 6 :text "first"}
                               {:id "claude-3-turn-9-f-s1-1" :kind "explain" :author "operator"
                                :start 10 :end 20 :text "second"}]})]
    (ma/align! {:store store :interpretation-reader read} id)
    (is (= ["claude-3-turn-9-f-s1-0" "claude-3-turn-9-f-s1-1"]
           (mapv :act (:acts (alignment store id)))))))

(deftest no-operator-acts-published-is-an-empty-reading-not-pending
  (let [[store id] (record! {:xiang_interpretations {:written 0}})
        out (ma/align! {:store store :interpretation-reader
                        (fn [& _] (throw (AssertionError. "reader not needed")))} id)]
    (is (map? out))
    (is (nil? (:alignment_pending (ts/read-record store id))))))

(deftest pending-interpretations-never-overwrite-real-alignment
  (let [[store id] (record! {:alignment {:version 1 :acts [{:act "real"}]}})
        out (ma/align! {:store store :interpretation-reader
                        (fn [& _] {:pending :interpretations-pending})} id)]
    (is (= :interpretations-pending (:state out)))
    (is (= [{:act "real"}] (get-in (ts/read-record store id) [:alignment :acts])))
    (is (= "interpretations-pending"
           (get-in (ts/read-record store id) [:alignment_pending :state])))))

(deftest a-targeted-reply-act-answers-classically-without-a-bell
  (let [[store id] (record! {:acts [{:id "claude-3-turn-9-f-s1-0" :kind "delegate"
                                     :author "operator" :text "do the thing"}
                                    {:id "claude-3-turn-9-f-s1-1" :kind "explain"
                                     :author "operator" :text "why it matters"}
                                    ;; quotes the operator's words: classical
                                    {:id "claude-3-turn-9-r-0" :kind "report"
                                     :author "claude-3" :mark "㊢"
                                     :target "claude-3-turn-9-f-s1-0"
                                     :text "\"do the thing\" — done"}
                                    {:id "claude-3-turn-9-r-1" :kind "explain"
                                     :author "claude-3" :mark "🈖"
                                     :target "claude-3-turn-9-f-s1-1"
                                     :text "\"why it matters\" — explained"}]})
        asked (atom [])
        events (atom [])]
    (is (map? (ma/align! {:store store
                          :ask (fn [payload] (swap! asked conj payload) nil)
                          :notify! #(swap! events conj %)}
                         id)))
    (is (empty? @asked) "no bell for an act the quote pass closed")
    (let [a (by-act store id "claude-3-turn-9-f-s1-0")]
      (is (= "answered" (:verdict a)))
      (is (= "claude-3-turn-9-r-0" (:by a)))
      (is (= "㊢" (:mark a)))
      (is (= "quote" (:method a))))
    (is (= [{:event :aligned :id id}] @events))))

(deftest classical-pass-reads-marks-only-from-typed-reply-acts
  (let [record {:turn_id "claude-3-turn-9"
                :reply_text "㊩ [prose] This must not be parsed."
                :operator_interpretation_acts
                [{:id "claude-3-turn-9-f-s1-0" :kind "delegate"
                  :author "operator" :text "do it"}]
                :acts [{:id "claude-3-turn-9-r-0" :kind "report"
                        :author "claude-3" :mark "㊢"
                        :target "claude-3-turn-9-f-s1-0" :text "done"}]}
        out (with-redefs [turn-record/reply-marks
                          (fn [& _] (throw (AssertionError. "reply prose was reparsed")))]
              (ma/classical-pass record))]
    (is (= "㊢" (get-in out [:entries 0 :mark])))
    (is (= 0 (:reply-marks-absent out)))))

(deftest classical-pass-types-an-absent-reply-mark
  (let [record {:turn_id "claude-3-turn-9"
                :operator_interpretation_acts
                [{:id "claude-3-turn-9-f-s1-0" :kind "delegate"
                  :author "operator" :text "do it"}]
                :reply_paragraphs [{:id "claude-3-turn-9-r-0" :mark nil :text "done"}]
                :acts [{:id "claude-3-turn-9-r-0" :kind "report"
                        :author "claude-3" :target "claude-3-turn-9-f-s1-0"
                        :text "done"}]}
        out (ma/classical-pass record)]
    (is (nil? (get-in out [:entries 0 :mark])))
    (is (= 1 (:reply-marks-absent out)))))

(deftest model-pass-can-cite-a-non-act-gist-paragraph
  (let [[store id] (record! {:reply_text "㊥ the answer is carried here."
                             :reply_paragraphs
                             [{:id "claude-3-turn-9-r-0" :mark "㊥"
                               :text "the answer is carried here."}]
                             :acts [{:id "claude-3-turn-9-f-s1-0" :kind "delegate"
                                     :author "operator" :text "give the answer"}]})
        seen (atom nil)
        answer (str "[{\"act\":\"claude-3-turn-9-f-s1-0\","
                    "\"verdict\":\"answered\",\"by\":\"claude-3-turn-9-r-0\","
                    "\"quote\":\"the answer is carried here\"}]")]
    (with-redefs [turn-record/reply-marks
                  (fn [& _] (throw (AssertionError. "reply prose was reparsed")))]
      (ma/align! {:store store :ask (fn [payload] (reset! seen payload) answer)} id))
    (is (str/includes? (:prompt @seen) "claude-3-turn-9-r-0"))
    (is (= "answered" (:verdict (by-act store id "claude-3-turn-9-f-s1-0"))))
    (is (= "claude-3-turn-9-r-0" (:by (by-act store id "claude-3-turn-9-f-s1-0"))))))

(deftest absent-reply-paragraphs-falls-back-to-acts-and-is-typed
  (let [record {:turn_id "claude-3-turn-9"
                :operator_interpretation_acts
                [{:id "claude-3-turn-9-f-s1-0" :kind "delegate"
                  :author "operator" :text "do it"}]
                :acts [{:id "claude-3-turn-9-r-0" :kind "report"
                        :author "claude-3" :mark "㊢"
                        :target "claude-3-turn-9-f-s1-0" :text "done"}]}
        out (ma/classical-pass record)]
    (is (= "㊢" (get-in out [:entries 0 :mark])))
    (is (true? (:reply-paragraphs-absent out)))))

(deftest an-open-act-is-answered-by-the-model-with-a-verbatim-quote
  (let [[store id] (record!)
        asked (atom [])
        reply (str "[{\"act\": \"claude-3-turn-9-f-s1-1\", \"verdict\": \"deferred\", "
                   "\"by\": \"claude-3-turn-9-r-1\", "
                   "\"quote\": \"deferred to tomorrow\"}]")]
    (ma/align! {:store store
                :seat "馬-sonnet"
                :ask (fn [payload] (swap! asked conj payload) reply)}
               id)
    (is (= 1 (count @asked)) "one bell for all open acts")
    (is (str/includes? (:prompt (first @asked)) "claude-3-turn-9-f-s1-1"))
    (let [a (by-act store id "claude-3-turn-9-f-s1-1")]
      (is (= "deferred" (:verdict a)))
      (is (= "claude-3-turn-9-r-1" (:by a)))
      (is (= "deferred to tomorrow" (:quote a)))
      (is (= "🈝" (:mark a)) "the answering paragraph's mark, for model verdicts too")
      (is (= "model" (:method a))))
    (is (= 1 (:unread (alignment store id))))))

(deftest a-model-verdict-without-a-verbatim-quote-is-not-accepted
  (let [[store id] (record!)
        reply (str "[{\"act\": \"claude-3-turn-9-f-s1-1\", \"verdict\": \"answered\", "
                   "\"by\": \"claude-3-turn-9-r-1\", "
                   "\"quote\": \"words that appear nowhere in the reply\"}]")]
    (ma/align! {:store store :ask (constantly reply)} id)
    (let [a (by-act store id "claude-3-turn-9-f-s1-1")]
      (is (= "unread" (:verdict a)) "the reader does not certify itself")
      (is (= "quote not found" (:reason a)))
      ;; the other open act, unmentioned by the model, is unread too
      (is (= 2 (:unread (alignment store id)))))))

(deftest no-seat-or-silence-stores-the-classical-pass-with-seat-null
  (testing "no ask effect wired"
    (let [[store id] (record!)]
      (ma/align! {:store store} id)
      (let [al (alignment store id)]
        (is (nil? (:seat al)))
        ;; both operator acts are open and no seat is wired: both unread
        (is (= 2 (:unread al)))
        (is (= 2 (count (filter #(= "unread" (:verdict %)) (:acts al)))))
        (is (zero? (count (filter #(= "answered" (:verdict %)) (:acts al))))
            "the quote pass is absent here too — this record has no :target"))))
  (testing "the seat stays silent past the bound"
    (let [[store id] (record!)]
      (ma/align! {:store store :ask (constantly nil)} id)
      (let [al (alignment store id)]
        (is (nil? (:seat al)))
        (is (= 2 (:unread al)))))))

(deftest a-bell-turn-is-not-read
  (let [[store id] (record! {:origin "agent"})
        events (atom [])]
    (is (nil? (ma/align! {:store store :notify! #(swap! events conj %)} id)))
    (is (nil? (alignment store id)))
    (is (empty? @events))))

(deftest the-ported-event-triggers-the-reading-on-a-live-pipeline
  (let [store (temp-store)
        clock (atom 1000000)
        scheduled (atom [])
        s (svc/service {:store store
                        ;; hermetic: no 猴/mention/象-interpretation writes to a live store
                        :hou-enabled? false :mentions-enabled? false
                        :interpretations-enabled? false
                        :now-ms (fn [] @clock)
                        :schedule! (fn [delay thunk] (swap! scheduled conj [delay thunk]))
                        :evidence-async! (fn [thunk] (thunk))
                        :draft-async! (fn [thunk] (thunk))
                        :bell! (constantly {:ok false :status 0 :error "no seat"})
                        :job-status (constantly nil)})
        {:keys [id]} (svc/record-turn!
                             s {:text "Please continue. I withdraw that pattern."
                              :agent-id "claude-17" :session-id "sess-ma"
                              :turn-id "claude-17-turn-3" :evidence-id "emacs-ma"
                              :dispatch :now})
        [s1 s2] (:sentences (ts/read-record store id))
        analysis {:labeller "象-1"
                  :sentences [{:id "s1"
                               :fragments [{:start (:start s1) :end (:end s1)
                                            :text (:text s1)
                                            :intent "continue" :target "the work"
                                            :rationale "r" :relations ["action"]
                                            :display_cues [{:start 0 :end 15
                                                            :text "Please continue"}]}]}
                              {:id "s2"
                               :fragments [{:start (:start s2) :end (:end s2)
                                            :text (:text s2)
                                            :intent "withdraw" :target nil
                                            :rationale "r" :relations ["action"]
                                            :display_cues [] :no_surface_cue "implicit"}]}]}
        aligned (atom [])]
    (ma/start! {:store store :notify! #(swap! aligned conj %)})
    (try
      (svc/attach-happened! s id {:reply "㊢ did things exactly as asked" :commits []})
      (ts/update-record! store id #(assoc % :happened_summary "done"))
      (svc/publish-analysis! s id analysis {})
      (while (seq @scheduled)
        (let [[_ thunk] (first @scheduled)]
          (swap! scheduled subvec 1)
          (thunk)))
      ;; the reading runs on ma-reading's own thread: wait for it, bounded
      (loop [n 0]
        (when (and (< n 100) (nil? (:alignment (ts/read-record store id))))
          (Thread/sleep 20)
          (recur (inc n))))
      (is (map? (:alignment (ts/read-record store id)))
          "the :ported event ran the 馬 reading on the settled turn")
      (is (= 1 (count (filter #(= :aligned (:event %)) @aligned))))
      (finally (svc/remove-turn-listener! ::ma/ma-reading)))))

(deftest a-quote-must-be-real-words-from-the-reply
  ;; Review (claude-3): an empty quote is contained in every text, so it let a
  ;; model verdict through unchecked; and "answered" must say by which paragraph.
  (let [reply "I fixed the stepper display and added the summary line."]
    (is (ma/quote-holds? "answered" "fixed the stepper display" reply))
    (is (not (ma/quote-holds? "answered" "" reply)) "empty quote")
    (is (not (ma/quote-holds? "answered" nil reply)) "missing quote")
    (is (not (ma/quote-holds? "deferred" "the" reply)) "one word matches anything")
    (is (not (ma/quote-holds? "answered" "fixed the voxterm display" reply)) "not verbatim")
    (is (ma/quote-holds? "not-answered" nil reply) "not answered needs no quote")))

(deftest noted-is-a-verdict-and-needs-its-quote
  ;; Joe, 2026-10-19: answered, not answered or noted is for the reading to say.
  (let [[store id] (record!)
        ok (str "[{\"act\": \"claude-3-turn-9-f-s1-1\", \"verdict\": \"noted\", "
                "\"by\": \"claude-3-turn-9-r-1\", \"quote\": \"deferred to tomorrow\"}]")]
    (ma/align! {:store store :ask (constantly ok)} id)
    (is (= "noted" (:verdict (by-act store id "claude-3-turn-9-f-s1-1")))))
  (let [[store id] (record!)
        no-quote (str "[{\"act\": \"claude-3-turn-9-f-s1-1\", \"verdict\": \"noted\", "
                      "\"by\": null, \"quote\": \"\"}]")]
    (ma/align! {:store store :ask (constantly no-quote)} id)
    (is (= "unread" (:verdict (by-act store id "claude-3-turn-9-f-s1-1")))
        "noted without evidence is not accepted")))
