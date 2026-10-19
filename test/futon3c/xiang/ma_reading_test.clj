(ns futon3c.xiang.ma-reading-test
  "The 馬 alignment reading: classical quote pass, bounded model pass whose
  verdicts are accepted only with a verbatim quote, the seat-absent fallback,
  and bell turns left unread. Run:
  FUTON3C_INVOKE_JOBS_FILE=$(mktemp -d)/i.edn clojure -M:test -n futon3c.xiang.ma-reading-test"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.xiang.ma-reading :as ma]
            [futon3c.xiang.turn-service :as svc]
            [futon3c.xiang.turn-store :as ts]))

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
              :ports_status {:state "computed"}
              :acts [{:id "claude-3-turn-9-f-s1-0" :kind "delegate" :author "operator"
                      :text "do the thing"}
                     {:id "claude-3-turn-9-f-s1-1" :kind "explain" :author "operator"
                      :text "why it matters"}
                     {:id "claude-3-turn-9-r-0" :kind "report" :author "claude-3"
                      :text "did the thing exactly as asked."}]}
        {:keys [id]} (ts/write-record! store (merge base over))]
    [store id])))

(defn- alignment [store id] (:alignment (ts/read-record store id)))

(defn- by-act [store id act]
  (some #(when (= act (:act %)) %) (:acts (alignment store id))))

(deftest a-targeted-reply-act-answers-classically-without-a-bell
  (let [[store id] (record! {:acts [{:id "claude-3-turn-9-f-s1-0" :kind "delegate"
                                     :author "operator" :text "do the thing"}
                                    {:id "claude-3-turn-9-f-s1-1" :kind "explain"
                                     :author "operator" :text "why it matters"}
                                    ;; quotes the operator's words: classical
                                    {:id "claude-3-turn-9-r-0" :kind "report"
                                     :author "claude-3" :target "claude-3-turn-9-f-s1-0"
                                     :text "\"do the thing\" — done"}
                                    {:id "claude-3-turn-9-r-1" :kind "explain"
                                     :author "claude-3" :target "claude-3-turn-9-f-s1-1"
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
      (is (= "quote" (:method a))))
    (is (= [{:event :aligned :id id}] @events))))

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
