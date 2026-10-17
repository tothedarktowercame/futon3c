(ns futon3c.agency.turn-events-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.turn-events :as te]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.evidence.backend :as backend]))

(def final-reply
  {:evidence/id "e-final" :evidence/session-id "s1" :evidence/author "claude-3"
   :evidence/type :coordination :evidence/claim-type :observation
   :evidence/subject {:ref/type :session :ref/id "s1"} :evidence/at "2026-10-17T22:00:00Z"
   :evidence/tags [:chat-turn]
   :evidence/body {:event "chat-turn" :role "assistant" :segment-final true :unified-text "done"}})

(deftest which-rows-finalise-a-turn
  (testing "Emacs's final reply segment"
    (is (= :reply (:kind (te/final-turn final-reply)))))
  (testing "an interim segment is not final (the bad case: 馬 observing a half-written turn)"
    (is (nil? (te/final-turn (assoc-in final-reply [:evidence/body :segment-final] false)))))
  (testing "a Zai turn's final round, including a truncated one that stopped"
    (is (= :harness-turn (:kind (te/final-turn {:evidence/body {:event "turn-round" :final true}}))))
    (is (= :harness-turn (:kind (te/final-turn {:body {"event" "turn-round" "final" true "stop" "max-tool-rounds"}})))))
  (testing "a mid-turn round is not"
    (is (nil? (te/final-turn {:evidence/body {:event "turn-round" :final false}}))))
  (testing "user turns and other rows are not"
    (is (nil? (te/final-turn (assoc-in final-reply [:evidence/body :role] "user"))))
    (is (nil? (te/final-turn {:evidence/body "edn string"})))))

(deftest boundary-raises-pre-before-and-post-after-the-append
  (let [seen (atom []) done (promise)
        b (backend/->AtomBackend (atom {:entries {} :order []}))]
    (te/add-listener! ::pre :pre (fn [ev] (swap! seen conj [:pre (:phase ev) (count (:order @(:!store b)))])))
    (te/add-listener! ::post :post (fn [ev] (swap! seen conj [:post (:evidence-id ev)]) (deliver done true)))
    (te/add-listener! ::boom :pre (fn [_] (throw (ex-info "listener bug" {}))))
    (try
      (let [r (boundary/append! b final-reply)]
        (is (:ok r) "a failing listener never blocks the append")
        (deref done 2000 nil)
        (is (= [[:pre :pre 0] [:post "e-final"]] @seen) ":pre sees the store before the row, :post after"))
      (finally
        (te/remove-listener! ::pre) (te/remove-listener! ::post) (te/remove-listener! ::boom)))))
