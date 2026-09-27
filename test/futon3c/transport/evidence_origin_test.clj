(ns futon3c.transport.evidence-origin-test
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [futon3c.transport.http :as http]
            [futon3c.evidence.origin :as origin]
            [futon3c.agency.clock-decision :as clock]))

(deftest emacs-origin-survives-the-real-http-normalizer
  (doseq [kind [:operator :agent :harness]]
    (let [db (atom {:entries {} :order []})
          stamp (:origin (origin/stamp {:author "joe"} {:kind kind :actor "p6o-test"} "emacs-test"))
          request {:request-method :post :uri "/api/alpha/evidence"
                   :body (json/generate-string
                          {:subject {:ref/type :session :ref/id "p6o-test"}
                           :type :coordination :claim-type :question :author "joe" :origin stamp
                           :body {:event "chat-turn" :role "user" :text "p6o test"}})}
          result (with-redefs [clock/record! (fn [& _] nil)]
                   ((http/make-handler {:evidence-store db}) request))
          entry (:entry (json/parse-string (:body result) true))]
      (is (= 201 (:status result)))
      (is (= (name kind) (get-in entry [:evidence/origin :kind])))
      (is (= "joe" (:evidence/author entry)))
      (is (= kind (get-in @db [:entries (:evidence/id entry) :evidence/origin :kind]))))))

(deftest unattributable-user-turn-is-recorded-without-a-clock-decision
  ;; The shape of the 31 records stranded in the Emacs outbox's failed/ until
  ;; 2026-09-27: a REPL awaiting its session, no turn-id, no agent-id, so the
  ;; clock cannot name an agent. The real clock/record! runs (no redef) and
  ;; throws clock/missing-identity; the turn must still be stored.
  (let [db (atom {:entries {} :order []})
        request {:request-method :post :uri "/api/alpha/evidence"
                 :body (json/generate-string
                        {:evidence-id "emacs-unattributable-turn"
                         :subject {:ref/type "session" :ref/id "claude-11 (awaiting session)"}
                         :type "coordination" :claim-type "question" :author "joe"
                         :session-id "claude-11 (awaiting session)"
                         :body {:event "chat-turn" :transport "emacs-claude-repl"
                                :role "user" :turn-id nil :text "hello"
                                :mission-id "M-autoclock-in"}})}
        result ((http/make-handler {:evidence-store db}) request)]
    (is (= 201 (:status result)) (:body result))
    (is (contains? (:entries @db) "emacs-unattributable-turn"))))
