(ns futon3c.peripheral.emacs-cursor-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.registry :as reg]
            [futon3c.evidence.store :as store]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.emacs-cursor :as emacs-cursor]
            [futon3c.peripheral.runner :as runner]
            [futon3c.peripheral.tools :as tools]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.peripheral-events :as peripheral-events]))

(deftest emacs-cursor-spec-loads-from-resources
  (testing "load-spec returns the registered :emacs-cursor spec"
    (let [spec (common/load-spec :emacs-cursor)]
      (is (= :emacs-cursor (:peripheral/id spec)))
      (is (= #{:cursor-context :cursor-mode :cursor-focus :cursor-minibuffer}
             (:peripheral/tools spec))))))

(deftest emacs-cursor-start-produces-default-follow-state
  (let [p (emacs-cursor/make-emacs-cursor (tools/make-mock-backend))
        start (runner/start p {:session-id "sess-cursor-1"
                               :agent-id "claude-1"
                               :editor-id "emacs-main"})]
    (is (:ok start))
    (fix/assert-valid! shapes/EvidenceEntry (:evidence start))
    (is (= :follow (get-in start [:state :mode])))
    (is (= "claude-1" (get-in start [:state :agent-id])))
    (is (= "emacs-main" (get-in start [:state :editor-id])))))

(deftest emacs-cursor-context-step-updates-state-and-emits-event
  (let [sent (atom [])
        projected (atom [])
        p (emacs-cursor/make-emacs-cursor (tools/make-mock-backend))]
    (with-redefs [peripheral-events/send-peripheral-event!
                  (fn [agent-id peripheral-id event payload]
                    (swap! sent conj {:agent-id agent-id
                                      :peripheral-id peripheral-id
                                      :event event
                                      :payload payload})
                    true)
                  reg/report-surface-projection!
                  (fn [agent-id source projection]
                    (swap! projected conj {:agent-id agent-id
                                           :source source
                                           :projection projection})
                    {:ok true})]
      (let [start (runner/start p {:session-id "sess-cursor-2"
                                   :agent-id "claude-1"
                                   :editor-id "emacs-main"})
            step (runner/step p (:state start)
                              {:tool :cursor-context
                               :args [{:buffer {:name "foo.clj"
                                                :file "/tmp/foo.clj"
                                                :major-mode "clojure-mode"
                                                :visible {:text "(ns foo.core)\n(def x 1)"}}
                                       :user-cursor {:point 42
                                                     :line 3
                                                     :column 1}
                                       :agent-cursor {:point 1
                                                      :line 1
                                                      :column 1}}]})]
        (is (:ok step))
        (is (= "foo.clj" (get-in step [:state :latest-context :buffer :name])))
        (is (= 1 (get-in step [:state :events-emitted])))
        (is (= :step (get-in step [:evidence :evidence/claim-type])))
        (is (= {:agent-id "claude-1"
                :source "emacs-cursor:emacs-main"}
               (select-keys (last @projected) [:agent-id :source])))
        (is (= "emacs-cursor" (get-in (last @projected) [:projection :surface])))
        (is (= "minibuffer"
               (get-in (last @projected) [:projection :write-surface])))
        (is (= [{:agent-id "claude-1"
                 :peripheral-id :emacs-cursor
                 :event :cursor-state
                 :payload {:mode "follow"
                           :editor-id "emacs-main"
                           :focus nil
                           :buffer-surface {:mode "follow"
                                            :editor-id "emacs-main"
                                            :buffer {:name "foo.clj"
                                                     :file "/tmp/foo.clj"
                                                     :major-mode "clojure-mode"
                                                     :visible {:text "(ns foo.core)\n(def x 1)"}}
                                            :user-cursor {:point 42
                                                          :line 3
                                                          :column 1}
                                            :agent-cursor {:point 1
                                                           :line 1
                                                           :column 1}
                                            :focus nil
                                            :debug {}
                                            :visible-preview "(ns foo.core)\n(def x 1)"}
                            :minibuffer-surface {}
                            :buffer {:name "foo.clj"
                                     :file "/tmp/foo.clj"
                                     :major-mode "clojure-mode"
                                     :visible {:text "(ns foo.core)\n(def x 1)"}}
                            :user-cursor {:point 42
                                          :line 3
                                          :column 1}
                            :agent-cursor {:point 1
                                           :line 1
                                           :column 1}}}]
               @sent))))))

(deftest emacs-cursor-mode-validation-and-focus
  (let [sent (atom 0)
        p (emacs-cursor/make-emacs-cursor (tools/make-mock-backend))]
    (with-redefs [peripheral-events/send-peripheral-event!
                  (fn [_agent-id _peripheral-id _event _payload]
                    (swap! sent inc)
                    true)]
      (let [start (runner/start p {:session-id "sess-cursor-3" :agent-id "claude-1"})
            bad (runner/step p (:state start) {:tool :cursor-mode :args ["pair"]})
            mode-step (runner/step p (:state start) {:tool :cursor-mode :args ["observe"]})
            focus-step (runner/step p (:state mode-step)
                                    {:tool :cursor-focus
                                     :args [{:buffer "bar.clj"
                                             :point 7
                                             :mark 12}]})]
        (fix/assert-valid! shapes/SocialError bad)
        (is (= :invalid-cursor-mode (:error/code bad)))
        (is (:ok mode-step))
        (is (= :observe (get-in mode-step [:state :mode])))
        (is (:ok focus-step))
        (is (= {:buffer "bar.clj" :point 7 :mark 12}
               (get-in focus-step [:state :focus])))
        (is (= 2 @sent))))))

(deftest emacs-cursor-minibuffer-step-updates-surface
  (let [sent (atom [])
        p (emacs-cursor/make-emacs-cursor (tools/make-mock-backend))]
    (with-redefs [peripheral-events/send-peripheral-event!
                  (fn [_agent-id _peripheral-id _event payload]
                    (swap! sent conj payload)
                    true)]
      (let [start (runner/start p {:session-id "sess-cursor-3b" :agent-id "claude-1"})
            step (runner/step p (:state start)
                              {:tool :cursor-minibuffer
                               :args [{:request-id "req-1"
                                       :command "echo"
                                       :response "hello"}]})]
        (is (:ok step))
        (is (= {:request-id "req-1"
                :command "echo"
                :response "hello"}
               (get-in step [:state :latest-minibuffer])))
        (is (= {:request-id "req-1"
                :command "echo"
                :response "hello"}
               (:minibuffer-surface (first @sent))))))))

(deftest emacs-cursor-stop-returns-fruit-and-appends-evidence
  (let [evidence-store (atom {:entries {} :order []})
        p (emacs-cursor/make-emacs-cursor (tools/make-mock-backend))]
    (with-redefs [peripheral-events/send-peripheral-event! (fn [& _] false)
                  reg/report-surface-projection! (fn [& _] {:ok true})
                  reg/clear-surface-projection!
                  (fn [agent-id source]
                    {:ok true :agent-id agent-id :source source})]
      (let [start (runner/start p {:session-id "sess-cursor-4"
                                   :agent-id "claude-1"
                                   :evidence-store evidence-store})
            step (runner/step p (:state start)
                              {:tool :cursor-context
                               :args [{:buffer {:name "baz.clj"} :user-cursor {:point 9}}]})
            stop (runner/stop p (:state step) "done")
            entries (store/query* evidence-store {})]
        (is (:ok stop))
        (is (= :conclusion (get-in stop [:evidence :evidence/claim-type])))
        (is (= {:session-id "sess-cursor-4"
                :agent-id "claude-1"
                :editor-id "sess-cursor-4"}
               (:context stop)))
        (is (= "baz.clj" (get-in stop [:fruit :last-buffer])))
        (is (= 1 (get-in stop [:fruit :updates-processed])))
        (is (= 0 (get-in stop [:fruit :events-emitted])))
        (is (map? (get-in stop [:fruit :buffer-surface])))
        (is (map? (get-in stop [:fruit :minibuffer-surface])))
        (is (= 3 (count entries)))))))
