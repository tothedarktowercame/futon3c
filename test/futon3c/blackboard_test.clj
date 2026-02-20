(ns futon3c.blackboard-test
  "Tests for the blackboard projection system.

   Tests the render-blackboard adaptors (pure functions) and the
   blackboard! primitive (mocked — we don't require a running Emacs
   in the test suite)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.blackboard :as bb]))

;; =============================================================================
;; render-blackboard — pure rendering tests
;; =============================================================================

(deftest default-returns-nil
  (testing "Unknown peripheral returns nil"
    (is (nil? (bb/render-blackboard :nonexistent {:some "state"})))))

;; -----------------------------------------------------------------------------
;; :mission adaptor
;; -----------------------------------------------------------------------------

(deftest mission-basic-rendering
  (testing "Mission peripheral renders phase and cycle info"
    (let [state {:mission-id "M-test"
                 :current-phase :execute
                 :cycles-completed 2
                 :steps []}
          result (bb/render-blackboard :mission state)]
      (is (string? result))
      (is (str/includes? result "M-test"))
      (is (str/includes? result "execute"))
      (is (str/includes? result "Cycles: 2")))))

(deftest mission-with-obligations
  (testing "Mission peripheral renders obligations from loaded state"
    (let [state {:mission-id "M-evidence-parity"
                 :current-phase :validate
                 :cycles-completed 1
                 :steps [{:tool :mission-load
                          :args []
                          :result {:mission/obligations
                                   {"O-1" {:item/id "O-1"
                                           :item/label "HttpBackend query filters"
                                           :item/status :done
                                           :item/depends-on #{}}
                                    "O-2" {:item/id "O-2"
                                           :item/label "Tag/since/limit forwarding"
                                           :item/status :partial
                                           :item/depends-on #{"O-1"}}
                                    "O-3" {:item/id "O-3"
                                           :item/label "Integration test"
                                           :item/status :open
                                           :item/depends-on #{"O-2"}}}}}]}
          result (bb/render-blackboard :mission state)]
      (is (str/includes? result "Obligations:"))
      (is (str/includes? result "done"))
      (is (str/includes? result "partial"))
      (is (str/includes? result "open"))
      (is (str/includes? result "HttpBackend query filters")))))

(deftest mission-setup-phase
  (testing "Mission peripheral handles nil phase (setup)"
    (let [state {:mission-id "M-new"
                 :current-phase nil
                 :cycles-completed 0
                 :steps []}
          result (bb/render-blackboard :mission state)]
      (is (string? result))
      (is (str/includes? result "setup")))))

;; -----------------------------------------------------------------------------
;; :mission-control adaptor
;; -----------------------------------------------------------------------------

(deftest mission-control-basic-rendering
  (testing "Mission control renders step count"
    (let [state {:steps [{:tool :mc-inventory :args [] :result {}}]
                 :latest-review nil}
          result (bb/render-blackboard :mission-control state)]
      (is (string? result))
      (is (str/includes? result "Mission Control"))
      (is (str/includes? result "Steps: 1")))))

(deftest mission-control-with-review
  (testing "Mission control renders portfolio review"
    (let [state {:steps []
                 :latest-review
                 {:portfolio/missions
                  {"M-1" {:mission/status :complete}
                   "M-2" {:mission/status :in-progress}
                   "M-3" {:mission/status :blocked}}
                  :portfolio/summary "3 missions scanned"}}
          result (bb/render-blackboard :mission-control state)]
      (is (str/includes? result "Missions: 3"))
      (is (str/includes? result "1 complete"))
      (is (str/includes? result "1 in-progress"))
      (is (str/includes? result "1 blocked"))
      (is (str/includes? result "3 missions scanned")))))

;; -----------------------------------------------------------------------------
;; :alfworld adaptor
;; -----------------------------------------------------------------------------

(deftest alfworld-basic-rendering
  (testing "ALFWorld renders game state"
    (let [state {:task-description "heat egg"
                 :step-count 5
                 :last-observation "You see a fridge 1."
                 :bells-sent [{:to "joe" :message "help"}]
                 :whistles-sent []}
          result (bb/render-blackboard :alfworld state)]
      (is (string? result))
      (is (str/includes? result "ALFWorld"))
      (is (str/includes? result "heat egg"))
      (is (str/includes? result "Steps: 5"))
      (is (str/includes? result "fridge 1"))
      (is (str/includes? result "Bells: 1")))))

(deftest alfworld-truncates-long-observation
  (testing "ALFWorld truncates long observations"
    (let [long-obs (apply str (repeat 100 "x"))
          state {:task-description "task"
                 :step-count 0
                 :last-observation long-obs
                 :bells-sent []
                 :whistles-sent []}
          result (bb/render-blackboard :alfworld state)]
      (is (str/includes? result "...")))))

(deftest alfworld-shows-score-and-status
  (testing "ALFWorld renders score and win status from alfworld-state"
    (let [state {:alfworld-state {:task "put vase in safe"
                                  :observation "You put the vase in the safe."
                                  :score 1.0
                                  :done true
                                  :won true}
                 :step-count 6
                 :bells-sent []
                 :whistles-sent []}
          result (bb/render-blackboard :alfworld state)]
      (is (str/includes? result "put vase in safe"))
      (is (str/includes? result "Score: 1.0"))
      (is (str/includes? result "WON"))
      (is (str/includes? result "vase in the safe")))))

;; =============================================================================
;; blackboard! primitive — elisp construction
;; =============================================================================

(deftest escape-elisp-string-handles-special-chars
  (testing "Escape function handles backslashes, quotes, newlines"
    (let [escape #'futon3c.blackboard/escape-elisp-string]
      (is (= "hello" (escape "hello")))
      (is (= "line1\\nline2" (escape "line1\nline2")))
      (is (= "say \\\"hi\\\"" (escape "say \"hi\"")))
      (is (= "back\\\\slash" (escape "back\\slash"))))))

;; =============================================================================
;; project! — integration of render + blackboard!
;; =============================================================================

(deftest project-skips-nil-render
  (testing "project! returns nil when render-blackboard returns nil"
    ;; :default returns nil, so project! should be a no-op
    (is (nil? (bb/project! :nonexistent {:any "state"})))))

;; Note: project! with a real peripheral-id would call emacsclient,
;; which we don't want in the test suite. The render tests above
;; validate the content; the emacsclient integration is tested manually.
