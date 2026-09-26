(ns futon3c.agency.clock-store-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.agency.clock-store :as clock-store]))

(use-fixtures :each
  (fn [f]
    (clock-store/reset-store!)
    (f)
    (clock-store/reset-store!)))

(defn- temp-doc!
  [root rel]
  (let [file (io/file root rel)]
    (.mkdirs (.getParentFile file))
    (spit file "# test\n")
    (.getCanonicalPath file)))

(deftest repeated-agent-edits-switch-session-clock
  (testing "three edits to the same witnessed mission doc switch the session"
    (let [root (doto (java.io.File/createTempFile "futon3c-clock-" "")
                 (.delete))
          path (temp-doc! root "holes/missions/M-foo.md")
          opts {:now-ms 1000}]
      (clock-store/record-edit! "claude-1" "sid" path opts)
      (clock-store/record-edit! "claude-1" "sid" path (assoc opts :now-ms 2000))
      (let [{:keys [state]} (clock-store/record-edit! "claude-1" "sid" path
                                                       (assoc opts :now-ms 3000))]
        (is (= {:campaign-id nil :mission-id "M-foo" :excursion-id nil}
               (:clock state)))
        (is (= "agent-edit-activity"
               (get-in state [:last-auto-clock-witness :rule])))
        (is (= 3 (get-in state [:last-auto-clock-witness :edit-count])))))))

(deftest one-agent-edit-does-not-switch
  (testing "a single stray edit is not enough evidence"
    (let [root (doto (java.io.File/createTempFile "futon3c-clock-" "")
                 (.delete))
          path (temp-doc! root "holes/missions/M-foo.md")]
      (clock-store/record-edit! "claude-1" "sid" path {:now-ms 1000})
      (is (= {:campaign-id nil :mission-id nil :excursion-id nil}
             (clock-store/current-clock "claude-1" "sid"))))))

(deftest alternating-agent-edits-do-not-thrash
  (testing "dominance margin blocks alternating target churn"
    (let [root (doto (java.io.File/createTempFile "futon3c-clock-" "")
                 (.delete))
          foo (temp-doc! root "holes/missions/M-foo.md")
          bar (temp-doc! root "holes/missions/M-bar.md")]
      (doseq [[idx path] (map-indexed vector [foo bar foo bar foo])]
        (clock-store/record-edit! "claude-1" "sid" path {:now-ms (* 1000 (inc idx))}))
      (is (= {:campaign-id nil :mission-id nil :excursion-id nil}
             (clock-store/current-clock "claude-1" "sid"))))))

(deftest mission-edit-preserves-single-active-clock
  (testing "M-* edit switches to mission only and clears other active levels"
    (let [root (doto (java.io.File/createTempFile "futon3c-clock-" "")
                 (.delete))
          mission-path (temp-doc! root "holes/missions/M-foo.md")
          campaign-path (temp-doc! root "holes/campaigns/C-old.md")]
      (dotimes [idx 3]
        (clock-store/record-edit! "claude-1" "sid" campaign-path
                                  {:now-ms (* 1000 (inc idx))}))
      (is (= {:campaign-id "C-old" :mission-id nil :excursion-id nil}
             (clock-store/current-clock "claude-1" "sid")))
      (doseq [idx (range 3)]
        (clock-store/record-edit! "claude-1" "sid" mission-path
                                  {:now-ms (+ 700000 (* 1000 (inc idx)))}))
      (is (= {:campaign-id nil :mission-id "M-foo" :excursion-id nil}
             (clock-store/current-clock "claude-1" "sid"))))))

(deftest dispatch-mission-id-sets-session-clock
  (testing "secondary dispatch signal clocks the session immediately"
    (clock-store/set-dispatch-mission! "claude-1" "sid" "M-dispatched")
    (is (= {:campaign-id nil :mission-id "M-dispatched" :excursion-id nil}
           (clock-store/current-clock "claude-1" "sid")))
    (is (= {"mission-id" "M-dispatched"
            "clocked-mission" "M-dispatched"
            "auto-clock-witness" {:rule "dispatch-mission-id"
                                  :source "invoke-receipt"
                                  :old-target "no mission"
                                  :new-target "M-dispatched"}}
           (clock-store/evidence-clock-fields "claude-1" "sid")))))

(deftest dispatch-target-id-normalization
  (testing "typed dispatch ids stay typed; bare ids remain mission shorthand"
    (clock-store/set-dispatch-mission! "claude-1" "excursion" "E-evidence-flow")
    (is (= {:campaign-id nil :mission-id nil :excursion-id "E-evidence-flow"}
           (clock-store/current-clock "claude-1" "excursion")))

    (clock-store/set-dispatch-mission! "claude-1" "campaign" "C-cascade-real")
    (is (= {:campaign-id "C-cascade-real" :mission-id nil :excursion-id nil}
           (clock-store/current-clock "claude-1" "campaign")))

    (clock-store/set-dispatch-mission! "claude-1" "bare" "live-efe-map")
    (is (= {:campaign-id nil :mission-id "M-live-efe-map" :excursion-id nil}
           (clock-store/current-clock "claude-1" "bare")))

    (clock-store/set-dispatch-mission! "claude-1" "mission" "M-live-efe-map")
    (is (= {:campaign-id nil :mission-id "M-live-efe-map" :excursion-id nil}
           (clock-store/current-clock "claude-1" "mission")))))

(deftest tool-use-recording-is-edit-tool-and-file-path-only
  (testing "non-edit tools and missing paths are ignored"
    (let [root (doto (java.io.File/createTempFile "futon3c-clock-" "")
                 (.delete))
          path (temp-doc! root "holes/missions/M-foo.md")]
      (is (nil? (clock-store/record-tool-use! "claude-1" "sid"
                                              {:name "Read"
                                               :input {:file_path path}})))
      (is (nil? (clock-store/record-tool-use! "claude-1" "sid"
                                              {:name "Edit"
                                               :input {:old_string "x"}})))
      (is (some? (clock-store/record-tool-use! "claude-1" "sid"
                                               {:name "Edit"
                                                :input {:file_path path}}
                                               {:now-ms 1000}))))))
