(ns futon3c.logic.disposition-edn-test
  "Tests for the .futon-disposition.edn parser + :review-by self-decay
   rule. Covers ARGUE-2's schema and BECAUSE clause about stale
   in-progress entries becoming drain symptoms.

   Mission: M-bounded-in-flight-state INSTANTIATE Block 3."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.logic.disposition-edn :as disposition])
  (:import [java.io File]
           [java.time LocalDate]
           [java.nio.file Files]))

(defn- write-temp-disposition!
  "Create a temp dir, write `.futon-disposition.edn` into it with EDN-CONTENT,
   return the dir as a File."
  ^File [edn-content]
  (let [dir (.toFile (Files/createTempDirectory "fb-disposition-" (into-array java.nio.file.attribute.FileAttribute [])))
        f (java.io.File. dir disposition/disposition-file-name)]
    (spit f edn-content)
    dir))

(deftest load-returns-nil-for-missing-file
  (let [dir (.toFile (Files/createTempDirectory "fb-no-disp-" (into-array java.nio.file.attribute.FileAttribute [])))]
    (is (nil? (disposition/load-state (.getPath dir))))))

(deftest load-parses-minimal-schema
  (let [dir (write-temp-disposition!
             (str "{:in-progress {\"src/futon3c/dev.clj\" "
                  "  {:reasoning \"WS-bridge fix interleaved with WIP\" "
                  "   :since \"2026-05-02\" "
                  "   :review-by \"2026-05-17\"}} "
                  " :decided-at \"2026-05-03T19:13:00Z\"}"))
        state (disposition/load-state (.getPath dir))]
    (is (map? state))
    (is (= "2026-05-03T19:13:00Z" (:decided-at state)))
    (is (contains? (:in-progress state) "src/futon3c/dev.clj"))))

(deftest active-true-when-glob-matches-and-review-by-future
  (let [dir (write-temp-disposition!
             (str "{:in-progress {\"src/**/*.clj\" "
                  "  {:reasoning \"big refactor\" "
                  "   :since \"2026-05-01\" "
                  "   :review-by \"2026-12-31\"}}}"))
        state (disposition/load-state (.getPath dir))
        now (LocalDate/parse "2026-05-03")]
    (is (true? (disposition/active? state "src/futon3c/dev.clj" now)))
    (is (true? (disposition/active? state "src/futon3c/logic/probe.clj" now)))
    (is (false? (disposition/active? state "test/futon3c/dev_test.clj" now))
        "non-matching path returns false")))

(deftest active-false-when-review-by-past
  (let [dir (write-temp-disposition!
             (str "{:in-progress {\"src/**/*.clj\" "
                  "  {:reasoning \"forgotten refactor\" "
                  "   :since \"2026-04-01\" "
                  "   :review-by \"2026-04-15\"}}}"))
        state (disposition/load-state (.getPath dir))
        now (LocalDate/parse "2026-05-03")]
    (is (false? (disposition/active? state "src/futon3c/dev.clj" now))
        "review-by 2026-04-15 < now 2026-05-03; entry self-decayed")))

(deftest active-true-for-event-form-review-by
  (let [dir (write-temp-disposition!
             (str "{:in-progress {\"web/\" "
                  "  {:reasoning \"war-machine UI source tree\" "
                  "   :since \"2026-05-02\" "
                  "   :review-by \"M-single-entry-point INSTANTIATE complete\"}}}"))
        state (disposition/load-state (.getPath dir))
        now (LocalDate/parse "2026-05-03")]
    (is (true? (disposition/active? state "web/war-machine/src/foo.cljs" now))
        "event-form review-by is treated as live until evaluator is wired")))

(deftest self-decay-after-30-days-without-review-by
  (testing "Entry with :since but no :review-by self-decays after 30 days"
    (let [dir (write-temp-disposition!
               (str "{:in-progress {\"old-stuff/\" "
                    "  {:reasoning \"meant to come back to this\" "
                    "   :since \"2026-04-01\"}}}"))
          state (disposition/load-state (.getPath dir))]
      (is (true? (disposition/active? state "old-stuff/foo.clj"
                                       (LocalDate/parse "2026-04-15")))
          "15 days in: still active")
      (is (false? (disposition/active? state "old-stuff/foo.clj"
                                        (LocalDate/parse "2026-05-15")))
          "44 days in: self-decayed (>30 days)")))
  (testing "Self-decayed paths show up in self-decayed-paths"
    (let [dir (write-temp-disposition!
               (str "{:in-progress {\"old-stuff/\" "
                    "  {:reasoning \"...\" :since \"2026-04-01\"} "
                    " \"recent/\" "
                    "  {:reasoning \"...\" :since \"2026-05-01\"}}}"))
          state (disposition/load-state (.getPath dir))
          decayed (disposition/self-decayed-paths state (LocalDate/parse "2026-05-15"))]
      (is (= ["old-stuff/"] decayed)))))

(deftest summary-shape
  (let [dir (write-temp-disposition!
             (str "{:in-progress {\"a/\" {:reasoning \"\" :since \"2026-04-01\"} "
                  "                \"b/\" {:reasoning \"\" :since \"2026-05-01\"}} "
                  " :decided-at \"2026-05-03T19:13:00Z\"}"))
        state (disposition/load-state (.getPath dir))
        s (disposition/summary state (LocalDate/parse "2026-05-15"))]
    (is (= 2 (:in-progress-count s)))
    (is (= 1 (:self-decayed-count s)))   ; "a/" decayed (>30d), "b/" still live
    (is (= "2026-05-03T19:13:00Z" (:decided-at s)))))

(deftest malformed-edn-returns-nil
  (let [dir (write-temp-disposition! "{:in-progress {malformed")]
    (is (nil? (disposition/load-state (.getPath dir)))
        "garbage EDN does not throw; returns nil")))
