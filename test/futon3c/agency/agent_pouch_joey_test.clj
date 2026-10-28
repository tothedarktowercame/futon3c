(ns futon3c.agency.agent-pouch-joey-test
  "Joey gate: a pouch warms only a small session; a monster stays cold unless
   overridden. These exercise the policy predicate without spawning any process."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.agent-pouch :as pouch])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest joey-eligible-by-size
  (with-redefs [pouch/joey-max-tokens (constantly 1000)]
    (testing "small context → warm (joey)"
      (with-redefs [pouch/session-context-tokens (constantly 500)]
        (is (true? (boolean (pouch/joey-eligible? "a" "s1"))))))
    (testing "at the threshold → still a joey (<=)"
      (with-redefs [pouch/session-context-tokens (constantly 1000)]
        (is (true? (boolean (pouch/joey-eligible? "a" "s1"))))))
    (testing "over the threshold → monster, stays cold"
      (with-redefs [pouch/session-context-tokens (constantly 5000)]
        (is (false? (boolean (pouch/joey-eligible? "a" "s1"))))))
    (testing "unknown size (fresh session) → fail open to warm"
      (with-redefs [pouch/session-context-tokens (constantly nil)]
        (is (true? (boolean (pouch/joey-eligible? "a" "s1"))))))))

(deftest monster-override-warms-anyway
  (with-redefs [pouch/joey-max-tokens (constantly 1000)
                pouch/session-context-tokens (constantly 5000)]
    (is (false? (boolean (pouch/joey-eligible? "mon-1" "s")))
        "monster cold without override")
    (pouch/allow-monster! "mon-1")
    (try
      (is (true? (boolean (pouch/joey-eligible? "mon-1" "s")))
          "explicit override warms the monster")
      (is (false? (boolean (pouch/joey-eligible? "other" "s")))
          "override is per-agent, not global")
      (finally (pouch/disallow-monster! "mon-1")))
    (is (false? (boolean (pouch/joey-eligible? "mon-1" "s")))
        "override cleared")))

(deftest transcript-bytes-safe-on-missing
  (is (nil? (pouch/session-transcript-bytes "definitely-not-a-real-session-xyz")))
  (is (nil? (pouch/session-transcript-bytes nil)))
  (is (nil? (pouch/session-transcript-bytes ""))))

(deftest env-monster-allowlist-is-durable-override
  (with-redefs [pouch/joey-max-tokens (constantly 1000)
                pouch/session-context-tokens (constantly 5000)]
    (let [old (System/getProperty "FUTON3C_KANGAROO_MONSTER_ALLOWLIST")]
      (try
        (System/setProperty "FUTON3C_KANGAROO_MONSTER_ALLOWLIST" "claude-3, fable-1")
        (is (true? (boolean (pouch/joey-eligible? "claude-3" "s"))))
        (is (true? (boolean (pouch/joey-eligible? "fable-1" "s")))
            "whitespace around ids is tolerated")
        (is (false? (boolean (pouch/joey-eligible? "claude-4" "s")))
            "allowlist is per-agent, not global")
        (finally
          (if old
            (System/setProperty "FUTON3C_KANGAROO_MONSTER_ALLOWLIST" old)
            (System/clearProperty "FUTON3C_KANGAROO_MONSTER_ALLOWLIST")))))))

(deftest joey-max-tokens-reads-property
  (let [old (System/getProperty "FUTON3C_KANGAROO_JOEY_MAX_TOKENS")]
    (try
      (System/setProperty "FUTON3C_KANGAROO_JOEY_MAX_TOKENS" "12345")
      (is (= 12345 (pouch/joey-max-tokens)))
      (finally
        (if old
          (System/setProperty "FUTON3C_KANGAROO_JOEY_MAX_TOKENS" old)
          (System/clearProperty "FUTON3C_KANGAROO_JOEY_MAX_TOKENS"))))))

(deftest usage-context-tokens-sums-prompt-and-reply
  (is (= 47393 (pouch/usage-context-tokens {:input_tokens 3
                                             :cache_read_input_tokens 46000
                                             :cache_creation_input_tokens 1200
                                             :output_tokens 190})))
  (is (nil? (pouch/usage-context-tokens {:service_tier "standard"}))))

(defn- with-transcript
  "Write LINES as session SID's transcript under a throwaway user.home, call F."
  [sid lines f]
  (let [home (.toFile (Files/createTempDirectory "joey-home" (make-array FileAttribute 0)))
        dir (io/file home ".claude" "projects" "-proj")
        old (System/getProperty "user.home")]
    (.mkdirs dir)
    (spit (io/file dir (str sid ".jsonl"))
          (apply str (map #(str (json/generate-string %) "\n") lines)))
    (try
      (System/setProperty "user.home" (str home))
      (f)
      (finally (System/setProperty "user.home" old)))))

(deftest context-tokens-come-from-the-last-reply-not-the-file-size
  ;; The 2026-10-28 shape: Claude Code >= 2.1.300 logs each API request into the
  ;; transcript, so a few KB of conversation sit in front of MBs of request log.
  (let [blob {:type "api-request-blob" :sessionId "s" :hash "h"
              :message {:role "user" :content (apply str (repeat 4096 "x"))}}
        lines (concat
               [{:type "user" :uuid "u1" :message {:role "user" :content "hi"}}
                {:type "assistant" :uuid "a1" :parentUuid "u1"
                 :message {:role "assistant" :content [{:type "text" :text "old"}]
                           :usage {:input_tokens 900000}}}
                {:type "user" :uuid "u2" :parentUuid "a1"
                 :message {:role "user" :content "again"}}
                {:type "assistant" :uuid "a2" :parentUuid "u2"
                 :message {:role "assistant" :content [{:type "text" :text "ok"}]
                           :usage {:input_tokens 3
                                   :cache_read_input_tokens 46000
                                   :cache_creation_input_tokens 1200
                                   :output_tokens 190}}}]
               (repeat 1200 blob))]
    (with-transcript "s-blobs" lines
      (fn []
        (is (> (pouch/session-transcript-bytes "s-blobs") (* 4 1024 1024))
            "the request log alone puts the file past the old 2 MiB limit")
        (is (= 47393 (pouch/session-context-tokens "s-blobs"))
            "the newest reply is found behind 4 MB of log, and the older reply is not used")
        (with-redefs [pouch/joey-max-tokens (constantly 200000)]
          (is (true? (boolean (pouch/joey-eligible? "a" "s-blobs")))
              "a 47k-token session is a joey whatever its file size"))))
    (with-transcript "s-fresh" [{:type "user" :uuid "u1" :message {:role "user" :content "hi"}}]
      (fn []
        (is (nil? (pouch/session-context-tokens "s-fresh"))
            "no reply yet: unknown, which the gate treats as a joey")))))
