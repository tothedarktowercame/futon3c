(ns futon3c.agency.agent-pouch-joey-test
  "Joey gate: a pouch warms only a small session; a monster stays cold unless
   overridden. These exercise the policy predicate without spawning any process."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.agent-pouch :as pouch]
            [futon3c.agency.registry])
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
  "Write LINES as session SID's transcript under a throwaway user.home, call F
   with the transcript file."
  [sid lines f]
  (let [home (.toFile (Files/createTempDirectory "joey-home" (make-array FileAttribute 0)))
        dir (io/file home ".claude" "projects" "-proj")
        old (System/getProperty "user.home")]
    (.mkdirs dir)
    (spit (io/file dir (str sid ".jsonl"))
          (apply str (map #(str (json/generate-string %) "\n") lines)))
    (try
      (System/setProperty "user.home" (str home))
      (f (io/file dir (str sid ".jsonl")))
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
               (repeat 1200 blob)
               [{:type "assistant" :uuid "a3" :parentUuid "a2"
                 :message {:role "assistant" :model "<synthetic>"
                           :content [{:type "text" :text "You're out of usage credits."}]
                           :usage {:input_tokens 0 :output_tokens 0
                                   :cache_read_input_tokens 0
                                   :cache_creation_input_tokens 0}}}])]
    (with-transcript "s-blobs" lines
      (fn [_]
        (is (> (pouch/session-transcript-bytes "s-blobs") (* 4 1024 1024))
            "the request log alone puts the file past the old 2 MiB limit")
        (is (= 47393 (pouch/session-context-tokens "s-blobs"))
            "the newest model reply is found behind 4 MB of log; the synthetic reply after it and the older reply before it are not used")
        (with-redefs [pouch/joey-max-tokens (constantly 200000)]
          (is (true? (boolean (pouch/joey-eligible? "a" "s-blobs")))
              "a 47k-token session is a joey whatever its file size"))))
    (with-transcript "s-fresh" [{:type "user" :uuid "u1" :message {:role "user" :content "hi"}}]
      (fn [_]
        (is (nil? (pouch/session-context-tokens "s-fresh"))
            "no reply yet: unknown, which the gate treats as a joey")))))

(def ^:private big-reply
  {:type "assistant" :uuid "a1" :parentUuid "u1"
   :message {:role "assistant" :model "claude-opus-5-5"
             :content [{:type "text" :text "long session"}]
             :usage {:input_tokens 2 :cache_read_input_tokens 300000
                     :cache_creation_input_tokens 5000 :output_tokens 800}}})

(def ^:private boundary
  ;; The shape Claude Code 2.1.30x writes for /compact (same session id).
  {:type "system" :subtype "compact_boundary" :parentUuid nil
   :logicalParentUuid "a1" :uuid "b1" :content "Conversation compacted"
   :compactMetadata {:trigger "manual" :preTokens 305802}})

(defn- append! [file rec]
  (spit file (str (json/generate-string rec) "\n") :append true))

(deftest compact-boundary-after-the-last-reply-reads-as-unknown
  (with-transcript "s-c" [{:type "user" :uuid "u1" :message {:role "user" :content "hi"}}
                          big-reply]
    (fn [file]
      (is (= 305802 (pouch/session-context-tokens "s-c")))
      (append! file boundary)
      (is (nil? (pouch/session-context-tokens "s-c"))
          "after /compact the old usage no longer describes the context")
      (append! file (assoc-in big-reply [:message :usage]
                              {:input_tokens 2 :cache_read_input_tokens 30000
                               :cache_creation_input_tokens 0 :output_tokens 500}))
      (is (= 30502 (pouch/session-context-tokens "s-c"))
          "the first reply after the compaction reports the new size"))))

(deftest auto-compact-is-a-per-seat-flag
  (let [reg (atom {"watched" {:agent/metadata {:model "opus"}}
                   "unwatched" {:agent/metadata {:auto-compact? true}}
                   "restored" {:agent/metadata {"auto-compact?" true}}})]
    (with-redefs [futon3c.agency.registry/get-agent (fn [aid] (get @reg aid))
                  futon3c.agency.registry/update-agent!
                  (fn [aid & {:as u}] (let [r (merge (get @reg aid) u)]
                                        (swap! reg assoc aid r) r))]
      (is (false? (pouch/auto-compact-seat? "watched"))
          "unmarked seats are the operator's to compact")
      (is (true? (pouch/auto-compact-seat? "unwatched")))
      (is (true? (pouch/auto-compact-seat? "restored"))
          "a roster restored from JSON carries the key as a string")
      (is (false? (pouch/auto-compact-seat? "not-registered")))
      (is (:ok (pouch/set-auto-compact! "watched" true)))
      (is (true? (pouch/auto-compact-seat? "watched")))
      (is (= "opus" (get-in @reg ["watched" :agent/metadata :model]))
          "setting the flag keeps the rest of the metadata")
      (pouch/set-auto-compact! "restored" false)
      (is (false? (pouch/auto-compact-seat? "restored"))
          "turning it off also clears the string-keyed copy"))))

(deftest monster-is-compacted-by-claude-code-and-always-warms
  (with-redefs [pouch/joey-max-tokens (constantly 200000)
                pouch/auto-compact-seat? (constantly true)]
    (testing "a cold /compact that lands: called once, then the session is a joey"
      (with-transcript "s-m1" [{:type "user" :uuid "u1" :message {:role "user" :content "hi"}}
                               big-reply]
        (fn [file]
          (let [calls (atom 0)]
            (is (true? (pouch/joey-eligible-or-compact?
                        "m-1" "s-m1" #(do (swap! calls inc) (append! file boundary)))))
            (is (= 1 @calls))
            (is (nil? (pouch/session-context-tokens "s-m1")))))))
    (testing "a /compact that does nothing: still warm, and not retried"
      (with-transcript "s-m2" [{:type "user" :uuid "u1" :message {:role "user" :content "hi"}}
                               big-reply]
        (fn [file]
          (let [calls (atom 0)
                before (slurp file)]
            (is (true? (pouch/joey-eligible-or-compact? "m-2" "s-m2" #(swap! calls inc))))
            (is (true? (pouch/joey-eligible-or-compact? "m-2" "s-m2" #(swap! calls inc))))
            (is (= 1 @calls) "one failed attempt per (agent, session), not one per turn")
            (is (= before (slurp file)) "Agency never edits the transcript itself")))))
    (testing "a live warm pouch on the session is compacted in place, not beside it"
      (with-transcript "s-m5" [{:type "user" :uuid "u1" :message {:role "user" :content "hi"}}
                               big-reply]
        (fn [file]
          (let [cold (atom 0) warm (atom [])
                !pouches @#'pouch/!pouches]
            (swap! !pouches assoc "m-5" {:session-id "s-m5"})
            (try
              (with-redefs-fn {#'pouch/alive? (constantly true)
                               #'pouch/compact-pouch! (fn [aid opts]
                                                        (swap! warm conj [aid opts])
                                                        (append! file boundary)
                                                        {:ok true})}
                #(is (true? (pouch/joey-eligible-or-compact? "m-5" "s-m5" (fn [] (swap! cold inc))))))
              (finally (swap! !pouches dissoc "m-5")))
            (is (= [["m-5" {:wait? true}]] @warm))
            (is (zero? @cold) "no cold /compact beside a live pouch")))))
    (testing "a joey is not compacted; nor is a monster on an unmarked seat"
      (with-transcript "s-m3" [{:type "user" :uuid "u1" :message {:role "user" :content "hi"}}
                               (assoc-in big-reply [:message :usage] {:input_tokens 1000})]
        (fn [_]
          (let [calls (atom 0)]
            (is (true? (pouch/joey-eligible-or-compact? "m-3" "s-m3" #(swap! calls inc))))
            (is (zero? @calls)))))
      (with-transcript "s-m4" [{:type "user" :uuid "u1" :message {:role "user" :content "hi"}}
                               big-reply]
        (fn [_]
          (let [calls (atom 0)]
            (with-redefs [pouch/auto-compact-seat? (constantly false)]
              (is (true? (pouch/joey-eligible-or-compact? "m-4" "s-m4" #(swap! calls inc)))))
            (is (zero? @calls))))))))
