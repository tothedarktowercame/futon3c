(ns futon3c.agency.agent-pouch-joey-test
  "Joey gate: a pouch warms only a small session; a monster stays cold unless
   overridden. These exercise the policy predicate without spawning any process."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.agent-pouch :as pouch]))

(deftest joey-eligible-by-size
  (with-redefs [pouch/joey-max-bytes (constantly 1000)]
    (testing "small transcript → warm (joey)"
      (with-redefs [pouch/session-transcript-bytes (constantly 500)]
        (is (true? (boolean (pouch/joey-eligible? "a" "s1"))))))
    (testing "at the threshold → still a joey (<=)"
      (with-redefs [pouch/session-transcript-bytes (constantly 1000)]
        (is (true? (boolean (pouch/joey-eligible? "a" "s1"))))))
    (testing "over the threshold → monster, stays cold"
      (with-redefs [pouch/session-transcript-bytes (constantly 5000)]
        (is (false? (boolean (pouch/joey-eligible? "a" "s1"))))))
    (testing "unknown size (fresh session) → fail open to warm"
      (with-redefs [pouch/session-transcript-bytes (constantly nil)]
        (is (true? (boolean (pouch/joey-eligible? "a" "s1"))))))))

(deftest monster-override-warms-anyway
  (with-redefs [pouch/joey-max-bytes (constantly 1000)
                pouch/session-transcript-bytes (constantly 5000)]
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

(deftest joey-max-bytes-reads-property
  (let [old (System/getProperty "FUTON3C_KANGAROO_JOEY_MAX_BYTES")]
    (try
      (System/setProperty "FUTON3C_KANGAROO_JOEY_MAX_BYTES" "12345")
      (is (= 12345 (pouch/joey-max-bytes)))
      (finally
        (if old
          (System/setProperty "FUTON3C_KANGAROO_JOEY_MAX_BYTES" old)
          (System/clearProperty "FUTON3C_KANGAROO_JOEY_MAX_BYTES"))))))
