(ns futon3c.dev.config-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.dev.config :as config]))

;; Regression for the 2026-06-15 OOM/restore incident: an Emacs payload that
;; stringified nil sent the literal "nil" for session-id, which got written to
;; the session file + roster and then crashed invoke on `(subs "nil" 0 8)`.
;; The session-id readers must treat "nil"/"null"/blank as no-session.

(deftest valid-session-id-rejects-sentinels
  (testing "sentinel strings normalize to nil (no session)"
    (doseq [s ["nil" "NIL" "null" "Null" "none" "false" "  nil  " "" "   " nil]]
      (is (nil? (config/valid-session-id s))
          (str "expected nil for sentinel/blank input: " (pr-str s)))))
  (testing "real session ids pass through, trimmed"
    (is (= "abc123-uuid" (config/valid-session-id "  abc123-uuid  ")))
    (is (= "9f3c" (config/valid-session-id "9f3c")))))

(deftest read-session-id-self-heals-poisoned-file
  (testing "a session file containing the literal \"nil\" reads as no-session"
    (let [f (java.io.File/createTempFile "futon-session-id-test" ".txt")]
      (try
        (spit f "nil")
        (is (nil? (config/read-session-id f)))
        (spit f "real-session-7")
        (is (= "real-session-7" (config/read-session-id f)))
        (finally (.delete f))))))

(deftest persist-session-id-refuses-sentinels
  (testing "persist! never writes a sentinel to disk"
    (let [f (java.io.File/createTempFile "futon-session-id-persist" ".txt")]
      (try
        (.delete f)
        (config/persist-session-id! f "nil")
        (is (not (.exists f)) "sentinel must not be persisted")
        (config/persist-session-id! f "good-sid")
        (is (= "good-sid" (slurp f)))
        (finally (.delete f))))))
