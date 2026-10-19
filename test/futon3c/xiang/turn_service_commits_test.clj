(ns futon3c.xiang.turn-service-commits-test
  "The happened note lists every commit made in any repo while the turn ran;
  the turn is credited only with the ones whose git trailers name the turn's
  agent (its hand, its session, or work it dispatched). Foreign and unsigned
  commits are excluded on the record, never dropped silently."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.xiang.turn-acts :as turn-acts]
            [futon3c.xiang.turn-store :as ts]
            [futon3c.xiang.turn-service :as svc]))

(defn- git!
  [repo & args]
  (let [p (.start (doto (ProcessBuilder. (vec (cons "git" args)))
                    (.directory (io/file repo))))
        out (slurp (.getInputStream p))
        err (slurp (.getErrorStream p))]
    (when-not (zero? (.waitFor p))
      (throw (ex-info "git failed" {:args args :err err})))
    (str/trim out)))

(defn- temp-repo
  "An empty git repo with a base commit, ready for trailered commits."
  []
  (let [d (.toFile (java.nio.file.Files/createTempDirectory
                    "xiang-cmt-" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (git! d "init" "-q")
    (git! d "config" "user.email" "t@example.com")
    (git! d "config" "user.name" "t")
    (spit (io/file d "f.txt") "base\n")
    (git! d "add" ".")
    (git! d "commit" "-q" "-m" "base")
    d))

(defn- commit!
  "Commit MSG in REPO with TRAILERS, a map or a seq of [key val] pairs (a
  seq can repeat a key, as real commits do); returns the full sha."
  [repo msg trailers]
  (spit (io/file repo "f.txt") (str (rand) "\n"))
  (git! repo "add" ".")
  (apply git! repo "commit" "-q" "-m" msg
         (mapcat (fn [[k v]] [(str "--trailer=" k "=" v)]) trailers))
  (git! repo "rev-parse" "HEAD"))

(defn- temp-store []
  (ts/store (str (java.nio.file.Files/createTempDirectory
                  "xiang-svc-" (make-array java.nio.file.attribute.FileAttribute 0)))))

(defn- harness
  "The minimal service the happened path needs: a store, an inline scheduler
  and an inline evidence append, no bells (every turn is :dispatch :none)."
  []
  (let [scheduled (atom [])
        s (svc/service {:store (temp-store)
                        :now-ms (fn [] 1000000)
                        :schedule! (fn [delay thunk] (swap! scheduled conj [delay thunk]))
                        :evidence-async! (fn [thunk] (thunk))})]
    {:svc s :scheduled scheduled}))

(defn- turn!
  [h]
  (svc/record-turn! (:svc h) {:text "Please continue."
                              :agent-id "claude-17" :session-id "sess-1"
                              :turn-id "claude-17-turn-c"
                              :evidence-id "emacs-c"
                              :dispatch :none}))

(deftest happened-keeps-only-the-turns-agents-commits
  (let [repo (temp-repo)
        own-sha (commit! repo "own work" {"Agent-Id" "claude-17"})
        foreign-sha (commit! repo "foreign work" {"Agent-Id" "codex-6"})
        unsigned-sha (commit! repo "operator at a terminal" {})
        h (harness)
        {:keys [id]} (turn! h)]
    (svc/attach-happened!
     (:svc h) id
     {:reply "done"
      :commits [{:repo "tmp" :repo-path (str repo) :sha own-sha :subject "own work"}
                {:repo "tmp" :repo-path (str repo) :sha foreign-sha :subject "foreign work"}
                {:repo "tmp" :repo-path (str repo) :sha unsigned-sha
                 :subject "operator at a terminal"}]})
    (let [stored (ts/read-record (get-in (:svc h) [:config :store]) id)
          acts (turn-acts/happened-commits (:happened_summary stored))]
      (is (= 1 (count acts)) "one commit act: the agent's own")
      (is (= (subs own-sha 0 8) (:sha (first acts)))
          "the summary line carries the short sha the acts parse")
      (is (= "own work (+1 -1 over 1 files)" (:subject (first acts))))
      (is (not (str/includes? (:happened_summary stored) "foreign")))
      (is (not (str/includes? (:happened_summary stored) "terminal")))
      (testing "the excluded commits are on the record with reasons"
        (is (= #{[foreign-sha "another-seat"] [unsigned-sha "no-trailers"]}
               (set (map (juxt :sha :reason) (:commits_excluded stored))))
            "reasons survive the store round-trip as strings")
        (is (= "codex-6"
               (:agent-id (some (fn [e] (when (= foreign-sha (:sha e)) e))
                                (:commits_excluded stored))))
            "a foreign commit names whose it was")))))

(deftest dispatched-and-session-commits-are-the-turns
  (let [repo (temp-repo)
        by-hand-sha (commit! repo "session hand" {"Agent-Session" "sess-1"})
        dispatched-sha (commit! repo "dispatched work" {"Dispatched-By" "claude-17"})
        other-session-sha (commit! repo "another session" {"Agent-Session" "sess-9"})
        h (harness)
        {:keys [id]} (turn! h)]
    (svc/attach-happened!
     (:svc h) id
     {:reply "done"
      :commits (mapv (fn [[sha subject]]
                       {:repo "tmp" :repo-path (str repo) :sha sha :subject subject})
                     [[by-hand-sha "session hand"]
                      [dispatched-sha "dispatched work"]
                      [other-session-sha "another session"]])})
    (let [stored (ts/read-record (get-in (:svc h) [:config :store]) id)
          acts (turn-acts/happened-commits (:happened_summary stored))]
      (is (= #{(subs by-hand-sha 0 8) (subs dispatched-sha 0 8)}
             (set (map :sha acts)))
          "the session's hand and dispatched work are credited")
      (is (= [[other-session-sha "another-seat"]]
             (mapv (juxt :sha :reason) (:commits_excluded stored)))))))

(deftest real-trailer-forms-repeated-keys-and-agent-slash-session
  ;; The prepare-commit-msg hook writes Dispatched-By as "agent/session", and
  ;; commits repeat keys (two Agent-Id lines on codex-6/codex-68 commits, two
  ;; Agent-Session lines on zai-1's). Both must still credit the right turn.
  (let [repo (temp-repo)
        dispatched-sha (commit! repo "zai work for claude-17"
                                [["Agent-Id" "zai-6"] ["Agent-Session" "zai-x"]
                                 ["Dispatched-By" "claude-17/sess-1"]])
        repeated-sha (commit! repo "two ids" [["Agent-Id" "codex-6"] ["Agent-Id" "claude-17"]])
        prefix-sha (commit! repo "not a prefix match" [["Dispatched-By" "claude-170/sess-2"]])
        h (harness)
        {:keys [id]} (turn! h)]
    (svc/attach-happened!
     (:svc h) id
     {:reply "done"
      :commits (mapv (fn [[sha subject]]
                       {:repo "tmp" :repo-path (str repo) :sha sha :subject subject})
                     [[dispatched-sha "zai work for claude-17"]
                      [repeated-sha "two ids"]
                      [prefix-sha "not a prefix match"]])})
    (let [stored (ts/read-record (get-in (:svc h) [:config :store]) id)
          acts (turn-acts/happened-commits (:happened_summary stored))]
      (is (= #{(subs dispatched-sha 0 8) (subs repeated-sha 0 8)} (set (map :sha acts))))
      (is (= [prefix-sha] (mapv :sha (:commits_excluded stored)))
          "claude-170 is not claude-17"))))
