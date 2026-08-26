(ns futon3c.inbox-zero.sweeper-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.state :as state]
            [futon3c.inbox-zero.sweeper :as sweeper]))

(def now #inst "2026-08-24T12:00:00Z")

(defn observation [n path]
  {:record/type :inbox-zero/file-observation
   :observation/id (str "observation:" n) :repo/id "futon3c-d"
   :repo/root "/repo" :worktree/id "worktree:one" :path path
   :git/status :modified :content/hash (str "sha256:" n) :head/sha "head"
   :observed-at (java.util.Date. (+ 1000 n)) :source :multi-watcher})

(defn store [& paths]
  (state/replay (map-indexed #(observation (inc %1) %2) paths)))

(defn candidate [seat source-id]
  {:seat/id seat :rank 1 :confidence :corroborated
   :evidence [{:evidence/type :fixture :at now :source/id source-id}]
   :against []})

(defn result [path verdict candidates]
  {:path/key {:repo/id "futon3c-d" :worktree/id "worktree:one" :path path}
   :verdict verdict :candidates candidates})

(defn base-options [paths calls infer-fn]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "sweeper-test-"
                      (make-array java.nio.file.attribute.FileAttribute 0)))]
    {:state-path (str (java.io.File. dir "state.edn"))
     :proposals-path (str (java.io.File. dir "attribution-proposals.edn"))
     :load-state-fn (fn [_] (apply store paths))
     :now-fn (constantly now) :roster-fn (fn [] #{"seat:live:s1"})
     :build-bundle-fn (fn [path _] {:path (:path path)}) :infer-fn infer-fn
     :deliver! (fn [payload] (swap! calls conj [:deliver payload]) {:status 200})
     :ledger-fn (fn [_ decision] (swap! calls conj [:ledger decision]))
     :print-fn (fn [line] (swap! calls conj [:print line]))}))

(deftest live-proposal-delivers-stable-exact-seat-followup
  (let [calls (atom [])
        infer-fn (fn [path _]
                   (result (:path path) :propose
                           [(candidate "seat:live:s1" "evidence:one")]))
        options (base-options ["a.clj"] calls infer-fn)
        first-counts (sweeper/sweep-attributions! options)
        first-call (first @calls)
        first-key (get-in first-call [1 :dedupe-key])]
    (is (= {:swept 1 :proposed 1 :ledgered 0 :insufficient 0 :errored 0
            :already-proposed 0 :already-ledgered 0
            :unswept 0} first-counts))
    (is (= "live" (get-in first-call [1 :agent])))
    (is (= "s1" (get-in first-call [1 :session])))
    (is (re-find #"curl -sS -X POST .*confirm-attribution"
                 (get-in first-call [1 :prompt])))
    (is (str/includes? (get-in first-call [1 :prompt]) "a.clj"))
    (is (str/includes? (get-in first-call [1 :prompt])
                       "\"agent\":\"live\""))
    (is (str/includes? (get-in first-call [1 :prompt])
                       "\"session\":\"s1\""))
    (is (= first-key (get-in first-call [1 :dedupe-key])))
    (reset! calls [])
    (let [second-counts (sweeper/sweep-attributions! options)]
      (is (= 1 (:already-proposed second-counts)))
      (is (zero? (:proposed second-counts))))
    (is (empty? @calls))))

(deftest dead-seat-proposal-ledgers-without-delivery
  (let [calls (atom [])
        counts (sweeper/sweep-attributions!
                (base-options
                 ["a.clj"] calls
                 (fn [path _]
                   (result (:path path) :propose
                           [(candidate "seat:dead:s2" "evidence:dead")]))))]
    (is (= 1 (:proposed counts)))
    (is (= 1 (:ledgered counts)))
    (is (= 2 (get-in (first @calls) [1 :route/tier])))
    (is (not-any? #(= :deliver (first %)) @calls))))

(deftest ambiguous-ledgers-and-insufficient-stays-quiet
  (let [calls (atom [])
        counts (sweeper/sweep-attributions!
                (base-options
                 ["ambiguous.clj" "insufficient.clj"] calls
                 (fn [path _]
                   (if (= "ambiguous.clj" (:path path))
                     (result (:path path) :ambiguous
                             [(candidate "seat:a:s" "a")
                              (candidate "seat:b:s" "b")])
                     (result (:path path) :insufficient [])))))]
    (is (= {:swept 2 :proposed 0 :ledgered 1 :insufficient 1 :errored 0
            :already-proposed 0 :already-ledgered 0
            :unswept 0} counts))
    (is (= 1 (count (filter #(= :ledger (first %)) @calls))))))

(deftest max-paths-is-deterministic-and-reports-unswept
  (let [seen (atom []) calls (atom [])
        counts (sweeper/sweep-attributions!
                (assoc (base-options
                        ["z.clj" "a.clj" "m.clj"] calls
                        (fn [path _]
                          (swap! seen conj (:path path))
                          (result (:path path) :insufficient [])))
                       :max-paths 2))]
    (is (= ["a.clj" "m.clj"] @seen))
    (is (= 2 (:swept counts)))
    (is (= 1 (:unswept counts)))
    (is (some #(= :print (first %)) @calls))))

(deftest path-error-does-not-stop-the-pass
  (let [seen (atom []) calls (atom [])
        counts (sweeper/sweep-attributions!
                (base-options
                 ["a.clj" "b.clj"] calls
                 (fn [path _]
                   (swap! seen conj (:path path))
                   (if (= "a.clj" (:path path))
                     (throw (ex-info "bad path" {}))
                     (result (:path path) :insufficient [])))))]
    (is (= ["a.clj" "b.clj"] @seen))
    (is (= 1 (:errored counts)))
    (is (= 1 (:insufficient counts)))
    (is (= 2 (:swept counts)))))

(deftest mixed-return-counts-are-exact
  (let [calls (atom [])
        table {"amb.clj" [:ambiguous [(candidate "seat:a:s" "a")
                                      (candidate "seat:b:s" "b")]]
               "dead.clj" [:propose [(candidate "seat:dead:s" "dead")]]
               "ins.clj" [:insufficient []]
               "live.clj" [:propose [(candidate "seat:live:s1" "live")]]}
        counts (sweeper/sweep-attributions!
                (base-options
                 (keys table) calls
                 (fn [path _]
                   (let [[verdict candidates] (get table (:path path))]
                     (result (:path path) verdict candidates)))))]
    (is (= {:swept 4 :proposed 2 :ledgered 2 :insufficient 1 :errored 0
            :already-proposed 0 :already-ledgered 0
            :unswept 0} counts))))

(deftest changed-evidence-is-proposed-again
  (let [calls (atom []) source (atom "evidence:one")
        options (base-options
                 ["a.clj"] calls
                 (fn [path _]
                   (result (:path path) :propose
                           [(candidate "seat:live:s1" @source)])))]
    (sweeper/sweep-attributions! options)
    (reset! source "evidence:two")
    (sweeper/sweep-attributions! options)
    (is (= 2 (count (filter #(= :deliver (first %)) @calls))))))

(deftest ledgered-decisions-are-not-appended-twice
  (doseq [[verdict candidates]
          [[:propose [(candidate "seat:dead:s2" "dead")]]
           [:ambiguous [(candidate "seat:a:s" "a")
                        (candidate "seat:b:s" "b")]]]]
    (let [calls (atom [])
          options (base-options
                   ["a.clj"] calls
                   (fn [path _] (result (:path path) verdict candidates)))]
      (sweeper/sweep-attributions! options)
      (let [counts (sweeper/sweep-attributions! options)]
        (is (= 1 (:already-ledgered counts)))
        (is (= 1 (count (filter #(= :ledger (first %)) @calls))))))))

(deftest failed-delivery-is-retried
  (let [calls (atom [])
        options (assoc
                 (base-options
                  ["a.clj"] calls
                  (fn [path _]
                    (result (:path path) :propose
                            [(candidate "seat:live:s1" "same")])))
                 :deliver! (fn [payload]
                             (swap! calls conj [:deliver payload])
                             {:status 503}))]
    (is (= 1 (:errored (sweeper/sweep-attributions! options))))
    (is (= 1 (:errored (sweeper/sweep-attributions! options))))
    (is (= 2 (count (filter #(= :deliver (first %)) @calls))))))

(deftest corrupt-proposals-file-is-tolerated-once
  (let [calls (atom [])
        options (base-options
                 ["a.clj"] calls
                 (fn [path _]
                   (result (:path path) :propose
                           [(candidate "seat:live:s1" "same")])))]
    (spit (:proposals-path options) "not-edn[")
    (is (= 1 (:proposed (sweeper/sweep-attributions! options))))
    (is (= 1 (count (filter #(= :print (first %)) @calls))))
    (is (= 1 (count (filter #(= :deliver (first %)) @calls))))))
