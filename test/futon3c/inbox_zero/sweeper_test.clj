(ns futon3c.inbox-zero.sweeper-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.inbox-zero.sweeper :as sweeper])
  (:import [java.util Date]))

(def now (Date. 1789000000000))
(def now-ms (.getTime now))

(defn entry
  ([path minutes-ago] (entry path minutes-ago true))
  ([path minutes-ago untracked?]
   {:path path :status (if untracked? "??" " M") :untracked? untracked?
    :mtime-ms (- now-ms (* minutes-ago 60000))}))

(defn dirty-repo [n]
  (mapv #(entry (str "runs/out-" % ".edn") (inc %)) (range n)))

(defn window [agent from-minutes to-minutes]
  {:agent agent
   :start (- now-ms (* from-minutes 60000))
   :end (- now-ms (* to-minutes 60000))})

(defn temp-notices-path []
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "commit-notice-test-"
                      (make-array java.nio.file.attribute.FileAttribute 0)))]
    (str (java.io.File. dir "commit-notices.edn"))))

(defn base-options [entries-by-label calls]
  {:roots (mapv (fn [label] {:path (str "/repo/" label) :label label})
                (keys entries-by-label))
   :git-fn (fn [path] (get entries-by-label (last (str/split path #"/")) []))
   :windows-fn (fn [] [(window "codex-10" 600 0)])
   :roster-fn (fn [] {"codex-10" "session-10"})
   :now-fn (constantly now)
   :notices-path (temp-notices-path)
   :deliver! (fn [payload] (swap! calls conj payload) {:status 200})
   :print-fn (fn [line] (swap! calls conj [:print line]))})

(deftest a-repo-under-the-threshold-is-left-alone
  (let [calls (atom [])
        counts (sweeper/sweep-dirty-repos!
                (base-options {"futon2-d" (dirty-repo 9)} calls))]
    (is (= 0 (:over-threshold counts)))
    (is (= 0 (:notified counts)))
    (is (empty? (remove #(= :print (first %)) @calls)))))

(deftest at-the-threshold-the-writing-agent-is-told-to-commit
  (let [calls (atom [])
        counts (sweeper/sweep-dirty-repos!
                (base-options {"futon2-d" (dirty-repo 10)} calls))
        payload (first (remove #(= :print (first %)) @calls))]
    (is (= 1 (:over-threshold counts)))
    (is (= 1 (:notified counts)))
    (is (= "codex-10" (:agent payload)))
    (is (= "session-10" (:session payload)))
    (is (= "inbox-zero" (:type payload)))
    (is (= 10 (get-in payload [:metadata :dirty-count])))
    (is (= 10 (get-in payload [:metadata :implicated-count])))
    (is (str/includes? (:prompt payload) "10 dirty file(s)"))
    (is (str/includes? (:prompt payload) "10 of them were written"))
    (is (str/includes? (:prompt payload) "git -C /repo/futon2-d status"))
    (is (str/includes? (:prompt payload) "Newest first: runs/out-0.edn"))))

(deftest a-told-agent-is-not-told-again-until-the-backlog-grows
  (let [calls (atom [])
        options (base-options {"futon2-d" (dirty-repo 11)} calls)
        first-counts (sweeper/sweep-dirty-repos! options)
        second-counts (sweeper/sweep-dirty-repos! options)
        grown (sweeper/sweep-dirty-repos!
               (assoc options :git-fn (constantly (dirty-repo 21))))]
    (is (= 1 (:notified first-counts)))
    (is (= 0 (:notified second-counts)))
    (is (= 1 (:held second-counts)))
    (is (= 1 (:notified grown)))))

(deftest a-stale-notice-is-repeated
  (let [calls (atom [])
        options (base-options {"futon2-d" (dirty-repo 11)} calls)
        _ (sweeper/sweep-dirty-repos! options)
        later (Date. (+ now-ms (* 7 60 60 1000)))
        repeated (sweeper/sweep-dirty-repos!
                  (assoc options :now-fn (constantly later)))]
    (is (= 1 (:notified repeated)))))

(deftest dirt-nobody-live-wrote-is-named-as-operator-backlog
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :roster-fn (constantly {}))
        counts (sweeper/sweep-dirty-repos! options)]
    (is (= 1 (:unowned counts)))
    (is (= 0 (:notified counts)))
    (is (some (fn [call]
                (and (= :print (first call))
                     (str/includes? (second call) "operator backlog")))
              @calls))))

(deftest every-pass-reports-its-counts
  (let [calls (atom [])
        _ (sweeper/sweep-dirty-repos! (base-options {"futon2-d" []} calls))]
    (is (some (fn [call]
                (and (= :print (first call))
                     (str/includes? (second call) "commit-notice pass:")))
              @calls))))

(deftest attribution-is-by-write-time-not-by-tool
  (let [entries [(entry "a.edn" 30) (entry "b.edn" 30) (entry "c.edn" 300)]
        windows [(window "codex-10" 60 10) (window "zai-5" 400 200)]
        roster {"codex-10" "s10" "zai-5" "s5"}
        attributed (sweeper/attribute windows roster entries)]
    (is (= 2 (get-in attributed ["codex-10" :count])))
    (is (= 1 (get-in attributed ["zai-5" :count])))
    (is (= ["a.edn" "b.edn"]
           (sort (map :path (get-in attributed ["codex-10" :entries])))))))

(deftest a-file-written-outside-every-turn-implicates-nobody
  (is (= {} (sweeper/attribute [(window "codex-10" 60 10)]
                               {"codex-10" "s10"}
                               [(entry "old.edn" 5000)]))))

(deftest a-dead-seat-is-never-a-candidate
  (is (= {} (sweeper/attribute [(window "codex-16" 60 10)]
                               {"codex-10" "s10"}
                               [(entry "a.edn" 30)]))))

(deftest a-file-two-agents-could-have-written-goes-to-both-marked-shared
  (let [entries [(entry "shared.edn" 30)]
        windows [(window "codex-10" 60 10) (window "zai-5" 60 10)]
        roster {"codex-10" "s10" "zai-5" "s5"}
        attributed (sweeper/attribute windows roster entries)]
    (is (= 1 (get-in attributed ["codex-10" :count])))
    (is (= 1 (get-in attributed ["zai-5" :count])))
    (is (= ["zai-5"] (:shared-with (first (get-in attributed ["codex-10" :entries])))))
    (is (= ["codex-10"] (:shared-with (first (get-in attributed ["zai-5" :entries])))))))

(deftest a-shared-file-is-named-with-who-else-was-running
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :windows-fn (fn [] [(window "codex-10" 600 0)
                                           (window "zai-5" 600 0)])
                       :roster-fn (fn [] {"codex-10" "s10" "zai-5" "s5"})
                       :max-recipients 1)
        _ (sweeper/sweep-dirty-repos! options)
        payload (first (remove #(= :print (first %)) @calls))]
    (is (str/includes? (:prompt payload) "(also inside zai-5's turn)"))))

(deftest the-agent-with-the-most-files-is-told-first
  (let [attributed {"codex-10" {:count 130 :entries [{:path "a" :mtime-ms 2}]}
                    "codex-16" {:count 121 :entries [{:path "b" :mtime-ms 3}
                                                     {:path "c" :mtime-ms 1}]}}]
    (is (= [["codex-10" 130] ["codex-16" 121]]
           (mapv (juxt :agent :count) (sweeper/recipients attributed 2))))
    (is (= ["b" "c"]
           (mapv :path (:entries (second (sweeper/recipients attributed 2))))))))

(deftest a-failed-delivery-is-counted-and-not-recorded-as-told
  (let [calls (atom [])
        options (assoc (base-options {"futon2-d" (dirty-repo 11)} calls)
                       :deliver! (fn [_] {:status 503}))
        counts (sweeper/sweep-dirty-repos! options)
        retry (sweeper/sweep-dirty-repos! (assoc options :deliver!
                                                 (fn [_] {:status 200})))]
    (is (= 1 (:errored counts)))
    (is (= 0 (:notified counts)))
    (is (= 1 (:notified retry)))))
