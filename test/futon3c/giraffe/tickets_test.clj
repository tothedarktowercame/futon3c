(ns futon3c.giraffe.tickets-test
  "M-giraffe stage 1 packet 2 acceptance tests: the holes/tickets source
  adapter (M-giraffe.md §DERIVE: every edge records how it was obtained;
  §VERIFY pilot 1 on the corrected index state)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [futon3c.giraffe.pass :as gp]
            [futon3c.giraffe.tickets :as gt]))

(def tickets-dir "holes/tickets")
(def index-file "holes/tickets/tickets-index.edn")
(def edges-file "test/resources/giraffe/pilot1-edges.json")

(def pilot1-ids
  (mapv keyword
        (str/split
         (str/trim (slurp "test/resources/giraffe/pilot1-ids.txt")) #"\s+")))

(defn adapter
  ([] (adapter {:only pilot1-ids}))
  ([opts] (gt/tickets->input (merge {:tickets-dir tickets-dir
                                     :index-file index-file
                                     :edges-file edges-file}
                                    opts))))

(deftest pilot-1-on-the-corrected-index
  (let [{:keys [input rejected status-mapping]} (adapter)
        out (gp/pass input)]
    (testing "the two amended tickets left the open set: 13 open, not 15"
      (is (= 13 (get-in out [:counts :open])))
      (is (= :done (get-in out [:readiness :T-car3-phase2-impl :status])))
      (is (= :done (get-in out [:readiness :T-mq7-unaddressable-caller :status]))))
    (testing "pilot 1's done-with-open-needs anomaly (the tidy) is gone; the
              two newly-closed tickets now carry it, because their edges to
              the superseded routing ticket were never re-pointed (§222)"
      (is (empty? (filter #(= :T-durable-queue-rename-and-tidies
                              (:item %)) (:anomalies out))))
      (is (= #{:T-car3-phase2-impl :T-mq7-unaddressable-caller}
             (set (map :item (:anomalies out))))
          "reported, not resolved: the pass says these edges are stale"))
    (testing "no blocker-cancelled remains: the superseded routing ticket's
              in-edges come only from the two now-completed tickets"
      (is (empty? (filter #(= :needs/blocker-cancelled (:reason (val %)))
                          (:readiness out)))))
    (testing "the ready set is unchanged from pilot 1's 6: closing the two
              tickets met every cluster need (they had no other dependents)"
      (is (= #{:T-car3-queue-routing-design-note
               :T-fail-activation-authority-unavailable
               :T-fail-agent-not-found :T-fail-invoke-interrupted
               :T-fail-operator-cancelled :T-zai-chat-transient-timeout}
             (set (:ready-set out))))
      (is (= 13 (get-in out [:counts :ready-ignoring-externals]))))
    (testing "every pilot edge is admitted; the only rejects carried are the
              edges file's own rejected list"
      (is (= 16 (count (:edges input))))
      (is (= 8 (count (filter #(= :item (:kind %)) (:edges input)))))
      (is (= 10 (count rejected)))
      (is (every? #(= :edge/rejected-at-reading (:reject %)) rejected)))
    (testing "all prose-reading edges carry quote/file/line"
      (is (every? #(and (:quote %) (:file %) (:line %)) (:edges input)))
      (is (every? #(= :prose-reading (:source %)) (:edges input))))
    (testing "external needs are unmet external edges with string targets"
      (is (= 8 (count (filter #(= :external (:kind %)) (:edges input)))))
      (is (every? string? (map :to (filter #(= :external (:kind %))
                                           (:edges input)))))
      (is (every? #(false? (:met %))
                  (filter #(= :external (:kind %)) (:edges input)))))
    (testing "the status mapping covers every declared status in the subset,
              each to its pilot-1 group"
      (is (= #{[:done-in-fact :completed] [:partial :started]
               [:superseded :cancelled] [:still-open :unstarted]
               [:parked :backlog] [:watch :backlog]}
             (set (map (juxt :status :group) status-mapping))))
      (is (every? :mapped? status-mapping)))))

(defn- temp-root [tag]
  (let [root (str (System/getProperty "java.io.tmpdir") "/giraffe-" tag "-"
                  (System/nanoTime))]
    (.mkdirs (io/file root))
    root))

(defn- write-planted [root]
  ;; T-planted.md: the quote at line 3, and a lower sentence at line 8.
  (spit (str root "/T-planted.md")
        (str "Title\n\n"
             "needs **T-car3-queue-routing** done before this can land.\n"
             "more\nlines\nhere\nnow\npad\n"
             "the quote about warm pouches sits lower down\n"))
  (spit (str root "/T-other.md")
        "Other\n\nA warm    pouch quote  with odd spacing.\n")
  (spit (str root "/T-car3-queue-routing.md")
        "Routing

the phase-1 model, superseded by the impl.
")
  (spit (str root "/index.edn")
        (pr-str [{:ticket "T-planted.md" :status :still-open}
                 {:ticket "T-other.md" :status :mystery}
                 {:ticket "T-car3-queue-routing.md" :status :superseded}
                 {:ticket "T-gone.md" :status :watch}]))
  (let [quote-now "needs **T-car3-queue-routing** done before this can land."
        edges [{:from "T-planted" :to "T-car3-queue-routing"
                :to_kind "cluster" :kind "needs"
                :file "T-planted.md" :line 3 :quote quote-now}
               ;; whitespace-only differences still match
               {:from "T-other" :to "T-car3-queue-routing" :to_kind "cluster"
                :kind "needs" :file "T-other.md" :line 3
                :quote "A warm pouch quote with odd spacing."}
               ;; the quote exists but starts beyond the 5-line window
               {:from "T-planted" :to "T-car3-queue-routing" :to_kind "cluster"
                :kind "needs" :file "T-planted.md" :line 3
                :quote "the quote about warm pouches sits lower down"}
               ;; dangling ticket target
               {:from "T-planted" :to "T-not-in-items" :to_kind "cluster"
                :kind "needs" :file "T-planted.md" :line 3 :quote quote-now}
               ;; precedes is out of scope for stage 1
               {:from "T-planted" :to "T-car3-queue-routing" :to_kind "cluster"
                :kind "precedes" :file "T-planted.md" :line 3
                :quote quote-now}]]
    (spit (str root "/edges.json")
          (json/generate-string {:tickets [] :edges edges
                                 :rejected [{:note "carried through"}]}))
    edges))

(deftest planted-quote-and-target-cases
  (let [root (temp-root "planted")
        edges (write-planted root)
        {:keys [input rejected status-mapping unmapped-statuses]}
        (gt/tickets->input {:tickets-dir root
                            :index-file (str root "/index.edn")
                            :edges-file (str root "/edges.json")})
        items (:items input)]
    (testing "the quote at its cited line, and the whitespace-normalised
              quote, are both admitted; the moved quote is rejected, never
              passed into :edges"
      (is (= 2 (count (:edges input))))
      (is (= [(nth edges 2)]
             (map :edge (filter #(= :edge/quote-not-at-line (:reject %))
                                rejected)))))
    (testing "a dangling ticket target is rejected"
      (is (= [(nth edges 3)]
             (map :edge (filter #(= :edge/dangling-target (:reject %))
                                rejected)))))
    (testing "a precedes edge is rejected as out of scope"
      (is (= [(nth edges 4)]
             (map :edge (filter #(= :edge/precedes-not-in-stage-1 (:reject %))
                                rejected)))))
    (testing "the edges file's own rejected list is carried through unchanged"
      (is (= [{:note "carried through" :reject :edge/rejected-at-reading}]
             (filter #(= :edge/rejected-at-reading (:reject %)) rejected))))
    (testing "an index row whose file is missing is a typed reject, not an item"
      (is (= [{:reject :index/missing-file :ticket "T-gone.md" :status :watch}]
             (filter #(= :index/missing-file (:reject %)) rejected)))
      (is (= 3 (count items))))
    (testing "an unknown status is reported, not guessed"
      (is (= [{:id :T-other :status-unmapped :mystery}] unmapped-statuses))
      (is (= :unstarted (get-in input [:items 1 :state-group])))
      (is (= :mystery (get-in input [:items 1 :status-unmapped])))
      (is (= [{:status :mystery :group nil :mapped? false}]
             (remove :mapped? status-mapping))))))

(deftest blocked-status-keeps-its-declaration
  (let [root (temp-root "blocked")
        _ (spit (str root "/T-b.md") "Blocked ticket\n\nwaits on a ruling.\n")
        _ (spit (str root "/index.edn")
                (pr-str [{:ticket "T-b.md" :status :blocked}]))
        _ (spit (str root "/edges.json") "{}")
        {:keys [input status-mapping]}
        (gt/tickets->input {:tickets-dir root
                            :index-file (str root "/index.edn")
                            :edges-file (str root "/edges.json")})]
    (is (= :unstarted (get-in input [:items 0 :state-group])))
    (is (= :blocked (get-in input [:items 0 :declared-status])))
    (is (= [{:status :blocked :group :unstarted :mapped? true}]
           status-mapping))))

(deftest whole-index-without-only
  (let [{:keys [input rejected]} (adapter {:only nil})
        items (:items input)
        rows (count (edn/read-string (slurp index-file)))]
    (testing "every index row loads as an item or a typed missing-file reject"
      (is (= rows (+ (count items)
                     (count (filter #(= :index/missing-file (:reject %))
                                    rejected)))))
      (is (= 0 (count (filter #(= :index/missing-file (:reject %)) rejected)))
          "all 35 current index rows have files")
      (is (= 35 (count items)))
      (is (= 35 (count (distinct (map :source-order items))))
          "source-order is the position in the full index"))
    (testing "with no :only every pilot edge target resolves; nothing dangles"
      (is (empty? (filter #(#{:edge/dangling-source :edge/dangling-target}
                            (:reject %)) rejected))))))
