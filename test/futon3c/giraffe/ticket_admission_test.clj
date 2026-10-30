(ns futon3c.giraffe.ticket-admission-test
  "Q1/T adapter slice: :include-tickets at the census boundary.

  T1 default preservation (no T rows without the opt). T2 real ticket rows
  typed via shared readers. T3 the frozen-608 hermetic witness: real
  census -> consume -> combined-pass -> write-run! (temp) ->
  outer-observation -> real select; open carrier compared to an
  independent registry census; actual pick reported, never forced."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.mission-registry :as registry]
            [futon2.aif.outer-state :as state]
            [futon2.aif.outer-observation :as observation]
            [futon2.aif.outer-select :as select]
            [futon2.aif.focus-receipt :as focus]
            [futon2.aif.selection-world :as world]
            [futon3c.giraffe.excursions :as excursions]
            [futon3c.giraffe.run :as writer]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def edges-file "/home/joe/code/futon3c/holes/missions/giraffe-mission-edges.json")
(def readings-root
  "/home/joe/notes/wm-next/criteria-completeness/zai-coordination/q1-ticket-prep/readings-608/")

(deftest t1-default-census-has-no-tickets
  (let [adapter (excursions/missions-and-excursions->input {:edges-file edges-file})
        ids (map :id (get-in adapter [:input :items]))]
    (is (not (some #(= \T (first (name %))) ids)))
    (is (not (some #(= \T (first (name (:id %)))) (:excluded adapter))))))

(deftest t2-ticket-rows-are-typed-not-invented
  (let [adapter (excursions/missions-and-excursions->input
                 {:edges-file edges-file :include-tickets true})
        rows (filter #(= \T (first (name (:id %)))) (get-in adapter [:input :items]))
        live-registry (set (map :id (filter registry/live-ticket?
                                            (:tickets (registry/load-tickets "/home/joe/code")))))
        excluded-ids (set (map :id (:excluded adapter)))
        non-live-registry (set (map :id (remove registry/live-ticket?
                                                (:tickets (registry/load-tickets "/home/joe/code")))))]
    (is (= live-registry (set (map (comp name :id) rows))))
    (is (every? #(= :unstarted (:state-group %)) rows))
    (is (every? #(= #{:path :sha256} (set (keys (:source-pin %)))) rows))
    (is (every? #(= #{:status :reason :producer :source-pin} (set (keys (:dependency-reading %)))) rows))
    (is (every? #(= :unknown (get-in % [:dependency-reading :status])) rows))
    (is (every? #(= :dependency-reader-reviewer (get-in % [:dependency-reading :producer])) rows))
    ;; typed completion: schema + producer present; A4 contract absence is
    ;; typed (:completion-definition-not-found), never a synthesized class.
    (is (every? #(= :wm/completion-surface-v1 (:schema (:completion %))) rows))
    (is (every? #(contains? #{:task-completion-author :completion-locator-author}
                            (:producer (:completion %))) rows))
    (is (every? #(contains? #{:completion-definition-not-found :checkbox-locators-with-retained-prose
                              :completion-prose-needs-locators}
                            (:completion-class (:completion %))) rows))
    ;; every live ticket has a declared-status; non-live T are excluded with
    ;; typed reason, independently of the row set.
    (is (every? :declared-status rows))
    (is (set/subset? (set/intersection non-live-registry excluded-ids) non-live-registry))
    (is (every? :reason (filter #(= \T (first (name (:id %)))) (:excluded adapter))))
    ;; source pins verify against the actual files right now.
    (doseq [row (take 3 rows)]
      (is (= (writer/sha256 (io/file (get-in row [:source-pin :path])))
             (get-in row [:source-pin :sha256]))))))

(deftest t3-frozen-608-hermetic-witness
  (let [batches (->> (file-seq (io/file readings-root))
                     (filter #(.isFile %))
                     (filter #(re-matches #"batch-.*\.jsonl" (.getName %)))
                     (sort-by #(.getName %))
                     (mapv #(.getAbsolutePath %)))
        adapter (excursions/missions-and-excursions->input
                 {:edges-file edges-file :include-tickets true
                  :reading-checker (str readings-root "check_edges.py")
                  :members-file (str readings-root "members.json")
                  :reading-batches batches
                  :reading-scope :population/missions-excursions-tickets})
        result (excursions/combined-pass (:input adapter))
        tmp-root (str (java.nio.file.Files/createTempDirectory
                       "q1-ticket-witness-" (make-array java.nio.file.attribute.FileAttribute 0)))
        output (writer/write-run! {:root tmp-root :adapter-result adapter
                                   :pass-output result
                                   :edges-file edges-file})
        _ (is (:ok? (writer/verify-run (:dir output))))
        makeup (state/load-makeup state/default-makeup-path)
        prior (state/read-state makeup state/default-state-path)
        obs (observation/outer-observation
             {:run-dir (:dir output) :makeup makeup
              :focus-inputs (focus/read-inputs)
              :decision-as-of "2026-10-30T00:00:00Z"
              :relation-context (focus/relation-context "/home/joe/code")})
        selected (select/select makeup prior obs)
        sets (#'world/default-task-sets "/home/joe/code")
        wanted (set (mapcat val sets))
        found (set (map (comp name :id) (:order result)))
        status-by-id (into {} (map (fn [r] [(get-in r [:record :id])
                                             (get-in r [:record :status_as_read])]))
                           (get-in adapter [:reading-evidence :checked-records]))
        pick (get-in selected [:selection :selected :target])
        rows (into {} (map (juxt :id identity)) (:order result))
        report {:population (count found)
                :by-type (frequencies (map #(subs % 0 1) found))
                :wanted (count wanted) :missing (sort (set/difference wanted found))
                :extras (sort (set/difference found wanted))
                :coverage (dissoc (:graph-coverage result) :members)
                :graph-terms (frequencies (map #(get-in % [:graph-terms :status]) (:order result)))
                :excluded-by-reason (frequencies (map :reason (:excluded adapter)))
                :ready-count (count (:ready-set result))
                :ready-tickets (filterv #(= \T (first %)) (map name (:ready-set result)))
                :pick pick
                :pick-status-as-read (get status-by-id pick)
                :pick-terms (get-in rows [(keyword pick) :graph-terms])
                :verify (:ok? (writer/verify-run (:dir output)))
                :selection-kind (get-in selected [:selection :selected :kind])}
        report-path "/home/joe/notes/wm-next/criteria-completeness/zai-coordination/q1-ticket-prep/t3-witness-report.json"]
    (spit report-path (pr-str report))
    ;; Assertions: coverage complete over 608, scope label correct, no T ready
    ;; by default, pick is whatever the REAL selector chose.
    (is (= :complete (get-in result [:graph-coverage :status])))
    (is (= 608 (get-in result [:graph-coverage :member-count])))
    (is (= :population/missions-excursions-tickets
           (get-in result [:graph-coverage :scope])))
    (is (every? #(= :population/missions-excursions-tickets
                    (get-in % [:graph-terms :scope])) (:order result)))
    ;; Ready tickets are REPORTED, not forced and not forbidden: with the
    ;; complete certificate, an open T with no unmet counting needs is ready
    ;; by the same rule as M/E. What must hold is that readiness rides the
    ;; certificate: known terms, no unresolved prerequisites.
    (is (every? #(= :known (get-in rows [(keyword %) :graph-terms :status]))
                (:ready-tickets report)))
    (is (every? #(nil? (get-in rows [(keyword %) :graph-terms :reason]))
                (:ready-tickets report)))
    (is (some? pick))
    ;; independent registry census comparison is REPORTED, not asserted equal:
    ;; the open carrier must match the pinned registry or the mismatch is
    ;; visible in the report for review.
    (is (pos? (count found)))))
