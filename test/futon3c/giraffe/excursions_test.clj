(ns futon3c.giraffe.excursions-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.edn :as edn] [clojure.set :as set]
            [futon3c.giraffe.readings :as readings]
            [futon3c.giraffe.excursions :as e]
            [futon3c.giraffe.run :as writer]
            [futon2.aif.outer-score :as score]
            [futon2.aif.outer-observation :as observation]
            [futon2.aif.mission-registry :as registry]))

(deftest frozen-census-retains-real-completion-classes-and-unknown-graph
  (let [adapter (e/missions-and-excursions->input
                 {:edges-file "/home/joe/code/futon3c/holes/missions/giraffe-mission-edges.json"})
        result (e/combined-pass (:input adapter))
        rows (into {} (map (juxt :id identity)) (:order result))
        record (edn/read-string (slurp "/home/joe/code/futon2/data/wm-runs/tick-run-record-2026-10-29-8bb5e6dd-087a-46b1-beb9-bda595f8cc07.edn"))
        open (set (mapcat :ids (vals (get-in record [:world-at-selection :open-tasks]))))
        ids (set (map (comp name :id) (:order result)))
        missing (set/difference open ids)
        output (writer/write-run! {:root (str (java.nio.file.Files/createTempDirectory "q1-E-test-" (make-array java.nio.file.attribute.FileAttribute 0)))
                                   :adapter-result adapter :pass-output result})]
    (let [excursions (registry/load-excursions "/home/joe/code")
          observed (observation/giraffe-field
                    (:dir output) {:default-reach 1/2}
                    {:loaded-missions (registry/load-missions-from-files "/home/joe/code")
                     :loaded-excursions excursions :loaded-tickets {:tickets []}
                     :excursion-targets (mapv :id (:excursions excursions))})]
      (is (= 636 (+ (count (:field observed)) (count (:outer-field/excluded observed)))))
      (is (= :unknown (get-in observed [:facts "E-starmap-branch-merge" :graph-terms :status])))
      (is (= :selected-task-revision-baseline (get-in observed [:facts "E-starmap-branch-merge" :completion :producer]))))
    (is (= 636 (count ids)))
    (is (= 189 (count missing)))
    (is (every? #(.startsWith % "T-") missing))
    (is (empty? (set/difference ids open)))
    (doseq [[id class] [[:E-starmap-branch-merge :checkbox-locators-with-retained-prose]
                       [:E-kimi-task-124 :completion-prose-needs-locators]
                       [:E-2026-08-10-learnings :completion-definition-not-found]]]
      (is (= class (get-in rows [id :completion :completion-class]))))
    (is (= :task-completion-author (get-in rows [:E-2026-08-10-learnings :completion :producer])))
    (is (every? #(and (nil? (:cone %)) (nil? (:unblocks %))) (vals rows)))
    (is (= :undetermined (get-in rows [:E-starmap-branch-merge :status])))
    (is (:ok? (writer/verify-run (:dir output))))
    (doseq [step [:by-cone :by-unblocks]]
      (is (= :outer-score/graph-term-unavailable
             (try (score/apply-selection-step {"E-starmap-branch-merge" (rows :E-starmap-branch-merge)}
                                              [{:target "E-starmap-branch-merge"}] step)
                  nil (catch clojure.lang.ExceptionInfo ex (:kind (ex-data ex)))))))))

;; ---------------------------------------------- partial reading drift
;; claude-1, 2026-10-31: the population check used to demand exact equality,
;; so ONE new item blocked the whole field on every click.  Drift is partial:
;; :unread-new and :reading-source-changed items stay out of the graph with a
;; typed reason, are reported in the manifest and coverage, and every other
;; reading still counts.

(def drift-corpus-root "/home/joe/notes/wm-next/giraffe-excursions/")
(def drift-edges "/home/joe/code/futon3c/holes/missions/giraffe-mission-edges.json")

(defn drift-fixture
  "A real-checker fixture corpus over five members that are open items whose
  records the real checker still passes.  DROP removes one member; its record
  lines stay in the batch (unadmitted), so it has no current reading."
  [& {:keys [drop]}]
  (let [members (json/parse-string (slurp (str drift-corpus-root "members.json")) true)
        batches (->> (.list (io/file drift-corpus-root))
                     ;; top level only: subdirectories (batches/, closing/,
                     ;; stale/) hold working files, not the corpus
                     (filter #(re-matches #"batch-.*\.jsonl" %))
                     (sort))
        lines (mapcat #(str/split-lines (slurp (str drift-corpus-root %))) batches)
        records (keep #(try (json/parse-string % true) (catch Exception _ nil))
                      (remove str/blank? lines))
        with-records (set (map :id records))
        census (e/missions-and-excursions->input {:edges-file drift-edges})
        open-ids (->> (get-in census [:input :items])
                      (remove #(#{:completed :cancelled} (:state-group %)))
                      (map :id) (map name) (set))
        ;; candidates whose records exist -- some old records no longer pass
        ;; the checker (their sources drifted; that is why zai-6 re-read
        ;; members), so admit only five that still pass
        candidates (->> members
                        (filter #(contains? with-records (:id %)))
                        (filter #(contains? open-ids (:id %)))
                        (take 20))
        candidate-ids (mapv :id candidates)
        candidate-lines (filter #(contains? (set candidate-ids)
                                            (:id (json/parse-string % true)))
                                (remove str/blank? lines))
        probe-dir (.toFile (java.nio.file.Files/createTempDirectory
                            "giraffe-drift-probe-"
                            (make-array java.nio.file.attribute.FileAttribute 0)))
        _ (io/copy (io/file drift-corpus-root "check_edges.py")
                   (io/file probe-dir "check_edges.py"))
        _ (spit (io/file probe-dir "members.json") (json/generate-string candidates))
        _ (spit (io/file probe-dir "batch-probe.jsonl") (str/join "\n" candidate-lines))
        probe (readings/consume
               {:checker (str probe-dir "/check_edges.py")
                :members-file (str probe-dir "/members.json")
                :batches [(str probe-dir "/batch-probe.jsonl")]})
        passing (set (map :id (filter #(= :read (:status %))
                                      (get-in probe [:coverage :members]))))
        chosen (->> candidates (filter #(contains? passing (:id %))) (take 5))
        chosen-ids (mapv :id chosen)
        _ (assert (= 5 (count chosen-ids))
                  "fixture needs five open members whose records still pass")
        members' (cond->> chosen
                   drop (remove #(= drop (:id %))))
        dir (.toFile (java.nio.file.Files/createTempDirectory
                      "giraffe-drift-fixture-"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
        batch-lines (filter #(contains? (set chosen-ids) (:id (json/parse-string % true)))
                            (remove str/blank? lines))]
    (io/copy (io/file drift-corpus-root "check_edges.py") (io/file dir "check_edges.py"))
    (spit (io/file dir "members.json") (json/generate-string members'))
    (spit (io/file dir "batch-fixture.jsonl") (str/join "\n" batch-lines))
    {:dir (str dir) :chosen chosen-ids :dropped drop}))

(defn drift-adapter [fixture]
  (let [dir (:dir fixture)
        adapter (e/missions-and-excursions->input
                 {:edges-file drift-edges
                  :reading-checker (str dir "/check_edges.py")
                  :members-file (str dir "/members.json")
                  :reading-batches [(str dir "/batch-fixture.jsonl")]
                  :reading-scope :population/missions-excursions-tickets})
        pass (e/combined-pass (:input adapter))]
    {:adapter adapter :pass pass}))

(deftest missing-member-is-unread-new-and-everything-else-still-counts
  (let [fx (drift-fixture :drop (nth (:chosen (drift-fixture)) 0))
        {:keys [adapter pass]} (drift-adapter fx)
        order-ids (set (map :id (:order pass)))
        coverage (get-in adapter [:input :graph-coverage])
        drift (:drift coverage)]
    (is (= 4 (count (:order pass))) "only the four read members remain in the graph")
    (is (not (contains? order-ids (keyword (:dropped fx)))))
    (is (contains? (set (:unread-new drift)) (keyword (:dropped fx))))
    (is (= :partial (:status coverage)))
    (is (= 4 (get-in coverage [:drift-counts :read])))
    (is (= (- (:open-item-count coverage) 4)
           (+ (get-in coverage [:drift-counts :unread-new])
              (get-in coverage [:drift-counts :reading-source-changed]))))
    (is (= 0 (get-in coverage [:drift-counts :members-not-open])))
    (is (some #(and (= (:id %) (keyword (:dropped fx))) (= :unread-new (:reason %)))
              (:excluded adapter)))
    (let [root (str (java.nio.file.Files/createTempDirectory
                     "giraffe-drift-run-" (make-array java.nio.file.attribute.FileAttribute 0)))
          written (writer/write-run! {:root root :adapter-result adapter :pass-output pass})
          manifest (edn/read-string (slurp (str (:dir written) "/manifest.edn")))]
      (is (:ok? (writer/verify-run (:dir written))))
      (is (= :partial (get-in manifest [:reading-drift :status])))
      (is (= 4 (get-in manifest [:reading-drift :drift-counts :read])))
      (is (contains? (set (map name (get-in manifest [:reading-drift :drift :unread-new])))
                     (:dropped fx)))
      ;; from what the outer field reader consumes: the field carries ONLY
      ;; the read members -- the drifted one is not silently admitted -- and
      ;; its typed exclusion is in the run itself (excluded.edn row,
      ;; manifest :reading-drift, coverage :drift id lists, asserted above).
      ;; (Surfacing non-ticket drift as futon2 :outer-field/excluded rows is
      ;; a named futon2-side follow-up: futon2's absent-excluded path today
      ;; covers open tickets only.)
      (let [excursions (registry/load-excursions "/home/joe/code")
            observed (observation/giraffe-field
                      (:dir written) {:default-reach 1/2}
                      {:loaded-missions (registry/load-missions-from-files "/home/joe/code")
                       :loaded-excursions excursions :loaded-tickets {:tickets []}
                       :excursion-targets (mapv :id (:excursions excursions))})
            field-targets (set (map :target (:field observed)))]
        (is (= 4 (count field-targets)))
        (is (not (contains? field-targets (:dropped fx))))
        (is (contains? field-targets (nth (:chosen fx) 4)))))))

(deftest read-member-with-wrong-sha-is-reading-source-changed-and-does-not-count
  ;; The end-to-end construction of this case is impossible without touching
  ;; canonical files: check_edges.py itself requires the member pin's sha256
  ;; to equal the LIVE file digest, so a corpus member whose sha disagrees
  ;; with the adapter's source pin can only arise from the registry side (or
  ;; a live file changing after the corpus was made, which also fails the
  ;; checker).  The classification seam is therefore tested directly: a
  ;; member that IS read but whose sha no longer matches the pin must not
  ;; count as read.
  (let [pin {:path "/repo/E-x.md" :sha256 (apply str (repeat 64 \a))}
        item (fn [id sha] {:id (keyword id) :state-group :unstarted
                           :source-pin {:path "/repo/E-x.md" :sha256 sha}})
        open-items [(item "E-good" (apply str (repeat 64 \a)))
                    (item "E-bad" (apply str (repeat 64 \b)))]
        coverage {:members [{:id "E-good" :source pin :status :read}
                            {:id "E-bad" :source pin :status :read}
                            {:id "E-gone" :source pin :status :read}]}
        drift (e/reading-drift open-items coverage)]
    (is (= [:E-good] (:read drift)))
    (is (contains? (set (:reading-source-changed drift)) :E-bad))
    (is (not (contains? (set (:read drift)) :E-bad)))
    (is (= ["E-gone"] (:members-not-open drift)))))
