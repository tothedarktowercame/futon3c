(ns futon3c.watcher.multi-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.cyder :as cyder]
            [futon3c.watcher.multi :as sut]))

(defn- private-var [sym]
  (var-get (ns-resolve 'futon3c.watcher.multi sym)))

(defn- temp-mission-file [stem]
  (let [root (doto (java.io.File/createTempFile "scope-lane-" "")
               (.delete)
               (.mkdirs))
        dir (io/file root "holes" "missions")
        f (io/file dir (str stem ".md"))]
    (.mkdirs dir)
    (spit f "# Mission\n")
    f))

(deftest flexiarg-retraction-is-one-atomic-document-set
  (let [calls (atom [])
        entity-lookups (atom 0)
        path "/repo/library/demo/sample.flexiarg"]
    (with-redefs [sut/fetch-pattern-relations
                  (fn [pattern-id]
                    (is (= "demo/sample" pattern-id))
                    [{:relation/id "rel-a" :relation/type :pattern/has-if}
                     {:relation/id "rel-ignored" :relation/type :unrelated}])
                  sut/fetch-pattern-entity-ids
                  (fn [names]
                    (if (= 1 (swap! entity-lookups inc)) names []))
                  sut/retract-documents!
                  (fn [documents]
                    (swap! calls conj documents)
                    {:ok? true :count (count documents)})]
      (let [result (sut/retract-flexiarg! path)
            documents (first @calls)]
        (is (= 1 (count @calls)))
        (is (= 9 (:count result)))
        (is (= 1 (:batches result)))
        (is (= {:table :entities :id "demo/sample"} (first documents)))
        (is (= #{{:table :relations :id "rel-a"}}
               (set (filter #(= :relations (:table %)) documents))))
        (is (= 8 (count (filter #(= :entities (:table %)) documents))))))))

(deftest flexiarg-delete-retracts-instead-of-marking-code-stale
  (let [calls (atom [])]
    (with-redefs [sut/retract-flexiarg! (fn [path]
                                         (swap! calls conj [:retract path])
                                         {:ok? true :count 8})
                  sut/source-file-vertices (fn [& _]
                                             (throw (ex-info "code scan forbidden" {})))
                  sut/deletion-event! (fn [m] (swap! calls conj [:event (:path m)]))]
      (sut/handle-deletion! {:path "/repo/library/demo/gone.flexiarg"
                             :root "/repo" :label "repo" :run-id 1
                             :event-n 2 :hash "old"})
      (is (= [[:retract "/repo/library/demo/gone.flexiarg"]
              [:event "/repo/library/demo/gone.flexiarg"]]
             @calls)))))

(deftest flexiarg-retraction-drains-legacy-same-name-duplicates
  (let [lookups (atom [["demo/sample"] ["legacy-uuid"] []])
        batches (atom [])]
    (with-redefs [sut/fetch-pattern-entity-ids
                  (fn [_]
                    (let [result (first @lookups)]
                      (swap! lookups subvec 1)
                      result))
                  sut/fetch-pattern-relations (constantly [])
                  sut/retract-documents!
                  (fn [documents]
                    (swap! batches conj (vec documents))
                    {:ok true :count (count documents)})]
      (let [result (sut/retract-flexiarg! "/repo/library/demo/sample.flexiarg")]
        (is (= 2 (:batches result)))
        (is (= 2 (:count result)))
        (is (= ["demo/sample" "legacy-uuid"]
               (mapv (comp :id first) @batches)))))))

(deftest flexiarg-rename-ingests-new-before-retracting-old
  (let [calls (atom [])]
    (with-redefs [sut/ingest-event! (fn [m] (swap! calls conj [:ingest (:path m)]))
                  sut/retract-flexiarg! (fn [path]
                                         (swap! calls conj [:retract path])
                                         {:ok? true :count 8})
                  sut/rename-event! (fn [m] (swap! calls conj [:event (:from m) (:to m)]))]
      (sut/handle-rename! {:from "/repo/library/old/sample.flexiarg"
                           :to "/repo/library/new/sample.flexiarg"
                           :root "/repo" :label "repo" :run-id 1
                           :event-n 2 :hash "same"})
      (is (= [[:ingest "/repo/library/new/sample.flexiarg"]
              [:retract "/repo/library/old/sample.flexiarg"]
              [:event "/repo/library/old/sample.flexiarg"
               "/repo/library/new/sample.flexiarg"]]
             @calls)))))

(deftest run-cycle-skips-post-file-work-when-stop-requested
  (testing "shutdown gate suppresses heartbeat and commit ingest"
    (let [heartbeat-called? (atom false)
          commit-called? (atom false)
          cache (atom {})]
      (with-redefs [sut/build-plan (fn [_ _]
                                     {:root "/tmp/repo"
                                      :label "demo"
                                      :snapshot []
                                      :cache {}
                                      :moves {:renamed [] :deleted [] :added []}
                                      :ingest-paths []
                                      :first-cycle? false})
                    sut/detect-cross-root-moves (constantly [])
                    sut/heartbeat! (fn [& _] (reset! heartbeat-called? true))
                    sut/ingest-new-commits-for-root! (fn [& _] (reset! commit-called? true))]
        (reset! sut/!state {:stopping? true})
        (try
          (sut/run-cycle! {:roots [{:path "/tmp/repo" :label "demo"}]
                           :per-root-cache cache
                           :run-id 1
                           :event-n (atom 0)
                           :cycle-n (atom 0)
                           :cold-scan? false})
          (finally
            (reset! sut/!state nil)))
        (is (false? @heartbeat-called?))
        (is (false? @commit-called?))))))

(deftest watched-recognizes-stack-mission-docs
  (testing "watcher mission docs live under stack repos' holes/missions paths"
    (is (true? (sut/watched? "/home/joe/code/futon7/holes/missions/M-self-documenting-stack.md")))
    (is (true? (sut/watched? "/home/joe/code/futon2/resources/sorrys.edn")))
    (is (false? (sut/watched? "/home/joe/npt/missions/M-ukrns-wp.md")))
    (is (false? (sut/watched? "/home/joe/code/futon7/holes/missions/notes.md")))))

(deftest walk-root-prunes-noise-before-descending
  (let [root (doto (java.io.File/createTempFile "watch-prune-" "")
               (.delete)
               (.mkdirs))
        source (io/file root "src" "demo.clj")
        git-file (io/file root ".git" "objects" "ignored.clj")
        module-file (io/file root "node_modules" "pkg" "ignored.clj")]
    (doseq [f [source git-file module-file]]
      (.mkdirs (.getParentFile f))
      (spit f "(ns demo)\n"))
    (is (= #{(.getPath source)}
           (set (keys (sut/walk-root (.getPath root))))))))

(deftest scope-lane-flag-off-does-not-queue
  (testing "load-dark default leaves mission-scope lane with no pending work"
    (let [pending (private-var '!pending-missions)
          state (private-var '!mission-maintenance)
          mission-file (temp-mission-file "M-scope-lane")]
      (reset! pending {})
      (reset! state {:executor nil})
      (with-redefs [sut/scope-lane-enabled? (constantly false)]
        (is (nil? (#'sut/enqueue-mission-maintenance! (.getPath mission-file))))
        (is (empty? @pending))
        (is (false? (:enabled? (sut/mission-maintenance-status))))))))

(deftest scope-lane-debounce-waits-at-least-two-seconds
  (let [ready (#'sut/ready-maintenance-entries
               10000
               {"M-wait" {:stem "M-wait" :last-seen-at 8001}
                "M-ready" {:stem "M-ready" :last-seen-at 8000}})]
    (is (= ["M-ready"] (mapv :stem ready)))))

(deftest scope-lane-reingest-detects-ingests-and-broadcasts
  (testing "manual script mechanics are preserved without shelling to Drawbridge"
    (let [calls (atom [])]
      (with-redefs-fn {#'sut/detect-mission-scopes!
                       (fn [path]
                         (swap! calls conj [:detect path])
                         {:duration-ms 7})
                       #'sut/scope-tree-binders
                       (fn [stem]
                         (swap! calls conj [:binders stem])
                         ["eightfold-phase" "map-item"])
                       #'sut/ingest-scope-binder!
                       (fn [stem binder]
                         (swap! calls conj [:ingest stem binder])
                         "ok")
                       #'sut/broadcast-mission-scopes-updated!
                       (fn [stem]
                         (swap! calls conj [:broadcast {"type" "mission_scopes_updated"
                                                        "mission" stem}])
                         true)}
        (fn []
          (let [report (sut/reingest-mission-scopes!
                        {:stem "M-scope-lane"
                         :path "/home/joe/code/futon3c/holes/missions/M-scope-lane.md"})]
            (is (= [[:detect "/home/joe/code/futon3c/holes/missions/M-scope-lane.md"]
                    [:binders "M-scope-lane"]
                    [:ingest "M-scope-lane" "eightfold-phase"]
                    [:ingest "M-scope-lane" "map-item"]
                    [:broadcast {"type" "mission_scopes_updated"
                                 "mission" "M-scope-lane"}]]
                   @calls))
            (is (= "M-scope-lane" (:mission report)))
            (is (= ["eightfold-phase" "map-item"] (:binders report)))
            (is (= 2 (:binder-count report)))
            (is (true? (:broadcast? report)))))))))

(deftest run-cycle-can-skip-commit-ingest
  (testing "file-event cycles stay live when commit catch-up is disabled"
    (let [commit-called? (atom false)
          cache (atom {})]
      (with-redefs [sut/build-plan (fn [_ _]
                                     {:root "/tmp/repo"
                                      :label "demo"
                                      :snapshot []
                                      :cache {}
                                      :moves {:renamed [] :deleted [] :added []}
                                      :ingest-paths []
                                      :first-cycle? false})
                    sut/detect-cross-root-moves (constantly [])
                    sut/heartbeat! (fn [& _] nil)
                    sut/ingest-new-commits-for-root! (fn [& _] (reset! commit-called? true))]
        (reset! sut/!state {:stopping? false})
        (try
          (sut/run-cycle! {:roots [{:path "/tmp/repo" :label "demo"}]
                           :per-root-cache cache
                           :run-id 1
                           :event-n (atom 0)
                           :cycle-n (atom 0)
                           :cold-scan? false
                           :commit-ingest? false})
          (finally
            (reset! sut/!state nil)))
        (is (false? @commit-called?))))))

(deftest safe-cycle-updates-heartbeat-state
  (testing "successful cycles stamp timestamps and touch CYDER"
    (let [touched? (atom false)]
      (with-redefs [sut/run-cycle! (fn [_] :ok)
                    cyder/touch! (fn [id]
                                   (when (= "multi-watcher" id)
                                     (reset! touched? true)))]
        (reset! sut/!state {:stopping? false
                            :last-cycle-started-at nil
                            :last-cycle-finished-at nil
                            :last-error "old"
                            :last-subtask nil})
        (try
          (#'futon3c.watcher.multi/safe-cycle! {})
          (is (some? (:last-cycle-started-at @sut/!state)))
          (is (some? (:last-cycle-finished-at @sut/!state)))
          (is (nil? (:last-error @sut/!state)))
          (is (true? @touched?))
          (finally
            (reset! sut/!state nil)))))))
