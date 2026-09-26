(ns futon3c.apm.memory-caption-store-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.memory-caption-store :as sut]
            [futon3c.peripheral.memory-recall :as recall]))

(defn temp-dir []
  (.toString
   (java.nio.file.Files/createTempDirectory
    "memory-caption" (make-array java.nio.file.attribute.FileAttribute 0))))

(def memory-entry
  {:evidence/id "memory-1" :evidence/type :memory
   :evidence/body {:name "Interior derivative uniqueness"
                   :hook "ODE uniqueness with interior derivatives"
                   :body "Use the endpoint extension argument."}})

(def memory-revision (machine/ledger-digest [(:evidence/body memory-entry)]))

(def observation
  {:applicability/schema sut/observation-schema
   :memory/id "memory-1" :memory/revision memory-revision
   :problem-id "p1" :task-id "task-1" :attempt-id "attempt-1"
   :search-receipt-id "search-1"
   :useful-when "Useful when continuous ODE trajectories share initial data and derivatives are available in the interval interior."
   :epistemic-status :suggested
   :conditions [{:condition "trajectory continuity" :status :absent
                 :evidence-id "attempt-1"}
                {:condition "Lipschitz vector field" :status :unchecked}]
   :task-observation {:outcome :considered
                      :reason "The attempt did not establish continuity."}
   :suggested-contexts [{:context "endpoint uniqueness"
                         :status :suggested :basis "same local argument"}]
   :scope-limit "Does not establish continuity or necessity of the conditions."
   :evidence-ids ["attempt-1"] :supersedes []})

(defn ports [entries]
  {:fetch-search-receipt
   (fn [id] (when (= id "search-1")
              {:job-id "student-job" :result-ids ["memory-1"]}))
   :fetch-memory #(get @entries %)
   :memory-admissible? #(contains? @entries %)
   :fetch-entry #(get @entries %)
   :append-entry
   (fn [entry]
     (swap! entries assoc (:evidence/id entry) entry)
     {:ok true :entry entry})})

(defn admit-fixture! [root entries]
  (binding [sut/*store-root* root]
    (let [observed (sut/admit-observation!
                    {:job-id "student-job" :agent-id "student" :role :student}
                    observation (ports entries))
          observation-id (get-in observed [:record :evidence/id])
          caption {:caption/schema sut/schema :caption/version 1
                   :memory/id "memory-1" :memory/revision memory-revision
                   :text "Useful when continuous ODE trajectories share initial data; endpoint use is suggested and continuity must be checked."
                   :epistemic-status :suggested
                   :conditions (:conditions observation)
                   :suggested-contexts (:suggested-contexts observation)
                   :scope-limit (:scope-limit observation)
                   :observation-ids [observation-id]
                   :basis-evidence-ids ["attempt-1"]
                   :compression {:trigger :review}}
          proposed (sut/admit-caption!
                    {:job-id "scribe-job" :agent-id "scribe" :role :scribe}
                    caption (ports entries))]
      {:observed observed :caption caption :proposed proposed})))

(deftest observations-distinguish-absence-from-unchecked-and-require-exposure
  (let [root (temp-dir) entries (atom {"memory-1" memory-entry})]
    (binding [sut/*store-root* root]
      (let [result (sut/admit-observation!
                    {:job-id "student-job" :agent-id "student" :role :student}
                    observation (ports entries))]
        (is (:ok result) result)
        (is (= [:absent :unchecked]
               (mapv :status (get-in result [:record :body :conditions]))))
        (is (= :applicability-observation-invalid
               (:error/code
                (sut/admit-observation!
                 {:job-id "other-job" :agent-id "student" :role :student}
                 observation (ports entries)))))))))

(deftest caption-review-is-independent-versioned-and-append-only
  (let [root (temp-dir) entries (atom {"memory-1" memory-entry})
        {:keys [observed proposed]} (admit-fixture! root entries)
        caption-id (get-in proposed [:record :evidence/id])]
    (is (:ok observed))
    (is (:ok proposed))
    (binding [sut/*store-root* root]
      (testing "an author cannot approve their own caption"
        (is (some #{:caption-reviewer-is-author}
                  (:findings
                   (sut/review-caption!
                    {:job-id "review" :agent-id "scribe"
                     :role :promotion-proctor}
                    {:caption/id caption-id :verdict :approve :reason "ok"}
                    (ports entries))))))
      (let [reviewed (sut/review-caption!
                      {:job-id "review" :agent-id "reviewer"
                       :role :promotion-proctor}
                      {:caption/id caption-id :verdict :approve
                       :reason "Grounded and qualified."}
                      (ports entries))
            current (sut/current-caption "memory-1")]
        (is (:ok reviewed) reviewed)
        (is (= 1 (:caption/version current)))
        (is (= caption-id (:caption/id current)))
        (is (= (:evidence/body memory-entry)
               (:evidence/body (@entries "memory-1"))))
        (is (:ok (sut/review-caption!
                  {:job-id "retract" :agent-id "other-reviewer"
                   :role :promotion-proctor}
                  {:caption/id caption-id :verdict :retract
                   :reason "Contradicted by later evidence."}
                  (ports entries))))
        (is (nil? (sut/current-caption "memory-1")))))))

(deftest compression-preserves-suggestions-unknowns-provenance-and-size
  (let [record {:evidence/id "obs"
                :body (assoc observation :epistemic-status :unknown)}
        base {:caption/schema sut/caption-schema :caption/version 1
              :memory/id "memory-1" :memory/revision memory-revision
              :text "Useful when continuity is established."
              :conditions (:conditions observation)
              :scope-limit (:scope-limit observation)
              :observation-ids ["obs"] :basis-evidence-ids ["attempt-1"]
              :compression {:trigger :evidence-threshold :threshold 1}}]
    (is (some #{:caption-unknown-status-lost}
              (:findings (sut/propose-compression
                          (assoc base :epistemic-status :supported
                                 :suggested-contexts []) [record]))))
    (is (some #{:caption-suggestion-lost}
              (:findings (sut/propose-compression
                          (assoc base :epistemic-status :unknown
                                 :suggested-contexts []) [record]))))
    (is (some #{:caption-size-limit-exceeded}
              (:findings (sut/propose-compression
                          (assoc base :text (apply str (repeat 1100 "x"))
                                 :epistemic-status :unknown
                                 :suggested-contexts
                                 (:suggested-contexts observation)) [record]))))))

(deftest caption-hit-joins-to-original-reviewed-memory-and-records-version
  (let [root (temp-dir) entries (atom {"memory-1" memory-entry})
        {:keys [proposed]} (admit-fixture! root entries)
        caption-id (get-in proposed [:record :evidence/id])]
    (binding [sut/*store-root* root]
      (is (:ok (sut/review-caption!
                {:job-id "review" :agent-id "reviewer"
                 :role :promotion-proctor}
                {:caption/id caption-id :verdict :approve :reason "ok"}
                (ports entries))))
      (let [caption-entry (@entries caption-id)
            result
            (recall/propose-patterns-by-query
             {:domain :mathematics} "endpoint uniqueness"
             {:limit 3
              :search-evidence (fn [_ _]
                                 {:index-as-of "i1"
                                  :results [{:score 9 :entry caption-entry}]})
              :recall-batch-fn
              (fn [_ endpoints _]
                {:ok true
                 :recalls
                 (mapv (fn [id]
                         {:endpoint id
                          :memories
                          (if (= id "memory-1")
                            [{:memory/id "memory-1" :memory/pattern-ids []
                              :depositor "original-author"
                              :provenance {:problem-id "original"}}]
                            [])}) endpoints)})})
            match (first (:content-matches result))]
        (is (= "memory-1" (:memory/id match)))
        (is (= :caption-match (:via match)))
        (is (= 1 (:caption/version match)))
        (is (= 1 (:checked-caption-count result)))
        (testing "an irrelevant query and a disconnected original return nothing"
          (let [miss (recall/propose-patterns-by-query
                      {:domain :mathematics} "unrelated divisor lattice"
                      {:limit 3
                       :search-evidence (fn [_ _]
                                          {:index-as-of "i2" :results []})
                       :recall-batch-fn
                       (fn [_ _ _] {:ok true :recalls []})})
                disconnected
                (recall/propose-patterns-by-query
                 {:domain :mathematics} "endpoint uniqueness"
                 {:limit 3
                  :search-evidence (fn [_ _]
                                     {:index-as-of "i3"
                                      :results [{:score 9 :entry caption-entry}]})
                  :recall-batch-fn
                  (fn [_ endpoints _]
                    {:ok true :recalls
                     (mapv #(hash-map :endpoint % :memories []) endpoints)})})]
            (is (empty? (:content-matches miss)))
            (is (empty? (:content-matches disconnected)))))))))

(deftest unknown-memory-and-absent-caption-fail-closed-compatibly
  (let [root (temp-dir) entries (atom {})]
    (binding [sut/*store-root* root]
      (is (some #{:applicability-memory-unknown}
                (:findings
                 (sut/admit-observation!
                  {:job-id "student-job" :agent-id "student" :role :student}
                  observation (ports entries)))))
      (is (nil? (sut/current-caption "never-captioned")))
      (is (= [] (sut/resolve-search-rows
                 [{:score 1
                   :entry {:evidence/type :reflection
                           :evidence/body
                           {:event :memory-caption
                            :caption/schema sut/caption-schema
                            :caption/id "missing" :caption/version 1
                            :memory/id "memory-1"}}}]))))))

(deftest historical-observation-requires-explicit-source-verification
  (let [root (temp-dir) entries (atom {"memory-1" memory-entry})
        historical
        {:schema :apm-memory-applicability-v1
         :memory-id "memory-1" :memory-content-digest memory-revision
         :context {:kind :historical-source :problem-id "p1"
                   :frame-id "f1" :source-attempt-ids ["source-1"]}
         :useful-when "Useful when an endpoint extension closes uniqueness."
         :epistemic-status :unknown
         :conditions [{:condition "continuity" :status :unchecked}]
         :task-observation {:disposition :unresolved
                            :reason "This is source derivation, not later use."
                            :evidence-refs ["source-1"]}
         :basis [{:source-ref "source-1" :source-digest "digest"
                  :claim "Pinned source artifact."}]
         :suggested-contexts []
         :scope-limit "Historical source only."}
        authority {:job-id "history-job" :agent-id "historian" :role :scribe}]
    (binding [sut/*store-root* root]
      (is (some #{:applicability-historical-source-unverified}
                (:findings
                 (sut/admit-observation! authority historical
                                         (ports entries)))))
      (is (:ok (sut/admit-observation!
                authority historical
                (assoc (ports entries) :verify-source
                       #(= "source-1" (:source-ref %)))))))))
