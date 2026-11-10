(ns futon3c.arxana.lexicon-views-test
  "Views for reviewing lexicon relations in the 象 stepper: index of
  entities, and one entity's referents in both directions with the newest
  assessment deciding the review status."
  (:require [clojure.test :refer [deftest is]]
            [futon1b-arxana :as f1] [futon1b-gates :as gates] [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact] [futon3c.arxana.service :as service]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store-http :as store-http]
            [futon3c.arxana.lexicon-views :as sut]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(defn- with-stack [f]
  (gates/seed-mission-contract!)
  ;; The review table is one per JVM; each test has its own store.
  (sut/reset-table!)
  (let [db (File/createTempFile "lexicon-views-" ".db")] (.delete db)
    (try (with-open [node (xtn/start-node)]
           (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1"
                                                  :arxana-index-path (.getPath db)})
                 cfg {:graph-store (store-http/http-store (str "http://127.0.0.1:"
                                                               (.getPort (.getAddress server)))
                                                          f1/service-principal)
                      :artifact-provider (artifact/in-memory-provider)}]
             (try (f cfg) (finally (f1-server/stop-server! server)))))
         (finally (.delete db)))))

(defn- call [cfg id op body]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id id :operation op
                         :principal/id "lexicon-test" :body body}))

(defn- assert! [cfg id record]
  (is (:ok (call cfg id :clusion/assert {:record record}))))

(defn- entity [slug]
  (records/make-clusion {:type-revision [:lexicon/entity 1] :endpoints []
                         :asserted-content {:entity/slug slug}}))

(defn- entity-attestation [slug name aka]
  (records/make-attestation {:subject-clusion-id (sut/slug->entity-id slug)
                             :producer "claude-6 lexicon 2026-11-08"
                             :basis :model
                             :method {:id "lexicon/entity-survey" :version 1}
                             :standing :unverified :coverage :complete
                             :content {:entity/slug slug :name name :aka aka}}))

(defn- referent [source verb target]
  (records/make-clusion {:type-revision [:workplan/referent 1]
                         :endpoints [{:role :entity :kind :clusion :target {:clusion/id (sut/slug->entity-id source)}}
                                     {:role :target :kind :clusion :target {:clusion/id (sut/slug->entity-id target)}}]
                         :asserted-content {:referent/role verb}}))

(defn- referent-attestation [referent-id raw]
  (records/make-attestation {:subject-clusion-id referent-id
                             :producer "claude-6 lexicon 2026-11-08"
                             :basis :model
                             :method {:id "lexicon/relation" :version 1}
                             :standing :unverified :coverage :complete
                             :content {:relation/raw raw :candidate? true}}))

(defn- append! [cfg id record]
  (is (:ok (call cfg id :evidence/append {:record record}))))

(defn- seed! [cfg]
  (assert! cfg "e-agency" (entity "agency"))
  (assert! cfg "e-pouch" (entity "kangaroo-pouch"))
  (assert! cfg "e-other" (entity "evidence-landscape"))
  (append! cfg "a-agency" (entity-attestation "agency" "Agency" "the Agency,Agency JVM"))
  (append! cfg "a-pouch" (entity-attestation "kangaroo-pouch" "kangaroo pouch" "pouch"))
  (append! cfg "a-other" (entity-attestation "evidence-landscape" "evidence landscape" nil))
  (let [r1 (referent "kangaroo-pouch" "extends" "agency")
        r2 (referent "agency" "uses" "evidence-landscape")]
    (assert! cfg "r1" r1)
    (append! cfg "a-r1" (referent-attestation (:id r1) "extends agency"))
    (assert! cfg "r2" r2)
    (append! cfg "a-r2" (referent-attestation (:id r2) "agency uses evidence-landscape"))
  [(:id r1) (:id r2)]))
(deftest entity-index-reads-stored-attestations-only
  (with-stack
    (fn [cfg]
      (seed! cfg)
      (let [view (sut/entity-index-view
                  (fn [op body]
                    (service/dispatch cfg {:protocol/version "arxana.v1"
                                           :message/id (str "ix:" (hash [op body]))
                                           :operation op :principal/id "lexicon-test" :body body}))
                  ["agency" "kangaroo-pouch" "not-in-store" "evidence-landscape"])]
        ;; A slug the store does not hold never enters the index, even though
        ;; it was seeded.
        (is (= #{"agency" "kangaroo-pouch" "evidence-landscape"}
               (set (map :entity/slug (:entities view)))))
        (is (= {:entity/slug "agency" :entity/name "Agency" :entity/aka "the Agency,Agency JVM"}
               (first (filter #(= "agency" (:entity/slug %)) (:entities view)))))
        (is (= "evidence landscape"
               (:entity/name (first (filter #(= "evidence-landscape" (:entity/slug %)) (:entities view))))))))))

(deftest relations-run-both-directions-and-newest-assessment-decides
  (with-stack
    (fn [cfg]
      (let [[r1 r2] (seed! cfg)
            read! (fn [op body]
                    (service/dispatch cfg {:protocol/version "arxana.v1"
                                           :message/id (str "rel:" (hash [op body]))
                                           :operation op :principal/id "lexicon-test" :body body}))
            pouch (sut/entity-relations-view read! "kangaroo-pouch")
            agency (sut/entity-relations-view read! "agency")]
        (is (= :complete (:result/status pouch)))
        ;; kangaroo-pouch extends agency: outgoing from the pouch, incoming
        ;; for agency — same referent record, two directions.
        (is (= [{:record/id r1 :entity/slug "kangaroo-pouch" :direction :outgoing
                 :referent/role "extends" :other/slug "agency"
                 :proposed-by "claude-6 lexicon 2026-11-08" :relation/raw "extends agency"
                 :review/status :proposed :review/actor nil}]
               (:relations pouch)))
        (is (= 1 (count (filter #(and (= :incoming (:direction %)) (= "kangaroo-pouch" (:other/slug %)))
                                (:relations agency)))))
        (is (= "uses" (:referent/role (first (:relations agency)))))
        ;; A slug the store does not hold is missing, never empty-complete.
        (is (= :missing (:result/status (sut/entity-relations-view read! "not-in-store"))))
        ;; Rulings: accepted first, then rejected.  futon1b's index returns
        ;; assessments NEWEST first (it ignores :time/order), so the newest
        ;; ruling decides — not the oldest.
        (is (:ok (call cfg "acc" :workplan/assess-referent
                       {:subject/id r1 :verdict :accepted :actor "joe"
                        :reason "reviewed in 象 stepper" :time "2026-11-10T10:00:00.000Z"})))
        (is (= :accepted (:review/status (get-in (sut/entity-relations-view read! "kangaroo-pouch")
                                                 [:relations 0]))))
        (is (:ok (call cfg "rej" :workplan/assess-referent
                       {:subject/id r1 :verdict :rejected :actor "joe"
                        :reason "reviewed in 象 stepper" :time "2026-11-10T10:01:00.000Z"})))
        (let [row (get-in (sut/entity-relations-view read! "kangaroo-pouch") [:relations 0])]
          (is (= :rejected (:review/status row)))
          (is (= "joe" (:review/actor row))))
        ;; An already-ruled relation is reported with its verdict, so callers
        ;; filtering for proposed rows never re-offer it.
        (is (not (some #(= :proposed (:review/status %))
                       (:relations (sut/entity-relations-view read! "kangaroo-pouch")))))
        (is r2)))))

(deftest service-ops-are-public-reads
  (with-stack
    (fn [cfg]
      (seed! cfg)
      ;; The index op seeds from the loader's sources; only slugs the store
      ;; holds appear, so this fresh store yields the three seeded entities
      ;; only when the seed files name them — assert on store-side behaviour
      ;; instead: the relations op through the service spine.
      (let [view (:ok (call cfg "svc-rel" :lexicon/entity-relations {:entity/slug "agency"}))]
        (is (= :complete (:result/status view)))
        (is (= "agency" (:entity/slug view)))
        (is (= 2 (count (:relations view))))))))

(defn- reader [cfg tag]
  (fn [op body]
    (service/dispatch cfg {:protocol/version "arxana.v1"
                           :message/id (str tag ":" (hash [op body]))
                           :operation op :principal/id "lexicon-test" :body body})))

(def ^:private seeded-slugs ["agency" "kangaroo-pouch" "not-in-store" "evidence-landscape"])

(defn- build! [cfg]
  (with-redefs [sut/lexicon-slugs (constantly seeded-slugs)]
    (is (true? @(sut/start-table-build! (reader cfg "build"))))))

(defn- table [cfg known]
  (:ok (call cfg (str "table:" known) :lexicon/review-table
             (if known {:table/version known} {}))))

(deftest review-table-holds-every-entity-and-each-relation-once
  (with-stack
    (fn [cfg]
      (let [[r1 r2] (seed! cfg)]
        ;; Before any build the op answers at once and says so.
        (with-redefs [sut/lexicon-slugs (constantly seeded-slugs)
                      sut/start-table-build! (fn [_] nil)]
          (is (= :building (:result/status (table cfg nil)))))
        (build! cfg)
        (let [t (table cfg nil)]
          (is (= :complete (:result/status t)))
          (is (= [{:entity/slug "agency" :entity/name "Agency" :entity/aka "the Agency,Agency JVM"}
                  {:entity/slug "kangaroo-pouch" :entity/name "kangaroo pouch" :entity/aka "pouch"}
                  {:entity/slug "evidence-landscape" :entity/name "evidence landscape"}]
                 (:entities t)))
          ;; r1 is outgoing for the pouch and incoming for agency: one row.
          (is (= #{{:record/id r1 :entity/slug "kangaroo-pouch" :target/slug "agency"
                    :referent/role "extends" :proposed-by "claude-6 lexicon 2026-11-08"
                    :relation/raw "extends agency" :review/status :proposed :review/actor nil}
                   {:record/id r2 :entity/slug "agency" :target/slug "evidence-landscape"
                    :referent/role "uses" :proposed-by "claude-6 lexicon 2026-11-08"
                    :relation/raw "agency uses evidence-landscape"
                    :review/status :proposed :review/actor nil}}
                 (set (:relations t))))
          (is (= 2 (count (:relations t))))
          ;; A caller that holds this version is told so, with no table.
          (is (= {:result/status :unchanged :table/version (:table/version t)}
                 (select-keys (table cfg (:table/version t))
                              [:result/status :table/version :entities :relations])))
          ;; The per-entity op now answers from the table, with the rows the
          ;; store-reading view gives.
          (is (= (:relations (sut/entity-relations-view (reader cfg "direct") "agency"))
                 (:relations (:ok (call cfg "svc-agency" :lexicon/entity-relations
                                        {:entity/slug "agency"}))))))))))

(deftest ruling-updates-the-table-and-newest-ruling-decides
  (with-stack
    (fn [cfg]
      (let [[r1 _] (seed! cfg)
            row (fn [t] (first (filter #(= r1 (:record/id %)) (:relations t))))]
        (build! cfg)
        (let [v0 (:table/version (table cfg nil))]
          (is (:ok (call cfg "acc" :workplan/assess-referent
                         {:subject/id r1 :verdict :accepted :actor "joe"
                          :reason "reviewed in 象 stepper" :time "2026-11-10T10:00:00.000Z"})))
          (let [t1 (table cfg v0)]
            ;; No rebuild ran: the ruling alone changed the version and the row.
            (is (= :complete (:result/status t1)))
            (is (not= v0 (:table/version t1)))
            (is (= [:accepted "joe"] ((juxt :review/status :review/actor) (row t1))))
            ;; Accepted, then rejected: the store returns assessments newest
            ;; first, and the newest decides.  A table that kept the oldest
            ;; ruling (or the first one it saw) reads :accepted here.
            (is (:ok (call cfg "rej" :workplan/assess-referent
                           {:subject/id r1 :verdict :rejected :actor "joe"
                            :reason "reviewed in 象 stepper" :time "2026-11-10T10:01:00.000Z"})))
            (let [t2 (table cfg (:table/version t1))]
              (is (not= (:table/version t1) (:table/version t2)))
              (is (= [:rejected "joe"] ((juxt :review/status :review/actor) (row t2))))
              (is (= :rejected
                     (:review/status
                      (first (:relations (:ok (call cfg "svc-pouch" :lexicon/entity-relations
                                                    {:entity/slug "kangaroo-pouch"})))))))
              ;; A rebuild from the store agrees with the updated table, and
              ;; so leaves the version alone.
              (build! cfg)
              (is (= :unchanged (:result/status (table cfg (:table/version t2))))))))))))

(deftest ruling-during-a-build-is-not-lost
  (with-stack
    (fn [cfg]
      (let [[r1 _] (seed! cfg)
            ruled? (atom false)
            base (reader cfg "slow-build")
            ;; The build reads r1's assessments (none yet); before it gets
            ;; the answer back, Joe rejects r1.  The build then holds a stale
            ;; row, which it must re-read when it finishes.
            read! (fn [op body]
                    (let [r (base op body)]
                      (when (and (= :graph/query op) (= :assessments (:query/type body))
                                 (= r1 (:subject/id body))
                                 (compare-and-set! ruled? false true))
                        (is (:ok (call cfg "rej-mid-build" :workplan/assess-referent
                                       {:subject/id r1 :verdict :rejected :actor "joe"
                                        :reason "ruled during a build"
                                        :time "2026-11-10T10:02:00.000Z"}))))
                      r))]
        (with-redefs [sut/lexicon-slugs (constantly seeded-slugs)]
          (is (true? @(sut/start-table-build! read!))))
        (is @ruled?)
        (is (= :rejected
               (:review/status (first (filter #(= r1 (:record/id %))
                                              (:relations (table cfg nil)))))))))))
