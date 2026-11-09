(ns futon3c.arxana.workplan-backlinks-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [jsonista.core :as json]
            [futon1b-arxana :as f1] [futon1b-gates :as gates] [futon1b-server :as f1-server]
            [futon3c.arxana.artifact :as artifact] [futon3c.arxana.service :as service]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.store-http :as store-http]
            [futon3c.arxana.workplan-backlinks :as sut]
            [xtdb.node :as xtn])
  (:import [java.io File]))

(def fixtures "test/resources/arxana/backlinks")
(defn- with-stack [f]
  (gates/seed-mission-contract!)
  (let [db (File/createTempFile "backlinks-" ".db") idx (File/createTempFile "backlinks-derived-" ".edn") _ (.delete db)]
    (spit idx (slurp sut/default-index))
    (try (with-open [node (xtn/start-node)]
           (let [server (f1-server/start-server! {:node node :port 0 :bind-host "127.0.0.1" :arxana-index-path (.getPath db)})
                 cfg {:graph-store (store-http/http-store (str "http://127.0.0.1:" (.getPort (.getAddress server))) f1/service-principal)
                      :artifact-provider (artifact/in-memory-provider) :workplan/backlinks-index (.getPath idx)}]
             (try (f cfg) (finally (f1-server/stop-server! server)))))
         (finally (.delete db) (.delete idx)))))
(defn- call [cfg id op body]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id id :operation op :principal/id "backlinks-test" :body body}))

(deftest exact-forms-build-a-deterministic-derived-index
  (let [rules (sut/read-rules) responses (sut/recorded-responses fixtures)
        a (sut/build-index rules responses) b (sut/build-index rules responses)
        switched (update-in rules [:rules 2 :forms 0] assoc :enabled? false)
        c (sut/build-index switched responses)]
    (is (= a b))
    (is (= {:hits 50 :kept 4 :dropped/metadata-only 46 :occurrences 4}
           (dissoc (get-in a [:stats "Agency"]) :entity/slug)))
    (is (= 24 (count (:backlinks a))))
    (is (= 19 (count (:backlinks c))))
    (is (= (remove #(and (= "kangaroo-pouch" (:entity/slug %)) (= "kangaroo" (:form %))) (:backlinks a))
           (:backlinks c)))
    (is (= #{"Agency"} (set (map :form (filter #(= "agency" (:entity/slug %)) (:backlinks a))))))))

(deftest stored-rules-and-public-view
  (with-stack
    (fn [cfg]
      ;; No entity is pre-asserted: the rules materialiser must write the
      ;; entities its rules point at (kangaroo-pouch is named by no item).
      (let [first-run (sut/materialise-rules! #(service/dispatch cfg %) (sut/read-rules))
            second-run (sut/materialise-rules! #(service/dispatch cfg %) (sut/read-rules))]
        (is (every? #(true? (get-in % [1 :ok :created?])) first-run))
        ;; C2 message replay returns its first outcome; content ids prove the
        ;; replay addressed exactly the same nine immutable records.
        (is (= (mapv #(mapv (fn [r] (get-in r [:ok :record/id])) %) first-run)
               (mapv #(mapv (fn [r] (get-in r [:ok :record/id])) %) second-run)))
        (doseq [slug ["agency" "giraffe" "kangaroo-pouch"]]
          (let [view (:ok (call cfg (str "view:" slug) :workplan/entity-backlinks {:entity/slug slug :query/limit 100}))]
            (is (= slug (get-in view [:entity :asserted-content :entity/slug])))
            (is (= :complete (:result/status view)))
            (doseq [id (:record/ids view)]
              (is (= id (get-in (call cfg (str "get:" id) :entity/get {:entity/id id}) [:ok :record :id]))))))
        (is (= ["agency" "giraffe" "kangaroo-pouch"]
               (get-in (call cfg "list" :workplan/entity-backlinks {}) [:ok :entities])))))))

(deftest joe-introductions-use-real-evidence-and-stored-rules
  (with-stack
    (fn [cfg]
      (sut/materialise-rules! #(service/dispatch cfg %) (sut/read-rules))
      (let [raw (json/read-value (slurp "test/resources/arxana/sessionista/joe-introduces.json") json/keyword-keys-object-mapper)
            entry (:entry raw) text (get-in entry [:evidence/body :text]) eid (:evidence/id entry)
            inbox (records/make-clusion {:type-revision [:lexicon/entity 1] :endpoints []
                                         :asserted-content {:entity/slug "inbox-zero"}})
            _ (call cfg "seed-inbox" :clusion/assert {:record inbox})
            first-run (:ok (call cfg "intro-1" :workplan/introduce
                             {:evidence/id eid :text text :actor "joe" :time (:evidence/at entry)
                              :candidate/slugs ["inbox-zero"]}))
            second-run (:ok (call cfg "intro-2" :workplan/introduce
                              {:evidence/id eid :text text :actor "joe" :time (:evidence/at entry)
                               :candidate/slugs ["inbox-zero"]}))
            by-slug (into {} (map (juxt :entity/slug identity) (:introductions first-run)))
            session-view (:ok (call cfg "session-view" :workplan/entity-backlinks
                                    {:entity/slug "sessionista-mode" :query/limit 10}))]
        ;; The lexicon declares "warrant" as an alias of test-registry.
        (is (= #{"inbox-zero" "test-registry" "sessionista-mode"} (set (keys by-slug))))
        (is (= (:record/ids first-run) (:record/ids second-run)))
        (let [intro (first (:introductions session-view)) att (first (:attestations intro))
              [a b] (get-in att [:content :codepoint-range])]
          (is (= eid (get-in att [:content :evidence/id])))
          (is (= "[[sessionista-mode]]" (subs text a b))))
        (is (false? (get-in session-view [:forms/effective 0 :enabled?])))
        (let [candidate-id (get-in session-view [:referents 0 :record :id])]
          (is (:ok (call cfg "accept-candidate" :workplan/assess-referent
                         {:subject/id candidate-id :verdict :accepted :actor "joe"
                          :reason "the turn says fast like the existing feature"
                          :time "2026-11-09T18:10:00.000Z"})))
          (is (true? (get-in (call cfg "session-on" :workplan/entity-backlinks
                                   {:entity/slug "sessionista-mode"})
                             [:ok :forms/effective 0 :enabled?])))
          (is (:ok (call cfg "reject-candidate" :workplan/assess-referent
                         {:subject/id candidate-id :verdict :rejected :actor "joe"
                          :reason "negative fixture" :time "2026-11-09T18:11:00.000Z"})))
          ;; The latest ruling counts: accepted then rejected is off again.
          (is (false? (get-in (call cfg "session-off-again" :workplan/entity-backlinks
                                    {:entity/slug "sessionista-mode"})
                              [:ok :forms/effective 0 :enabled?]))))
        ;; A seeded rule keeps per-form switches: a newer attestation turning
        ;; "Kangaroo" off takes effect and leaves "kangaroo" on.
        (let [switched (update-in (sut/read-rules) [:rules 2 :forms 1] assoc :enabled? false)
              att (:attestation (nth (sut/rule-records switched) 2))]
          (is (:ok (call cfg "kangaroo-off" :evidence/append {:record att})))
          (is (= {"kangaroo" true "Kangaroo" false}
                 (into {} (map (juxt :form/text :enabled?)
                               (get-in (call cfg "kangaroo-view" :workplan/entity-backlinks
                                             {:entity/slug "kangaroo-pouch"})
                                       [:ok :forms/effective]))))))
        ;; A different introduced entity whose only candidate Joe rejects stays off.
        (let [wv (:ok (call cfg "warrant-view" :workplan/entity-backlinks {:entity/slug "test-registry"}))
              wid (get-in wv [:referents 0 :record :id])]
          (is (:ok (call cfg "reject-warrant" :workplan/assess-referent
                         {:subject/id wid :verdict :rejected :actor "joe"
                          :reason "negative fixture" :time "2026-11-09T18:12:00.000Z"})))
          (is (false? (get-in (call cfg "warrant-off" :workplan/entity-backlinks
                                    {:entity/slug "test-registry"})
                              [:ok :forms/effective 0 :enabled?]))))
        ;; The public view is now independent of the seed file.
        (with-redefs [sut/read-rules (fn [& _] (throw (ex-info "seed unavailable" {})))]
          (is (some #{"sessionista-mode"}
                    (get-in (call cfg "stored-list" :workplan/entity-backlinks {}) [:ok :entities]))))))))

(deftest explicit-referent-is-a-minimal-edit-of-the-real-turn
  (with-stack
    (fn [cfg]
      (sut/materialise-rules! #(service/dispatch cfg %) (sut/read-rules))
      (let [raw (json/read-value (slurp "test/resources/arxana/sessionista/joe-introduces.json") json/keyword-keys-object-mapper)
            entry (:entry raw) original (get-in entry [:evidence/body :text])
            ;; No real [[X | r]] turn existed.  Change only the one real token,
            ;; retaining the recorded response shape and evidence id.
            edited (str/replace original "[[sessionista-mode]]"
                                           "[[sessionista-mode | Agency]]")
            response (:ok (call cfg "explicit-intro" :workplan/introduce
                                {:evidence/id (:evidence/id entry) :text edited :actor "joe"
                                 :time (:evidence/at entry)}))]
        (is (true? (:enabled? (first (filter #(= "sessionista-mode" (:entity/slug %))
                                               (:introductions response))))))
        (is (true? (get-in (call cfg "explicit-view" :workplan/entity-backlinks
                                 {:entity/slug "sessionista-mode"})
                           [:ok :forms/effective 0 :enabled?])))))))
