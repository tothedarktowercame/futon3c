(ns futon3c.arxana.design-catalogue-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [cheshire.core :as json]
            [futon3c.arxana.design-catalogue :as d]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.external-ref :as external]
            [futon3c.arxana.records :as r]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.write :as write]
            [futon3c.arxana.type-registry :as types]
            [futon3c.diagramprover.sentence-records :as sentence]))

(def at "2026-11-18T00:00:00.000Z")
(defn manifestation [revision scope]
  (r/make-clusion
   {:type-revision [:sentence/manifestation revision]
    :endpoints [{:role :role :kind :clusion :target {:clusion/id "invented-role"}}]
    :asserted-content {:renderer-commit "invented-render" :renderer-commit-time at
                       :corpus-sha256 "invented-corpus" :wordings [] :count 0 :scope scope}}))
(def sentence-fixture
  (delay (sentence/records-from-switch-off
          "test/futon3c/diagramprover/fixtures/a97J08-sentence-switch-off.json"
          (edn/read-string (slurp "test/futon3c/diagramprover/fixtures/sentence-q83.edn")))))
(defn reference []
  (let [path "src/futon3c/arxana/type_registry.clj"]
    (r/make-external-reference
     {:ref/kind :file :store/id "futon3c/registry" :store/endpoint path
      :record/id "[:sentence/manifestation 3]" :schema/authority :file/sha256
      :valid-time at :revision/digest (digest/sha256-hex (.getBytes (slurp path) "UTF-8"))})))
(defn built []
  (d/worked-catalogue {:rules (:records @sentence-fixture) :reference (reference)
                      :positive (manifestation 3 :declaration) :negative (manifestation 3 :sentence)
                      :citations [:manifestation-scope :evidence-fibres]
                      :implementation-digest (digest/sha256-hex (.getBytes (slurp "src/futon3c/arxana/design_catalogue.clj") "UTF-8"))
                      :pins [{:fixture "a97J08-sentence-switch-off.json"
                              :sha256 (digest/sha256-hex (.getBytes (slurp "test/futon3c/diagramprover/fixtures/a97J08-sentence-switch-off.json") "UTF-8"))}]
                      :at at}))
(defn operation [record]
  (cond (:type-revision record) :clusion/assert
        (:ref/kind record) :external-reference/assert
        :else :evidence/append))
(defn validate [rs]
  (let [saved (atom {})
        memory (reify store/ArxanaGraphStore
                 (get-entity [_ id] {:ok (get @saved id)})
                 (conditional-write! [_ command]
                   (let [record (get-in command [:body :record])]
                     (swap! saved assoc (:id record) record)
                     {:ok {:created? true}})))]
    (mapv #(write/execute memory {:principal/id "catalogue-test" :message/id (:id %)} (operation %) %) rs)))

(deftest worked-records-and-real-write-boundary
  (let [{:keys [records choices runs]} (built)]
    (is (every? :ok (validate records)))
    (is (= :ok (:status (external/resolve-reference (reference)))))
    ;; The file resolver verifies the file digest, not the coordinate. Coordinate
    ;; lookup is separately explicit; it is not falsely attributed to the resolver.
    (is (types/declaration (edn/read-string (:record/id (reference)))))
    (doseq [c choices]
      (is (= (if (= :fibres (get-in c [:asserted-content :choice/key]))
               {:status :unknown :reason :no-check} {:status :enforced})
             (d/enforcement-status records (:id c)))))
    (is (every? #(= :pass (get-in % [:content :result])) runs))
    (is (= :violation (:result (d/enum-check (manifestation 3 :declaration) (manifestation 2 :sentence)))))
    (is (= :pass (:result (d/enum-check (manifestation 3 :declaration) (manifestation 3 :sentence)))))))

(deftest relational-plants
  (let [rules (vec (vals (:rules @sentence-fixture)))
        first-rule (first rules)
        duplicate (r/make-clusion (assoc-in first-rule [:asserted-content :rule/name] :invented-duplicate))]
    (is (= :pass (:result (d/rule-function-check rules))))
    (is (= :violation (:result (d/rule-function-check (conj rules duplicate))))))
  (let [choices (d/worked-choices)]
    (is (= {:result :violation :violations [:undocumented-feature]}
           (d/coverage-check choices [:manifestation-scope :undocumented-feature])))
    (is (= :pass (:result (d/coverage-check choices [:manifestation-scope])))))
  (let [{:keys [records choices checks links]} (built)
        bad (r/make-clusion
             (assoc-in (first links) [:endpoints 1 :target :clusion/id] (:id (first choices))))]
    (is (= :pass (:result (d/link-check records))))
    (let [ruling (first (filter #(= :ruling (get-in % [:asserted-content :kind])) choices))
          valid (d/link :design/shaped-by (first choices) ruling)
          bad-ruling (r/make-clusion (assoc-in valid [:endpoints 1 :target :clusion/id] (:id (first choices))))]
      (is (= :pass (:result (d/link-check (conj records valid)))))
      (is (= :violation (:result (d/link-check (conj records bad-ruling))))))
    (is (= :violation (:result (d/link-check (conj records bad)))))
    (is (= (:id bad) (:link (first (:violations (d/link-check (conj records bad)))))))
    ;; Metadata is documentary: direct write accepts this hand-made bad link.
    ;; Turning this into a write refusal requires a separate authority change.
    (is (:ok (last (validate (conj records bad)))))
    (is (thrown? clojure.lang.ExceptionInfo (d/link :design/checked-by (first choices) (first choices))))
    (is (thrown? clojure.lang.ExceptionInfo (d/link :design/shaped-by (first choices) (first choices))))
    (is (thrown? clojure.lang.ExceptionInfo (d/link :design/constrains (first choices) (first checks))))
    (doseq [t [:design/checked-by :design/shaped-by :design/constrains]]
      (is (not (:composition/typed-ports? (types/declaration [t 1])))))))

(deftest status-never-infers-a-missing-pass
  (let [{:keys [records choices checks runs]} (built)
        c (first choices) check (first checks)
        base (vec (remove #(= (:id %) (:id (first runs))) records))
        make-run #(d/check-run c check [] %1 {:result %2 :violations []})]
    (is (= {:status :unknown :reasons [:no-run]} (d/enforcement-status base (:id c))))
    (doseq [result [:unknown :dormant :error :stale]]
      (is (= {:status :unknown :reasons [result]}
             (d/enforcement-status (conj base (make-run at result)) (:id c)))))
    (is (= {:status :violated} (d/enforcement-status (conj records (make-run "2026-11-18T00:01:00Z" :violation)) (:id c))))
    (is (= {:status :enforced} (d/enforcement-status (conj records (make-run "2026-11-17T00:00:00Z" :violation)) (:id c))))
    (is (thrown? clojure.lang.ExceptionInfo (make-run at :green)))
    (testing "every linked check needs a latest passing run"
      (let [extra (d/check-record (assoc (:asserted-content check) :check/key :additional))
            linked (d/link :design/checked-by c extra)]
        (is (= {:status :unknown :reasons [:no-run]}
               (d/enforcement-status (conj records extra linked) (:id c))))))))

(def shared-fixture-path "test/resources/arxana/design-catalogue-checks.json")
(defn shared-fixtures
  "Invented, ground facts plus outcomes from the actual record-based core.logic
  checks. No second implementation computes expected results."
  []
  (let [coverage (fn [id documented citations]
                   {:id id :check :coverage :facts {:documented documented :citations citations}
                    :expected (d/coverage-check
                               [{:id "choice-a" :type-revision [:design/choice 1]
                                 :asserted-content {:applies-to documented}}] citations)})
        functions (fn [id facts]
                    {:id id :check :rule-function :facts {:rules facts}
                     :expected (d/rule-function-check
                                (mapv (fn [[rid qualname]]
                                        {:id rid :type-revision [:rule/content 1]
                                         :asserted-content {:rule/rewrite {:python/qualname qualname}}}) facts))})
        ref (r/make-external-reference {:ref/kind :file :store/id "invented"
                                       :store/endpoint "invented.edn" :record/id "coordinate"
                                       :schema/authority :file/sha256 :valid-time at :revision/digest "invented"})
        base [{:id "choice-a" :type-revision [:design/choice 1] :asserted-content {:kind :constraint}}
              {:id "ruling-a" :type-revision [:design/choice 1] :asserted-content {:kind :ruling}}
              {:id "check-a" :type-revision [:design/check 1]} ref]
        links (fn [id facts]
                (let [rs (mapv (fn [[lid type from to]]
                                 (let [[role kind key] (case type
                                                         :design/checked-by [:check :clusion :clusion/id]
                                                         :design/shaped-by [:ruling :clusion :clusion/id]
                                                         :design/constrains [:target :external-ref :ref/id])]
                                   {:id lid :type-revision [type 1]
                                    :endpoints [{:role :choice :kind :clusion :target {:clusion/id from}}
                                                {:role role :kind kind :target {key to}}]})) facts)]
                  {:id id :check :links
                   :facts {:choices ["choice-a" "ruling-a"] :checks ["check-a"]
                           :rulings ["ruling-a"] :refs [(:id ref)] :links facts}
                   :expected (d/link-check (into base rs))}))]
    {:format "design-catalogue-ground-fixtures-v1"
     :cases [(coverage "coverage-pass" [:feature-a] [:feature-a])
             (coverage "coverage-empty" [] [])
             (coverage "coverage-missing" [:feature-a] [:feature-a :feature-b :feature-b])
             (functions "function-pass" [["rule-a" "module.a"] ["rule-b" "module.b"]])
             (functions "function-duplicate" [["rule-a" "module.a"] ["rule-b" "module.a"]])
             (functions "function-missing" [["rule-a" nil] ["rule-b" ""] ["rule-c" 7]])
             (functions "function-null-duplicate" [["rule-a" nil] ["rule-b" nil]])
             (functions "function-empty-array-distinct-from-null" [["rule-a" nil] ["rule-b" []]])
             (links "links-pass" [["link-a" :design/checked-by "choice-a" "check-a"]
                                  ["link-b" :design/shaped-by "choice-a" "ruling-a"]
                                  ["link-c" :design/constrains "choice-a" (:id ref)]])
             (links "links-wrong-check" [["link-a" :design/checked-by "choice-a" "choice-a"]])
             (links "links-wrong-ruling" [["link-b" :design/shaped-by "choice-a" "choice-a"]])
             (links "links-missing-reference-and-choice" [["link-c" :design/constrains "absent" "absent-ref"]])]}))
(defn export-shared-fixtures!
  "Reproduce the committed JSON with the command recorded in the delivery notes."
  ([] (export-shared-fixtures! shared-fixture-path))
  ([path] (spit path (str (json/generate-string (shared-fixtures) {:pretty true}) "\n"))))
(deftest shared-fixtures-match-current-core-logic
  (doseq [case (:cases (shared-fixtures))]
    (is (= (if (#{"coverage-pass" "coverage-empty" "function-pass" "links-pass"} (:id case))
             :pass :violation)
           (get-in case [:expected :result])) (:id case)))
  (is (= (json/parse-string (json/generate-string (shared-fixtures)) true)
         (json/parse-string (slurp shared-fixture-path) true))))
