(ns futon3c.diagramprover.arxana-records-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.type-registry :as types]
            [futon3c.diagramprover.arxana-records :as ar]
            [futon3c.diagramprover.findings :as findings]
            [futon3c.diagramprover.lean-import :as li]
            [futon3c.diagramprover.host-pairs :as hp]
            [futon3c.diagramprover.term-schema :as ts]))

(def fixture "test/futon3c/diagramprover/fixtures/a02J03-borel_pi.jsonl")
(def schemas [(assoc ts/beta :rewrite-doc {:op :substitute :body '?B :argument '?A})
              (assoc li/lean-family :rewrite-doc '[:family-at [:lam ?B] ?A])])
(def sample
  (delay
    (let [hit (nth (:hits (li/load-hits fixture)) 18)
          loaded (li/host hit {:loader-sha "fixture-loader"})
          pins {:checker-sha "fixture-checker" :schema-digest (findings/schema-digest schemas)
                :mode :unordered :max-states 12 :event-time "2026-11-17T00:00:00.000Z"
                :host/locator {:capture/path fixture
                               :capture/sha256 (digest/sha256-hex (.getBytes (slurp fixture) "UTF-8")) :hit 18}}
          report (hp/host-pairs schemas (:host loaded) {:max-states 12 :settled-by? true})
          finding (first (filter #(= :non-joining (:finding/status %)) (findings/findings report pins)))
          rules (mapv #(ar/rule-record % :apm-printer) schemas)
          ids (zipmap (map :name schemas) (map :id rules))]
      {:source finding :rules rules :ids ids :converted (ar/finding-records finding ids {})})))

(deftest registry-and-invalid-endpoint-mutations
  (doseq [type [[:rule/content 1] [:conflict/finding 1] [:residual/accepted 1]]]
    (is (= (types/declaration type) (types/validate-declaration (types/declaration type)))))
  (let [record (get-in @sample [:converted :finding])]
    (is (map? (records/clusion-key record)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (records/clusion-key (update record :endpoints #(vec (reverse %))))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (records/clusion-key (assoc-in record [:endpoints 0 :kind] :entity))))))

(deftest identity-and-witness-mutations
  (let [{:keys [source ids converted]} @sample
        again (ar/finding-records source ids {})
        swapped (ar/finding-records (update source :finding/rules #(vec (reverse %))) ids {})
        other-pin (ar/finding-records (assoc-in source [:finding/pins :schema-digest] "different") ids {})
        other-evidence (ar/finding-records (assoc source :finding/occurrences 999
                                                 :finding/witness-path [{:rule :beta :position [0]}]) ids {})]
    (is (= converted again))
    (is (= (get-in converted [:finding :id]) (get-in swapped [:finding :id])))
    (is (not= (get-in converted [:finding :id]) (get-in other-pin [:finding :id])))
    (is (= (get-in converted [:finding :id]) (get-in other-evidence [:finding :id])))
    (is (not= (get-in converted [:evidence :id]) (get-in other-evidence [:evidence :id])))
    (is (= (:event-time (:finding/pins source)) (get-in converted [:finding :event-time])))))

(deftest real-finding-round-trip-and-residual
  (let [{:keys [source rules converted]} @sample
        record (:finding converted)
        residual (ar/residual-record (:id record) {:scope "fixture" :release "pilot" :reason "recorded exception"})]
    (is (= #{:nothing} (:finding/settled-by source)))
    (is (= :failing (get-in record [:asserted-content :evaluation])))
    (doseq [r (conj rules record residual)]
      (let [round (edn/read-string (pr-str r))]
        (is (= r round))
        (is (= (:id r) (records/clusion-id round)))
        (is (= (digest/canonical-digest r) (digest/canonical-digest round)))))
    (is (= (:id record) (get-in residual [:endpoints 0 :target :clusion/id])))
    (is (= record (get-in @sample [:converted :finding])))
    (is (not= (records/clusion-id record)
              (records/clusion-id (assoc-in record [:asserted-content :evaluation] :met))))
    (is (= :exact (get-in converted [:evidence :basis])))
    (is (= :complete (get-in converted [:evidence :coverage])))
    (is (= [{:path fixture :sha256 (get-in source [:finding/pins :host/locator :capture/sha256])}]
           (get-in converted [:evidence :source-refs])))
    (is (= (:id (:evidence converted)) (records/attestation-id (:evidence converted))))))

(deftest required-rewrite-data-and-acceptance
  (is (= :arxana.conflict/rewrite-doc-required
         (try (ar/rule-record ts/beta :apm-printer) nil
              (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
  (is (= :arxana.conflict/non-data
         (try (ar/rule-record (assoc ts/beta :rewrite-doc identity) :apm-printer) nil
              (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))
  (let [rule (first (:rules @sample))
        acceptance (ar/acceptance-attestation (:id rule) {:reviewer "fixture-reviewer" :packet "fixture-packet"
                                                         :merge-sha "fixture-merge" :repo "fixture-repo"})]
    (is (= :human (:basis acceptance)))
    (is (= (:id rule) (:subject-clusion-id acceptance)))
    (is (= "fixture-reviewer" (:producer acceptance)))
    (is (not= (:id acceptance) (:id (records/make-attestation (assoc acceptance :producer "other-reviewer")))))))

(deftest partial-evidence-and-explicit-locator
  (let [{:keys [source ids]} @sample
        partial (ar/finding-records (assoc source :finding/status :inconclusive :finding/evaluation :unknown) ids {})
        transcribed (ar/finding-records (assoc-in source [:finding/pins :host/locator] :transcribed) ids {})]
    (is (= :partial (get-in partial [:evidence :coverage])))
    (is (= [] (get-in transcribed [:evidence :source-refs])))
    (is (= :arxana.conflict/host-locator-required
           (try (ar/finding-records (update source :finding/pins dissoc :host/locator) ids {}) nil
                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))))
