(ns futon3c.arxana.benefit-filings-catalogue
  "In-memory documentation and finite probes; never publishes records."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon3c.arxana.benefit-filings :as filings]
            [futon3c.arxana.design-catalogue :as catalogue]
            [futon3c.arxana.queries :as queries]))

(defn manifest [] (edn/read-string (slurp (io/resource "arxana/demo-b-citations.edn"))))
(defn coverage [store at]
  (let [r (queries/records-by-type store {:type-revision [:design/choice 1] :limit 200 :valid-time at})]
    (if (or (not= :ok (:status r)) (:truncated? r) (not (sequential? (:records r))))
      {:status :inconclusive :response r}
      (assoc (catalogue/coverage-check (:records r) (mapv :feature (:reads (manifest)))) :status :ok))))

(defn probe
  "Known answers are explicit and pinned by the caller. H1's historical counts
  are 44 links in each lane, 51 naming rows, 27 named items and 32 entities;
  :items is intentionally not 27 because unnamed parents must survive."
  [store request expected]
  (when-not (seq expected)
    (throw (ex-info "A probe requires known answers" {:error :cascade/empty-probe})))
  (let [view (filings/read-filings store request)
        violations (vec (for [[path value] expected
                              :let [actual (get-in view path ::missing)]
                              :when (not= actual value)]
                          {:path path :expected value :actual actual}))]
    {:result (if (seq violations) :violation :pass) :violations violations}))

(def h1-answers
  [[[:status] :ok] [[:counts :serves-v1] 44] [[:counts :serves-v2] 44]
   [[:counts :naming-rows] 51] [[:counts :named-items] 27] [[:counts :entities] 32]])

(defn records
  "Caller supplies source revision, implementation digest and fixture locators.
  A check is declared; no run is fabricated or stored by this builder."
  [{:keys [revision implementation-digest fixtures]}]
  (let [choice (catalogue/choice
                {:choice/key :backend/cascade-benefit-filings :kind :constraint :lifecycle :accepted
                 :statement "Benefit filings are complete or inconclusive; unnamed parents remain and substrate identities never enter clusion naming joins."
                 :sources [{:repo "futon3c" :revision revision :path "src/futon3c/arxana/benefit_filings.clj" :section "read-filings and benefit-view"}]
                 :applies-to [:cascade/benefit-filings] :rationale "Expose historical filings with their evidence, without inventing current status or identity equivalence."
                 :alternatives []})
        check (catalogue/check-record
               {:check/key :cascade/benefit-filings :entry-point "futon3c.arxana.benefit-filings-catalogue/probe"
                :runtime :clojure :implementation-digest implementation-digest :fixtures fixtures
                :input-contract {:request :benefit-and-valid-time :expected :path-value-pairs :store :explicit-read-port}
                :output-contract {:result [:pass :violation] :coverage :partial}})]
    [choice check (catalogue/link :design/checked-by choice check)]))
