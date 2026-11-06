(ns futon3c.arxana.resolve-test
  "INSTANTIATE increment 4 (Resolve) tests: three-stage anchor cascade,
  deterministic ordering and replay, ambiguity preservation, legacy finder
  translations, stale-input, unsupported finders, preview refusal, and
  promotion with acceptance provenance."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.resolve.evaluator :as evaluator]
            [futon3c.arxana.resolve.legacy :as legacy]
            [futon3c.arxana.resolve.promotion :as promotion]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store :as store])
  (:import [java.nio.charset StandardCharsets]))

;;; Fixtures -------------------------------------------------------------------

(def anchor-text "the compactness argument")
(def base-text
  (str "Alpha intro section.\n"
       "Target: " anchor-text ".\n"
       "Omega outro."))

(defn- utf8 ^bytes [^String s] (.getBytes s StandardCharsets/UTF_8))

(defn- anchor-config
  "Anchor over anchor-text as it appears in base-text."
  [text]
  (let [cp (.toArray (.codePoints ^String text))
        target (.toArray (.codePoints ^String anchor-text))
        n (alength cp) m (alength target)
        off (first (filter (fn [i]
                             (and (<= (+ i m) n)
                                  (loop [j 0]
                                    (cond (= j m) true
                                          (not= (aget cp (+ i j)) (aget target j)) false
                                          :else (recur (inc j))))))
                           (range (inc (- n m)))))]
    {:anchor/content-hash (digest/sha256-hex (utf8 anchor-text))
     :anchor/content-length (count anchor-text)
     :anchor/context-before "Target: "
     :anchor/context-after ".\nOmega outro."
     :anchor/hint {:offset off :line 2}}))

(defn- anchor-strategy [cfg]
  {:arxana/semantics :arxana.resolution/v1
   :arxana/strategy-id "test/anchor"
   :arxana/scope {:repository "test"}
   :arxana/cascade-mode :first-unique
   :arxana/finders [{:arxana/type :arxana.finder/anchor
                     :arxana/on [:ambiguous :missing]
                     :arxana/config cfg}]})

(defn- fixture
  "Store+provider+registered anchor strategy. The anchor is captured against
  the ORIGINAL base-text (revision R1 semantics) and then evaluated against
  `text` — this is what makes prefix/suffix/duplicate edits meaningful."
  [text]
  (let [s (store/mock-store)
        p (artifact/in-memory-provider)
        {:keys [ok]} (artifact/ingest! p "repo:paper.tex" (utf8 text) "text/plain")
        rid (:artifact/revision-id ok)
        strategy (anchor-strategy (anchor-config base-text))
        srid (service/register-strategy! s strategy)]
    {:store s :provider p :strategy strategy
     :artifact/revision-id rid :strategy/revision-id srid}))

(defn- resolve!
  [fx & [{:keys [request-id options]}]]
  (evaluator/evaluate
   {:artifact-provider (:provider fx)
    :strategy-source (fn [srid]
                       (get-in (:ok (store/get-entity (:store fx) srid))
                               [:props :strategy]))}
   {:request/id (or request-id "req-1")
    :artifact/revision-id (:artifact/revision-id fx)
    :strategy/revision-id (:strategy/revision-id fx)
    :options options}))

(defn- sem-digest [res]
  (env/semantic-digest env/scope-resolve-projection-v1
                       (assoc res :operation :scope/resolve)))

;;; Three-stage cascade ---------------------------------------------------------

(deftest unrelated-prefix-edit-test
  (testing "prefix edit: hint/hash fails, context/hash finds one verified candidate"
    (let [edited (str "NEW PREAMBLE PARAGRAPH.\n\n" base-text)
          fx (fixture edited)
          res (resolve! fx)]
      (is (= :resolved (:result/status res)))
      (is (= 1 (count (:result/candidates res))))
      (is (= :verified-hash
             (:verification (first (:result/candidates res)))))
      (is (= :context/hash
             (get-in res [:result/evidence 0 :evidence/diagnostics 0 :stage])))
      (is (= (digest/sha256-hex (utf8 anchor-text))
             (:matched-text-digest (first (:result/candidates res))))))))

(deftest unrelated-suffix-edit-test
  (testing "suffix edit leaves the hint in bounds: hint/hash still resolves"
    (let [edited (str base-text "\nAppended afterword.")
          fx (fixture edited)
          res (resolve! fx)]
      (is (= :resolved (:result/status res)))
      (is (= :verified-exact
             (:verification (first (:result/candidates res)))))
      (is (= :hint/hash
             (get-in res [:result/evidence 0 :evidence/diagnostics 0 :stage]))))))

(deftest duplicate-ambiguity-test
  (testing "a duplicate paragraph inserted earlier yields ambiguous — never a silent first-match"
    (let [edited (str "Target: " anchor-text ".\nOmega outro.\n\n" base-text)
          fx (fixture edited)
          res (resolve! fx)]
      (is (= :ambiguous (:result/status res)))
      (is (= 2 (count (:result/candidates res))))
      (testing "ordering is deterministic and non-semantic, not selection"
        (let [ranges (mapv #(get-in % [:location :range]) (:result/candidates res))]
          (is (= ranges (sort-by (juxt :start :end) ranges)))
          (is (apply not= (map :candidate/id (:result/candidates res))))))
      (testing "re-evaluation reproduces the same candidate set, in the same order"
        (is (= (mapv :candidate/id (:result/candidates res))
               (mapv :candidate/id (:result/candidates (resolve! fx {:request-id "req-2"})))))))))

;;; Stale input / preview / unsupported -------------------------------------------

(deftest stale-input-test
  (testing "bytes whose digest differs from the named revision: stale-input, no finder runs"
    (let [fx (fixture base-text)
          fake-rid "arxana:revision:sha256:0000000000000000000000000000000000000000000000000000000000000000"
          _ (swap! (:state (:provider fx)) assoc-in [:bytes fake-rid] (utf8 "tampered"))
          res (resolve! (assoc fx :artifact/revision-id fake-rid))]
      (is (= :stale-input (:result/status res)))
      (is (empty? (:result/evidence res))
          "no finder runs when integrity fails")
      (is (= :arxana.artifact/integrity-mismatch
             (get-in res [:result/diagnostics 0 :code]))))))

(deftest preview-refusal-test
  (testing "client-preview locators are refused before any finder runs"
    (let [fx (fixture base-text)
          preview (artifact/preview-segment anchor-text)
          res (resolve! (assoc fx :artifact/revision-id preview))]
      (is (= :missing (:result/status res)))
      (is (= :arxana.artifact/authority-revision-required
             (get-in res [:result/diagnostics 0 :code])))
      (is (empty? (:result/evidence res))))))

(deftest unsupported-finder-test
  (testing "embedding-proximity has no v1 executor: unsupported, never a successful no-op"
    (let [fx (fixture base-text)
          strategy (assoc (:strategy fx)
                          :arxana/finders
                          [{:arxana/type :arxana.finder/embedding-proximity
                            :arxana/on [:ambiguous :missing]
                            :arxana/config {:model-id "m" :model-digest "d"
                                            :vector-space-version 1 :threshold 0.9
                                            :tie-rule :none
                                            :neighbour-input-revision "r"}}])
          srid (service/register-strategy! (:store fx) strategy)
          res (resolve! (assoc fx :strategy/revision-id srid))]
      (is (= :unsupported (:result/status res)))
      (is (empty? (:result/candidates res)))
      (is (= :arxana.resolve/finder-not-executable
             (get-in res [:result/diagnostics 0 :code])))))
  (testing "an unknown finder type stays visible and is unsupported"
    (let [fx (fixture base-text)
          strategy (assoc (:strategy fx)
                          :arxana/finders
                          [{:arxana/type :arxana.finder/salience
                            :arxana/on [:ambiguous :missing]
                            :arxana/config {}}])
          srid (service/register-strategy! (:store fx) strategy)
          res (resolve! (assoc fx :strategy/revision-id srid))]
      (is (= :unsupported (:result/status res))))))

(deftest fuzzy-unverified-test
  (testing "inside-anchor edit: hash tiers fail; fuzzy recovery is unverified, never relabeled"
    (let [edited (str "Alpha intro section.\nTarget: the compactness ARGUMENT shifted.\nOmega outro.")
          s (store/mock-store)
          p (artifact/in-memory-provider)
          {:keys [ok]} (artifact/ingest! p "repo:paper.tex" (utf8 edited) "text/plain")
          cfg (assoc (anchor-config base-text)
                     :anchor/context-after ""
                     :anchor/fuzzy {:length-tolerance 0 :min-context-score 0.9})
          strategy (anchor-strategy cfg)
          srid (service/register-strategy! s strategy)
          fx {:store s :provider p :strategy strategy
              :artifact/revision-id (:artifact/revision-id ok)
              :strategy/revision-id srid}
          res-off (resolve! fx)
          res-on (resolve! fx {:options {:allow-unverified-fuzzy? true}})]
      (is (= :missing (:result/status res-off))
          "fuzzy disabled by default")
      (is (= :resolved-unverified (:result/status res-on)))
      (is (= 1 (count (:result/candidates res-on))))
      (is (= :unverified-fuzzy
             (:verification (first (:result/candidates res-on)))))
      (is (some? (:score-components (first (:result/candidates res-on))))
          "similarity features travel with fuzzy evidence"))))

;;; Determinism -------------------------------------------------------------------

(deftest deterministic-replay-test
  (testing "replaying identical input reproduces semantic result and evidence digests despite different request IDs"
    (let [fx (fixture base-text)
          r1 (resolve! fx {:request-id "req-a"})
          r2 (resolve! fx {:request-id "req-b"})]
      (is (= (sem-digest r1) (sem-digest r2)))
      (is (= (:result/evidence-digests r1) (:result/evidence-digests r2)))
      (is (not= (:request/id r1) (:request/id r2)))))
  (testing "evaluator version is digest-identified"
    (is (re-find #"^arxana:evaluator:sha256:[0-9a-f]{64}$"
                 evaluator/evaluator-version)))
  (testing "finder order feeds the strategy revision digest"
    (let [fx (fixture base-text)
          f0 (get-in fx [:strategy :arxana/finders 0])
          f1 (assoc f0 :arxana/config (assoc (:arxana/config f0) :anchor/hint {:offset 0 :line 1}))
          two (update (:strategy fx) :arxana/finders conj f1)]
      (is (not= (evaluator/strategy-revision-id two)
                (evaluator/strategy-revision-id
                 (update two :arxana/finders (comp vec reverse))))))))

;;; Legacy finder translations ------------------------------------------------------

(deftest legacy-classification-test
  (testing "all five legacy forms have explicit v1 status"
    (is (= :executable (legacy/classify :explicit)))
    (is (= :executable-when-configured (legacy/classify :symbol-as-term)))
    (is (= :executable-when-configured (legacy/classify :filename-mention)))
    (is (= :advisory-only (legacy/classify :embedding-proximity)))
    (is (= :translated-composition (legacy/classify :surface-form-hybrid)))
    (is (= :unsupported (legacy/classify :something-unknown)))))

(deftest legacy-import-and-translate-test
  (let [imported (legacy/import-legacy
                  {:legacy/id "legacy-strategy-7"
                   :legacy/raw-json "{\"finders\":[...]}"
                   :legacy/finders
                   [{:legacy/type :explicit
                     :legacy/config {:anchor {:anchor/content-hash "h"
                                              :anchor/content-length 3
                                              :anchor/context-before ""
                                              :anchor/context-after ""}
                                     :artifact/revision-id "arxana:revision:sha256:abc"}}
                    {:legacy/type :symbol-as-term
                     :legacy/config {:patterns ["defn <term>"] :language :clojure
                                     :normalization :unicode-nfc :term "foo"}}
                    {:legacy/type :filename-mention
                     :legacy/config {:repository-revision "r1" :root "src/"
                                     :path-normalization :posix
                                     :mention-grammar :bare-name
                                     :filename "paper.tex"}}
                    {:legacy/type :embedding-proximity :legacy/config {}}
                    {:legacy/type :surface-form-hybrid :legacy/config {}}]})]
    (testing "import is declaration-only and loses nothing"
      (is (legacy/declaration-only? imported))
      (is (= 5 (count (:arxana/declared-finders imported))))
      (is (= "{\"finders\":[...]}" (:arxana/legacy-json imported))))
    (testing "declaration-only execution is refused with a translation requirement"
      (let [fx (fixture base-text)
            srid (service/register-strategy! (:store fx) imported)
            res (resolve! (assoc fx :strategy/revision-id srid))]
        (is (= :unsupported (:result/status res)))))
    (testing "unpinned embedding step blocks translation of the whole strategy"
      (let [{:keys [ok error]} (legacy/translate-strategy
                                {:strategy-id "test/legacy-7"
                                 :scope {:repository "test"}
                                 :cascade-mode :first-unique
                                 :author "tester"}
                                imported)]
        (is (nil? ok))
        (is (= :unsupported (:error/category error)))
        (is (= :arxana.legacy/translation-incomplete (:error/code error)))))
    (testing "a fully translatable record produces an executable v1 strategy with provenance"
      (let [clean (update imported :arxana/declared-finders
                          (fn [ds] (vec (take 3 ds))))
            {:keys [ok]} (legacy/translate-strategy
                          {:strategy-id "test/legacy-7"
                           :scope {:repository "test"}
                           :cascade-mode :first-unique
                           :author "tester"}
                          clean)]
        (is (= :arxana.resolution/v1 (:arxana/semantics ok)))
        (is (= "legacy-strategy-7" (get-in ok [:arxana/translation :legacy/id])))
        (is (= "tester" (get-in ok [:arxana/translation :author])))
        (is (= 3 (count (:arxana/finders ok))))
        (is (every? #(= "legacy-strategy-7"
                        (get-in % [:arxana/provenance :legacy/id]))
                    (:arxana/finders ok)))))
    (testing "the surface-form-hybrid tag alone is unsupported"
      (let [{:keys [error]} (legacy/translate-finder
                             "legacy-strategy-7" 4
                             {:legacy/type :surface-form-hybrid :legacy/config {}})]
        (is (= :arxana.legacy/hybrid-tag-alone (:error/code error)))))
    (testing "offsets alone are insufficient for explicit"
      (let [{:keys [error]} (legacy/translate-finder
                             "legacy-strategy-7" 0
                             {:legacy/type :explicit
                              :legacy/config {:offset 42}})]
        (is (= :arxana.legacy/insufficient-translation (:error/code error)))))
    (testing "unknown legacy types are visible and unsupported, never dropped"
      (let [{:keys [error]} (legacy/translate-finder
                             "legacy-strategy-7" 9
                             {:legacy/type :telepathy :legacy/config {}})]
        (is (= :arxana.legacy/unknown-finder-type (:error/code error)))))))

;;; Promotion -----------------------------------------------------------------------

(defn- promote-body [fx res & [acceptance]]
  (cond-> {:result/digest (sem-digest res)
           :result/evidence {:request {:request/id (:request/id res)
                                       :artifact/revision-id (:artifact/revision-id fx)
                                       :strategy/revision-id (:strategy/revision-id fx)}
                             :result res}}
    acceptance (assoc :acceptance acceptance)))

(def acceptance
  {:acceptance/actor "agent:kimi-1"
   :acceptance/policy "policy:human-reviewed-ambiguity"
   :acceptance/policy-revision "sha256:pol1"
   :acceptance/reason "both duplicates are legitimate mentions; citing both"
   :acceptance/candidate-ids ["*"]})

(deftest promotion-test
  (let [fx (fixture base-text)
        deps {:graph-store (:store fx)
              :artifact-provider (:provider fx)
              :strategy-source (fn [srid]
                                 (get-in (:ok (store/get-entity (:store fx) srid))
                                         [:props :strategy]))}
        res (resolve! fx)]
    (testing "resolved promotes after authority replay; assertion carries digests and evaluator version"
      (let [{:keys [ok]} (promotion/promote deps (promote-body fx res))]
        (is (:created? ok))
        (is (re-find #"^arxana:resolution-assertion:sha256:[0-9a-f]{64}$"
                     (:assertion/id ok)))
        (let [stored (:ok (store/get-entity (:store fx) (:assertion/id ok)))]
          (is (= :arxana/resolution-assertion (:type stored)))
          (is (= evaluator/evaluator-version
                 (get-in stored [:props :evaluator/version])))
          (is (= (:result/evidence-digests res)
                 (get-in stored [:props :result/evidence-digests]))))))
    (testing "promotion is idempotent: same computation upserts the same assertion"
      (let [{aid1 :assertion/id} (:ok (promotion/promote deps (promote-body fx res)))
            second (:ok (promotion/promote deps (promote-body fx res)))]
        (is (= aid1 (:assertion/id second)))
        (is (false? (:created? second)))))
    (testing "replay mismatch is refused as conflict"
      (let [{:keys [error]} (promotion/promote
                             deps
                             (assoc-in (promote-body fx res)
                                       [:result/evidence :request :artifact/revision-id]
                                       (:artifact/revision-id (fixture (str base-text " ")))))]
        (is (= :conflict (:error/category error)))
        (is (= :arxana.promotion/replay-mismatch (:error/code error)))))
    (testing "non-promotable statuses are refused"
      (let [fx2 (assoc fx :strategy/revision-id
                       "arxana:strategy-revision:sha256:nope")
            missing-res (resolve! fx2)
            {:keys [error]} (promotion/promote deps (promote-body fx2 missing-res))]
        (is (= :invalid (:error/category error)))
        (is (= :arxana.promotion/status-not-promotable (:error/code error)))))
    (testing "non-promoted results leave no durable evidence in the store"
      (let [ents (:ok (store/find-entities
                       (:store fx) {:type :arxana/resolution-assertion}))]
        (is (= 1 (count ents)) "only the promoted assertion exists")))))

(deftest acceptance-provenance-test
  (let [edited (str "Target: " anchor-text ".\nOmega outro.\n\n" base-text)
        fx (fixture edited)
        deps {:graph-store (:store fx)
              :artifact-provider (:provider fx)
              :strategy-source (fn [srid]
                                 (get-in (:ok (store/get-entity (:store fx) srid))
                                         [:props :strategy]))}
        res (resolve! fx)]
    (is (= :ambiguous (:result/status res)))
    (testing "ambiguous promotion without acceptance is refused"
      (let [{:keys [error]} (promotion/promote deps (promote-body fx res))]
        (is (= :invalid (:error/category error)))
        (is (= :arxana.promotion/unattributed-acceptance (:error/code error)))))
    (testing "an unattributed Boolean acknowledgement is invalid"
      (let [{:keys [error]} (promotion/promote deps (promote-body fx res {:ack true}))]
        (is (= :arxana.promotion/unattributed-acceptance (:error/code error)))))
    (testing "attributed acceptance is stored separately and linked by accepts-resolution"
      (let [{:keys [ok]} (promotion/promote deps (assoc (promote-body fx res acceptance)
                                                        :principal/id "tester"))]
        (is (:created? ok))
        (is (some? (:acceptance/id ok)))
        (let [acc (:ok (store/get-entity (:store fx) (:acceptance/id ok)))]
          (is (= :arxana/resolution-acceptance (:type acc)))
          (is (= "agent:kimi-1" (get-in acc [:props :acceptance/actor])))
          (is (= "policy:human-reviewed-ambiguity"
                 (get-in acc [:props :acceptance/policy])))
          (is (= "sha256:pol1" (get-in acc [:props :acceptance/policy-revision])))
          (is (= (:assertion/id ok) (get-in acc [:props :assertion/id])))
          (is (= "tester" (get-in acc [:props :promoted-by]))))
        (let [edges (:ok (store/find-hyperedges
                          (:store fx) {:hx/type :arxana/accepts-resolution}))]
          (is (= 1 (count edges)))
          (is (= (:acceptance/id ok) (:hx/from (first edges))))
          (is (= (:assertion/id ok) (:hx/to (first edges)))))))))

;;; Service dispatch integration ----------------------------------------------------

(deftest dispatch-resolve-test
  (let [fx (fixture base-text)]
    (testing "scope/resolve flows through envelope + schema validation"
      (let [resp (service/dispatch
                  (:store fx) (:provider fx)
                  {:protocol/version "arxana.v1"
                   :message/id "m-42"
                   :operation :scope/resolve
                   :principal/id "tester"
                   :sent-at "2026-11-06T00:00:00Z"
                   :body {:artifact/revision-id (:artifact/revision-id fx)
                          :strategy/revision-id (:strategy/revision-id fx)}})]
        (is (nil? (:error resp)))
        (is (= :resolved (get-in resp [:ok :result/status])))))
    (testing "assertion/promote flows through dispatch with acceptance"
      (let [edited (str "Target: " anchor-text ".\nOmega outro.\n\n" base-text)
            fx2 (fixture edited)
            res (resolve! fx2)
            resp (service/dispatch
                  (:store fx2) (:provider fx2)
                  {:protocol/version "arxana.v1"
                   :message/id "m-43"
                   :operation :assertion/promote
                   :principal/id "tester"
                   :sent-at "2026-11-06T00:00:00Z"
                   :body (promote-body fx2 res acceptance)})]
        (is (nil? (:error resp)))
        (is (some? (get-in resp [:ok :assertion/id])))
        (is (some? (get-in resp [:ok :acceptance/id])))))))
