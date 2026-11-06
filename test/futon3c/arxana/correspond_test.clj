(ns futon3c.arxana.correspond-test
  "Unit tests for the D4 correspondence model: minting/uniqueness, immutable
  directional assertions, standing, assessments, filters, and caller-owned
  ordering. Fixture-backed tests live in correspond-fixture-test."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.correspond :as corr]))

(def ^:private rev-src "arxana:revision:sha256:aaaa")
(def ^:private rev-html "arxana:revision:sha256:bbbb")

(defn- authored [role-key subject]
  {:manifestation/authority "test"
   :artifact/family-id "fam"
   :manifestation/role-key role-key
   :manifestation/label role-key
   :manifestation/media-role :text/x-tex
   :manifestation/role :authored
   :manifestation/subject subject})

(defn- generated [role-key subject]
  {:manifestation/authority "test"
   :artifact/family-id "fam"
   :manifestation/role-key role-key
   :manifestation/label role-key
   :manifestation/media-role :text/html
   :manifestation/role :generated
   :manifestation/subject subject
   :manifestation/origin {:origin/tool "latexml_oxide" :origin/version "0.7.5"
                          :origin/source-revisions [rev-src]}})

(defn- mk-repo []
  (let [repo (corr/in-memory-repository)
        src (corr/register-manifestation! repo (authored "source/s1" "move-1"))
        html (corr/register-manifestation! repo (generated "html/s1" "move-1"))]
    {:repo repo :src src :html html}))

(defn- base-assertion [src html & {:keys [method verification direction]
                                   :or {method [:quote] verification :verified
                                        direction :source-to-target}}]
  {:source {:manifestation/id (:manifestation/id src)
            :artifact/revision-id rev-src
            :locator {:kind :quote-context :value "q" :line 1}}
   :target {:manifestation/id (:manifestation/id html)
            :artifact/revision-id rev-html
            :locator {:kind :dom-id :value "X1"}}
   :direction direction
   :cardinality :one-to-one
   :method method
   :verification verification
   :evidence [{:type :quote :value "q"}]
   :producer {:tool "t" :version "1"}})

;;; Manifestation registry -----------------------------------------------------

(deftest minting-determinism-test
  (let [repo (corr/in-memory-repository)
        def (authored "source/s1" "move-1")
        a (corr/register-manifestation! repo def)
        b (corr/register-manifestation! repo def)]
    (is (= (:manifestation/id a) (:manifestation/id b)))
    (is (re-find #"^arxana:manifestation:sha256:[0-9a-f]{64}$"
                 (:manifestation/id a)))))

(deftest conflicting-tuple-refused-test
  (let [repo (corr/in-memory-repository)]
    (corr/register-manifestation! repo (authored "source/s1" "move-1"))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"conflicting manifestation"
         (corr/register-manifestation!
          repo (assoc (authored "source/s1" "move-1")
                      :manifestation/label "a different definition"))))
    (testing "role-key reuse across roles on the same tuple is refused"
      (is (thrown? clojure.lang.ExceptionInfo
                   (corr/put-manifestation!
                    repo (assoc (authored "source/s1" "move-1")
                                :manifestation/role :generated
                                :manifestation/origin
                                {:origin/tool "t" :origin/version "1"
                                 :origin/source-revisions [rev-src]})))))))

(deftest authored-generated-never-alias-test
  (let [repo (corr/in-memory-repository)
        a (corr/register-manifestation! repo (authored "source/s1" "move-1"))
        g (corr/register-manifestation! repo (generated "html/s1" "move-1"))]
    (is (not= (:manifestation/id a) (:manifestation/id g)))
    (testing "generated without provenance is refused"
      (is (thrown? clojure.lang.ExceptionInfo
                   (corr/register-manifestation!
                    repo (dissoc (generated "html/s2" "move-1")
                                 :manifestation/origin)))))))

(deftest alias-requires-registered-test
  (let [repo (corr/in-memory-repository)
        a (corr/register-manifestation! repo (authored "source/s1" "move-1"))
        b (corr/register-manifestation! repo (authored "source/s1-renamed" "move-1"))]
    (corr/put-alias! repo (:manifestation/id a) (:manifestation/id b) "rename")
    (is (= 1 (count (corr/aliases repo (:manifestation/id a)))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (corr/put-alias! repo (:manifestation/id a) "arxana:manifestation:sha256:nope" "bad")))))

;;; Assertions ------------------------------------------------------------------

(deftest duplicate-tuple-refusal-test
  (let [{:keys [repo src html]} (mk-repo)
        a (corr/assert-correspondence! repo (base-assertion src html))]
    (is (re-find #"^arxana:correspondence:sha256:" (:id a)))
    (testing "semantically identical assertion is a duplicate, even with new transport metadata"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"already exists"
           (corr/assert-correspondence!
            repo (assoc (base-assertion src html) :asserted-at "2026-11-06T00:00:00Z")))))))

(deftest directionality-test
  (let [{:keys [repo src html]} (mk-repo)
        fwd (corr/assert-correspondence! repo (base-assertion src html))]
    (testing "direction filter distinguishes endpoints"
      (is (= [(:id fwd)]
             (map :id (corr/query repo {:manifestation/id (:manifestation/id src)
                                        :direction :source-to-target}))))
      (is (empty? (corr/query repo {:manifestation/id (:manifestation/id src)
                                    :direction :target-to-source})))
      (is (= [(:id fwd)]
             (map :id (corr/query repo {:manifestation/id (:manifestation/id html)
                                        :direction :target-to-source})))))
    (testing "quote method supports a reverse projection"
      (let [r (corr/reverse-assertion fwd)]
        (is (= :target-to-source (:direction r)))
        (is (= (:id fwd) (:projection/of r)))))
    (testing "transformation is directional; reverse projection refused"
      (let [t (corr/assert-correspondence!
               repo (-> (base-assertion src html :method [:transformation])
                        (assoc :evidence [{:type :transformation :from "a" :to "b"}])))]
        (is (not (corr/reversible? t)))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reversibility"
                              (corr/reverse-assertion t)))
        (is (corr/reversible? (assoc-in t [:method-options :reversible] true)))))))

(deftest partial-endpoint-test
  (let [{:keys [repo src html]} (mk-repo)
        p (corr/assert-correspondence!
           repo (-> (base-assertion src html :verification :partial)
                    (assoc-in [:source :locator] {:kind :codepoint-point :offset 42})))]
    (is (= :partial (:verification p)))
    (is (= :codepoint-point (get-in p [:source :locator :kind])))
    (is (nil? (get-in p [:source :locator :end])))
    (testing "point locators carrying an end are refused"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"no fabricated ranges"
           (corr/assert-correspondence!
            repo (assoc-in (base-assertion src html :verification :partial)
                           [:source :locator]
                           {:kind :codepoint-point :offset 1 :end 9})))))))

(deftest ambiguity-non-selection-test
  (let [{:keys [repo src html]} (mk-repo)
        html2 (corr/register-manifestation! repo (generated "html/s1-dup" "move-1"))
        a1 (corr/assert-correspondence! repo (base-assertion src html :verification :ambiguous))
        a2 (corr/assert-correspondence! repo (base-assertion src html2 :verification :ambiguous))
        found (corr/query repo {:manifestation/id (:manifestation/id src)
                                :verification :ambiguous})]
    (is (= 2 (count found)))
    (is (= #{(:id a1) (:id a2)} (set (map :id found))))
    (testing "candidates is direction-agnostic and selects nothing"
      (is (= [(:id a1)]
             (map :id (corr/candidates repo (:manifestation/id src)
                                       (:manifestation/id html)))))
      (is (= [(:id a2)]
             (map :id (corr/candidates repo (:manifestation/id src)
                                       (:manifestation/id html2))))))))

(deftest stale-assertion-immutable-history-test
  (let [{:keys [repo src html]} (mk-repo)
        s (corr/assert-correspondence!
           repo (-> (base-assertion src html)
                    (assoc :verification :stale
                           :evidence [{:type :quote :value "q-old"}])))]
    (is (= [(:id s)] (map :id (corr/query repo {:manifestation/id (:manifestation/id src)}))))
    (is (empty? (corr/query repo {:manifestation/id (:manifestation/id src)
                                  :current-only? true})))
    (is (empty? (corr/query repo {:manifestation/id (:manifestation/id src)
                                  :verification-floor :unverified})))))

;;; Assessments -------------------------------------------------------------------

(deftest assessment-conflict-coexists-test
  (let [{:keys [repo src html]} (mk-repo)
        a (corr/assert-correspondence! repo (base-assertion src html))
        yes (corr/assess! repo {:subject {:assertion/id (:id a)}
                                :verdict :accepted
                                :actor/id "human-1" :policy/id "p1" :policy/revision "r1"
                                :reason "looks right" :evidence/ids []
                                :assessed-at "2026-11-06T00:00:00Z"})
        no (corr/assess! repo {:subject {:assertion/id (:id a)}
                               :verdict :rejected
                               :actor/id "human-2" :policy/id "p1" :policy/revision "r1"
                               :reason "anchor drifted" :evidence/ids []
                               :assessed-at "2026-11-06T01:00:00Z"})]
    (is (= 2 (count (corr/assessments-for repo (:id a)))))
    (is (= :arxana/correspondence-assessment (:type yes)))
    (testing "assessments never mutate the assertion"
      (is (= a (corr/get-assertion repo (:id a)))))
    (testing "assessment-verdict filter"
      (is (= [(:id a)] (map :id (corr/query repo {:assessment-verdict :rejected}))))
      (is (empty? (corr/query repo {:assessment-verdict :absent}))))
    (is (not= (:assessment/id yes) (:assessment/id no)))))

(deftest explicit-bounded-absence-test
  (let [repo (corr/in-memory-repository)
        abs (corr/assess! repo {:subject {:expected/role :html-render
                                          :source/entity "move-9"
                                          :artifact/revision-id rev-src
                                          :search/bounded-by {:registry "test"
                                                              :family "fam"
                                                              :roles [:generated]}}
                                :verdict :absent
                                :actor/id "human-1" :policy/id "p1" :policy/revision "r1"
                                :reason "searched all generated manifestations"
                                :evidence/ids [] :assessed-at "2026-11-06T00:00:00Z"})]
    (is (= :absent (:verdict abs)))
    (testing "absence requires a bounded envelope"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"bounded search"
           (corr/assess! repo {:subject {:expected/role :html-render}
                               :verdict :absent
                               :actor/id "h" :policy/id "p"}))))
    (testing "a missing row is not evidence of absence: no assertion, no judgment"
      (is (empty? (corr/query repo {:assessment-verdict :absent}))))))

;;; Caller-owned ordering ----------------------------------------------------------

(deftest next-absent-and-ambiguous-test
  (let [repo (corr/in-memory-repository)
        s1 (corr/register-manifestation! repo (authored "source/m1" "move-1"))
        h1 (corr/register-manifestation! repo (generated "html/m1" "move-1"))
        s2 (corr/register-manifestation! repo (authored "source/m2" "move-2"))
        h2 (corr/register-manifestation! repo (generated "html/m2" "move-2"))
        h2b (corr/register-manifestation! repo (generated "html/m2-alt" "move-2"))
        _ (corr/register-manifestation! repo (authored "source/m3" "move-3"))
        _ (corr/register-manifestation! repo (authored "source/m4" "move-4"))
        _a1 (corr/assert-correspondence! repo (base-assertion s1 h1))
        _a2 (corr/assert-correspondence! repo (base-assertion s2 h2 :verification :ambiguous))
        _a3 (corr/assert-correspondence! repo (base-assertion s2 h2b :verification :ambiguous))]
    (testing "caller-owned order decides; Arxana does not invent sequencing"
      (is (= "move-3" (corr/next-absent repo ["move-1" "move-2" "move-3"])))
      (is (= "move-3" (corr/next-absent repo ["move-3" "move-4" "move-1"])))
      (is (= "move-4" (corr/next-absent repo ["move-4" "move-3" "move-1"])))
      (is (= "move-2" (corr/next-ambiguous repo ["move-1" "move-2" "move-3"])))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"order"
                            (corr/next-absent repo nil))))
    (testing "all-manifestations returns every manifestation without identity merger"
      (is (= 3 (count (corr/all-manifestations repo "move-2")))))))

;;; Usefulness records ---------------------------------------------------------------

(deftest usefulness-namespaced-opaque-test
  (let [{:keys [repo src html]} (mk-repo)
        a (corr/assert-correspondence! repo (base-assertion src html))]
    (corr/put-usefulness! repo {:futon3c.usefulness/subject "move-1"
                                :futon3c.usefulness/rating :useful
                                :futon3c.usefulness/instrument "masked-exercise"})
    (is (= 1 (count (corr/usefulness-for repo "move-1"))))
    (testing "core arxana keys and unnamespaced keys are refused"
      (is (thrown? clojure.lang.ExceptionInfo
                   (corr/put-usefulness! repo {:arxana/verification :stale})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (corr/put-usefulness! repo {:rating :useful}))))
    (testing "a rating cannot change correspondence state"
      (is (= a (corr/get-assertion repo (:id a)))))))

;;; Query filters ------------------------------------------------------------------------

(deftest query-filters-test
  (let [{:keys [repo src html]} (mk-repo)
        v (corr/assert-correspondence! repo (base-assertion src html))
        u (corr/assert-correspondence!
           repo (-> (base-assertion src html :method [:source-map] :verification :unverified)
                    (assoc :evidence [{:type :source-map :value "0:1"}])))]
    (is (= [(:id v)] (map :id (corr/query repo {:methods #{:quote}}))))
    (is (= [(:id v)] (map :id (corr/query repo {:verification-floor :partial}))))
    (is (= [(:id u)] (map :id (corr/query repo {:verification :unverified}))))
    (is (= [(:id v)] (map :id (corr/query repo {:role :authored
                                                :verification-floor :verified}))))
    (is (= #{(:id v) (:id u)}
           (set (map :id (corr/query repo {:artifact/revision-id rev-src
                                           :direction :source-to-target})))))
    (is (= [(:id v)] (map :id (corr/query repo {:artifact/revision-id rev-src
                                                :direction :source-to-target
                                                :methods #{:quote}}))))))
