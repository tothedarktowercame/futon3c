(ns futon3c.arxana.scope-at-position-test
  ":scope/at-position (M-jvm-arxana two-pane demo): smallest enclosing
  REGISTERED scope over stored correspondence assertions; ambiguity
  preserved; typed refusals for no-scope; subject filter."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.arxana.apm-fixture :as apm]
            [futon3c.arxana.correspond :as corr]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.service :as service]))

(defn- apm-stack []
  (let [repo (corr/in-memory-repository)
        apm (apm/assert-into-repository! repo)]
    {:cfg {:graph-store nil
           :artifact-provider nil
           :correspondence-repo repo
           :journal nil
           :subscriptions nil}
     :apm apm}))

(defn- req [op body]
  {:protocol/version env/protocol-version
   :message/id (str "test-" (name op))
   :operation op
   :principal/id "futon6-demo/apm"
   :sent-at "2026-11-07T00:00:00Z"
   :body body})

(defn- dispatch [cfg op body]
  (service/dispatch cfg (req op body)))

(deftest innermost-paragraph-with-enclosing-subsection
  (let [{:keys [cfg apm]} (apm-stack)
        tex-rev (get-in apm [:revisions "apm-a93J02.tex"])
        ;; inside paragraph informal-p1 (2727..3353), also inside subsection-1
        {:keys [ok error]} (dispatch cfg :scope/at-position
                                     {:artifact/revision-id tex-rev
                                      :position/codepoint 3000})]
    (is (nil? error))
    (is (= :resolved (:result/status ok)))
    (is (= "a93J02/informal-p1" (get-in ok [:result/innermost :subject/id])))
    (is (= 2 (count (:result/scopes ok))) "paragraph + enclosing subsection")
    (is (every? #(contains? % :assertion/id) (:result/scopes ok)))
    (is (string? (:semantic/digest ok)))))

(deftest position-in-lean-subsection-resolves-subsection-scope
  (let [{:keys [cfg apm]} (apm-stack)
        tex-rev (get-in apm [:revisions "apm-a93J02.tex"])
        ;; subsection-2 (Lean) has no paragraph scopes: the subsection is innermost
        a-sub2 (get-in apm [:assertions "a93J02/subsection-2"])
        mid (/ (+ (get-in a-sub2 [:source :locator :start])
                  (get-in a-sub2 [:source :locator :end])) 2)
        {:keys [ok]} (dispatch cfg :scope/at-position
                               {:artifact/revision-id tex-rev
                                :position/codepoint mid})]
    (is (= :resolved (:result/status ok)))
    (is (= "a93J02/subsection-2" (get-in ok [:result/innermost :subject/id])))
    (is (= :dom-id (get-in ok [:result/innermost :target :locator :kind])))))

(deftest preamble-position-is-a-typed-refusal
  (let [{:keys [cfg apm]} (apm-stack)
        tex-rev (get-in apm [:revisions "apm-a93J02.tex"])
        ;; codepoint 10 is in the preamble: outside every registered scope
        {:keys [ok error]} (dispatch cfg :scope/at-position
                                     {:artifact/revision-id tex-rev
                                      :position/codepoint 10})]
    (is (nil? ok))
    (is (= :missing (:error/category error)))
    (is (= :arxana.scope/no-enclosing-scope (:error/code error)))))

(deftest unknown-revision-has-no-registered-scopes
  (let [{:keys [cfg]} (apm-stack)
        {:keys [error]} (dispatch cfg :scope/at-position
                                  {:artifact/revision-id "arxana:revision:sha256:deadbeef"
                                   :position/codepoint 100})]
    (is (= :arxana.scope/no-enclosing-scope (:error/code error)))))

(deftest equal-size-innermost-scopes-stay-ambiguous
  ;; The exact bad case the rule is named for: two overlapping registered
  ;; scopes of identical size must NOT be silently ordered; the innermost set
  ;; is a candidate set and :result/status is :ambiguous.
  (let [{:keys [cfg apm]} (apm-stack)
        tex-rev (get-in apm [:revisions "apm-a93J02.tex"])
        repo (:correspondence-repo cfg)
        mk (fn [role-key subject start end]
             (let [src (corr/register-manifestation!
                        repo {:manifestation/authority (:authority apm)
                              :artifact/family-id (:family apm)
                              :manifestation/role-key role-key
                              :manifestation/label role-key
                              :manifestation/media-role :text/x-tex
                              :manifestation/role :authored
                              :manifestation/subject subject})
                   tgt (corr/register-manifestation!
                        repo {:manifestation/authority (:authority apm)
                              :artifact/family-id (:family apm)
                              :manifestation/role-key (str role-key "/html")
                              :manifestation/label role-key
                              :manifestation/media-role :text/html
                              :manifestation/role :generated
                              :manifestation/subject subject
                              :manifestation/origin {:origin/tool "test"
                                                     :origin/version "0"
                                                     :origin/source-revisions [tex-rev]}})]
               (corr/assert-correspondence!
                repo {:source {:manifestation/id (:manifestation/id src)
                               :artifact/revision-id tex-rev
                               :locator {:kind :codepoint-range :start start :end end}}
                      :target {:manifestation/id (:manifestation/id tgt)
                               :artifact/revision-id (get-in apm [:revisions "index.html"])
                               :locator {:kind :dom-id :value "Sx1.SSx1"}}
                      :direction :source-to-target
                      :cardinality :one-to-one
                      :method [:quote]
                      :verification :ambiguous
                      :evidence [{:type :quote :value "twin"}]
                      :producer {:tool "test" :version "0"}})))
        _ (mk "twin-a" "a93J02/twin-a" 2800 2900)
        _ (mk "twin-b" "a93J02/twin-b" 2800 2900)
        {:keys [ok]} (dispatch cfg :scope/at-position
                               {:artifact/revision-id tex-rev
                                :position/codepoint 2850})]
    (is (= :ambiguous (:result/status ok)))
    (is (nil? (:result/innermost ok)))
    (is (= 2 (count (filter #(#{"a93J02/twin-a" "a93J02/twin-b"} (:subject/id %))
                            (:result/scopes ok)))))))

(deftest subject-filter-restricts-to-one-subjects-manifestations
  (let [{:keys [cfg apm]} (apm-stack)
        tex-rev (get-in apm [:revisions "apm-a93J02.tex"])
        {:keys [ok]} (dispatch cfg :scope/at-position
                               {:artifact/revision-id tex-rev
                                :position/codepoint 3000
                                :subject/id "a93J02/informal-p1"})]
    (is (= :resolved (:result/status ok)))
    (is (= ["a93J02/informal-p1"] (mapv :subject/id (:result/scopes ok))))))
