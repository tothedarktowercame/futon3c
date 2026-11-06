(ns futon3c.arxana.spine-test
  "Focused tests for the INSTANTIATE increment-3 spine: capability truth,
  unknown-field refusal, canonical digest stability, and preview-client gating."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.schema :as schema]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store :as store])
  (:import [java.nio.charset StandardCharsets]))

(defn- req [op body]
  {:protocol/version "arxana.v1"
   :message/id "m-1"
   :operation op
   :principal/id "tester"
   :sent-at "2026-11-06T00:00:00Z"
   :body body})

;;; Capability truth ----------------------------------------------------------

(deftest capability-truth-test
  (let [s (store/mock-store)
        caps (store/capabilities s)]
    (testing "mock reports D1's truth table, including absences"
      (is (= store/mock-capabilities caps))
      (is (false? (:entity-compare-and-set caps)))
      (is (false? (:subscription caps)))
      (is (false? (:entity-as-of caps))))
    (testing "absent capability yields :unsupported, never simulated success"
      (let [{:keys [ok error]} (store/require-capability s :entity-compare-and-set)]
        (is (nil? ok))
        (is (= :unsupported (:error/category error))))
      (is (:ok (store/require-capability s :entity-read))))
    (testing "conditional persistence stub refuses honestly (kimi-2 owns the route)"
      (let [{:keys [ok error]} (store/conditional-write! s {:message/id "m-9"})]
        (is (nil? ok))
        (is (= :unsupported (:error/category error)))
        (is (= :arxana.store/conditional-write-unavailable (:error/code error)))))
    (testing "handshake reports store truth and registered schema versions"
      (let [hs (service/handshake s)]
        (is (= caps (:store/capabilities hs)))
        (is (= 1 (get-in hs [:operations :scope/resolve])))
        (is (false? (:subscription hs)))
        (is (not (contains? (:operations hs) :correspondence/query))
            "unimplemented operations are absent, not advertised")))))

;;; Unknown-field refusal ------------------------------------------------------

(deftest unknown-field-refusal-test
  (testing "unknown envelope field is refused, naming the offenders"
    (let [{:keys [error]} (env/validate-envelope
                           (assoc (req :entity/get {:entity/id "e1"})
                                  :trace-id "abc123"))]
      (is (= :invalid (:error/category error)))
      (is (= :arxana.envelope/unknown-field (:error/code error)))
      (is (= [:trace-id] (get-in error [:error/detail :unknown-fields])))))
  (testing "unknown body field is refused"
    (let [{:keys [error]} (schema/validate-body :entity/get {:entity/id "e1" :debug true})]
      (is (= :invalid (:error/category error)))
      (is (= :arxana.schema/unknown-body-field (:error/code error)))))
  (testing "wrong protocol version is :unsupported"
    (let [{:keys [error]} (env/validate-envelope
                           (assoc (req :entity/get {:entity/id "e1"})
                                  :protocol/version "arxana.v0"))]
      (is (= :unsupported (:error/category error)))))
  (testing "unknown operation is :unsupported, not silently accepted"
    (let [{:keys [error]} (schema/validate-body :salience/score {:entity/id "e1"})]
      (is (= :unsupported (:error/category error)))))
  (testing "missing required body field is :missing, distinct from :invalid"
    (let [{:keys [error]} (schema/validate-body :scope/resolve {:artifact/revision-id "r"})]
      (is (= :missing (:error/category error))))))

;;; Canonical digest stability -------------------------------------------------

(def ^:private resolve-result
  {:protocol/semantics-version :arxana.resolution/v1
   :operation :scope/resolve
   :result/status :resolved
   :artifact/revision-id "arxana:revision:sha256:aaa"
   :strategy/revision-id "arxana:strategy-revision:sha256:bbb"
   :result/candidates [{:candidate/id "sha256:c1"
                        :location {:artifact/revision-id "arxana:revision:sha256:aaa"
                                   :unit :unicode-codepoint
                                   :range {:start 120 :end 137}}
                        :verification :verified-hash}]
   :result/evidence-digests ["sha256:e1"]})

(deftest semantic-digest-stability-test
  (testing "transport metadata changes leave the semantic digest stable"
    (let [d0 (env/semantic-digest env/scope-resolve-projection-v1 resolve-result)
          noisy (merge resolve-result
                       {:message/id "different-message"
                        :request/id "req-9"
                        :principal/id "someone-else"
                        :sent-at "2026-11-06T01:23:45Z"
                        :response/duration-ms 42
                        :trace/id "t-77"
                        :result/presentation "Resolved: foo"
                        :result/diagnostics ["timing note"]})]
      (is (= d0 (env/semantic-digest env/scope-resolve-projection-v1 noisy)))))
  (testing "a semantic change alters the digest"
    (let [d0 (env/semantic-digest env/scope-resolve-projection-v1 resolve-result)
          moved (update-in resolve-result
                           [:result/candidates 0 :location :range]
                           assoc :start 121)
          restatus (assoc resolve-result :result/status :ambiguous)]
      (is (not= d0 (env/semantic-digest env/scope-resolve-projection-v1 moved)))
      (is (not= d0 (env/semantic-digest env/scope-resolve-projection-v1 restatus)))))
  (testing "error digest is stable across localized prose, changes with category"
    (let [e1 (env/error :stale :arxana.store/head-moved
                        {:message "Your edit is stale."} [:refresh :rebase-and-resubmit])
          e2 (env/error :stale :arxana.store/head-moved
                        {:message "Deine Bearbeitung ist veraltet."} [:refresh :rebase-and-resubmit])
          e3 (assoc e1 :error/category :conflict)]
      (is (= (env/error-digest :revision/ingest e1)
             (env/error-digest :revision/ingest e2)))
      (is (not= (env/error-digest :revision/ingest e1)
                (env/error-digest :revision/ingest e3))))))

;;; Preview clients / revision identity ----------------------------------------

(deftest preview-client-gating-test
  (let [provider (artifact/in-memory-provider)
        text "Section 4.2 — the compactness argument."
        bs (.getBytes text StandardCharsets/UTF_8)
        preview (artifact/preview-segment text)]
    (testing "a segment hash alone is client-preview, not an authority revision"
      (is (artifact/preview-id? preview))
      (is (not (artifact/revision-id? preview))))
    (testing "artifact-dependent operations refuse preview locators with :missing"
      (let [{:keys [ok error]} (artifact/require-authority-revision preview)]
        (is (nil? ok))
        (is (= :missing (:error/category error)))
        (is (= [:revision/ingest] (:error/recovery error)))))
    (testing "malformed ids are :invalid"
      (is (= :invalid (:error/category
                       (:error (artifact/require-authority-revision "random-text"))))))
    (testing "authority ingest returns a whole-artifact content-addressed revision"
      (let [{:keys [ok]} (artifact/ingest! provider "repo:paper.tex" bs "text/x-tex")
            rid (:artifact/revision-id ok)]
        (is (artifact/revision-id? rid))
        (is (= {:ok rid} (artifact/require-authority-revision rid)))
        (is (= (artifact/revision-id-for-bytes bs) rid)
            "revision identity covers whole canonical bytes")
        (is (not= rid (artifact/revision-id-for-bytes
                       (.getBytes (str text " ") StandardCharsets/UTF_8)))
            "one changed byte means a new revision (D1-DR2)")
        (testing "provider serves immutable bytes that verify against the ID"
          (is (:ok (artifact/verify-bytes provider rid)))
          (let [{:keys [ok]} (artifact/revision-bytes provider rid)]
            (is (= "text/x-tex" (:media-type ok)))))
        (testing "same bytes ingest idempotently to the same revision ID"
          (let [{rid2 :ok} (artifact/ingest! provider "repo:paper.tex" bs "text/x-tex")]
            (is (= rid (:artifact/revision-id rid2)))))))
    (testing "unknown revision reads are :missing, never nil-flattened"
      (let [{:keys [error]} (artifact/revision-bytes
                             provider "arxana:revision:sha256:does-not-exist")]
        (is (= :missing (:error/category error)))))))

;;; Dispatch plumbing ------------------------------------------------------------

(deftest dispatch-spine-test
  (let [s (store/mock-store)
        p (artifact/in-memory-provider)]
    (testing "handshake flows through full envelope validation"
      (let [resp (service/dispatch s p (req :capabilities/handshake {}))]
        (is (= "m-1" (:message/id resp)))
        (is (= "arxana.v1" (:protocol/version resp)))
        (is (map? (:ok resp)))
        (is (nil? (:error resp)))))
    (testing "response carries exactly one of :ok / :error"
      (let [resp (service/dispatch s p (req :scope/resolve
                                            {:artifact/revision-id "r"
                                             :strategy/revision-id "s" :extra 1}))]
        (is (:error resp))
        (is (nil? (:ok resp)))
        (is (= :arxana.schema/unknown-body-field
               (get-in resp [:error :error/code]))))
      (testing "registered-but-unimplemented ops answer :unsupported honestly"
        (let [resp (service/dispatch s p (req :scope/resolve
                                              {:artifact/revision-id "r"
                                               :strategy/revision-id "s"}))]
          (is (= :unsupported (get-in resp [:error :error/category]))))))))
