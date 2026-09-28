(ns futon3c.agency.disclosure-record-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.act-harness :as act-harness]
            [futon3c.agency.disclosure-record :as disclosure])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def prompt "Implement the queue. You did not specify ordering.")

(defn sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256")
                        (.getBytes text StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" (bit-and (int %) 0xff)) digest))))

(def edge
  {:evidence/id "e-edge-1"
   :evidence/body {:edge/id "invoke-job-1" :edge/kind :invoke
                   :edge/from "claude-17" :edge/to "codex-5"}})

(defn choice [id chosen]
  {:id id :kind :disclosure/choice :schema 1
   :author "codex-5" :at "2026-09-28T22:00:00Z"
   :source-job "invoke-job-1"
   :unspecified "queue ordering" :chosen chosen
   :affects {:kind :git-commit :id "abcdef1" :path "src/queue.clj"}
   :inside-request {:basis :source-span
                    :quote "You did not specify ordering."
                    :text-sha256 (sha256 prompt)}
   :act/stamp {:executor "codex-5" :signer "codex-5"
               :authority {:dispatch-edge "e-edge-1"}
               :executor-basis :declared}
   :act/harness (act-harness/plain "test:disclosure")})

(defn reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest two-disclosures-on-one-job-round-trip
  (let [a (choice "act:choice-a" "FIFO")
        b (choice "act:choice-b" "deadline then id")]
    (doseq [record [a b]]
      (is (= record (disclosure/validate-against-source! record prompt edge)))
      (let [hx (disclosure/->hyperedge record)]
        (is (some #{"job:invoke-job-1"} (:hx/endpoints hx)))
        (is (= record (disclosure/hyperedge->record hx)))))
    (is (not= (:id a) (:id b)))))

(deftest source-validation-refuses-concrete-bad-cases
  (testing "the quoted span is absent"
    (let [record (assoc-in (choice "act:no-span" "FIFO")
                           [:inside-request :quote] "not in the request")]
      (is (= :span-not-in-request
             (reason #(disclosure/validate-against-source! record prompt edge))))))
  (testing "the prompt was edited after its hash was recorded"
    (is (= :request-hash-mismatch
           (reason #(disclosure/validate-against-source!
                     (choice "act:edited" "FIFO") (str prompt " edited") edge)))))
  (testing "the orchestrator cannot disclose for the assignee"
    (let [record (-> (choice "act:wrong-author" "FIFO")
                     (assoc :author "claude-17")
                     (assoc-in [:act/stamp :executor] "claude-17")
                     (assoc-in [:act/stamp :signer] "claude-17"))]
      (is (= :not-the-assignee
             (reason #(disclosure/validate-against-source! record prompt edge))))))
  (testing "a grant cannot stand in for the dispatch edge"
    (let [record (assoc-in (choice "act:grant" "FIFO")
                           [:act/stamp :authority] {:grant "act:grant-1"})]
      (is (= :authority-not-dispatch-edge
             (reason #(disclosure/validate-against-source! record prompt edge))))))
  (testing "missing and duplicate dispatch edges never guess"
    (is (= :orchestrator-unknown
           (reason #(disclosure/validate-against-source!
                     (choice "act:none" "FIFO") prompt []))))
    (is (= :orchestrator-ambiguous
           (reason #(disclosure/validate-against-source!
                     (choice "act:two" "FIFO") prompt [edge edge])))))
  (testing "grant-like keys fail the closed record"
    (is (= :unexpected-key
           (reason #(disclosure/validate! (assoc (choice "act:scope" "FIFO")
                                                 :scope {:description "too much"})))))
    (is (= :unexpected-key
           (reason #(disclosure/validate! (assoc (choice "act:until" "FIFO")
                                                 :grant-until "2027-01-01T00:00:00Z")))))))

(deftest unrecorded-citations-are-explicit-only
  (is (= [{:id "act:missing" :reason :disclosure-unrecorded}]
         (disclosure/unrecorded-citations
          "Choices act:stored and act:missing; prose without an id is unknowable."
          ["act:stored"]))))
