(ns futon3c.transport.evidence-replication-test
  "Tests for evidence replication over WebSocket.

   These tests verify the evidence replication protocol — the mechanism by which
   a laptop-side agent pushes locally-logged evidence to a server-side store
   over the existing Agency WS channel.

   The interesting invariants:
   - Loop protection: entries tagged :replicated are refused (no infinite hops)
   - Author preservation: original author survives replication (unlike messages)
   - Idempotency: duplicate IDs are caught by the store, not silently clobbered
   - Authentication gating: evidence only accepted after handshake
   - Store isolation: replication only works when an evidence store is configured
   - Tag mutation: server adds :replicated tag to incoming entries
   - Provenance: server records who replicated and when"
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [futon3c.transport.ws :as ws]
            [futon3c.transport.protocol :as proto]
            [futon3c.transport.ws.invoke :as ws-invoke]
            [futon3c.social.shapes :as shapes]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.social.persist :as persist]
            [futon3c.evidence.store :as estore]
            [futon3c.agency.registry :as reg]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (persist/reset-sessions!)
    (f)))

;; =============================================================================
;; Test helpers
;; =============================================================================

(defn- register-mock-agent!
  "Register a mock agent in the live registry."
  [agent-id-str type]
  (reg/register-agent!
   {:agent-id {:id/value agent-id-str :id/type :continuity}
    :type type
    :invoke-fn (fn [_prompt _session-id] {:result "ok" :session-id nil})
    :capabilities [:explore :edit]}))

(defn- make-test-ws
  "Create WS callbacks with mock send/close and an evidence store.
   Returns callbacks + :sent atom + :closed atom + :evidence-store."
  [& {:keys [evidence-store]
      :or {evidence-store (atom {:entries {} :order []})}}]
  (let [sent (atom [])
        closed (atom [])
        config {:registry (fix/mock-registry
                           {:peripheral-config {:evidence-store evidence-store}})
                :patterns (fix/mock-patterns)
                :send-fn (fn [ch data] (swap! sent conj {:ch ch :data data}))
                :close-fn (fn [ch] (swap! closed conj ch))}
        callbacks (ws/make-ws-callbacks config)]
    (assoc callbacks
           :sent sent
           :closed closed
           :evidence-store evidence-store)))

(defn- make-test-ws-no-store
  "Create WS callbacks WITHOUT an evidence store in the registry."
  []
  (let [sent (atom [])
        closed (atom [])
        config {:registry (fix/mock-registry)  ;; no :peripheral-config
                :patterns (fix/mock-patterns)
                :send-fn (fn [ch data] (swap! sent conj {:ch ch :data data}))
                :close-fn (fn [ch] (swap! closed conj ch))}
        callbacks (ws/make-ws-callbacks config)]
    (assoc callbacks :sent sent :closed closed)))

(defn- mock-request []
  {:request-method :get :uri "/ws" :request-uri "/ws"})

(defn- last-sent
  "Get the last sent frame as a parsed map."
  [sent-atom]
  (when-let [entry (last @sent-atom)]
    (json/parse-string (:data entry) true)))

(defn- ready-frame [agent-id]
  (json/generate-string {"type" "ready" "agent_id" agent-id}))

(defn- evidence-frame
  "Build an evidence replication frame."
  [entry]
  (json/generate-string {"type" "evidence" "entry" entry}))

(defn- make-sample-entry
  "A minimal well-formed evidence entry as it would arrive from the laptop.
   Uses string keys (JSON wire format) — Cheshire keywordizes these on parse.
   Values that are Clojure keywords get serialized as strings in JSON and
   arrive back as strings, matching the real-world round-trip."
  [& {:keys [id tags author]
      :or {id "e-laptop-001"
           tags ["test"]
           author "codex-laptop"}}]
  ;; String keys: JSON round-trip turns "evidence/id" → :evidence/id via Cheshire
  ;; String values for enums: "reflection" (not :reflection) — that's what JSON gives
  {"evidence/id" id
   "evidence/subject" {"ref/type" "mission" "ref/id" "M-test"}
   "evidence/type" "reflection"
   "evidence/claim-type" "observation"
   "evidence/author" author
   "evidence/at" "2026-02-23T10:00:00Z"
   "evidence/body" {"text" "discovered an interesting pattern"}
   "evidence/tags" tags})

(defn- connect!
  "Open connection + complete handshake. Returns channel keyword."
  [{:keys [on-open on-receive sent]} agent-id]
  (let [ch (keyword (str "ch-" agent-id))]
    (on-open ch (mock-request))
    (on-receive ch (ready-frame agent-id))
    (reset! sent [])
    ch))

;; =============================================================================
;; Protocol parsing tests
;; =============================================================================

(deftest parse-evidence-frame-valid
  (testing "well-formed evidence frame parses to :ws/type :evidence"
    (let [entry (make-sample-entry)
          result (proto/parse-ws-message (evidence-frame entry))]
      (is (= :evidence (:ws/type result)))
      (is (map? (:evidence/entry result)))
      ;; After Cheshire keywordize: "evidence/id" → :evidence/id
      (is (= "e-laptop-001" (:evidence/id (:evidence/entry result)))))))

(deftest parse-evidence-frame-missing-entry
  (testing "evidence frame without 'entry' field → error"
    (let [result (proto/parse-ws-message
                  (json/generate-string {"type" "evidence"}))]
      (is (= :invalid-frame (:error/code result))))))

(deftest parse-evidence-frame-non-map-entry
  (testing "evidence frame with non-map entry → error"
    (let [result (proto/parse-ws-message
                  (json/generate-string {"type" "evidence" "entry" "not-a-map"}))]
      (is (= :invalid-frame (:error/code result))))))

(deftest parse-evidence-frame-preserves-all-fields
  (testing "parser preserves entry field values through JSON round-trip"
    (let [entry (make-sample-entry :id "e-42" :author "joe-laptop")
          result (proto/parse-ws-message (evidence-frame entry))
          parsed-entry (:evidence/entry result)]
      ;; Keys get keywordized by Cheshire, values stay as strings
      (is (= "e-42" (:evidence/id parsed-entry)))
      (is (= "joe-laptop" (:evidence/author parsed-entry)))
      (is (= "reflection" (:evidence/type parsed-entry)))
      (is (= "observation" (:evidence/claim-type parsed-entry)))
      (is (= "2026-02-23T10:00:00Z" (:evidence/at parsed-entry))))))

;; =============================================================================
;; Render tests
;; =============================================================================

(deftest render-evidence-ack-structure
  (testing "evidence_ack frame has correct structure"
    (let [frame (proto/render-evidence-ack "e-laptop-001")
          parsed (json/parse-string frame true)]
      (is (= "evidence_ack" (:type parsed)))
      (is (= "e-laptop-001" (:evidence_id parsed)))
      (is (true? (:ok parsed))))))

;; =============================================================================
;; WS handler — authentication gating
;; =============================================================================

(deftest evidence-before-handshake-rejected
  (testing "evidence frame before readiness handshake → not-ready error"
    (let [{:keys [on-open on-receive sent]} (make-test-ws)
          ch :ch-pre-handshake]
      (on-open ch (mock-request))
      ;; Send evidence WITHOUT completing handshake
      (on-receive ch (evidence-frame (make-sample-entry)))
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))
        (is (= "not-ready" (:code frame)))))))

;; =============================================================================
;; WS handler — store configuration
;; =============================================================================

(deftest evidence-without-store-returns-error
  (testing "evidence replication with no evidence store configured → error"
    (let [{:keys [on-open on-receive sent] :as ws} (make-test-ws-no-store)
          ch (keyword "ch-no-store")]
      (on-open ch (mock-request))
      (on-receive ch (ready-frame "claude-1"))
      (reset! sent [])
      (on-receive ch (evidence-frame (make-sample-entry)))
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))
        (is (= "no-evidence-store" (:code frame)))))))

;; =============================================================================
;; WS handler — happy path
;; =============================================================================

(deftest evidence-replication-happy-path
  (testing "valid evidence entry → appended to store, ack sent"
    (let [{:keys [on-open on-receive sent evidence-store] :as ws} (make-test-ws)
          ch (connect! ws "claude-1")]
      (on-receive ch (evidence-frame (make-sample-entry :id "e-happy-1")))
      ;; Verify ack
      (let [frame (last-sent sent)]
        (is (= "evidence_ack" (:type frame)))
        (is (= "e-happy-1" (:evidence_id frame)))
        (is (true? (:ok frame))))
      ;; Verify stored
      (let [stored (estore/get-entry* evidence-store "e-happy-1")]
        (is (some? stored) "Entry should exist in evidence store")
        (is (= "codex-laptop" (:evidence/author stored))
            "Original author should be preserved (not overridden)")))))

;; =============================================================================
;; WS handler — tag mutation (the :replicated tag)
;; =============================================================================

(deftest evidence-replication-adds-replicated-tag
  (testing "server adds :replicated to entry tags"
    (let [{:keys [evidence-store] :as ws} (make-test-ws)
          ch (connect! ws "claude-1")]
      ((:on-receive ws) ch (evidence-frame (make-sample-entry :id "e-tag-1" :tags [:test])))
      (let [stored (estore/get-entry* evidence-store "e-tag-1")]
        (is (some #{:replicated} (:evidence/tags stored))
            "Stored entry should have :replicated tag")
        (is (some #{:test} (:evidence/tags stored))
            "Original tags should be preserved")))))

(deftest evidence-replication-adds-replicated-tag-to-empty-tags
  (testing "server adds :replicated even when entry has no tags"
    (let [{:keys [evidence-store] :as ws} (make-test-ws)
          ch (connect! ws "claude-1")]
      ((:on-receive ws) ch (evidence-frame (make-sample-entry :id "e-tag-2" :tags [])))
      (let [stored (estore/get-entry* evidence-store "e-tag-2")]
        (is (some #{:replicated} (:evidence/tags stored)))))))

;; =============================================================================
;; WS handler — provenance tracking
;; =============================================================================

(deftest evidence-replication-records-provenance
  (testing "server records who replicated the entry and when"
    (let [{:keys [evidence-store] :as ws} (make-test-ws)
          ch (connect! ws "claude-1")]
      ((:on-receive ws) ch (evidence-frame (make-sample-entry :id "e-prov-1")))
      (let [stored (estore/get-entry* evidence-store "e-prov-1")]
        (is (= "claude-1" (:evidence/replicated-by stored))
            "Should record which WS agent replicated this entry")
        (is (string? (:evidence/replicated-at stored))
            "Should record when replication happened")))))

;; =============================================================================
;; WS handler — author preservation
;; =============================================================================

(deftest evidence-replication-preserves-original-author
  (testing "unlike messages, evidence replication preserves original author"
    (let [{:keys [evidence-store] :as ws} (make-test-ws)
          ;; claude-1 is the WS connection, but the evidence was authored by codex-laptop
          ch (connect! ws "claude-1")]
      ((:on-receive ws) ch (evidence-frame (make-sample-entry :id "e-author-1"
                                                               :author "codex-laptop")))
      (let [stored (estore/get-entry* evidence-store "e-author-1")]
        (is (= "codex-laptop" (:evidence/author stored))
            "Original author codex-laptop should not be overridden to claude-1")))))

;; =============================================================================
;; WS handler — loop protection
;; =============================================================================

(deftest evidence-replication-rejects-already-replicated
  (testing "entry already tagged :replicated → refused (loop protection)"
    (let [{:keys [sent] :as ws} (make-test-ws)
          ch (connect! ws "claude-1")]
      ;; Tags come through JSON as strings
      ((:on-receive ws) ch (evidence-frame
                            (make-sample-entry :id "e-loop-1"
                                               :tags ["test" "replicated"])))
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))
        (is (= "replication-loop" (:code frame)))))))

(deftest evidence-replication-rejects-string-replicated-tag
  (testing "loop protection also catches string 'replicated' tag (JSON round-trip)"
    (let [{:keys [sent] :as ws} (make-test-ws)
          ch (connect! ws "claude-1")]
      ;; JSON round-trip turns keywords into strings
      ((:on-receive ws) ch (evidence-frame
                            (make-sample-entry :id "e-loop-2"
                                               :tags ["test" "replicated"])))
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))
        (is (= "replication-loop" (:code frame)))))))

;; =============================================================================
;; WS handler — duplicate detection (idempotency)
;; =============================================================================

(deftest evidence-replication-duplicate-id-returns-error
  (testing "duplicate evidence ID → store's duplicate-id error forwarded"
    (let [{:keys [evidence-store sent] :as ws} (make-test-ws)
          ch (connect! ws "claude-1")]
      ;; First: succeeds
      ((:on-receive ws) ch (evidence-frame (make-sample-entry :id "e-dup-1")))
      (is (= "evidence_ack" (:type (last-sent sent))))
      (reset! sent [])
      ;; Second: same ID → duplicate
      ((:on-receive ws) ch (evidence-frame (make-sample-entry :id "e-dup-1")))
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))
        (is (= "duplicate-id" (:code frame)))))))

;; =============================================================================
;; WS handler — multiple entries in sequence
;; =============================================================================

(deftest evidence-replication-multiple-entries
  (testing "multiple distinct entries can be replicated in sequence"
    (let [{:keys [evidence-store sent] :as ws} (make-test-ws)
          ch (connect! ws "claude-1")]
      (doseq [i (range 5)]
        ((:on-receive ws) ch (evidence-frame
                              (make-sample-entry :id (str "e-seq-" i)))))
      ;; All should produce acks
      (is (= 5 (count @sent)))
      (is (every? #(= "evidence_ack" (:type %))
                  (map #(json/parse-string (:data %) true) @sent)))
      ;; All should be in the store
      (doseq [i (range 5)]
        (is (some? (estore/get-entry* evidence-store (str "e-seq-" i))))))))

;; =============================================================================
;; WS handler — entry with reply chain reference
;; =============================================================================

(deftest evidence-replication-with-reply-chain
  (testing "entry with in-reply-to works when parent exists"
    (let [{:keys [evidence-store sent] :as ws} (make-test-ws)
          ch (connect! ws "claude-1")]
      ;; First: parent entry
      ((:on-receive ws) ch (evidence-frame (make-sample-entry :id "e-parent-1")))
      (reset! sent [])
      ;; Second: child referencing parent (key becomes :evidence/in-reply-to via Cheshire)
      ((:on-receive ws) ch (evidence-frame
                            (assoc (make-sample-entry :id "e-child-1")
                                   "evidence/in-reply-to" "e-parent-1")))
      (let [frame (last-sent sent)]
        (is (= "evidence_ack" (:type frame))
            "Reply to existing parent should succeed"))))

  (testing "entry with in-reply-to to missing parent → store error"
    (let [{:keys [sent] :as ws} (make-test-ws)
          ch (connect! ws "claude-1")]
      ((:on-receive ws) ch (evidence-frame
                            (assoc (make-sample-entry :id "e-orphan-1")
                                   "evidence/in-reply-to" "e-nonexistent")))
      (let [frame (last-sent sent)]
        (is (= "error" (:type frame)))
        (is (= "reply-not-found" (:code frame)))))))

;; =============================================================================
;; Protocol round-trip: frame → parse → handle → ack → parse
;; =============================================================================

(deftest evidence-replication-round-trip
  (testing "evidence frame survives full round-trip: serialize → parse → coerce → store → ack"
    (let [original-entry (make-sample-entry :id "e-rt-1" :author "codex-laptop")
          ;; Step 1: serialize the frame
          frame-json (evidence-frame original-entry)
          ;; Step 2: parse it
          parsed (proto/parse-ws-message frame-json)]
      (is (= :evidence (:ws/type parsed)))
      ;; Step 3: verify entry data preserved (keywordized by Cheshire)
      (let [entry (:evidence/entry parsed)]
        (is (= "e-rt-1" (:evidence/id entry)))
        (is (= "codex-laptop" (:evidence/author entry)))
        ;; Step 4: coerce for store
        (let [coerced (proto/coerce-replication-entry entry)]
          (is (= :reflection (:evidence/type coerced))
              "String 'reflection' should be coerced to keyword")
          (is (= :observation (:evidence/claim-type coerced)))
          (is (= :mission (get-in coerced [:evidence/subject :ref/type]))))
        ;; Step 5: render ack
        (let [ack-json (proto/render-evidence-ack "e-rt-1")
              ack (json/parse-string ack-json true)]
          (is (= "evidence_ack" (:type ack)))
          (is (= "e-rt-1" (:evidence_id ack))))))))
