(ns futon3c.bridge
  "Bridge between futon3c social pipeline and futon3b gate pipeline.

   Converts dispatch receipts and evidence into gate pipeline inputs,
   runs them through G5→G0, and records outcomes in a futon3a sidecar store.

   This is the integration point where real-time coordination (futon3c)
   meets quality gates (futon3b) meets audit trail (futon3a).

   Usage:
     (require '[futon3c.bridge :as bridge])

     ;; After a dispatch produces a receipt:
     (bridge/submit-to-gates! receipt
       {:mission-ref \"M-social-exotype\"
        :evidence-sink my-sink-fn})

     ;; Full triangle: dispatch → gates → sidecar
     (bridge/record-triangle! receipt evidence-entries sidecar-store
       {:mission-ref \"M-social-exotype\"})"
  (:require [futon3.gate.pipeline :as pipeline]
            [futon3.gate.util :as u]
            [sidecar.store :as sidecar])
  (:import [java.time Instant]))

;; =============================================================================
;; Receipt → Gate Pipeline Input
;; =============================================================================

(defn receipt->artifact
  "Convert a DispatchReceipt into a gate artifact map.
   The receipt itself is evidence that coordination happened."
  [receipt]
  {:artifact/type (if (:receipt/peripheral-id receipt)
                    :futon3c/peripheral-dispatch
                    :futon3c/coordination-dispatch)
   :artifact/ref {:receipt/msg-id (:receipt/msg-id receipt)
                  :receipt/route (:receipt/route receipt)
                  :receipt/session-id (:receipt/session-id receipt)
                  :receipt/peripheral-id (:receipt/peripheral-id receipt)
                  :receipt/delivered? (:receipt/delivered? receipt)}
   :exec/success? (true? (:receipt/delivered? receipt))})

(defn receipt->par
  "Convert a DispatchReceipt into a PAR (Post-Action Review) for gate G0."
  [receipt]
  {:par/session-ref (or (:receipt/session-id receipt)
                        (str "dispatch-" (:receipt/msg-id receipt)))
   :par/what-worked (str "Dispatch via " (:receipt/route receipt)
                         (when-let [pid (:receipt/peripheral-id receipt)]
                           (str " → " (name pid) " peripheral")))
   :par/what-didnt (when-not (:receipt/delivered? receipt)
                     "Delivery failed")
   :par/prediction-errors []
   :par/suggestions []})

(defn receipt->gate-input
  "Build a complete gate pipeline input from a dispatch receipt.

   opts:
     :mission-ref   — string (default: \"M-social-exotype\")
     :missions      — missions map (inline, avoids filesystem)
     :patterns      — pattern config (inline)
     :registry      — agent registry
     :evidence-sink — fn for G0 durability (default: writes to atom)
     :intent        — string describing the work"
  [receipt opts]
  (let [mission-ref (or (:mission-ref opts) "M-social-exotype")
        missions (or (:missions opts)
                     {mission-ref {:mission/id mission-ref
                                   :mission/state :active}})
        patterns (or (:patterns opts)
                     {:patterns/ids #{:rendezvous-handshake
                                      :delivery-receipt
                                      :single-routing-authority}})
        registry (or (:registry opts)
                     {:agents {"claude" {:capabilities [:explore :edit :coordination/execute]}
                               "codex"  {:capabilities [:edit :coordination/execute]}}})
        agent-id (or (:agent-id opts) "claude")
        intent (or (:intent opts)
                    (str "Dispatch " (:receipt/msg-id receipt)
                         " via " (:receipt/route receipt)))
        artifact (receipt->artifact receipt)
        par (receipt->par receipt)
        sink-atom (atom nil)
        sink (or (:evidence-sink opts)
                 (fn [proof-data]
                   (reset! sink-atom proof-data)
                   {:ok true :path/id (get-in proof-data [:proof-path :path/id])}))]
    {:input
     {:I-request {:task {:task/id (u/gen-id "futon3c")
                         :task/mission-ref mission-ref
                         :task/intent intent
                         :task/scope {:in ["futon3c/social/dispatch"]
                                      :out ["futon3c/bridge"]}
                         :task/success-criteria [:dispatch-delivered]}
                  :agent-id agent-id
                  :psr {:psr/type :gap
                        :gap? true
                        :psr/rationale (str "Cross-repo integration — " intent)}
                  :artifact artifact
                  :par par
                  :evidence/sink sink}
      :I-missions missions
      :I-patterns patterns
      :I-registry registry
      :I-environment {}
      :opts {:budget/ms 5000}}
     :sink-atom sink-atom}))

(defn submit-to-gates!
  "Submit a dispatch receipt through the futon3b gate pipeline (G5→G0).

   Returns the pipeline result: {:ok true :O-proof-path ... :O-evidence ...}
   or {:ok false :gate/id ... :error/key ... :message ...}."
  [receipt & [opts]]
  (let [{:keys [input sink-atom]} (receipt->gate-input receipt (or opts {}))
        result (pipeline/run input)]
    (assoc result :persisted-proof-path @sink-atom)))

;; =============================================================================
;; Receipt → Sidecar Proposal (futon3a)
;; =============================================================================

(defn receipt->proposal
  "Convert a dispatch receipt into a sidecar proposal for futon3a."
  [receipt]
  (let [proposal-id (str "prop-dispatch-" (:receipt/msg-id receipt))]
    {:proposal/id proposal-id
     :proposal/kind :coordination
     :proposal/status :accepted
     :proposal/score (if (:receipt/delivered? receipt) 0.9 0.3)
     :proposal/method :dispatch
     :proposal/evidence {:route (:receipt/route receipt)
                         :session-id (:receipt/session-id receipt)
                         :peripheral-id (:receipt/peripheral-id receipt)
                         :delivered? (:receipt/delivered? receipt)}}))

(defn proof-path->evidence
  "Convert a futon3b proof-path into sidecar evidence for futon3a."
  [proof-path proposal-id]
  {:evidence/id (str "ev-" (u/gen-id "proof"))
   :evidence/target {:type :proposal :id proposal-id}
   :evidence/method :gate-pipeline
   :evidence/payload {:path/id (:path/id proof-path)
                      :gate-count (count (:events proof-path))
                      :gates (mapv :gate/id (:events proof-path))}})

;; =============================================================================
;; Full Triangle: futon3c → futon3b → futon3a
;; =============================================================================

(defn record-triangle!
  "Execute the full futon3 triangle:
   1. Submit dispatch receipt through futon3b gate pipeline
   2. Record proposal in futon3a sidecar store
   3. Record proof-path as sidecar evidence
   4. Return combined result

   Returns:
     {:gate-result   — pipeline result from futon3b
      :proposal      — sidecar proposal result from futon3a
      :evidence      — sidecar evidence result from futon3a
      :ok            — true if all three steps succeeded}"
  [receipt sidecar-store & [opts]]
  (let [;; Step 1: futon3b gates
        gate-result (submit-to-gates! receipt opts)

        ;; Step 2: futon3a proposal
        proposal (receipt->proposal receipt)
        proposal-result (sidecar/record-proposal! sidecar-store proposal)

        ;; Step 3: futon3a evidence (if gates produced a proof-path)
        evidence-result
        (when (and (:ok gate-result) (:ok proposal-result))
          (let [proof-path (or (:O-proof-path gate-result)
                               (get-in gate-result [:persisted-proof-path :proof-path]))
                evidence (proof-path->evidence proof-path (:proposal/id proposal))]
            (sidecar/record-evidence! sidecar-store evidence)))]

    {:gate-result gate-result
     :proposal proposal-result
     :evidence evidence-result
     :ok (and (:ok gate-result)
              (:ok proposal-result)
              (or (nil? evidence-result) (:ok evidence-result)))}))
