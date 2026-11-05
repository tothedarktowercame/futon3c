(ns futon3c.file-change.hygiene-delivery
  "Runtime evidence adapter for the Lean FileChangeHygieneDelivery contract.

  This namespace computes admission; callers cannot supply an :admitted flag.
  The first adapter covers the synchronous test-registry currency seam.  A
  typed registry refusal remains a useful response, but is not an admitted
  successful hygiene handoff.")

(def schema :file-change/hygiene-delivery-v1)
(def lean-contract
  "DarkTower.WarMachine.FileChangeHygieneDelivery.admitted_iff_checked")

(def check-keys
  [:identity-checked
   :authority-reconstructable
   :consumer-owned
   :drain-within-bound
   :settlement-boundary-checked
   :retention-or-rebuild-checked
   :red-routes-to-work
   :hold-has-deadline
   :ended-owner-transfers])

(defn admitted?
  "The executable image of Lean's conjunction. Missing and non-Boolean fields
  are false, so schema drift fails closed."
  [checks]
  (every? true? (map checks check-keys)))

(defn currency-evidence
  "Construct proof-carrying evidence for one completed check-currency! call.

  REQUEST is the selected endpoint payload and RESULT is the registry result.
  Successful admission requires a current result bound to the requested entry.
  The remaining checks name why this synchronous, authority-recomputed seam
  discharges obligations that are inapplicable to durable queues or seats."
  [request result]
  (let [entry-id (:entry-id request)
        current? (true? (:current? result))
        checks
        {:identity-checked (and current? (some? entry-id)
                                (= entry-id (:entry-id result)))
         :authority-reconstructable current?
         :consumer-owned true
         :drain-within-bound true
         :settlement-boundary-checked true
         :retention-or-rebuild-checked current?
         :red-routes-to-work true
         :hold-has-deadline true
         :ended-owner-transfers true}
        admitted (admitted? checks)]
    {:schema schema
     :contract :file-change/test-registry-currency-v1
     :lean-contract lean-contract
     :admitted? admitted
     :checks checks
     :basis
     {:identity-checked :request-entry-equals-result-entry
      :authority-reconstructable :registry-closure-rehash
      :consumer-owned :test-registry-currency-endpoint
      :drain-within-bound :synchronous-no-queue
      :settlement-boundary-checked :check-currency-returned
      :retention-or-rebuild-checked :registry-recomputes-from-authority
      :red-routes-to-work :not-applicable-no-durable-red
      :hold-has-deadline :not-applicable-no-hold
      :ended-owner-transfers :not-applicable-not-seat-owned}}))
