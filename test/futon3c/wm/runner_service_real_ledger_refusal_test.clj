(ns futon3c.wm.runner-service-real-ledger-refusal-test
  "F4a entry test (external review bf0d10e2f / reconciliation
  external-review-reconciliation-2026-10-31.json).

  Unlike ledger-unreadable-still-refuses-before-ordinary-ration (which INJECTS
  the refusal through *resolve-var*), this test produces the pre-ration
  refusal from genuinely invalid ledger BYTES: a synthetic medic-bearing
  ledger is built with the real futon2 append/verify machinery, written to a
  hermetic temp directory, then one pinned sha inside the medic authorization
  entry is doctored on disk. The refusal must be derived from those bytes by
  the real production path — the service entry futon3c.wm.runner-service/click!
  (the in-process boundary the HTTP handler calls; this test exercises the
  service entry, NOT the HTTP transport) with the production *resolve-var*
  (no partial resolver is installed) and a real medic-authority provider
  context, so acquire-medic-authority resolves a genuine verifier and the
  durable load fails on the actual doctored bytes, not on a deny-all default.

  Assertions:
  - click! throws the typed 503 refusal
    {:error :meta-self-heal/ledger-unreadable :before-ration-spend? true}
  - the ordinary-click ration callback is NEVER invoked (charge cannot move
    before validation — the negative control for reordering)
  - the untampered bytes of the SAME ledger pass preflight with the SAME
    resolver (the refusal is byte-derived, not resolver-bypassed)
  - the temp ledger bytes are unchanged by the refused attempt (no write)."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon2.aif.meta-injury-ledger :as ledger]
            [futon2.aif.meta-medic-authority :as authority]
            [futon2.aif.meta-medic-authority-test :as authority-fixture]
            [futon2.aif.meta-self-heal-production :as self-heal]
            [futon3c.wm.runner-service :as service])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(use-fixtures :each
  (fn [test-fn]
    (service/install-medic-authority-provider! nil)
    (try (test-fn)
         (finally (service/install-medic-authority-provider! nil)))))

(defn- sha256 [path]
  (let [d (.digest (java.security.MessageDigest/getInstance "SHA-256")
                   (Files/readAllBytes (.toPath (io/file path))))]
    (apply str (map #(format "%02x" %) d))))

(defn- synthetic-medic-ledger
  "A valid medic-bearing ledger built with the real append/verify machinery:
  the authority-fixture bundle, a medic-repaired append under its verifier,
  plus one unrelated open injury so the medic clause is load-bearing."
  []
  (let [bundle (authority-fixture/fixture)
        context (select-keys bundle [:intent :artifacts :evaluation-pin
                                     :review-pin :authorization-pin :trust])
        verifier (authority/authority-verifier context)
        medic (ledger/append-medic-repaired
               (:ledger bundle)
               {:path "synthetic/medic.edn" :sha256 (apply str (repeat 64 "8"))}
               (:operation bundle) verifier)
        unrelated (ledger/append-transition
                   (:ledger medic)
                   {:injury-id "injury/unrelated" :state :open :attempt 1
                    :cause-record-pin {:path "synthetic/cause.edn"
                                       :sha256 (apply str (repeat 64 "9"))}
                    :failure-stage :construction :failure-kind :synthetic
                    :detected-click "click/synthetic"}
                   verifier)]
    {:context context
     :ledger-str (str (pr-str (:ledger unrelated)) "\n")}))

(defn- temp-paths []
  (let [root (str (Files/createTempDirectory "f4a-real-ledger-"
                                             (make-array FileAttribute 0)))]
    {:root root
     :ledger-path (str root "/ledger.edn")
     :transaction-path (str root "/transaction.edn")}))

(deftest doctored-ledger-bytes-refuse-at-service-entry-before-ration
  (let [{:keys [context ledger-str]} (synthetic-medic-ledger)
        {:keys [ledger-path transaction-path]} (temp-paths)
        issued (atom 0)
        ;; The genuine doctored bytes: one pinned authorization sha inside the
        ;; medic entry is flipped, so the real verifier fails on these bytes.
        doctored (.replace ledger-str
                           (apply str (repeat 64 "8"))
                           (str (apply str (repeat 32 "8"))
                                (apply str (repeat 32 "9"))))]
    ;; sanity: the doctoring must actually change the bytes
    (is (not= ledger-str doctored))
    (spit ledger-path doctored)
    (service/install-medic-authority-provider! (constantly context))
    (let [before (sha256 ledger-path)
          failure (try
                    (service/click! {:ordinary-click/issue!
                                     (fn [& _] (swap! issued inc))
                                     :meta-injury-ledger-path ledger-path
                                     :meta-repair-transaction-path transaction-path})
                    nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (testing "typed pre-ration refusal derived from the actual bytes"
        (is (map? failure) "click! must refuse, not accept")
        (is (= 503 (:status failure)))
        (is (= :meta-self-heal/ledger-unreadable (:error failure)))
        (is (true? (:before-ration-spend? failure)))
        (is (= :refused
               (get-in failure [:readiness :ledger-result :status])))
        (is (= :store/tampered-ledger
               (get-in failure [:readiness :ledger-result :reason]))
            "the refusal names tampered bytes, not a deny-all default"))
      (testing "the ration callback never ran"
        (is (zero? @issued)
            "charge moved before validation would break this assertion"))
      (testing "no ledger write by the refused attempt"
        (is (= before (sha256 ledger-path)))))))

(deftest untampered-bytes-of-the-same-ledger-pass-preflight
  ;; Negative control for resolver bypass: if the refusal above came from the
  ;; resolver construction rather than the doctored bytes, the UNTAMPERED
  ;; bytes under the SAME provider/verifier would also refuse. They must not.
  ;; Entry-boundary note: this control exercises
  ;; service/meta-self-heal-preflight! — the exact boundary inside click!
  ;; after acquire-medic-authority — because driving the full click! entry
  ;; would start a real worker (not hermetic); the service-entry refusal
  ;; itself is the test above.
  (let [{:keys [context ledger-str]} (synthetic-medic-ledger)
        {:keys [ledger-path transaction-path]} (temp-paths)]
    (spit ledger-path ledger-str)
    (service/install-medic-authority-provider! (constantly context))
    (let [admitted (#'service/acquire-medic-authority
                    {:meta-injury-ledger-path ledger-path
                     :meta-repair-transaction-path transaction-path})
          durable (get-in (self-heal/diagnosis-context admitted)
                          [:durable-ledger])
          ;; preflight returns the (non-refused) configured opts when it does
          ;; not throw; a byte-independent refusal would have thrown here.
          passed (try {:threw false
                       :preflight (service/meta-self-heal-preflight! admitted)}
                      (catch clojure.lang.ExceptionInfo e
                        {:threw true :error (:error (ex-data e))}))]
      (is (= :loaded (:status durable))
          "untampered bytes load under the same real resolver")
      (is (false? (:threw passed))
          (str "preflight refused untampered bytes: " (pr-str passed))))))
