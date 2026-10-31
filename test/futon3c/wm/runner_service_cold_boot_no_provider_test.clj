(ns futon3c.wm.runner-service-cold-boot-no-provider-test
  "Cold-boot (no-provider) refinement of failure packet 8505, on top of the
  F4a packet branch (test/f4a-real-ledger-refusal).

  The provider-present F4a arm supplies a real medic-authority provider, so
  it can never see the live defect: on a cold JVM (or any state where no
  provider is installed) acquire-medic-authority resolves NO provider
  context, and — pre-fix — installed a deny-all verifier into the admission
  opts before meta-self-heal-preflight! ran. This namespace drives the full
  production service entry futon3c.wm.runner-service/click! with the
  provider EXPLICITLY ABSENT, the default standing authority untouched, and
  the exact approved historical 129-entry ledger (sha-pinned hermetic temp
  copy; skip where the pinned bytes are absent):

  - positive: untampered bytes must REACH the admission boundary under the
    standing scope — ration charged exactly once, acceptance persisted,
    :status :queued returned, and the worker entered with admission opts
    carrying the no-provider identity. (Failed pre-fix: refused
    :meta-self-heal/ledger-unreadable before the ration.)
  - negative: tampered ledger bytes (:state :open -> :state :doctored, the
    same doctoring as the F3 witness) must refuse with the typed 503
    pre-ration error, ZERO ration charge, and untouched ledger bytes.

  Stubs are downstream ports only: *source-readiness* (read-only source
  verifier) is bound to :current, the acceptance dir is bound to the temp
  root, and the worker entry run-click! is replaced by a recording stub —
  nothing upstream of admission (admit-ordinary-click-trigger!,
  acquire-medic-authority, meta-self-heal-preflight!) is stubbed."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.wm.runner-service :as service])
  (:import [java.nio.file Files]
           [java.nio.file CopyOption]
           [java.nio.file.attribute FileAttribute]))

(def live-ledger "/home/joe/code/futon2/data/wm-meta-injuries-live-2026-10-22/ledger.edn")
(def live-ledger-sha256 "f7b5a7aac642b35302f30e1254c707805e95188ffd3c8158c51ae6bde56a0074")

(defn- sha256 [path]
  (let [d (.digest (java.security.MessageDigest/getInstance "SHA-256")
                   (Files/readAllBytes (.toPath (io/file path))))]
    (apply str (map #(format "%02x" %) d))))

(defn- temp-root []
  (str (Files/createTempDirectory "f4a-cold-boot-"
                                  (make-array FileAttribute 0))))

(defn- hermetic-copy
  "Temp copy of the pinned live 129-entry ledger; nil (witness skips) when
  the pinned bytes are absent in this environment; throws on drift."
  [root]
  (when (.isFile (io/file live-ledger))
    (let [copy (str root "/ledger.edn")]
      (Files/copy (.toPath (io/file live-ledger))
                  (.toPath (io/file copy))
                  (into-array CopyOption []))
      (when-not (= live-ledger-sha256 (sha256 copy))
        (throw (ex-info "pinned live ledger bytes drifted; witness must not run"
                        {:expected live-ledger-sha256 :actual (sha256 copy)})))
      copy)))

(defn- status-atom []
  (deref (resolve 'futon3c.wm.runner-service/!status)))

(use-fixtures :each
  (fn [test-fn]
    (service/install-medic-authority-provider! nil)
    (try
      (test-fn)
      (finally
        (service/install-medic-authority-provider! nil)
        (reset! (status-atom) service/initial-status)
        (reset! (deref (resolve 'futon3c.wm.runner-service/!completion)) nil)))))

(defn- drive-click!
  "Call the production service entry with NO provider installed. The worker
  entry (run-click!) is replaced by a recording stub — the only behavioural
  stub — so the test observes exactly what admission handed the worker. The
  ration issue callback is counted, never stubbed away."
  [{:keys [ledger-path transaction-path]}]
  (let [root (temp-root)
        acceptance-dir (str root "/acceptance")
        issued (atom 0)
        worker-entries (atom [])
        run-click-var (resolve 'futon3c.wm.runner-service/run-click!)]
    (.mkdirs (io/file acceptance-dir))
    (binding [service/*click-acceptance-dir* acceptance-dir
              service/*source-readiness* (constantly {:status :current})]
      (with-redefs-fn {run-click-var
                       (fn [click-id opts completion _admission]
                         (swap! worker-entries conj
                                {:click-id click-id
                                 :identity (get opts :meta-medic-authority-identity)
                                 :verifier-present? (some? (get opts :meta-medic-authority-verifier))})
                         (deliver completion {:status :ok :worker :stubbed})
                         (reset! (status-atom) service/initial-status))}
        (fn []
          {:result (try
                     (service/click! {:ordinary-click/issue!
                                      (fn [& _] (swap! issued inc))
                                      ;; The HTTP handler always supplies a
                                      ;; sanitized acceptance payload
                                      ;; (select-keys of the request JSON);
                                      ;; the raw runner opts it merges under
                                      ;; carry non-authority function ports
                                      ;; (retry plumbing) and are rightly
                                      ;; refused by the serializability guard.
                                      ;; This mirrors the production entry
                                      ;; shape, it does not bypass any guard.
                                      :acceptance-payload
                                      {:trigger :duree-click-on-demand
                                       :task "f4a-cold-boot-no-provider-witness"}
                                      :meta-injury-ledger-path ledger-path
                                      :meta-repair-transaction-path transaction-path})
                     :accepted true
                     (catch clojure.lang.ExceptionInfo e
                       (assoc (ex-data e) :accepted false)))
           :issued @issued
           :worker-entries @worker-entries
           :acceptance-dir acceptance-dir})))))

(deftest cold-boot-no-provider-reaches-admission-boundary-under-standing-scope
  (let [root (temp-root)]
    (if-let [copy (hermetic-copy root)]
      (let [before (sha256 copy)
            {:keys [result issued worker-entries acceptance-dir]}
            (drive-click! {:ledger-path copy
                           :transaction-path (str root "/transaction.edn")})]
        (testing "untampered pinned bytes are accepted under the standing resolver"
          (is (true? (:accepted result))
              (str "click! refused valid no-provider cold-boot bytes: "
                   (pr-str (select-keys result [:status :error :before-ration-spend?]))))
          (is (= :queued (:status result)))
          (is (string? (:click-id result))))
        (testing "the ration was charged exactly once — admission boundary reached"
          (is (= 1 issued)))
        (testing "durable acceptance persisted inside the hermetic root"
          (is (= 1 (count (.list (io/file acceptance-dir))))))
        (testing "the worker was entered with the no-provider identity"
          (is (= 1 (count worker-entries)))
          (let [{:keys [identity verifier-present?]} (first worker-entries)]
            (is (= 0 (:generation identity)))
            (is (= :absent (:status (:authority identity)))
                "cold boot carries the absent-provider identity, not a forged one")
            (is (false? verifier-present?)
                "admission must NOT hand the worker a deny-all verifier; the standing resolver composes downstream")))
        (testing "the hermetic ledger bytes are unchanged by the accepted run"
          (is (= before (sha256 copy)))))
      (is true "pinned live ledger absent in this environment; witness skipped"))))

(deftest cold-boot-no-provider-tampered-ledger-refuses-with-zero-charge
  (let [root (temp-root)]
    (if-let [copy (hermetic-copy root)]
      (do
        ;; Same doctoring as the F3 witness: :state :open -> :state :doctored.
        (spit copy (.replace (slurp copy) ":state :open" ":state :doctored"))
        (let [before (sha256 copy)
              {:keys [result issued worker-entries]}
              (drive-click! {:ledger-path copy
                             :transaction-path (str root "/transaction.edn")})]
          (testing "typed pre-ration refusal derived from the actual bytes"
            (is (false? (:accepted result)))
            (is (= 503 (:status result)))
            (is (= :meta-self-heal/ledger-unreadable (:error result)))
            (is (true? (:before-ration-spend? result))))
          (testing "zero ration charge — no spend on refused bytes"
            (is (zero? issued)))
          (testing "the worker was never entered"
            (is (empty? worker-entries)))
          (testing "no ledger write by the refused attempt"
            (is (= before (sha256 copy))))))
      (is true "pinned live ledger absent in this environment; witness skipped"))))
