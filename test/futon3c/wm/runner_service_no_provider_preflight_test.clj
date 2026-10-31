(ns futon3c.wm.runner-service-no-provider-preflight-test
  "Defect pin for the live pre-ration refusal of run
  2026-10-31-bc036e43-6bd8-40db-aaa2-2002da1654cd (renewed-click-02-reconciled).

  acquire-medic-authority used to install a deny-all (constantly false)
  verifier into the ADMISSION opts whenever no medic-authority provider was
  installed. meta-self-heal-preflight! runs BEFORE the runtime (whose
  run-opportunity-with-authority! would have dissolved that verifier), and
  futon2's medic-authority-verifier prefers the opts verifier over the
  standing resolver — so the pre-ration ledger read ran under deny-all and
  refused VALID pinned ledger bytes with :meta-self-heal/ledger-unreadable.
  The live 'ready twice' probes used opts WITHOUT a verifier (standing
  default) and therefore could not see this: admission opts != probe opts.

  Tests here exercise the exact path acquire-medic-authority ->
  meta-self-heal-preflight! with NO provider installed:
  - pinned valid bytes must pass preflight (failed before the fix)
  - unknown-authority synthetic bytes must still refuse (removing the
    deny-all placeholder opened nothing; the standing resolver is the
    fail-closed default), and the refusal reason is retained."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon2.aif.meta-injury-ledger :as ledger]
            [futon2.aif.meta-medic-authority :as authority]
            [futon2.aif.meta-medic-authority-test :as authority-fixture]
            [futon3c.wm.runner-service :as service])
  (:import [java.nio.file Files]
           [java.nio.file CopyOption]
           [java.nio.file.attribute FileAttribute]))

(def live-ledger "/home/joe/code/futon2/data/wm-meta-injuries-live-2026-10-22/ledger.edn")
(def live-ledger-sha256 "f7b5a7aac642b35302f30e1254c707805e95188ffd3c8158c51ae6bde56a0074")

(use-fixtures :each
  (fn [test-fn]
    (service/install-medic-authority-provider! nil)
    (try (test-fn)
         (finally (service/install-medic-authority-provider! nil)))))

(defn- sha256 [path]
  (let [d (.digest (java.security.MessageDigest/getInstance "SHA-256")
                   (Files/readAllBytes (.toPath (io/file path))))]
    (apply str (map #(format "%02x" %) d))))

(defn- temp-paths []
  (let [root (str (Files/createTempDirectory "no-provider-preflight-"
                                             (make-array FileAttribute 0)))]
    {:root root
     :ledger-path (str root "/ledger.edn")
     :transaction-path (str root "/transaction.edn")}))

(defn- admitted-opts [{:keys [ledger-path transaction-path]}]
  ((var-get (resolve 'futon3c.wm.runner-service/acquire-medic-authority))
   {:meta-injury-ledger-path ledger-path
    :meta-repair-transaction-path transaction-path
    :meta-self-heal/readiness-route :ordinary}))

(deftest no-provider-admission-preflights-valid-pinned-bytes
  ;; The exact live-failure path. Skips (passes trivially) where the pinned
  ;; live bytes are absent; throws if the pinned bytes drifted.
  (if-not (.isFile (io/file live-ledger))
    (is true "pinned live ledger absent in this environment; witness skipped")
    (let [{:keys [ledger-path transaction-path] :as paths} (temp-paths)]
      (Files/copy (.toPath (io/file live-ledger))
                  (.toPath (io/file ledger-path))
                  (into-array CopyOption []))
      (when-not (= live-ledger-sha256 (sha256 ledger-path))
        (throw (ex-info "pinned live ledger bytes drifted; witness must not run"
                        {})))
      (let [admitted (admitted-opts paths)]
        (testing "admission installs no deny-all verifier with no provider"
          (is (or (nil? (:meta-medic-authority-verifier admitted))
                  (not (false? ((:meta-medic-authority-verifier admitted) {}))))
              "a deny-all verifier here poisons the pre-ration ledger read"))
        (testing "preflight on the ADMITTED opts must not refuse valid bytes"
          (let [refusal (try
                          (service/meta-self-heal-preflight! admitted)
                          nil ;; did not throw: pass
                          (catch clojure.lang.ExceptionInfo e (ex-data e)))]
            (is (nil? refusal)
                (str "preflight must pass on admission opts; refused: "
                     (pr-str (select-keys refusal [:status :error]))))))))))

(deftest no-provider-admission-still-refuses-unknown-authority-bytes
  ;; Fail-closed control: removing the deny-all placeholder must not admit
  ;; ledgers the standing resolver does not authorize.
  (let [{:keys [ledger-path transaction-path] :as paths} (temp-paths)
        bundle (authority-fixture/fixture)
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
    (spit ledger-path (str (pr-str (:ledger unrelated)) "\n"))
    (let [admitted (admitted-opts paths)
          failure (try
                    (service/meta-self-heal-preflight! admitted)
                    nil
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (map? failure) "unknown-authority bytes must still refuse")
      (is (= 503 (:status failure)))
      (is (= :meta-self-heal/ledger-unreadable (:error failure)))
      (is (true? (:before-ration-spend? failure)))
      (is (= :refused
             (get-in failure [:readiness :ledger-result :status]))))))
