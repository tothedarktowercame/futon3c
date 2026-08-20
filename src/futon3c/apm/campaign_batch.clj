(ns futon3c.apm.campaign-batch
  "Content-addressed authorization envelopes for bounded campaign batches."
  (:require [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine])
  (:import [java.time Instant]))

(defn issue
  "Create a content-addressed permit. Issuance records authority; it does not
  itself establish that ISSUER is trusted by a runner."
  [{:keys [campaign-id manifest-hash start-version start-ledger-digest
           issuer actor max-actions allowed-kinds issued-at valid-before]}]
  (let [body {:permit/type :campaign-batch :permit/version 1
              :campaign/id campaign-id :campaign/manifest-hash manifest-hash
              :campaign/start-version start-version
              :campaign/start-ledger-digest start-ledger-digest
              :permit/issuer issuer :permit/actor actor
              :permit/max-actions max-actions
              :permit/allowed-kinds (set allowed-kinds)
              :permit/issued-at issued-at :permit/valid-before valid-before}]
    (assoc body :permit/id (machine/ledger-digest [body]))))

(defn- nonblank? [x]
  (and (string? x) (not (str/blank? x))))

(defn- parse-time [x]
  (when (string? x)
    (try (Instant/parse x) (catch Throwable _ nil))))

(defn authorize
  "Authorize OBLIGATION at zero-based ACTION-INDEX against CERTIFICATE."
  [{:keys [permit trusted-permit-id trusted-issuer actor
           certificate obligation action-index]}]
  (let [issued-at (parse-time (:permit/issued-at permit))
        valid-before (parse-time (:permit/valid-before permit))
        observed-at (parse-time (:generated-at certificate))
        kind (get-in obligation [:obligation/action :kind])]
    (cond
      (not (map? permit))
      {:ok false :error/code :campaign-batch-permit-required}

      (not= (:permit/id permit)
            (machine/ledger-digest [(dissoc permit :permit/id)]))
      {:ok false :error/code :campaign-batch-permit-content-invalid}

      (not= :campaign-batch (:permit/type permit))
      {:ok false :error/code :campaign-batch-permit-type-invalid}

      (or (not (nonblank? trusted-permit-id))
          (not= trusted-permit-id (:permit/id permit)))
      {:ok false :error/code :campaign-batch-permit-id-untrusted}

      (not (and (= 1 (:permit/version permit))
                (nonblank? (:campaign/id permit))
                (nonblank? (:campaign/manifest-hash permit))
                (nat-int? (:campaign/start-version permit))
                (nonblank? (:campaign/start-ledger-digest permit))
                (nonblank? (:permit/issuer permit))
                (nonblank? (:permit/actor permit))
                (pos-int? (:permit/max-actions permit))
                (set? (:permit/allowed-kinds permit))
                (every? keyword? (:permit/allowed-kinds permit))))
      {:ok false :error/code :campaign-batch-permit-shape-invalid}

      (or (not (nonblank? trusted-issuer))
          (not= trusted-issuer (:permit/issuer permit)))
      {:ok false :error/code :campaign-batch-permit-issuer-untrusted}

      (not= actor (:permit/actor permit))
      {:ok false :error/code :campaign-batch-permit-actor-mismatch}

      (not= (:campaign/id certificate) (:campaign/id permit))
      {:ok false :error/code :campaign-batch-permit-campaign-mismatch}

      (not= (:campaign/manifest-hash certificate)
            (:campaign/manifest-hash permit))
      {:ok false :error/code :campaign-batch-permit-manifest-mismatch}

      (or (not (integer? action-index)) (neg? action-index)
          (>= action-index (:permit/max-actions permit)))
      {:ok false :error/code :campaign-batch-permit-quota-exhausted}

      (not= action-index
            (get (:campaign/permit-usage certificate) (:permit/id permit) 0))
      {:ok false :error/code :campaign-batch-permit-usage-mismatch
       :expected (get (:campaign/permit-usage certificate) (:permit/id permit) 0)
       :actual action-index}

      (not (contains? (:permit/allowed-kinds permit) kind))
      {:ok false :error/code :campaign-batch-permit-action-forbidden :kind kind}

      (or (nil? issued-at) (nil? valid-before) (nil? observed-at)
          (.isAfter issued-at valid-before))
      {:ok false :error/code :campaign-batch-permit-time-invalid}

      (not (.isBefore observed-at valid-before))
      {:ok false :error/code :campaign-batch-permit-expired}

      (.isBefore observed-at issued-at)
      {:ok false :error/code :campaign-batch-permit-not-yet-valid}

      (< (:campaign/version certificate) (:campaign/start-version permit))
      {:ok false :error/code :campaign-batch-permit-version-before-start}

      (and (= (:campaign/version certificate) (:campaign/start-version permit))
           (not= (:ledger/digest certificate)
                 (:campaign/start-ledger-digest permit)))
      {:ok false :error/code :campaign-batch-permit-start-ledger-mismatch}

      :else
      {:ok true :authorized? true :permit/id (:permit/id permit)
       :action-index action-index :kind kind})))
