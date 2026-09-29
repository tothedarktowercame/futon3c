(ns futon3c.agency.blocker-escalation-record-cli
  "Mint and verify blocker escalation hyperedges."
  (:require [clojure.string :as str]
            [futon3c.agency.blocker-escalation-record :as blocker]
            [futon3c.agency.rule-record :as store])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def ^:private pending-id "act:pending-mint")

(defn- refuse! [reason field]
  (throw (ex-info "Invalid blocker escalation write" {:reason reason :field field})))

(defn payload [record idempotency-key]
  (when-not (and (string? idempotency-key) (not (str/blank? idempotency-key)))
    (refuse! :invalid-idempotency-key :idempotency-key))
  (when (contains? record :id) (refuse! :caller-assigned-storage-field :id))
  (-> (blocker/->hyperedge (assoc record :id pending-id))
      (dissoc :hx/id)
      (assoc :hx/mint-id true :hx/idempotency-key idempotency-key)))

(defn- list-path [job at]
  (str "/api/alpha/hyperedges?type=escalation%2Fblocker&end="
       (URLEncoder/encode (str "job:" job) "UTF-8")
       "&limit=1000&include-total=false&system-as-of="
       (URLEncoder/encode at "UTF-8") "&valid-as-of="
       (URLEncoder/encode at "UTF-8")))

(defn- readback [base record id]
  (let [now (str (Instant/now))]
    (some #(when (= id (:hx/id %)) (blocker/hyperedge->record %))
          (:hyperedges (store/request! base "GET"
                                       (list-path (:source-job record) now) nil)))))

(def ^:private identity-keys
  [:source-job :author :orchestrator :blocker :psr-id :pur-id :pattern-id])

(defn- existing [base record]
  (let [now (str (Instant/now))]
    (some (fn [edge]
            (let [stored (blocker/hyperedge->record edge)]
              (when (= (select-keys stored identity-keys)
                       (select-keys record identity-keys))
                stored)))
          (:hyperedges (store/request! base "GET"
                                       (list-path (:source-job record) now) nil)))))

(defn write! [base record idempotency-key]
  (let [p (payload record idempotency-key)
        receipt (try (store/request! base "POST" "/api/alpha/hyperedge" p)
                     (catch clojure.lang.ExceptionInfo e
                       (if (= 409 (:status (ex-data e)))
                         (if-let [stored (existing base record)]
                           {::existing stored}
                           (throw (ex-info "Blocker idempotency conflict"
                                           {:reason :idempotency-conflict})))
                         (throw e))))
        prior (::existing receipt)
        id (or (:id prior) (:hx/id receipt))]
    (if prior
      {:receipt {:ok true :hx/id id :existing? true :verified? true}
       :record prior}
      (do
        (when-not (and (:ok receipt) (string? id) (str/starts-with? id "act:"))
          (refuse! :missing-minted-receipt :receipt))
        (let [stored (readback base record id)]
          (when-not stored (refuse! :readback-missing :receipt))
          (when-not (= (dissoc p :hx/mint-id :hx/idempotency-key)
                       (dissoc (blocker/->hyperedge stored) :hx/id))
            (refuse! :readback-mismatch :receipt))
          {:receipt (assoc receipt :verified? true) :record stored})))))
