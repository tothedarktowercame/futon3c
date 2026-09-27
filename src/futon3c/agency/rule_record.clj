(ns futon3c.agency.rule-record
  "P13a: validated rule descriptions stored as P6b minted hyperedges. No activation,
   adoption, withdrawal or overwrite is performed. Explicit valid-from describes
   this record's validity, NOT proof of historical runtime activation (P13b).
   CLI defaults to validation only; --write appends and verifies the returned id."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.agency.rule-timeline :as timeline])
  (:import [java.net URI URLEncoder]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Instant Duration LocalDate]))

(defn- refuse! [field message]
  (throw (ex-info message {:reason :invalid-rule-record :field field})))
(defn- text! [field value]
  (when-not (and (string? value) (not (str/blank? value)))
    (refuse! field "Rule field must be nonblank text")))
(defn- instant! [field value]
  (text! field value)
  (try (Instant/parse value)
       (catch Exception _ (refuse! field "Rule time must be an absolute ISO-8601 instant"))))

(def record-keys
  #{:rule/key :rule/name :rule/kind :rule/internal :rule/input-output
    :rule/accomplishment :rule/world-assumption :rule/however :rule/incident
    :rule/withdrawal-condition :rule/provenance :rule/timeline :rule/description-ref})

(defn validate!
  "Return RECORD or throw typed refusal. A known HOWEVER needs failure mode AND
   observable signal. An explicitly unknown failure mode needs a review date or instant.
   Temporary measures require an explicit incident ref, never a temporal guess."
  [record]
  (when-not (and (map? record) (every? record-keys (keys record)))
    (refuse! :record "Unknown rule fields or non-map record"))
  (doseq [k [:rule/key :rule/name :rule/world-assumption]] (text! k (get record k)))
  (when-not (contains? #{:temporary :standing} (:rule/kind record))
    (refuse! :rule/kind "Rule kind must be temporary or standing"))
  (doseq [[layer fields] [[:rule/internal [:description]]
                          [:rule/input-output [:description :refusal-example]]
                          [:rule/accomplishment [:description :observable]]]]
    (when-not (map? (get record layer)) (refuse! layer "Missing specification layer"))
    (doseq [field fields] (text! [layer field] (get-in record [layer field]))))
  (let [however (:rule/however record)]
    (text! [:rule/however :failure-mode] (:failure-mode however))
    (case (:status however)
      :known (text! [:rule/however :signal] (:signal however))
      :unknown (do
                 (text! [:rule/however :review-at] (:review-at however))
                 (try (LocalDate/parse (:review-at however))
                      (catch Exception _ (instant! [:rule/however :review-at] (:review-at however)))))
      (refuse! :rule/however "HOWEVER must be explicitly known or unknown")))
  (when (or (= :temporary (:rule/kind record)) (contains? record :rule/incident))
    (let [ref (:rule/incident record)]
      (when-not (and (map? ref) (keyword? (:ref/type ref))
                     (string? (:ref/id ref)) (not (str/blank? (:ref/id ref))))
        (refuse! :rule/incident "Temporary measure requires a typed owning incident reference"))))
  (when (= :temporary (:rule/kind record))
    (text! :rule/withdrawal-condition (:rule/withdrawal-condition record)))
  (let [p (:rule/provenance record)]
    (text! [:rule/provenance :author] (:author p))
    (when-not (contains? #{:contemporary-description :historical-reconstruction} (:basis p))
      (refuse! :rule/provenance "Record must distinguish reconstruction from contemporary description"))
    (when-not (and (vector? (:sources p)) (seq (:sources p)))
      (refuse! :rule/provenance "Rule provenance needs source references"))
    (doseq [source (:sources p)] (text! [:rule/provenance :sources] source)))
  (when-let [t (:rule/timeline record)] (timeline/validate! t))
  record)

(defn payload
  "Always opt into minting. No caller can supply an existing hyperedge id or op.
   The optional delivery key is passed to P6b's durable idempotency authority."
  [{:keys [record valid-from idempotency-key] :as request}]
  (when-not (and (map? request) (every? #{:record :valid-from :idempotency-key} (keys request)))
    (refuse! :request "Unknown write options (existing ids and operations are forbidden)"))
  (validate! record)
  (instant! :valid-from valid-from)
  (when (contains? request :idempotency-key) (text! :idempotency-key idempotency-key))
  (cond-> {:hx/type :rule/record :hx/mint-id true :hx/valid-time valid-from
           :hx/endpoints (cond-> [(str "rule:" (:rule/key record))]
                           (:rule/incident record) (conj (get-in record [:rule/incident :ref/id])))
           :hx/props (assoc record :rule/schema 1 :rule/valid-from valid-from)}
    idempotency-key (assoc :hx/idempotency-key idempotency-key)))

(defn request!
  "EDN transport preserves keyword-valued properties. Errors never look like a receipt."
  [base method path value]
  (let [builder (-> (HttpRequest/newBuilder (URI/create (str base path)))
                    (.timeout (Duration/ofSeconds 60))
                    (.header "Accept" "application/edn")
                    (.header "Content-Type" "application/edn")
                    (.header "X-Penholder" "api"))
        req (-> builder
                (.method method (if value (HttpRequest$BodyPublishers/ofString (pr-str value))
                                    (HttpRequest$BodyPublishers/noBody))) .build)
        response (.send (HttpClient/newHttpClient) req (HttpResponse$BodyHandlers/ofString))]
    (when-not (<= 200 (.statusCode response) 299)
      (throw (ex-info "Rule store request refused" {:status (.statusCode response) :body (.body response)})))
    (edn/read-string (.body response))))

(defn write!
  "Append one validated act, then read by returned id. Existing keyed acts are
   acknowledged by P6b, never updated; conflicting keys fail in the store."
  [base request]
  (let [p (payload request)
        receipt (request! base "POST" "/api/alpha/hyperedge" p)
        id (:hx/id receipt)]
    (when-not (and (:ok receipt) (string? id) (str/starts-with? id "act:"))
      (throw (ex-info "Missing minted rule receipt" receipt)))
    (let [path (str "/api/alpha/hyperedges?type=rule%2Frecord&limit=1000&end="
                    (URLEncoder/encode (first (:hx/endpoints p)) "UTF-8")
                    "&valid-as-of=" (URLEncoder/encode (:valid-from request) "UTF-8"))
          page (request! base "GET" path nil)
          matches (filter #(= id (:hx/id %)) (:hyperedges page))
          _ (when-not (= 1 (count matches))
              (throw (ex-info "Minted rule not found at valid time" {:id id :valid-from (:valid-from request)})))
          stored (first matches)]
      (when-not (= (select-keys p [:hx/type :hx/endpoints :hx/props])
                   (select-keys stored [:hx/type :hx/endpoints :hx/props]))
        (throw (ex-info "Rule readback mismatch" {:id id})))
      (assoc receipt :verified? true))))

(defn -main [& args]
  (try
    (let [[write? file] (if (= "--write" (first args)) [true (second args)] [false (first args)])
          _ (when-not (and file (= (count args) (if write? 2 1)))
              (throw (ex-info "Usage: rule-record [--write] FILE.edn" {})))
          request (edn/read-string (slurp file))]
      (prn (if write? (write! (or (System/getenv "FUTON1B_URL") "http://127.0.0.1:7073") request)
               {:ok true :dry-run? true :payload (payload request)}))
      (shutdown-agents))
    (catch Exception e
      (binding [*out* *err*] (prn {:ok false :message (.getMessage e) :detail (ex-data e)}))
      (shutdown-agents)
      (System/exit 1))))
