#!/usr/bin/env -S clojure -M
(ns promote
  "Promote a96J02 memories and leave their attachments proposed."
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def root "holes/labs/M-zai-learning-loop/a96J02-scribe")
(def base-url (or (System/getenv "FUTON_SUBSTRATE_URL")
                  "http://127.0.0.1:7073"))
(def commit? (some #{"--commit"} *command-line-args*))
(def asserter "codex-12")
(def session-id "M-zai-learning-loop/a96J02-scribe")
(def mission-id "M-zai-learning-loop")
(def problem-id "a96J02")
(def job-ids ["invoke-1785854903455-984-ebd8887d"
              "invoke-1785855266102-987-05ed970b"
              "invoke-1785856907892-990-3ec676f1"])
(def commit-id "318160d89257eab8482e8066e284afb91a7ec6ac")

(def plan
  [{:slug "translation-symmdiff-preimage-api"
    :file "translation-symmdiff-preimage-api.md"
    :level :lemma-location
    :kind :reference
    :hook "Translate L1-indicator continuity goals to symmetric-difference preimage convergence"
    :tags [:L1 :translation :continuity :indicator :function :dominated
           :convergence :measure :inter :translate :symmetric-difference
           :ContinuousPreimage]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-80de2c3d-3c71-4b9b-9090-b994124581fa"
                    "e-840afcca-1d9a-4a7d-aff6-b2caad5e587e"
                    "e-de860d2b-ae07-4cd4-bb5f-e280384d0563"]
    :pattern "math/measure-integration-api"
    :attachment-why "It locates and instantiates the exact measure-theoretic API that discharged the missing analytic bridge."}
   {:slug "overlap-continuity-via-measured-sets"
    :file "overlap-continuity-via-measured-sets.md"
    :level :tactic
    :kind :reference
    :hook "Lift symmetric-difference convergence through nonexpansive intersection in MeasuredSets"
    :tags [:measurable :set :finite :measure :intersection :positive :overlap
           :MeasuredSets :symmetric-difference]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-6c935a1b-46b5-4fd0-9577-5ab34113b195"
                    "e-de860d2b-ae07-4cd4-bb5f-e280384d0563"]
    :pattern "math/measure-integration-api"
    :attachment-why "It packages a reusable MeasuredSets emetric proof pattern for continuity of overlap measures."}
   {:slug "steinhaus-positive-convolution-open-locus"
    :file "steinhaus-positive-convolution-open-locus.md"
    :level :strategy
    :kind :reference
    :hook "Prove a positive-measure sumset has interior via a continuous indicator convolution"
    :tags [:Steinhaus :theorem :sumset :measurable :sets :positive :measure
           :open :interval :Minkowski :sum :additive :combinatorics :theory
           :convolution :Fubini]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :turn-evidence ["e-d1537c6c-8cef-4c79-8d5a-f42ab3816728"
                    "e-7fda96a7-4653-4e29-92bc-eab906ac784d"
                    "e-80de2c3d-3c71-4b9b-9090-b994124581fa"
                    "e-de860d2b-ae07-4cd4-bb5f-e280384d0563"]
    :pattern "math/measure-integration-api"
    :attachment-why "It organizes indicator integrability, convolution/Fubini, continuity, and positivity into a compiled measure-integration strategy."}
   {:slug "open-hunger-lebesgue-density-theorem"
    :file "open-hunger-lebesgue-density-theorem.md"
    :level :open-hunger
    :kind :feedback
    :hook "Ground the exact Mathlib density-point API and a compiled positive-measure witness"
    :tags [:Lebesgue :density :theorem :measurable :set :point :of]
    :confidence {:n-instances 1 :scope :single-unmet-query :status :open}
    :turn-evidence ["e-7fda96a7-4653-4e29-92bc-eab906ac784d"
                    "e-de860d2b-ae07-4cd4-bb5f-e280384d0563"]
    :pattern "math/missing-dependency-protocol"
    :attachment-why "It records an unresolved API-location demand with its exact query, proof stage, and required future witness without asserting absence."}])

(defn- encode [s] (URLEncoder/encode (str s) "UTF-8"))

(defn- parse-body [response]
  (let [body (:body response)]
    (if (string? body)
      (edn/read-string {:default (fn [_tag value] value)} body)
      body)))

(defn- request! [method path & [payload]]
  (let [opts (cond-> {:headers {"accept" "application/edn"}
                      :timeout 60000
                      :throw false}
               payload (assoc :headers {"accept" "application/edn"
                                        "content-type" "application/edn"
                                        "x-penholder" "api"}
                              :body (pr-str payload)))]
    (loop [attempt 0]
      (let [result (try
                     {:response ((case method :get http/get :post http/post)
                                 (str base-url path) opts)}
                     (catch Throwable t {:exception t}))
            response (:response result)
            retry? (or (:exception result) (= 503 (:status response)))]
        (cond
          (and retry? (< attempt 8))
          (do (Thread/sleep 1000) (recur (inc attempt)))

          (:exception result) (throw (:exception result))
          :else {:status (:status response) :body (parse-body response)})))))

(defn- evidence-id [item] (str "e-j02-" (:slug item)))
(defn- hyperedge-id [item] (str "hx-j02-" (:slug item)))
(defn- draft-text [item] (slurp (str root "/" (:file item))))

(defn- entry [item]
  {:evidence/id (evidence-id item)
   :evidence/subject {:ref/type :task :ref/id problem-id}
   :evidence/type :memory
   :evidence/claim-type :assert
   :evidence/author asserter
   :evidence/session-id session-id
   :evidence/at (str (Instant/now))
   :evidence/body
   {:name (:slug item)
    :hook (:hook item)
    :kind (:kind item)
    :body (draft-text item)
    :why (:attachment-why item)
    :how-to-apply (:hook item)
    :requested-memory-level (:level item)
    :confidence (:confidence item)
    :provenance {:problem problem-id
                 :job-ids job-ids
                 :commit commit-id
                 :turn-evidence-ids (:turn-evidence item)}
    :promotion {:gate :owner-commissioned-same-pass
                :commissioned-by "apm-driver"
                :date "2026-08-04"}}
   :evidence/tags (vec (concat [:memory :memory/assert :mathematics]
                               (:tags item)))})

(defn- edge [item]
  (let [eid (evidence-id item)
        pattern (:pattern item)
        subjects [problem-id pattern]
        witnesses (:turn-evidence item)]
    {:hx/id (hyperedge-id item)
     :hx/type :memory/assert
     :hx/endpoints (vec (distinct (concat [eid] subjects witnesses
                                          [session-id mission-id])))
     :hx/props {:roles {:entry eid
                        :subjects subjects
                        :distills witnesses
                        :session session-id
                        :mission mission-id
                        :patterns [pattern]}
                :kind (:kind item)
                :name (:slug item)
                :hook (:hook item)
                :facets (mapv name (:tags item))
                :volatile? false
                :state :current
                :domain :mathematics
                :witness-status :self-asserted
                :attachment-status :proposed}}))

(defn- existing [path key]
  (let [response (request! :get path)]
    (when (= 200 (:status response))
      (let [body (:body response)]
        (or (key body) (when ((case key :entry :evidence/id :hyperedge :hx/id) body)
                         body))))))

(defn- existing-entry [item]
  (existing (str "/api/alpha/evidence/" (encode (evidence-id item))) :entry))

(defn- existing-edge [item]
  (some-> (existing (str "/api/alpha/hyperedge/" (encode (hyperedge-id item)))
                    :hyperedge)
          (select-keys [:hx/id :hx/type :hx/endpoints :hx/props])))

(defn- same-entry? [expected actual]
  (= (dissoc expected :evidence/at) (dissoc actual :evidence/at)))

(defn- post-missing! [path value present? label]
  (when (and commit? (not present?))
    (let [response (request! :post path value)]
      (when-not (<= 200 (:status response) 299)
        (throw (ex-info (str label " append failed") response))))))

(defn- ensure-item! [item]
  (let [expected-entry (entry item)
        expected-edge (edge item)
        before-entry (existing-entry item)
        before-edge (existing-edge item)]
    (when (and before-entry (not (same-entry? expected-entry before-entry)))
      (throw (ex-info "stable memory id has different content"
                      {:id (evidence-id item)})))
    (when (and before-edge (not= expected-edge before-edge))
      (throw (ex-info "stable attachment id has different content"
                      {:id (hyperedge-id item)})))
    (post-missing! "/api/alpha/evidence" expected-entry before-entry "memory")
    (post-missing! "/api/alpha/hyperedge" expected-edge before-edge "attachment")
    (let [after-entry (existing-entry item)
          after-edge (existing-edge item)]
      (when commit?
        (when-not (same-entry? expected-entry after-entry)
          (throw (ex-info "memory read-back mismatch" {:id (evidence-id item)})))
        (when-not (= expected-edge after-edge)
          (throw (ex-info "attachment read-back mismatch"
                          {:id (hyperedge-id item)}))))
      {:name (:slug item)
       :memory-id (evidence-id item)
       :hyperedge-id (hyperedge-id item)
       :pattern (:pattern item)
       :tags (:tags item)
       :attachment-status (get-in after-edge [:hx/props :attachment-status])
       :verified-present? (boolean (and after-entry after-edge))})))

(defn- session-entries []
  (let [response (request!
                  :get
                  (str "/api/alpha/evidence?type=memory&limit=100&session-id="
                       (encode session-id)))]
    (when-not (= 200 (:status response))
      (throw (ex-info "tag query failed" response)))
    (get-in response [:body :entries])))

(defn -main [& _args]
  (when-not (= 200 (:status (request! :get "/health")))
    (throw (ex-info "store health probe failed" {:base-url base-url})))
  (doseq [pattern (distinct (map :pattern plan))]
    (when-not (= 200 (:status
                      (request! :get (str "/api/alpha/entity/"
                                          (encode pattern)))))
      (throw (ex-info "attachment pattern is absent" {:pattern pattern}))))
  (let [results (mapv ensure-item! plan)
        entries (session-entries)
        queries (mapv
                 (fn [item]
                   (let [found (->> entries
                                    (filter
                                     (fn [entry]
                                       (let [entry-tags (set (:evidence/tags entry))]
                                         (every? entry-tags (:tags item)))))
                                    (mapv :evidence/id))
                         expected (evidence-id item)]
                     {:tags (:tags item)
                      :expected expected
                      :found (filterv #{expected} found)
                      :verified? (contains? (set found) expected)}))
                 plan)
        report {:date "2026-08-04"
                :mode (if commit? :commit :dry-run)
                :asserter asserter
                :attachment-reviewer-pending "claude-10"
                :source {:job-ids job-ids :commit commit-id}
                :results results
                :tag-query-verification queries}]
    (when (and commit? (not-every? :verified? queries))
      (throw (ex-info "tag retrieval verification failed" {:queries queries})))
    (spit (str root "/promotion-report.edn") (str (pr-str report) "\n"))
    (prn report)))

(apply -main *command-line-args*)
