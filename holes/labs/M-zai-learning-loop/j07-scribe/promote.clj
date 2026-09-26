#!/usr/bin/env -S clojure -M
(ns promote
  "Promote a96J07 math and desk-research memories with proposed attachments."
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def root "holes/labs/M-zai-learning-loop/j07-scribe")
(def base-url (or (System/getenv "FUTON_SUBSTRATE_URL")
                  "http://127.0.0.1:7073"))
(def commit? (some #{"--commit"} *command-line-args*))
(def asserter "codex-12")
(def session-id "M-zai-learning-loop/j07-scribe-a96J07")
(def mission-id "M-zai-learning-loop")
(def problem-id "a96J07")
(def job-id "invoke-1785772570109-947-08373761")
(def commit-id "462b48a3e1047b0fec1fa13436cee9391236599d")
(def turn-evidence
  ["e-6822cd1c-1cef-4dbd-ac5b-4a0a627b4e43"
   "e-51f2dd3a-b182-46a3-87c9-fe4f40adc263"])

(def plan
  [{:slug "reuse-two-pole-partial-fraction-contour-pattern"
    :file "reuse-two-pole-partial-fraction-contour-pattern.md"
    :level :tactic
    :kind :reference
    :hook "Reuse the compiled partial-fraction/Cauchy-kernel contour skeleton"
    :tags [:complex-analysis :contour-integral :partial-fractions :Cauchy-formula]
    :confidence {:n-instances 2
                 :scope :underlying-pattern
                 :problems ["a92J06" "a96J07"]}
    :pattern "math/entire-and-singularity-api"
    :attachment-why "The compiled tactic decomposes an entire-function rational contour into Cauchy kernels at isolated poles."}
   {:slug "derive-liouville-directly-from-two-pole-ml-decay"
    :file "derive-liouville-directly-from-two-pole-ml-decay.md"
    :level :strategy
    :kind :reference
    :hook "Use a zero second pole so the normalized ML bound becomes M/(R-‖a‖)"
    :tags [:Liouville :ML-estimate :two-pole :difference-quotient]
    :confidence {:n-instances 1 :scope :single-problem}
    :pattern "math/entire-and-singularity-api"
    :attachment-why "The strategy is a direct bounded-entire Liouville proof using contour decay at infinity."}
   {:slug "search-solved-neighbors-before-building-machinery"
    :file "search-solved-neighbors-before-building-machinery.md"
    :level :process
    :kind :feedback
    :hook "Search compiled problem neighbors before rebuilding formal machinery"
    :tags [:desk-research :neighbor-search :consultation :cross-problem-reuse]
    :confidence {:n-instances 1 :scope :single-documented-payoff}
    :pattern "math/corpus-trust-protocol"
    :attachment-why "The practice verifies current compiled corpus neighbors before trusting or rebuilding a route."}
   {:slug "record-consultation-discards-with-reasons"
    :file "record-consultation-discards-with-reasons.md"
    :level :process
    :kind :feedback
    :hook "Record what each consulted resource returned and why it was discarded"
    :tags [:desk-research :discard-with-reasons :consultation :route-selection]
    :confidence {:n-instances 1 :scope :single-session :discard-count 3}
    :pattern "math/corpus-trust-protocol"
    :attachment-why "The practice records reasoned non-use and partial trust for each consulted resource."}])

(defn- encode [s]
  (URLEncoder/encode (str s) "UTF-8"))

(defn- parse-body [response]
  (when-let [body (:body response)]
    (edn/read-string {:default (fn [_tag value] value)} body)))

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

          (:exception result)
          (throw (:exception result))

          :else
          {:status (:status response) :body (parse-body response)})))))

(defn- evidence-id [item]
  (str "e-j07-" (:slug item)))

(defn- hyperedge-id [item]
  (str "hx-j07-" (:slug item)))

(defn- draft-text [item]
  (slurp (str root "/" (:file item))))

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
                 :job-id job-id
                 :commit commit-id
                 :turn-evidence-ids turn-evidence}
    :promotion {:gate :owner-commissioned-same-pass
                :commissioned-by "claude-10"
                :date "2026-08-03"}}
   :evidence/tags (vec (concat [:memory :memory/assert :mathematics]
                               (:tags item)))})

(defn- edge [item]
  (let [eid (evidence-id item)
        pattern (:pattern item)
        subjects [problem-id pattern]
        endpoints (vec (distinct (concat [eid] subjects turn-evidence
                                         [session-id mission-id])))]
    {:hx/id (hyperedge-id item)
     :hx/type :memory/assert
     :hx/endpoints endpoints
     :hx/props {:roles {:entry eid
                        :subjects subjects
                        :distills turn-evidence
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

(defn- existing-entry [item]
  (let [response (request! :get (str "/api/alpha/evidence/"
                                    (encode (evidence-id item))))]
    (when (= 200 (:status response))
      (let [body (:body response)]
        (or (:entry body) (when (:evidence/id body) body))))))

(defn- existing-edge [item]
  (let [response (request! :get (str "/api/alpha/hyperedge/"
                                    (encode (hyperedge-id item))))]
    (when (= 200 (:status response))
      (let [body (:body response)]
        (or (:hyperedge body)
            (when (:hx/id body)
              (select-keys body [:hx/id :hx/type :hx/endpoints :hx/props])))))))

(defn- same-entry? [expected actual]
  (= (dissoc expected :evidence/at) (dissoc actual :evidence/at)))

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
    (when (and commit? (nil? before-entry))
      (let [response (request! :post "/api/alpha/evidence" expected-entry)]
        (when-not (<= 200 (:status response) 299)
          (throw (ex-info "memory append failed" response)))))
    (when (and commit? (nil? before-edge))
      (let [response (request! :post "/api/alpha/hyperedge" expected-edge)]
        (when-not (<= 200 (:status response) 299)
          (throw (ex-info "attachment append failed" response)))))
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
       :result (cond
                 (and before-entry before-edge) :existing
                 commit? :promoted
                 :else :would-promote)})))

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
                                       (let [entry-tags
                                             (set (:evidence/tags entry))]
                                         (every? entry-tags (:tags item)))))
                                    (mapv :evidence/id))
                         expected (evidence-id item)]
                     {:tags (:tags item)
                      :expected expected
                      :found (filterv #{expected} found)
                      :verified? (contains? (set found) expected)}))
                 plan)
        report {:date "2026-08-03"
                :mode (if commit? :commit :dry-run)
                :asserter asserter
                :attachment-reviewer-pending "claude-10"
                :source {:job-id job-id
                         :commit commit-id
                         :turn-evidence-ids turn-evidence}
                :results results
                :tag-query-verification queries}]
    (when (and commit? (not-every? :verified? queries))
      (throw (ex-info "tag retrieval verification failed" {:queries queries})))
    (spit (str root "/promotion-report.edn") (str (pr-str report) "\n"))
    (prn report)))

(-main)
