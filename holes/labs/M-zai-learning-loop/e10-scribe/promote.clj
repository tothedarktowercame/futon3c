#!/usr/bin/env -S clojure -M
(ns promote
  "Promote a96J08 E10 memories and leave their attachments proposed."
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def root "holes/labs/M-zai-learning-loop/e10-scribe")
(def base-url (or (System/getenv "FUTON_SUBSTRATE_URL")
                  "http://127.0.0.1:7073"))
(def commit? (some #{"--commit"} *command-line-args*))
(def asserter "codex-12")
(def session-id "M-zai-learning-loop/e10-scribe-a96J08")
(def mission-id "M-zai-learning-loop")
(def problem-id "a96J08")
(def job-ids ["invoke-1785772892476-949-1c6b0d34"
              "invoke-1785773411377-951-bce95078"])
(def commit-id "37192e165973a5280114a45ca2b66bf8429e4c37")

(def plan
  [{:slug "rectangular-contour-residue-theorem-gap"
    :file "rectangular-contour-residue-theorem-gap.md"
    :level :lemma-location
    :kind :reference
    :hook "Budget disk excision or shifted-edge convergence when rectangle Cauchy--Goursat has an interior pole"
    :tags [:rectangular-contour :residue-theorem :Cauchy-Goursat :contour-integral]
    :confidence {:n-instances 1 :scope :single-problem :revision-scoped true}
    :turn-evidence ["e-bd7c225c-30e9-4164-90e0-4651524917b3"
                    "e-925e472d-f5e4-47d7-bad0-fee41c2cf68b"
                    "e-8225ad52-bbc9-4f5b-abf9-73ae83772208"
                    "e-5475c4e0-c87e-4ca4-8c4a-7905bc5eaedb"]
    :pattern "math/missing-dependency-protocol"
    :attachment-why "The memory identifies a searched Mathlib API gap, its nearest available theorem, and two explicit local-construction routes."}
   {:slug "time-recon-after-route-selection"
    :file "time-recon-after-route-selection.md"
    :level :process
    :kind :feedback
    :hook "Repeat bounded reconnaissance with obstacle vocabulary after selecting a proof route"
    :tags [:desk-research :recon-timing :route-selection :discard-with-reasons]
    :confidence {:n-instances 1 :scope :paired-session :discard-count 6}
    :turn-evidence ["e-d149bd0c-10b4-4fd5-8759-dfb9fb84991f"
                    "e-249fac6b-f72a-467a-b816-9a0f6c2aae6b"
                    "e-5475c4e0-c87e-4ca4-8c4a-7905bc5eaedb"]
    :pattern "math/corpus-trust-protocol"
    :attachment-why "The process grades retrieved hints by route fit, records reasoned non-use, and re-queries the corpus with obstacle vocabulary."}
   {:slug "leave-consultation-trail-in-boundary-artifact"
    :file "leave-consultation-trail-in-boundary-artifact.md"
    :level :process
    :kind :feedback
    :hook "Put the needed bridge, nearest API, empty searches, and next routes beside the remaining sorry"
    :tags [:desk-research :consultation-trail :boundary-comment :session-relay]
    :confidence {:n-instances 1 :scope :single-instrumented-artifact}
    :turn-evidence ["e-925e472d-f5e4-47d7-bad0-fee41c2cf68b"
                    "e-8225ad52-bbc9-4f5b-abf9-73ae83772208"
                    "e-5475c4e0-c87e-4ca4-8c4a-7905bc5eaedb"]
    :pattern "math/missing-dependency-protocol"
    :attachment-why "The artifact relay records the dependency frontier, bounded searches, and exact unblock routes for the next session."}])

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

(defn- evidence-id [item] (str "e-e10-" (:slug item)))
(defn- hyperedge-id [item] (str "hx-e10-" (:slug item)))
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
                :commissioned-by "claude-10"
                :date "2026-08-03"}}
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
        report {:date "2026-08-03"
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
