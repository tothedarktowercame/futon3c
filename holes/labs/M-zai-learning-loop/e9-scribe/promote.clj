#!/usr/bin/env -S clojure -M
(ns promote
  "Promote the reviewed E9 a96J04 drafts and propose pattern attachments.

   Stable ids make the pass replay-safe.  Attachment review is deliberately
   separate: this script leaves every memory/assert edge :proposed so the
   reviewing owner, not the scribe/asserter, applies the lifecycle transition."
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def root "holes/labs/M-zai-learning-loop/e9-scribe")
(def base-url (or (System/getenv "FUTON_SUBSTRATE_URL")
                  "http://127.0.0.1:7073"))
(def commit? (some #{"--commit"} *command-line-args*))
(def asserter "codex-12")
(def session-id "M-zai-learning-loop/e9-scribe-a96J04")
(def mission-id "M-zai-learning-loop")
(def problem-id "a96J04")

(def plan
  [{:slug "open-set-interval-decomposition-gap"
    :file "open-set-interval-decomposition-gap.md"
    :level :lemma-location
    :kind :reference
    :hook "Open-cover API exists; interval decomposition was a revision-scoped gap"
    :tags [:open-set :interval-decomposition :Lebesgue :null-set]
    :pattern "math/missing-dependency-protocol"
    :distills ["e-9bc8848e-8cec-4b65-879b-ae8ccccc9949"
               "e-4b7ea8b6-0d88-49a8-bb7c-b76fcbf30bbc"
               "e-d7b604b2-24e4-4703-ad17-1bd756df25fa"
               "e-afcdc7ee-2507-473b-95ac-8e74c2a7de67"]
    :attachment-why "A bounded Mathlib search exposed a missing theorem/API bridge and an explicit local-construction frontier."}
   {:slug "monotone-image-interval-containment"
    :file "monotone-image-interval-containment.md"
    :level :tactic
    :kind :reference
    :hook "Build ambient interval membership before applying MonotoneOn"
    :tags [:monotone :interval-image :MonotoneOn :Icc]
    :pattern "math/measure-integration-api"
    :distills ["e-0fafe91b-b9ab-4ddf-abb4-876ba9447d1e"
               "e-f6f409e6-b6b8-47f3-bfec-3f6e63e79b23"
               "e-b7709d4e-6537-415f-ab8a-e06912cf313b"
               "e-e55d539e-492e-4ba5-9298-e9b8147edac6"]
    :attachment-why "The compiled helper is an interval-image step used inside a Lebesgue-null image argument."}
   {:slug "null-image-via-open-cover-and-finite-ac"
    :file "null-image-via-open-cover-and-finite-ac.md"
    :level :strategy
    :kind :feedback
    :hook "Factor a null-image proof into cover, intervals, images, and finite AC"
    :tags [:Lebesgue :null-set :absolute-continuity :finite-intervals]
    :pattern "math/measure-integration-api"
    :distills ["e-d7b604b2-24e4-4703-ad17-1bd756df25fa"
               "e-956a1b1b-0bd6-4366-9d3d-f7ddcff55ca4"
               "e-47ffb25c-9cc6-4df5-848b-62a464bb1c61"
               "e-afcdc7ee-2507-473b-95ac-8e74c2a7de67"]
    :attachment-why "The record is explicitly a measure/integration proof architecture using null covers and finite absolute continuity."}
   {:slug "localize-an-observed-blocker-at-one-sorry"
    :file "localize-an-observed-blocker-at-one-sorry.md"
    :level :process
    :kind :feedback
    :hook "Localize one honest sorry at the exact observed dependency gap"
    :tags [:Lean :sorry :proof-boundary :dependency-blocker]
    :pattern "math/missing-dependency-protocol"
    :distills ["e-4b7ea8b6-0d88-49a8-bb7c-b76fcbf30bbc"
               "e-d7b604b2-24e4-4703-ad17-1bd756df25fa"
               "e-e55d539e-492e-4ba5-9298-e9b8147edac6"
               "e-47ffb25c-9cc6-4df5-848b-62a464bb1c61"
               "e-afcdc7ee-2507-473b-95ac-8e74c2a7de67"]
    :attachment-why "The process rule governs how to preserve a compiling frontier when a searched dependency is missing."}
   {:slug "closer-open-component-decomposition"
    :file "closer-open-component-decomposition.md"
    :level :lemma-location
    :kind :reference
    :hook "Construct open components via rational witnesses, sInf, and sSup"
    :tags [:open-set :interval-decomposition :Lebesgue :null-set]
    :pattern "math/missing-dependency-protocol"
    :distills []
    :attachment-why "The closer turns the same missing packaged dependency into a concrete local construction recipe."}])

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
      (let [response ((case method :get http/get :post http/post)
                      (str base-url path) opts)]
        (if (and (= 503 (:status response)) (< attempt 5))
          (do (Thread/sleep 1000) (recur (inc attempt)))
          {:status (:status response) :body (parse-body response)})))))

(defn- evidence-id [{:keys [slug]}]
  (str "e-e9-a96j04-" slug))

(defn- hyperedge-id [item]
  (str "hx-e9-a96j04-" (:slug item)))

(defn- draft-text [{:keys [file]}]
  (slurp (str root "/" file)))

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
    ;; The reviewed Markdown is the authoritative body and is stored byte for
    ;; byte; promotion metadata remains outside it.
    :body (draft-text item)
    :why (:attachment-why item)
    :how-to-apply (:hook item)
    :requested-memory-level (:level item)
    :confidence {:n-instances 1 :scope :single-problem}
    :promotion {:gate :claude-owner-review
                :reviewer "claude-10"
                :date "2026-08-03"}}
   :evidence/tags (vec (concat [:memory :memory/assert :mathematics]
                               (:tags item)))})

(defn- edge [item]
  (let [eid (evidence-id item)
        pattern (:pattern item)
        subjects [problem-id pattern]
        endpoints (vec (distinct (concat [eid] subjects (:distills item)
                                         [session-id mission-id])))]
    {:hx/id (hyperedge-id item)
     :hx/type :memory/assert
     :hx/endpoints endpoints
     :hx/props {:roles {:entry eid
                        :subjects subjects
                        :distills (:distills item)
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
  (let [response (request! :get (str "/api/alpha/evidence/" (encode (evidence-id item))))]
    (when (= 200 (:status response))
      (let [body (:body response)]
        (or (:entry body)
            (when (:evidence/id body) body))))))

(defn- existing-edge [item]
  (let [response (request! :get (str "/api/alpha/hyperedge/" (encode (hyperedge-id item))))]
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
          (throw (ex-info "attachment read-back mismatch" {:id (hyperedge-id item)}))))
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

(defn- tag-query [tags]
  ;; The tool contract's memory_search tag semantics are conjunction.  Scope
  ;; the read to this promotion session, then apply that same predicate to the
  ;; returned durable entries.  This avoids a broad corpus scan and remains a
  ;; read-only memory_search-equivalent verification.
  (let [query (str "/api/alpha/evidence?type=memory&limit=100&session-id="
                   (encode session-id))
        response (request! :get query)]
    (when-not (= 200 (:status response))
      (throw (ex-info "tag query failed" response)))
    (->> (get-in response [:body :entries])
         (filter (fn [entry]
                   (let [entry-tags (set (:evidence/tags entry))]
                     (every? entry-tags tags))))
         (mapv :evidence/id))))

(defn -main [& _args]
  (when-not (= 200 (:status (request! :get "/health")))
    (throw (ex-info "store health probe failed" {:base-url base-url})))
  (doseq [pattern (distinct (map :pattern plan))]
    (when-not (= 200 (:status (request! :get (str "/api/alpha/entity/" (encode pattern)))))
      (throw (ex-info "attachment pattern is absent" {:pattern pattern}))))
  (let [results (mapv ensure-item! plan)
        queries (->> plan
                     (group-by :tags)
                     (mapv (fn [[tags items]]
                             (let [found (tag-query tags)
                                   expected (mapv evidence-id items)]
                               {:tags tags
                                :expected expected
                                :found (filterv (set expected) found)
                                :verified? (every? (set found) expected)}))))
        report {:date "2026-08-03"
                :mode (if commit? :commit :dry-run)
                :asserter asserter
                :reviewer "claude-10"
                :results results
                :tag-query-verification queries}]
    (when (and commit? (not-every? :verified? queries))
      (throw (ex-info "one or more tag queries did not retrieve every expected memory"
                      {:queries queries})))
    (spit (str root "/promotion-report.edn") (str (pr-str report) "\n"))
    (prn report)))

(-main)
