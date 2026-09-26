#!/usr/bin/env -S clojure -M
(ns promote
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn])
  (:import [java.net URLEncoder]
           [java.time Instant]))

(def root "holes/labs/M-zai-learning-loop/a94A09-scribe")
(def base-url "http://127.0.0.1:7073")
(def commit? (some #{"--commit"} *command-line-args*))
(def session-id "M-zai-learning-loop/a94A09-scribe")
(def job-id "invoke-1786369654355-3517-0994cab1")
(def commit-id "22c5b80c064ae36e83a3b8759607ccf430c76169")
(def witnesses ["e-fab2e3d9-6877-444a-9949-a11720305918"
                "e-memory-outcome-sweeper-6e8a041ab7506a025951c3b4"])

(def plan
  [{:slug "shrink-radius-rouche-fixed-point"
    :file "shrink-radius-rouche-fixed-point.md"
    :level :strategy
    :hook "Choose t < r < 1 so a weak disk-map bound becomes strict Rouché domination"
    :tags [:Rouche :scaled :fixed :point :shrink :radius :closedBall
           :zeroCountInClosedBall :holomorphic :disk]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :pattern "math/holomorphic-disk-api"
    :why "It records a compiled holomorphic-disk construction using the importable Rouché zero-count theorem."}
   {:slug "compact-endpoint-limit-for-scaled-fixed-points"
    :file "compact-endpoint-limit-for-scaled-fixed-points.md"
    :level :tactic
    :hook "Pass strict radial-contraction fixed points to t = 1 by compact subsequences"
    :tags [:compact :endpoint :limit :scaled :fixed :points :closedBall
           :subsequence :ContinuousOn :tendsto]
    :confidence {:n-instances 1 :scope :single-compiled-problem}
    :pattern "math/holomorphic-disk-api"
    :why "It packages the compactness-and-continuity endpoint step for disk self-maps."}
   {:slug "open-hunger-disk-automorphism-schwarz-pick-two-fixed-points"
    :file "open-hunger-disk-automorphism-schwarz-pick-two-fixed-points.md"
    :level :open-hunger
    :hook "Locate or build unit-disk Möbius conjugation and Schwarz-Pick two-fixed-point rigidity"
    :tags [:disk :automorphisms :Schwarz-Pick :two :fixed :points
           :Mobius :uniqueness]
    :confidence {:n-instances 1 :scope :single-unmet-query :status :open}
    :pattern "math/missing-dependency-protocol"
    :why "It records an unresolved dependency with literal demand vocabulary and a precise target."}])

(defn encode [s] (URLEncoder/encode s "UTF-8"))
(defn parse-body [r]
  (let [b (:body r)]
    (if (string? b) (edn/read-string {:default (fn [_ v] v)} b) b)))
(defn request [method path & [payload]]
  (let [opts (cond-> {:headers {"accept" "application/edn"}
                            :timeout 60000 :throw false}
               payload (assoc :headers {"accept" "application/edn"
                                        "content-type" "application/edn"
                                        "x-penholder" "api"}
                              :body (pr-str payload)))
        r ((case method :get http/get :post http/post)
           (str base-url path) opts)]
    {:status (:status r) :body (parse-body r)}))
(defn memory-id [x] (str "e-a09-" (:slug x)))
(defn edge-id [x] (str "hx-a09-" (:slug x)))
(defn entry [x]
  {:evidence/id (memory-id x)
   :evidence/subject {:ref/type :task :ref/id "a94A09"}
   :evidence/type :memory
   :evidence/claim-type :assert
   :evidence/author "ams-scribe-1"
   :evidence/session-id session-id
   :evidence/at (str (Instant/now))
   :evidence/body {:name (:slug x)
                   :hook (:hook x)
                   :kind (if (= :open-hunger (:level x)) :feedback :reference)
                   :body (slurp (str root "/" (:file x)))
                   :why (:why x)
                   :how-to-apply (:hook x)
                   :requested-memory-level (:level x)
                   :confidence (:confidence x)
                   :provenance {:problem "a94A09" :job-ids [job-id]
                                :commit commit-id
                                :turn-evidence-ids witnesses}
                   :promotion {:gate :owner-commissioned-same-pass
                               :commissioned-by "ams-claude-1"
                               :date "2026-08-10"}}
   :evidence/tags (vec (concat [:memory :memory/assert :mathematics]
                               (:tags x)))})
(defn edge [x]
  {:hx/id (edge-id x)
   :hx/type :memory/assert
   :hx/endpoints (vec (concat [(memory-id x) "a94A09" (:pattern x)]
                              witnesses [session-id "M-zai-learning-loop"]))
   :hx/props {:roles {:entry (memory-id x)
                      :subjects ["a94A09" (:pattern x)]
                      :distills witnesses
                      :session session-id
                      :mission "M-zai-learning-loop"
                      :patterns [(:pattern x)]}
              :kind (if (= :open-hunger (:level x)) :feedback :reference)
              :name (:slug x) :hook (:hook x)
              :facets (mapv name (:tags x))
              :volatile? false :state :current :domain :mathematics
              :witness-status :self-asserted
              :attachment-status :proposed}})
(defn fetched [path k]
  (let [r (request :get path)]
    (when (= 200 (:status r))
      (or (k (:body r)) (:body r)))))
(defn ensure-one [x]
  (let [eid (memory-id x) hid (edge-id x)
        before-e (fetched (str "/api/alpha/evidence/" (encode eid)) :entry)
        before-h (fetched (str "/api/alpha/hyperedge/" (encode hid)) :hyperedge)]
    (when (and commit? (nil? before-e))
      (assert (<= 200 (:status (request :post "/api/alpha/evidence" (entry x))) 299)))
    (when (and commit? (nil? before-h))
      (assert (<= 200 (:status (request :post "/api/alpha/hyperedge" (edge x))) 299)))
    (let [after-e (fetched (str "/api/alpha/evidence/" (encode eid)) :entry)
          after-h (fetched (str "/api/alpha/hyperedge/" (encode hid)) :hyperedge)]
      (when commit?
        (assert (= eid (:evidence/id after-e)))
        (assert (= hid (:hx/id after-h)))
        (assert (= :proposed (get-in after-h [:hx/props :attachment-status]))))
      {:memory-id eid :hyperedge-id hid :pattern (:pattern x)
       :attachment-status (get-in after-h [:hx/props :attachment-status])
       :verified-present? (boolean (and after-e after-h))})))

(defn -main [& _]
  (assert (= 200 (:status (request :get "/health"))))
  (doseq [p (distinct (map :pattern plan))]
    (assert (= 200 (:status (request :get (str "/api/alpha/entity/" (encode p)))))))
  (prn {:committed? (boolean commit?) :results (mapv ensure-one plan)}))

(apply -main *command-line-args*)
