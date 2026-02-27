(ns futon3c.portfolio.heartbeat
  "Weekly heartbeat consumer — fetches bids from futon5's nonstarter API
   and computes action-level prediction errors (D-8 + D-11).

   The bid/clear shape is action-level with effort bands:
   - Bid:   [{:action :work-on :mission \"M-foo\" :effort :hard} ...]
   - Clear: [{:action :work-on :mission \"M-foo\" :effort :hard :outcome :partial} ...]

   Prediction errors:
   1. Action mismatch — planned action not taken, or unplanned action taken
   2. Effort mismatch — actual effort band differs from predicted
   3. Outcome mismatch — action outcome was :partial or :abandoned
   4. Mode mismatch — predicted mode differs from observed mode
   5. Unplanned work — actions in clear not present in bid"
  (:require [cheshire.core :as json]
            [org.httpkit.client :as http]))

;; =============================================================================
;; Effort bands (aligned with Nonstarter estimate.clj size scale)
;; =============================================================================

(def effort-bands
  "Ordinal effort scale. Higher = more work."
  {:trivial 1 :easy 2 :medium 3 :hard 4 :epic 5})

(defn effort-distance
  "Ordinal distance between two effort bands."
  [predicted actual]
  (let [p (get effort-bands predicted 3)
        a (get effort-bands actual 3)]
    (- a p)))

;; =============================================================================
;; Prediction error computation (D-11)
;; =============================================================================

(defn- bid-key [{:keys [action mission]}]
  [action mission])

(defn compute-action-errors
  "Compare bid actions against clear actions, return structured prediction errors.

   Returns:
   {:action-mismatches  [{:bid <bid> :status :not-taken}]
    :effort-mismatches  [{:mission ... :predicted :hard :actual :medium :distance -1}]
    :outcome-mismatches [{:mission ... :outcome :partial}]
    :unplanned          [{:clear <clear-entry>}]
    :summary            {:planned N :taken N :unplanned N :effort-error-sum N}}"
  [bids clears]
  (let [bid-index (into {} (map (fn [b] [(bid-key b) b])) bids)
        clear-index (into {} (map (fn [c] [(bid-key c) c])) clears)
        ;; Actions in bid but not in clear
        not-taken (for [[k b] bid-index
                        :when (not (contains? clear-index k))]
                    {:bid b :status :not-taken})
        ;; Actions in both — check effort and outcome
        matched (for [[k b] bid-index
                      :let [c (get clear-index k)]
                      :when c]
                  {:bid b :clear c})
        effort-mismatches (for [{:keys [bid clear]} matched
                                :let [d (effort-distance (:effort bid) (:effort clear))]
                                :when (not= 0 d)]
                            {:mission (:mission bid)
                             :action (:action bid)
                             :predicted (:effort bid)
                             :actual (:effort clear)
                             :distance d})
        outcome-mismatches (for [{:keys [clear]} matched
                                 :when (and (:outcome clear)
                                            (not= :complete (:outcome clear)))]
                             {:mission (:mission clear)
                              :action (:action clear)
                              :outcome (:outcome clear)})
        ;; Actions in clear but not in bid
        unplanned (for [[k c] clear-index
                        :when (not (contains? bid-index k))]
                    {:clear c})
        effort-error-sum (reduce + 0 (map #(Math/abs (double (:distance %))) effort-mismatches))]
    {:action-mismatches (vec not-taken)
     :effort-mismatches (vec effort-mismatches)
     :outcome-mismatches (vec outcome-mismatches)
     :unplanned (vec unplanned)
     :summary {:planned (count bids)
               :taken (count matched)
               :not-taken (count not-taken)
               :unplanned (count unplanned)
               :effort-error-sum effort-error-sum}}))

(defn compute-mode-error
  "Compare predicted mode against observed mode."
  [predicted observed]
  (when (and predicted observed (not= predicted observed))
    {:predicted predicted :observed observed}))

;; =============================================================================
;; futon5 API client
;; =============================================================================

(def default-api-url "http://localhost:7071")

(defn fetch-heartbeat
  "Fetch a heartbeat from the futon5 nonstarter API."
  ([week-id] (fetch-heartbeat default-api-url week-id))
  ([api-url week-id]
   (let [{:keys [status body error]}
         @(http/get (str api-url "/api/heartbeat")
                    {:query-params {"week-id" week-id}})]
     (when (and (= 200 status) (not error))
       (json/parse-string body true)))))

(defn post-bid!
  "Post a bid to the futon5 nonstarter API."
  ([bid-data] (post-bid! default-api-url bid-data))
  ([api-url {:keys [week-id bids mode-prediction] :as bid-data}]
   (let [{:keys [status body error]}
         @(http/post (str api-url "/api/heartbeat/bid")
                     {:headers {"Content-Type" "application/json"}
                      :body (json/generate-string bid-data)})]
     (when (and (= 200 status) (not error))
       (json/parse-string body true)))))

(defn post-clear!
  "Post a clear to the futon5 nonstarter API."
  ([clear-data] (post-clear! default-api-url clear-data))
  ([api-url {:keys [week-id clears mode-observed delta aif-snapshot] :as clear-data}]
   (let [{:keys [status body error]}
         @(http/post (str api-url "/api/heartbeat/clear")
                     {:headers {"Content-Type" "application/json"}
                      :body (json/generate-string clear-data)})]
     (when (and (= 200 status) (not error))
       (json/parse-string body true)))))
