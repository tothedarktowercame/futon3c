(ns futon3c.logic.arxana-bridge
  "Project structural-law obligations into Arxana/futon1a hyperedges.

   This bridge follows the three-column stack convention:
   - hyperedge type: invariant/violation
   - endpoints: [inv:<family-or-rule>, violating-entity...]
   - stable id: hx:{type}:{sorted-endpoints}

   The bridge is intentionally thin. It transforms already-classified
   obligations into hyperedge docs and can optionally POST them to futon1a."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import (java.net URI)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.time Instant)))

(def ^:private default-futon1a-url "http://localhost:7071")

(defn invariant-endpoint
  [obligation]
  (str "inv:" (name (or (:family obligation) :unclassified))
       "/" (name (or (:violation-key obligation) :unknown))))

(defn- sanitize-fragment
  [x]
  (-> (str x)
      (str/replace #"\s+" "-")
      (str/replace #"[^A-Za-z0-9:_./#-]" "_")))

(defn- sha1-short
  [s]
  (let [digest (.digest (doto (MessageDigest/getInstance "SHA-1")
                          (.update (.getBytes (str s) StandardCharsets/UTF_8))))]
    (subs (apply str (map #(format "%02x" (bit-and 0xff %)) digest)) 0 10)))

(defn payload->entity-endpoints
  "Best-effort endpoint projection for an obligation payload.

   Endpoints are strings, not required to be existing futon1a entities."
  [obligation]
  (let [domain-id (:domain-id obligation)
        violation-key (:violation-key obligation)
        payload (:payload obligation)]
    (cond
      (and (= domain-id :mission) (vector? payload) (= 3 (count payload)))
      [(str "mission-cycle:" (sanitize-fragment (first payload)))
       (str "mission-obligation:" (sanitize-fragment (second payload)))]

      (and (= domain-id :mission) (map? payload))
      (cond-> []
        (:cycle payload) (conj (str "mission-cycle:" (sanitize-fragment (:cycle payload))))
        (:phase payload) (conj (str "mission-phase:" (sanitize-fragment (:phase payload)))))

      (and (= domain-id :proof) (vector? payload) (= 3 (count payload)))
      [(str "proof-item:" (sanitize-fragment (first payload)))
       (str "proof-item:" (sanitize-fragment (second payload)))]

      (and (= domain-id :proof) (map? payload))
      (cond-> []
        (:item payload) (conj (str "proof-item:" (sanitize-fragment (:item payload))))
        (:dep payload) (conj (str "proof-item:" (sanitize-fragment (:dep payload))))
        (:cycle payload) (conj (str "proof-cycle:" (sanitize-fragment (:cycle payload)))))

      (and (= domain-id :agency) (vector? payload) (= 2 (count payload)))
      [(str "peripheral:" (sanitize-fragment (first payload)))
       (str "peripheral:" (sanitize-fragment (second payload)))]

      (and (= domain-id :agency) (string? payload))
      [(str "agent:" (sanitize-fragment payload))]

      (and (= domain-id :agency) (map? payload) (:agent-id payload))
      [(str "agent:" (sanitize-fragment (:agent-id payload)))]

      (and (= domain-id :tickle) (string? payload))
      [(str "tickle-event:" (sanitize-fragment payload))]

      (and (= domain-id :tickle) (vector? payload))
      (mapv (fn [x]
              (str "tickle-ref:" (sanitize-fragment x)))
            payload)

      (and (= domain-id :tickle) (map? payload))
      (cond-> []
        (:agent-id payload) (conj (str "agent:" (sanitize-fragment (:agent-id payload))))
        (:event-id payload) (conj (str "tickle-event:" (sanitize-fragment (:event-id payload)))))

      (and (= domain-id :codex) (vector? payload))
      (case violation-key
        :jobs-for-unregistered-agents
        [(str "job:" (sanitize-fragment (first payload)))
         (str "agent:" (sanitize-fragment (second payload)))]

        :running-jobs-on-idle-agents
        [(str "job:" (sanitize-fragment (first payload)))
         (str "agent:" (sanitize-fragment (second payload)))
         (str "registry-status:" (sanitize-fragment (nth payload 2 nil)))]

        :running-count-mismatches
        [(str "agent:" (sanitize-fragment (first payload)))
         (str "running-count:" (sanitize-fragment (second payload)))
         (str "registry-status:" (sanitize-fragment (nth payload 2 nil)))]

        :orphan-announcements
        [(str "announcement:" (sanitize-fragment (first payload)))
         (str "job:" (sanitize-fragment (second payload)))]

        :announcement-agent-mismatches
        [(str "announcement:" (sanitize-fragment (first payload)))
         (str "job:" (sanitize-fragment (second payload)))
         (str "agent:" (sanitize-fragment (nth payload 2 nil)))
         (str "canonical-agent:" (sanitize-fragment (nth payload 3 nil)))]

        :running-session-mismatches
        [(str "job:" (sanitize-fragment (first payload)))
         (str "agent:" (sanitize-fragment (second payload)))
         (str "job-session:" (sanitize-fragment (nth payload 2 nil)))
         (str "registry-session:" (sanitize-fragment (nth payload 3 nil)))]

        (mapv (fn [x] (str "codex-ref:" (sanitize-fragment x))) payload))

      (and (= domain-id :portfolio) (vector? payload))
      (mapv (fn [x] (str "mission:" (sanitize-fragment x))) payload)

      :else
      [(str "violation:" (name domain-id) "/" (name violation-key) "/"
            (sha1-short (pr-str payload)))])))

(defn hyperedge-id
  [hx-type endpoints]
  (let [sorted-eps (sort endpoints)]
    (str "hx:" hx-type ":" (str/join "|" sorted-eps))))

(defn obligation->hyperedge
  [obligation]
  (let [hx-type "invariant/violation"
        endpoints (vec (distinct (cons (invariant-endpoint obligation)
                                       (payload->entity-endpoints obligation))))
        detected-at (str (Instant/now))
        message (or (:summary obligation) (:label obligation))]
    {:hx/id (hyperedge-id hx-type endpoints)
     :hx/type hx-type
     :hx/endpoints endpoints
     :props {:rule (name (or (:violation-key obligation) :unknown))
             :family (name (or (:family obligation) :unclassified))
             :message message
             :label (:label obligation)
             :detected-at detected-at
             :state "active"
             :active true
             :actionability (name (or (:actionability obligation) :needs-review))
             :domain (name (or (:domain-id obligation) :unknown))
             :obligation-id (:id obligation)
             :dispatchable? (boolean (:dispatchable? obligation))}
     :hx/labels ["invariant-violation"
                 (str "domain/" (name (or (:domain-id obligation) :unknown)))
                 (str "actionability/" (name (or (:actionability obligation) :needs-review)))]}))

(defn aggregate->hyperedges
  [aggregate]
  (mapv obligation->hyperedge (:obligations aggregate)))

(defn activate-hyperedge
  [hyperedge]
  (-> hyperedge
      (assoc-in [:props :state] "active")
      (assoc-in [:props :active] true)
      (update :props dissoc :cleared-at)))

(defn deactivate-hyperedge
  [hyperedge]
  (-> hyperedge
      (assoc-in [:props :state] "cleared")
      (assoc-in [:props :active] false)
      (assoc-in [:props :cleared-at] (str (Instant/now)))))

(defn- http-post-json
  [client url body]
  (let [req (-> (HttpRequest/newBuilder (URI/create url))
                (.POST (HttpRequest$BodyPublishers/ofString (json/encode body)))
                (.header "content-type" "application/json")
                (.header "accept" "application/json")
                (.build))
        resp (.send client req (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode resp)
     :body (json/decode (.body resp) true)}))

(defn post-hyperedge!
  ([hyperedge] (post-hyperedge! hyperedge {}))
  ([hyperedge {:keys [futon1a-url penholder client]
               :or {futon1a-url default-futon1a-url
                    penholder "tester"}}]
   (http-post-json (or client (HttpClient/newHttpClient))
                   (str futon1a-url "/api/alpha/hyperedge")
                   {:penholder penholder
                    :hx/id (:hx/id hyperedge)
                    :hx/type (:hx/type hyperedge)
                    :hx/endpoints (:hx/endpoints hyperedge)
                    :props (:props hyperedge)
                    :hx/labels (:hx/labels hyperedge)})))

(defn emit-hyperedges!
  ([hyperedges] (emit-hyperedges! hyperedges {}))
  ([hyperedges opts]
   (let [results (mapv #(post-hyperedge! % opts) hyperedges)
         ok-count (count (filter #(<= 200 (:status %) 299) results))]
     {:hyperedges hyperedges
      :results results
      :count (count hyperedges)
      :ok-count ok-count})))

(defn emit-aggregate!
  "Emit all obligation-derived hyperedges to futon1a."
  ([aggregate] (emit-aggregate! aggregate {}))
  ([aggregate opts]
   (emit-hyperedges! (mapv activate-hyperedge (aggregate->hyperedges aggregate)) opts)))

(defn reconcile-aggregate!
  "Emit the current live aggregate and mark previously emitted-but-now-cleared
   hyperedges inactive.

   `previous-hyperedges` should be the last active set emitted by the caller."
  ([aggregate] (reconcile-aggregate! aggregate {}))
  ([aggregate {:keys [previous-hyperedges] :as opts}]
   (let [current-hyperedges (mapv activate-hyperedge (aggregate->hyperedges aggregate))
         current-ids (set (map :hx/id current-hyperedges))
         stale-hyperedges (->> previous-hyperedges
                              (filter :hx/id)
                              (remove #(contains? current-ids (:hx/id %)))
                              (mapv deactivate-hyperedge))
         all-hyperedges (vec (concat current-hyperedges stale-hyperedges))
         emitted (emit-hyperedges! all-hyperedges (dissoc opts :previous-hyperedges))]
     (assoc emitted
            :current-hyperedges current-hyperedges
            :stale-hyperedges stale-hyperedges
            :current-count (count current-hyperedges)
            :cleared-count (count stale-hyperedges)))))
