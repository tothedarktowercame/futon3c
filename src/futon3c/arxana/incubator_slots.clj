(ns futon3c.arxana.incubator-slots
  "Session-only form overlays. No store dependency; every act has an explicit actor.
  Editorial acceptance is neither source acceptance nor benefit realization."
  (:require [clojure.string :as str]
            [futon3c.arxana.draft-edition :as draft]))

;; Coordinates in the pinned 96-line capture, not invented form limits/options.
(def fields
  [[:title 2 :prose] [:summary 3 :prose] [:principles 5 :multi 6 8]
   [:stage 9 :single] [:fit-signals 12 :multi 13 18] [:users 20 :single]
   [:outputs 23 :multi 24 31] [:accessibility 32 :single 33 36]
   [:frontier-api 38 :single 39 40] [:model-size 42 :single]
   [:charitable 45 :single 46 47] [:public-benefit 48 :prose]
   [:openness-options 50 :multi 51 57] [:technology 62 :prose]
   [:small 65 :prose] [:intentional 67 :prose] [:tailored 69 :prose]
   [:traction 71 :prose] [:blockers 73 :prose] [:team 75 :prose]
   [:success 78 :prose] [:sustainability 80 :prose] [:grant 82 :prose]
   [:openness 84 :prose] [:novelty 86 :prose] [:links 88 :url]
   [:video 89 :url] [:disclosure 96 :selection-unknown]])
(defn slots [text expected-digest]
  (when-not (= expected-digest (draft/sha text)) (draft/fail :form-digest-mismatch {}))
  (let [lines (str/split-lines text)]
    (when-not (= 96 (count lines)) (draft/fail :form-shape-mismatch {}))
    (mapv (fn [[key line kind lo hi]]
            {:id (name key) :prompt (nth lines (dec line)) :kind kind
             :options (when lo (subvec (vec lines) (dec lo) hi))
             :options-known? (boolean lo) :limit {:status :unknown}
             :source {:digest expected-digest :line line}
             :duration-seconds (when (= key :video) 120)}) fields)))
(def assignments
  [[1 2 [:team :traction]] [3 7 [:tailored :traction]] [8 11 [:tailored :technology]]
   [12 12 [:technology]] [13 14 [:technology :summary :novelty]]
   [15 17 [:public-benefit]] [18 19 [:intentional]] [20 22 [:intentional :public-benefit]]
   [23 23 [:summary :technology]] [24 25 [:small :novelty]] [26 27 [:small]]
   [28 30 [:small :technology]] [31 32 [:tailored :public-benefit]]
   [33 36 [:public-benefit :tailored]] [37 38 [:traction]] [39 41 [:traction :tailored]]
   [42 44 [:success]] [45 45 [:public-benefit :tailored]]
   [46 48 [:sustainability :grant]] [49 51 [:grant :sustainability]]])
(defn placements [segments]
  (vec (for [s segments [lo hi targets] assignments :when (<= lo (:index s) hi) target targets]
         {:id (str "placement/" (:index s) "/" (name target)) :slot (name target)
          :segment (:index s) :span (:span s) :text (:text s) :sources (:sources s)
          :reason "Proposed topical placement; not an accepted answer (Packet 36 §3)."})))
(def budget-challenge
  {:id "challenge/budget-ambiguity" :segment 51 :kind :challenge :status :unresolved
   :reason "Does 'each' mean each person's total or yearly salary? Both readings are possible."
   :readings [{:basis :per-person-total :totals [300000 300000 300000]}
              {:basis :annual-salary :totals [300000 600000 900000]}]})
(defn session [form segments]
  {:status :ok :authority :pinned-external-files :stored? false
   :slots form :segments segments :placements (placements segments) :acts []
   :claims (mapv #(hash-map :id (str "claim/" (:index %)) :segment (:index %) :span (:span %)) segments)
   :relations [] :challenges [budget-challenge]})
(defn act
  "Append a scoped act. Caller supplies identity from command context, never a candidate."
  [s {:keys [actor time kind subject destination reason scope] :as a}]
  (when-not (and (string? actor) (not (str/blank? actor)) (string? time)
                 (#{:accept :reject :move :challenge :warrant :undecide} kind)
                 (= scope :placement)
                 (some #(= subject (:id %)) (:placements s)))
    (draft/fail :invalid-session-act {:act a}))
  (java.time.Instant/parse time)
  (when (and (#{:challenge :warrant} kind) (str/blank? reason)) (draft/fail :reason-required {}))
  (when (and (= :move kind) (not (some #(= destination (:id %)) (:slots s))))
    (draft/fail :unknown-slot {:slot destination}))
  (update s :acts conj (assoc a :id (str "act/" (count (:acts s))))))
(defn current-placements [s]
  (reduce (fn [ps a]
            (mapv (fn [p]
                    (if (= (:subject a) (:id p))
                      (case (:kind a)
                        :move (assoc p :slot (:destination a) :moved-by (:actor a))
                        :accept (assoc p :decision :accepted :actor (:actor a))
                        :reject (assoc p :decision :rejected :actor (:actor a))
                        :undecide (dissoc p :decision :actor)
                        p) p)) ps)) (:placements s) (:acts s)))
(defn relation [s {:keys [id kind from to warrant] :as r}]
  (let [claims (set (map :id (:claims s))) edges (set (map :id (:relations s)))]
    (when-not (and id (not (contains? edges id)) (#{:supports :attacks :undercuts} kind)
                   (claims from) ((if (= kind :undercuts) edges claims) to))
      (draft/fail :invalid-argument-relation {:relation r}))
    (update s :relations conj (assoc r :warrant warrant))))
(defn argument-reading [s claim-id]
  ;; A fixed-point attack labelling; support/evidence is a separate dimension.
  (let [rs (filter :warrant (:relations s)) ids (map :id (:claims s))
        attacks (filter #(= :attacks (:kind %)) rs)
        labels (loop [ls (zipmap ids (repeat :undec))]
                 (let [next (into {} (for [id ids :let [as (map :from (filter #(= id (:to %)) attacks))]]
                                      [id (cond (some #(= :in (ls %)) as) :out
                                                (every? #(= :out (ls %)) as) :in :else :undec)]))]
                   (if (= ls next) ls (recur next))))
        cuts (set (map :to (filter #(and (= :undercuts (:kind %)) (= :in (labels (:from %)))) rs)))
        supports (filter #(and (= :supports (:kind %)) (= claim-id (:to %))
                               (= :in (labels (:from %)))) rs)]
    {:label (get labels claim-id :unassessed)
     :supports (mapv :id (remove #(cuts (:id %)) supports))
     :undercut (mapv :id (filter #(cuts (:id %)) supports))
     :reason "IN means undefeated in this explicit graph, not proven or externally beneficial."}))
(defn reading [s slot]
  (let [ps (filter #(and (= (:id slot) (:slot %)) (not= :rejected (:decision %))) (current-placements s))
        words (count (re-seq #"\S+" (str/join " " (map :text ps))))
        limit (:limit slot) relevant (set (map :id ps))
        challenges (filter #(and (= :challenge (:kind %)) (relevant (:subject %))) (:acts s))]
    {:placement (cond (empty? ps) :empty (every? #(= :accepted (:decision %)) ps) :accepted :else :proposed)
     :words words :limit-status (if (= :known (:status limit))
                                 (if (> words (:words limit)) :over-limit :within-limit) :unknown)
     :evidence (if (some #(seq (:sources %)) ps) :proposed :none)
     :benefit :projected
     :argument (mapv #(argument-reading s (str "claim/" (:segment %))) ps)
     :reasons (vec (concat (when (empty? ps) ["Needs material"])
                           (when (= :unknown (:status limit)) ["Word limit absent from captured form"])
                           (when (= "success" (:id slot)) ["36-month proposal present; 12-month target and measurement baseline need review"])
                           (when (some #(= 51 (:segment %)) ps) [(:reason budget-challenge)])
                           (map :reason challenges)))}))
(defn view [s] (assoc s :slots (mapv #(assoc % :reading (reading s %)) (:slots s))))
(defn read-proposals [config]
  (try
    (let [b (draft/build (draft/load-inputs (:draft config))) v (draft/edition-view b)
          refs (zipmap (map :index (:anchors b)) (:target-refs b))
          segments (mapv #(assoc % :span (refs (:index %)) :sources (vec (concat (:lexical %) (:proposals %)))) (:segments v))]
      (view (session (slots (slurp (:form-path config) :encoding "UTF-8") (:form-digest config)) segments)))
    (catch Exception e {:status :inconclusive :reason (or (:reason (ex-data e)) :unavailable)})))

(defn default-config [] {:form-path "/home/joe/mozilla-incubator.txt" :form-digest "b603227cf59e6a34a8c9c420ba09913574e2e407a4a8dfead06d33fb37fd45b2" :draft (draft/default-config)})
