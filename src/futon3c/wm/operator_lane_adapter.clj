(ns futon3c.wm.operator-lane-adapter
  "Forward-model EDN adapter for E-wm-operator-lane.

   Reads the two descriptive streams from the futon forward model and emits
   item maps ready for futon3c.wm.operator-lane/classify-item and
   futon3c.wm.operator-bulletin/build-bulletin. The derived booleans are
   current-state/descriptive only; no predictive importance signal is read."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def default-code-root "/home/joe/code")

;; Mission docs live in different sub-trees per repo (futon7/holes/,
;; futon3c/holes/missions/, futon5a/holes/excursions/, …). :name carries the
;; filename (with .md); :repo the repo. We resolve the real repo-relative path
;; by probing the known mission homes — so the UI opens the correct file rather
;; than a hardcoded repo (claude-1, 2026-06-05).
(def mission-subdirs ["holes/missions" "holes" "holes/excursions" "holes/labs"])

(def default-semilattice-path
  "/home/joe/code/futon7/holes/M-futon-forward-model.semilattice.edn")

(def default-mint-path
  "/home/joe/code/futon7/holes/M-futon-forward-model.mint.edn")

(def framing-blocked-tokens
  #{"identify" "head" "proposal" "draft"})

(defn- read-edn-file [path]
  (edn/read-string (slurp path)))

(defn- top-quartile-count [n]
  (if (pos? n)
    (long (Math/ceil (/ n 4.0)))
    0))

(defn- c-joint-score [mission]
  (double (or (:c-joint mission) 0.0)))

(defn top-quartile-c-joint-names
  "Return the mission ids in the top quartile of the backlog by :c-joint."
  [backlog]
  (->> backlog
       (sort-by (comp - c-joint-score))
       (take (top-quartile-count (count backlog)))
       (map :name)
       set))

(defn- leading-token [s]
  (some->> s str (re-find #"[A-Za-z]+") str/lower-case))

(defn framing-blocked-declared?
  "True when :declared starts in a phase that requires operator framing."
  [declared]
  (contains? framing-blocked-tokens (leading-token declared)))

(defn resolve-mission-path
  "Repo-relative path of mission NAME within REPO under CODE-ROOT, or nil.
   Probes the known mission sub-trees; first existing file wins."
  [code-root repo name]
  (when (and repo name)
    (some (fn [sub]
            (let [rel (str repo "/" sub "/" name)]
              (when (.exists (io/file code-root rel)) rel)))
          mission-subdirs)))

(defn- mission-why [{:keys [days-since declared]} futon-important? framing-blocked?]
  (str/join ", "
            (cond-> []
              framing-blocked? (conj (str "awaiting framing (" (or (leading-token declared) "unknown") ")"))
              (and days-since (> days-since 30)) (conj (str "stale " days-since "d"))
              futon-important? (conj "central"))))

(defn- mission->item [code-root important-names {:keys [name c-joint days-since declared repo] :as mission}]
  (let [futon-important?    (contains? important-names name)
        framing-blocked?    (framing-blocked-declared? declared)
        operator-dependent? framing-blocked?
        risk-mode?          (boolean (and futon-important? days-since (> days-since 30)))]
    (assoc mission
           :id name
           :target name
           :title name
           :salience c-joint
           :source :mission
           :repo repo
           :path (resolve-mission-path code-root repo name)
           :futon-important? futon-important?
           :in-joes-model? futon-important?
           :risk-mode? risk-mode?
           :framing-blocked? framing-blocked?
           :operator-dependent? operator-dependent?
           :why (mission-why mission futon-important? framing-blocked?))))

(def operator-dependent-discharge-pattern
  #"(?i)(sent|issue|send|deliver)")

(defn operator-dependent-discharge? [discharge]
  (boolean (and discharge (re-find operator-dependent-discharge-pattern discharge))))

(defn- business-risk-mode? [{:keys [blocked? p]}]
  (boolean (or blocked? (= p :unsampled))))

(defn- business-title [{:keys [note discharge sorry]}]
  (or note discharge (some-> sorry name)))

(defn- business-why [{:keys [blocked? p sorry]}]
  (str/join ", "
            (cond-> []
              (= p :unsampled) (conj "unpriced crux, unsampled")
              blocked? (conj (str "blocked on " (name sorry)))
              (and (not= p :unsampled) (not blocked?)) (conj "business-salient"))))

(defn- business->item [{:keys [sorry expected-lift discharge] :as item}]
  (assoc item
         :id sorry
         :target sorry
         :title (business-title item)
         :salience expected-lift
         :source :business-sorry
         :repo nil
         :path nil
         :futon-important? true
         :in-joes-model? true
         :risk-mode? (business-risk-mode? item)
         :framing-blocked? false
         :operator-dependent? (operator-dependent-discharge? discharge)
         :why (business-why item)))

(defn mission-items
  ([semilattice] (mission-items semilattice default-code-root))
  ([semilattice code-root]
   (let [backlog (:backlog semilattice)
         important-names (top-quartile-c-joint-names backlog)]
     (mapv (partial mission->item code-root important-names) backlog))))

(defn business-items [mint]
  (mapv business->item
        (concat (:priced-sorries mint) (:unpriced-sorries mint))))

(defn forward-model-items
  "Read semilattice + mint EDN streams and return classifier-ready item maps."
  ([]
   (forward-model-items {:semilattice-path default-semilattice-path
                         :mint-path default-mint-path}))
  ([{:keys [semilattice-path mint-path code-root]
     :or {semilattice-path default-semilattice-path
          mint-path default-mint-path
          code-root default-code-root}}]
   (let [semilattice (read-edn-file semilattice-path)
         mint        (read-edn-file mint-path)]
     (vec (concat (mission-items semilattice code-root)
                  (business-items mint))))))
