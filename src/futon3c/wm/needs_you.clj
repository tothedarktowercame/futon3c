(ns futon3c.wm.needs-you
  "Emitter for WM operator-dependent actions.

   The operator bulletin reads `data/wm/needs-you.edn` as a vector of items.
   The pilot loop rewrites the whole vector each guarded cycle; removing an
   item from the vector clears it from the bulletin."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [futon3c.wm.guardrails :as guardrails]))

(def needs-you-path "data/wm/needs-you.edn")

(def ^:dynamic *needs-you-path*
  needs-you-path)

(def default-top-k 10)

(defn- now-iso []
  (str (java.time.Instant/now)))

(defn- kw-name [x]
  (cond
    (keyword? x) (name x)
    (nil? x) "unknown"
    :else (str x)))

(defn- stable-token [x]
  (-> (kw-name x)
      (str/replace #"[^A-Za-z0-9._-]+" "-")
      (str/replace #"^-+|-+$" "")
      (not-empty)
      (or "unknown")))

(defn- action-class [dT-entry]
  (some-> (get-in dT-entry [:action :type]) keyword))

(defn- action-target [dT-entry]
  (or (get-in dT-entry [:action :target])
      (get-in dT-entry [:action :path])
      (get-in dT-entry [:action :target-file])
      (get-in dT-entry [:action :mission-path])
      "unknown"))

(defn- action-path [dT-entry]
  (or (get-in dT-entry [:action :path])
      (get-in dT-entry [:action :target-file])
      (get-in dT-entry [:action :mission-path])
      (let [target (action-target dT-entry)]
        (when (and (string? target)
                   (or (str/includes? target "/")
                       (str/ends-with? target ".md")))
          target))))

(defn- action-repo [dT-entry path]
  (or (get-in dT-entry [:action :repo])
      (when (string? path)
        (first (str/split path #"/")))
      (when-let [target (action-target dT-entry)]
        (when (string? target)
          (first (str/split target #"/"))))))

(defn- target-area [target]
  (let [s (kw-name target)]
    (or (second (re-find #"/([^/]+)$" s)) s)))

(defn- unblock-action [class target path]
  (case class
    :open-mission
    (str "Open " (or path target)
         ", confirm scope, greenlight (or decline).")

    :advance-mission
    (str "Advance " (or path target)
         ": work its open holes, or confirm close-readiness (close is yours).")

    :learn-action-class
    (str "Feed the WM an input source for " (target-area target)
         " (see M-war-machine-input-sources).")

    (str "Review " (or path target)
         " and either clear the operator dependency or mark it out of scope.")))

(defn- why-blocked [class target]
  (case class
    (:open-mission :advance-mission)
    (str "WM ranked " target
         ", but guardrails require operator confirmation unless it is a bounded advancement of an open mission with open holes.")

    :learn-action-class
    (str "WM wants to learn action class " target
         ", which is a bootstrap/input-source decision and needs operator framing.")

    (str "WM ranked " target
         ", but guardrails classify the action as operator-dependent.")))

(defn- pattern-label [x]
  (if (keyword? x) (subs (str x) 1) (str x)))

(defn- pattern-warrant [dT-entry]
  (or (:pattern-warrant dT-entry)
      (:guardrails/pattern-warrant dT-entry)
      (get-in dT-entry [:action :pattern-warrant])
      (guardrails/nag-warrant (:action dT-entry) {})))

(defn sorry-joe-line
  [item]
  (when-let [{:keys [pattern-id gap]} (:pattern-warrant item)]
    (str "Sorry Joe, because of " (pattern-label pattern-id) ": " gap)))

(defn action->needs-you-item
  "Build one needs-you item from a stepped-past dT entry."
  [dT-entry run-id]
  (let [class (action-class dT-entry)
        target (action-target dT-entry)
        path (action-path dT-entry)
        repo (action-repo dT-entry path)
        g-total (double (or (:g-total dT-entry) 0.0))
        warrant (pattern-warrant dT-entry)]
    (cond-> {:id (str "wm-needs-" (stable-token class) "-" (stable-token target))
             :title (str "WM needs Joe: " (kw-name class) " " (target-area target))
             :why (why-blocked class target)
             :unblock-action (or (:unblock warrant) (unblock-action class target path))
             :lane "nag"
             :source "wm-needs-you"
             :target target
             :path path
             ;; :g-total is raw expected free energy (LOWER = more important). The
             ;; bulletin NAG lane sorts :salience DESCENDING (higher = more salient,
             ;; matching the positive mission saliences), so display salience is the
             ;; negated EFE: most-important action (most-negative g-total) => highest.
             :salience (- g-total)
             :repo repo
             :wm-action-class class
             :g-total g-total
             :emitted-at (now-iso)
             :run-id run-id}
      warrant (assoc :pattern-warrant warrant))))

(defn- dedupe-last-wins [items]
  (->> items
       (filter map?)
       (reduce (fn [m item] (assoc m (:id item) item)) {})
       vals
       vec))

(defn- importance
  "Capping key: raw EFE :g-total (LOWER = more important), so the kept top-K are
   the most-important. Distinct from display :salience (= negated EFE)."
  [item]
  (double (or (:g-total item) 0.0)))

(defn- overflow-item [dropped-count run-id]
  {:id "wm-needs-overflow"
   :title (str dropped-count " more WM needs-you items")
   :why "WM stepped past more operator-dependent actions than the needs-you cap can show."
   :unblock-action "Review the WM ranked-actions or raise the needs-you cap for this run."
   :lane "nag"
   :source "wm-needs-you"
   :target "wm-needs-you-overflow"
   :path needs-you-path
   ;; lowest display salience => the advisory sorts LAST in the NAG lane
   :salience (- Double/MAX_VALUE)
   :repo "futon3c"
   :wm-action-class :needs-you-overflow
   :g-total Double/MAX_VALUE
   :emitted-at (now-iso)
   :run-id run-id})

(defn- cap-items [items top-k]
  (let [top-k (long (or top-k default-top-k))
        sorted (->> items (sort-by importance) vec)
        n (count sorted)]
    (cond
      (or (not (pos? top-k)) (<= n top-k)) sorted
      (= top-k 1) [(overflow-item n (:run-id (first sorted)))]
      :else (let [kept (subvec sorted 0 (dec top-k))
                  dropped-count (- n (count kept))]
              (conj kept (overflow-item dropped-count (:run-id (first sorted))))))))

(defn emit-needs-you!
  "Write ITEMS as the current needs-you vector. Last item wins by :id, then the
   vector is capped by salience with an explicit overflow advisory item."
  ([items] (emit-needs-you! items {}))
  ([items {:keys [path top-k]
           :or {top-k default-top-k}}]
   (let [path (or path *needs-you-path*)
         out (-> items dedupe-last-wins (cap-items top-k))
         file (io/file path)]
     (io/make-parents file)
     (spit file (with-out-str (pprint/pprint out)))
     {:path path
      :emitted-count (count out)
      :input-count (count items)
      :deduped-count (count (dedupe-last-wins items))
      :capped? (> (count (dedupe-last-wins items)) (long top-k))
      :items out})))
