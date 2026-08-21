(ns futon3c.apm.countdown-manifest
  "Fail-closed validation for the ordered, immutable countdown manifest."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]))

(def sha1-pattern #"[0-9a-f]{40}")

(defn- git [repository & args]
  (apply shell/sh (concat ["git" "-C" repository] args)))

(defn- output [result]
  (when (and (map? result) (zero? (:exit result)))
    (str/trim (:out result))))

(defn- validate-git-pin [{:keys [repository branch revision path blob]}]
  (let [shape? (and (every? #(and (string? %) (not (str/blank? %)))
                            [repository branch revision path blob])
                    (re-matches sha1-pattern revision)
                    (re-matches sha1-pattern blob))
        commit (when shape? (git repository "cat-file" "-e" (str revision "^{commit}")))
        branch-ref (str "refs/heads/" branch)
        branch-result (when (zero? (or (:exit commit) 1))
                        (git repository "rev-parse" "--verify" branch-ref))
        ancestry (when (zero? (or (:exit branch-result) 1))
                   (git repository "merge-base" "--is-ancestor" revision branch-ref))
        observed-result (when (zero? (or (:exit ancestry) 1))
                          (git repository "rev-parse" (str revision ":" path)))
        observed (output observed-result)]
    {:valid? (and shape? (zero? (or (:exit commit) 1))
                  (zero? (or (:exit branch-result) 1))
                  (zero? (or (:exit ancestry) 1)) (= blob observed))
     :observed-blob observed :branch-head (output branch-result)}))

(defn qualify-unit
  "Execute the pinned unit only when the repository checkout is exactly its
   revision. This prevents a stale survey or a mutable HEAD from certifying an
   eligibility baseline for a different Git object."
  [unit]
  (let [{:keys [repository revision path blob]} (:problem unit)
        head (output (git repository "rev-parse" "HEAD"))
        observed-blob (output (git repository "rev-parse" (str revision ":" path)))]
    (if-not (and (= revision head) (= blob observed-blob))
      {:valid? false :finding :countdown-qualification-checkout-mismatch
       :expected-revision revision :observed-revision head
       :expected-blob blob :observed-blob observed-blob}
      (let [result (apply shell/sh ["lake" "env" "lean" path :dir repository])
            lines (str/split-lines (str (:out result) (:err result)))
            observation {:exit (:exit result)
                         :warnings (count (filter #(str/includes? % "warning:") lines))
                         :sorry-warnings (count (filter #(str/includes? % "declaration uses `sorry`") lines))
                         :errors (count (filter #(str/includes? % "error:") lines))}]
        {:valid? (= (:eligibility/baseline unit) observation)
         :observation observation :expected (:eligibility/baseline unit)}))))

(defn validate
  "Validate shape, ordering, content addresses, classification, and Git pins."
  [manifest]
  (let [units (:units manifest)
        one-off? (= :one-off (:manifest/scope manifest))
        expected-frames (mapv #(str "f" %) (range 18 28))
        apparatus (:apparatus manifest)
        apparatus-body (dissoc apparatus :pin/id)
        apparatus-id (machine/ledger-digest [apparatus-body])
        apparatus-files (for [[_ pin] (:artifacts apparatus)]
                          (merge (select-keys apparatus [:repository :branch :revision]) pin))
        problem-observations (mapv #(validate-git-pin (:problem %)) units)
        apparatus-observations (mapv validate-git-pin apparatus-files)
        eligibility-observations (when (= 2 (:manifest/version manifest))
                                   (mapv qualify-unit units))
        findings
        (cond-> []
          (not (if one-off? (= 1 (count units)) (= 10 (count units))))
          (conj (if one-off? :one-off-manifest-not-one-unit
                    :countdown-manifest-not-ten-units))
          (not (if one-off?
                 (= [1] (mapv :ordinal units))
                 (= expected-frames (mapv :frame/id units))))
          (conj :countdown-manifest-frame-order-invalid)
          (not= (count units) (count (set (map :problem/id units))))
          (conj :countdown-manifest-problem-duplicate)
          (not= (count units) (count (set (map :frame/id units))))
          (conj :countdown-manifest-frame-duplicate)
          (some #(not= :non-topology (:classification/value %)) units)
          (conj :countdown-manifest-classification-invalid)
          (and (= 2 (:manifest/version manifest))
               (some (fn [unit]
                       (not (and (= 0 (get-in unit [:eligibility/baseline :exit]))
                                 (pos? (get-in unit [:eligibility/baseline :sorry-warnings] 0))
                                 (= 0 (get-in unit [:eligibility/baseline :errors])))))
                     units))
          (conj :countdown-manifest-eligibility-shape-invalid)
          (and (= 2 (:manifest/version manifest))
               (some (complement :valid?) eligibility-observations))
          (conj :countdown-manifest-eligibility-observation-invalid)
          (some #(or (str/blank? (:classification/evidence %))
                     (not= :operator-reviewed-statement (:classification/source %))) units)
          (conj :countdown-manifest-classification-evidence-missing)
          (not= apparatus-id (:pin/id apparatus))
          (conj :countdown-manifest-apparatus-address-invalid)
          (some #(not= apparatus-id (:apparatus/pin-id %)) units)
          (conj :countdown-manifest-unit-apparatus-mismatch)
          (some (complement :valid?) problem-observations)
          (conj :countdown-manifest-problem-pin-invalid)
          (or (empty? apparatus-observations)
              (some (complement :valid?) apparatus-observations))
          (conj :countdown-manifest-apparatus-pin-invalid)
          (not= (:manifest/id manifest)
                (machine/ledger-digest [(dissoc manifest :manifest/id)]))
          (conj :countdown-manifest-content-address-invalid))]
    {:valid? (empty? findings)
     :findings findings
     :manifest/id (:manifest/id manifest)
     :problem-observations problem-observations
     :eligibility-observations eligibility-observations
     :apparatus-observations apparatus-observations
     :worktree-head-consulted? false}))
