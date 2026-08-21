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

(defn validate
  "Validate shape, ordering, content addresses, classification, and Git pins."
  [manifest]
  (let [units (:units manifest)
        expected-frames (mapv #(str "f" %) (range 18 28))
        apparatus (:apparatus manifest)
        apparatus-body (dissoc apparatus :pin/id)
        apparatus-id (machine/ledger-digest [apparatus-body])
        apparatus-files (for [[_ pin] (:artifacts apparatus)]
                          (merge (select-keys apparatus [:repository :branch :revision]) pin))
        problem-observations (mapv #(validate-git-pin (:problem %)) units)
        apparatus-observations (mapv validate-git-pin apparatus-files)
        findings
        (cond-> []
          (not= 10 (count units)) (conj :countdown-manifest-not-ten-units)
          (not= expected-frames (mapv :frame/id units))
          (conj :countdown-manifest-frame-order-invalid)
          (not= 10 (count (set (map :problem/id units))))
          (conj :countdown-manifest-problem-duplicate)
          (not= 10 (count (set (map :frame/id units))))
          (conj :countdown-manifest-frame-duplicate)
          (some #(not= :non-topology (:classification/value %)) units)
          (conj :countdown-manifest-classification-invalid)
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
     :apparatus-observations apparatus-observations
     :worktree-head-consulted? false}))
