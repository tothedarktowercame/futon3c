(ns futon3c.apm.memory-snapshot
  "Atomic admission and verification of reviewed memories for one frame.

  This boundary does not perform attachment review. It admits only review
  results already visible in the substrate and supplied by an evidence reader."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.evidence.futon1b-backend :as f1b]
            [futon3c.evidence.store :as estore]
            [futon3c.substrate.client :as substrate])
  (:import [java.nio.file Files Path StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(defn- nonblank? [x]
  (and (string? x) (not (str/blank? x))))

(defn- reviewed? [x]
  (= "reviewed" (some-> x name)))

(defn validate-candidate
  [{:keys [memory-id depositor reviewer review-evidence-id
           attachment-status pattern-ids]}]
  (cond
    (not (every? nonblank? [memory-id depositor reviewer review-evidence-id]))
    {:ok false :finding :snapshot-candidate-identity-missing}
    (= depositor reviewer)
    {:ok false :finding :snapshot-reviewer-is-depositor}
    (not (reviewed? attachment-status))
    {:ok false :finding :snapshot-attachment-not-reviewed}
    (not (and (vector? pattern-ids) (seq pattern-ids)
              (every? nonblank? pattern-ids)))
    {:ok false :finding :snapshot-patterns-missing}
    :else {:ok true}))

(defn snapshot-body [frame-id problem-id candidates]
  (let [ordered (vec (sort-by :memory-id candidates))]
    {:snapshot/version 1
     :snapshot/frame-id frame-id
     :snapshot/problem-id problem-id
     :snapshot/review-policy :persisted-independent-review
     :snapshot/memories ordered}))

(defn candidate-visible?
  "Freshly verify that CANDIDATE describes the current reviewed attachment and
  its independently authored persisted review evidence."
  ([candidate]
   (let [backend (f1b/make-futon1b-backend (substrate/configured-url))]
     (candidate-visible? candidate substrate/hyperedges-by-end
                         #(estore/get-entry* backend %))))
  ([{:keys [memory-id depositor reviewer review-evidence-id
            attachment-status pattern-ids]}
    fetch-hyperedges fetch-entry]
   (let [edge (->> (fetch-hyperedges memory-id)
                   (filter #(= :memory/assert (:hx/type %)))
                   (filter #(= :current (get-in % [:hx/props :state])))
                   first)
         memory (fetch-entry memory-id)
         review (fetch-entry review-evidence-id)]
     (and edge memory review
          (reviewed? attachment-status)
          (reviewed? (get-in edge [:hx/props :attachment-status]))
          (= (set pattern-ids)
             (set (get-in edge [:hx/props :roles :patterns])))
          (= review-evidence-id
             (get-in edge [:hx/props :review :evidence-id]))
          (= depositor (:evidence/author memory))
          (= reviewer (:evidence/author review))
          (not= depositor reviewer)
          (= memory-id (get-in review [:evidence/subject :ref/id]))))))

(defn publish!
  "Validate CANDIDATES, publish one immutable EDN snapshot atomically, and
  verify it by a fresh read. Existing identical content is an idempotent replay;
  existing different content fails closed."
  [{:keys [frame-id problem-id candidates path evidence-visible?]}]
  (let [validations (mapv validate-candidate candidates)
        invisible (when (fn? evidence-visible?)
                    (->> candidates
                         (remove evidence-visible?)
                         (mapv :memory-id)))
        body (snapshot-body frame-id problem-id candidates)
        digest (machine/ledger-digest [body])
        snapshot (assoc body :snapshot/id digest :snapshot/digest digest)
        target (Path/of (str path) (make-array String 0))]
    (cond
      (or (not (nonblank? frame-id)) (not (nonblank? problem-id))
          (empty? candidates))
      {:ok false :error/code :memory-snapshot-input-invalid}
      (some (complement :ok) validations)
      {:ok false :error/code :memory-snapshot-candidate-invalid
       :findings (mapv :finding (remove :ok validations))}
      (seq invisible)
      {:ok false :error/code :memory-snapshot-review-not-visible
       :memory-ids invisible}
      (Files/exists target (make-array java.nio.file.LinkOption 0))
      (let [existing (edn/read-string (slurp (.toFile target)))]
        (if (= snapshot existing)
          {:ok true :snapshot snapshot :path (str target) :idempotent? true}
          {:ok false :error/code :memory-snapshot-existing-mismatch}))
      :else
      (let [parent (or (.getParent target)
                       (Path/of "." (make-array String 0)))
            _ (Files/createDirectories parent (make-array FileAttribute 0))
            tmp (Files/createTempFile parent ".memory-snapshot-"
                                      ".edn" (make-array FileAttribute 0))]
        (try
          (spit (.toFile tmp) (str (pr-str snapshot) "\n"))
          (Files/move tmp target
                      (into-array StandardCopyOption
                                  [StandardCopyOption/ATOMIC_MOVE]))
          (let [observed (edn/read-string (slurp (.toFile target)))]
            (if (= snapshot observed)
              {:ok true :snapshot snapshot :path (str target)
               :idempotent? false}
              {:ok false :error/code :memory-snapshot-postcondition-failed}))
          (finally
            (Files/deleteIfExists tmp)))))))

(defn verify-student-access
  [{:keys [path expected frame-id problem-id accessible-memory-ids]}]
  (try
    (let [observed (edn/read-string (slurp (str path)))
          body (dissoc observed :snapshot/id :snapshot/digest)
          digest (machine/ledger-digest [body])
          expected-ids (set (map :memory-id (:snapshot/memories observed)))
          findings (cond-> []
                     (not= frame-id (:snapshot/frame-id observed))
                     (conj :snapshot-frame-mismatch)
                     (not= problem-id (:snapshot/problem-id observed))
                     (conj :snapshot-problem-mismatch)
                     (not= digest (:snapshot/digest observed))
                     (conj :snapshot-content-mismatch)
                     (not= expected (:snapshot/digest observed))
                     (conj :snapshot-expected-digest-mismatch)
                     (not= expected-ids (set accessible-memory-ids))
                     (conj :student-access-set-mismatch))]
      (if (seq findings)
        {:ok false :error/code :student-memory-access-invalid :findings findings}
        {:ok true :snapshot observed :accessible-memory-ids expected-ids}))
    (catch Throwable t
      {:ok false :error/code :student-memory-snapshot-unreadable
       :finding {:message (.getMessage t)}})))
