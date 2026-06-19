(ns futon3c.logic.invariant-queue-freshness
  "Family `invariant-queue-freshness` — the Invariant Queue must be invariantly
   up to date (stop-the-line, Joe 2026-06-01).

   The motivating failure: the Arxana Browser → Candidate Queue rendered ~month-old
   priority ranks (negative `-30…` rows from a May-2 generated JSON) as if current,
   and NOTHING flagged that the derived artifacts had drifted from their
   source-of-truth. The queue had no invariant enforcing its own freshness — so it
   was silently, confidently stale. (It even fooled an audit into a wrong count.)

   This is the obsolescence-recognition / subsumption-witness shape (sibling of
   `futon3c.logic.archaeology`): a DERIVED artifact A is STALE relative to its
   SOURCE P when P changed after A was generated, i.e. mtime(P) > mtime(A) (or,
   stronger, when A's recorded source-hash != current hash of P). A stale derived
   artifact feeding the queue means the queue is not up-to-date ⇒ :violation.

   Two layers (this ns ships layer 1; layer 2 is noted for a follow-up):
     1. derived-not-stale-vs-source  (BUILT here): every generated artifact that
        feeds the queue is at least as new as the source-of-truth inventory.
     2. no-expired-targets-masquerading-as-current  (follow-up): priority runs
        whose target date is past with no closure are stale pressure, not live
        priority; the view must not silently top-sort them.

   Reads files directly (no cross-repo ns dependency), returns the standard probe
   shape `{:outcome :ok | :violation :detail <map>}` for the probe family registry.

   Mission: stop-the-line 2026-06-01 (invariant-queue audit follow-on)."
  (:require [clojure.java.io :as io]))

(def ^:private home (System/getProperty "user.home"))

(def source-of-truth
  "The canonical inventory; all queue-feeding artifacts derive from this."
  (str home "/code/futon3c/docs/structural-law-inventory.sexp"))

(def derived-artifacts
  "Generated artifacts that feed the Arxana Browser → Candidate Queue. Each must
   be at least as new as `source-of-truth`, else the queue renders stale data."
  [{:id :priority-queue
    :path (str home "/code/futon5a/data/stack-stereolithography-priority-queue.json")
    :role "priority ranks + I0 watchlist rows (the negative-priority block)"}
   {:id :invariant-model
    :path (str home "/code/futon4/futon-stack-invariant-model.edn")
    :role "Arxana hypergraph view of the inventory (Live Invariants)"}])

(defn- mtime [path]
  (let [f (io/file path)]
    (when (.exists f) (.lastModified f))))

(defn staleness
  "Return a vector of stale-artifact maps: those older than the source-of-truth
   (or missing). Empty vector ⇒ the queue's derived inputs are all fresh."
  ([] (staleness source-of-truth derived-artifacts))
  ([src arts]
   (let [src-mt (mtime src)]
     (if (nil? src-mt)
       ;; cannot judge freshness without the source; report it explicitly
       [{:id :source-of-truth :path src :reason :source-missing}]
       (reduce
         (fn [acc {:keys [id path role]}]
           (let [a-mt (mtime path)]
             (cond
               (nil? a-mt)
               (conj acc {:id id :path path :role role :reason :derived-missing})
               (> src-mt a-mt)
               (conj acc {:id id :path path :role role :reason :older-than-source
                          :source-mtime src-mt :artifact-mtime a-mt
                          :stale-by-ms (- src-mt a-mt)})
               :else acc)))
         []
         arts)))))

(defn check
  "Probe-family entry for `invariant-queue-freshness`. :ok when every queue-feeding
   derived artifact is at least as new as the inventory; :violation lists the stale
   ones. The stop-the-line condition: a stale derived artifact means the Candidate
   Queue is showing month-old data as if current."
  ([] (check source-of-truth derived-artifacts))
  ([src arts]
   (let [stale (staleness src arts)]
     (if (empty? stale)
       {:outcome :ok
        :detail {:note "all queue-feeding derived artifacts are at least as new as the inventory"
                 :source src}}
       {:outcome :violation
        :detail {:stale stale
                 :source src
                 :note "Invariant Queue is NOT up to date: derived artifacts drifted from the source-of-truth inventory; the Candidate Queue is rendering stale data. Regenerate (or recompute live) before trusting queue priorities."}}))))
