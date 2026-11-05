(ns futon3c.file-change.demand-projector
  "Pure projection from bounded file-change hints to authority reads.

  Hints select when and which authority to ask; they never answer currency.
  Giraffe recomputes its complete input. Test-registry checks require an
  explicit named demand, so this projector cannot turn a file save into a
  registry-wide warrant sweep."
  (:require [futon2.aif.mission-registry :as mission-registry]))

(def schema :file-change/demand-projector-v1)

(defn empty-state [debounce-ms max-wait-ms]
  {:pre [(pos-int? debounce-ms) (pos-int? max-wait-ms)
         (<= debounce-ms max-wait-ms)]}
  {:schema schema :debounce-ms debounce-ms :max-wait-ms max-wait-ms
   :giraffe-pending nil})

(defn- task-definition? [path]
  (some? (mission-registry/task-document-kind path)))

(defn- level-jump? [entry] (= :level-jump (:operation entry)))

(defn- relevant-paths [entries]
  (->> entries (remove level-jump?) (map :path) (filter string?) distinct sort vec))

(defn project
  "Absorb ENTRIES at NOW-MS and return [state immediate-demands].

  WARRANT-DEMANDS are explicit caller demands of shape
  {:entry-id :repo-root :paths}. Only named demands intersecting changed paths
  are projected; a level jump may recheck named demands but never discovers or
  enumerates warrants. Giraffe demand is retained until `due-demands`."
  [state entries warrant-demands now-ms]
  {:pre [(vector? entries) (vector? warrant-demands)
         (integer? now-ms) (not (neg? now-ms))]}
  (let [paths (relevant-paths entries)
        path-set (set paths)
        jump? (boolean (some level-jump? entries))
        task-paths (vec (filter task-definition? paths))
        giraffe-trigger? (or jump? (seq task-paths))
        pending (:giraffe-pending state)
        jump-basis (some->> entries (filter level-jump?) first :basis)
        state (if giraffe-trigger?
                (assoc state :giraffe-pending
                       {:first-at-ms (or (:first-at-ms pending) now-ms)
                        :last-at-ms now-ms
                        :changed-paths (vec (sort (into (set (:changed-paths pending))
                                                       task-paths)))
                        :level-jump? (or jump? (:level-jump? pending))
                        :trigger-basis (or jump-basis (:trigger-basis pending))})
                state)
        currency
        (->> warrant-demands
             (keep (fn [{:keys [entry-id repo-root paths] :as demand}]
                     (let [matched (vec (sort (filter path-set paths)))]
                       (when (and (string? entry-id) (not-empty entry-id)
                                  (string? repo-root) (not-empty repo-root)
                                  (or jump? (seq matched)))
                         {:demand/type :test-registry/currency
                          :entry-id entry-id :repo-root repo-root
                          :changed-paths (if jump? [] matched)
                          :basis (if jump? (or jump-basis :walk-level-jump)
                                     :named-path-intersection)
                          :demand/id (:demand/id demand)}))))
             vec)]
    [state currency]))

(defn project-rebuild
  "A completed consumer rebuild is itself a full-authority trigger. The level
  jump entry may sit behind the rebuilt cursor, so composition must not depend
  on reading that entry afterwards."
  [state rebuild-receipt warrant-demands now-ms]
  {:pre [(= :consumer-rebuilt (:outcome rebuild-receipt))]}
  (project state
           [{:repo "rebuild" :worktree "rebuild" :path "."
             :content-id (str "consumer-rebuild:" (:through-seq rebuild-receipt))
             :operation :level-jump :basis :consumer-rebuild
             :seq (:through-seq rebuild-receipt)}]
           warrant-demands now-ms))

(defn due-demands
  "Emit and clear a pending full-Giraffe recomputation once debounce or the
  maximum wait expires. Under continuous change the maximum wait wins."
  [state now-ms]
  (if-let [{:keys [first-at-ms last-at-ms changed-paths level-jump? trigger-basis]}
           (:giraffe-pending state)]
    (let [due-at (min (+ last-at-ms (:debounce-ms state))
                      (+ first-at-ms (:max-wait-ms state)))]
      (if (< now-ms due-at)
        [state []]
        [(assoc state :giraffe-pending nil)
         [{:demand/type :giraffe/full-input-recompute
           :changed-paths changed-paths
           :level-jump? (boolean level-jump?)
           :first-at-ms first-at-ms :last-at-ms last-at-ms
           :due-at-ms due-at
           :trigger-basis trigger-basis
           :basis :recompute-from-giraffe-authority}]]))
    [state []]))
