(ns futon3c.wm.operator-bulletin
  "E-wm-operator-lane — the morning bulletin.

   The pull surface Joe consults on his own clock. Projects a set of WM items
   (classified by `futon3c.wm.operator-lane`) into the two surfaced lanes:

   - :nag   — needs Joe now (the narrow, earned interrupt).
   - :brief — FYI when he's ready.

   :silent items were discharged autonomously overnight; the bulletin COUNTS
   them but never lists them — so it never hides what it dropped (no silent caps).

   The bulletin is also the acknowledgement boundary for novelty-flows-down:
   once an item has appeared in a pulled bulletin it becomes 'acknowledged',
   which is what later makes it eligible to escalate brief→nag (INV-4).
   `newly-acknowledged` computes that set as a pure function; where the
   acknowledged-set persists is wired separately (with the forward-model adapter).

   Item map: the classifier's descriptive attributes plus display fields
   (:id, optional :title, :target, :why)."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.wm.operator-lane :as lane]))

(defn classify-items
  "Tag each item with its lane via the classifier (pure)."
  [items]
  ;; WM-emitted needs-you items arrive pre-laned (:lane "nag"); respect it
  ;; (keywordised). Forward-model items have no :lane -> classify them.
  (mapv (fn [it] (if (:lane it)
                   (update it :lane keyword)
                   (assoc it :lane (lane/classify-item it))))
        items))

(defn build-bulletin
  "Project ITEMS into a morning bulletin:
     {:date d :nag [...] :brief [...] :silent-count n :total t}
   :nag and :brief preserve input order; :silent items are counted, not listed."
  [items & {:keys [date]}]
  (let [classified (classify-items items)
        by-lane    (group-by :lane classified)]
    {:date         date
     :nag          (vec (:nag by-lane))
     :brief        (vec (:brief by-lane))
     :silent-count (count (:silent by-lane))
     :total        (count classified)}))

(defn surfaced-ids
  "The ids the bulletin actively surfaces to Joe (nag + brief)."
  [bulletin]
  (set (keep :id (concat (:nag bulletin) (:brief bulletin)))))

(defn newly-acknowledged
  "Ids this bulletin surfaces that PRIOR-ACK (a set/seq) didn't already contain —
   i.e. what becomes acknowledged once Joe pulls this bulletin."
  [bulletin prior-ack]
  (set/difference (surfaced-ids bulletin) (set prior-ack)))

(defn- item-line [it]
  (str "- " (or (:title it) (:id it) (:target it) "(unnamed)")
       (when-let [w (:why it)] (str " — " w))))

(defn render-bulletin
  "Render BULLETIN as a readable 'Awaiting Joe' markdown surface."
  [{:keys [date nag brief silent-count total]}]
  (str "# War Machine — Awaiting Joe" (when date (str " (" date ")")) "\n\n"
       "## Nag — needs you now (" (count nag) ")\n"
       (if (seq nag) (str/join "\n" (map item-line nag)) "_(none)_") "\n\n"
       "## Brief — FYI when you're ready (" (count brief) ")\n"
       (if (seq brief) (str/join "\n" (map item-line brief)) "_(none)_") "\n\n"
       "_" silent-count " of " total " items discharged silently overnight._\n"))
