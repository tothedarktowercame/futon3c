(ns futon3c.diagramprover.fixtures.site-sample)
(defn reader-fn [m] (get m :f/declared-and-present))
;; mentions :f/present-not-declared and :cycle/environment-revision
