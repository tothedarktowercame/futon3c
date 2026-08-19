#!/usr/bin/env bb
;; Generate a frame registration from its predecessor by FIELD DIFF, with a
;; guard that refuses stale prose.
;;
;; WHY THIS EXISTS: D50 and D56. Every registration defect in this series has
;; the same shape -- the structured fields were correct and the PROSE was the
;; previous frame's. Six repaired on 2026-08-19 by hand; f13-guide found two
;; more at its orientation ack, one of which (:analyst-survives-two-frames) was
;; UNADJUDICABLE IN PRINCIPLE for the frame it shipped in.
;;
;; A copy-and-edit cannot be made safe by being careful; four re-readings by the
;; author missed what a fresh reader caught in one pass. So: transform the
;; fields, then REFUSE to emit if any forbidden token survives anywhere in the
;; output. The guard is the point, not the transform.
;;
;;   bb gen-registration.bb <from.edn> <to.edn> <edn-map-of-overrides> <forbidden-vec>
(require '[clojure.edn :as edn] '[clojure.pprint :as pp] '[clojure.string :as str])

(let [[from to overrides-s forbidden-s] *command-line-args*
      base      (edn/read-string (slurp from))
      overrides (edn/read-string overrides-s)
      forbidden (edn/read-string forbidden-s)
      merged    (reduce-kv (fn [m k v] (assoc m k v)) base overrides)
      out       (with-out-str (pp/pprint merged))
      hits      (for [f forbidden
                      :let [n (count (re-seq (re-pattern (java.util.regex.Pattern/quote (str f))) out))]
                      :when (pos? n)]
                  [f n])]
  (when (seq hits)
    (binding [*out* *err*]
      (println "REFUSING TO EMIT — stale tokens survive the field transform:")
      (doseq [[f n] hits] (println (format "  %-40s %d occurrence(s)" f n)))
      (println "\nEach one is prose that the field diff could not reach. Rewrite it")
      (println "deliberately in the overrides map, or the frame ships describing")
      (println "its predecessor. This is exactly how D50 and D56 shipped."))
    (System/exit 3))
  (spit to out)
  (println "wrote" to (str "(" (count (str/split-lines out)) " lines, "
                           (count (keys merged)) " keys)")))
