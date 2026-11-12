(ns futon3c.mission-ids
  "Canonical mission entity-id construction shared by the two mission/doc
  writers — the watcher's `ingest-mission-doc!` (file_ingest.clj) and the
  mission-scope ingester (mission_scope_ingest.clj) — so one mission
  document yields ONE entity id no matter which writer emits it.

  Identity rule (Joe 2026-11-12: 'let's sort out the duplicates … we
  generally don't need duplicates of anything'; plan
  holes/labs/M-repl-circus/SUBSTRATE-dedup-plan-2026-11-12.md):

  - The repo label is the WATCHER label from futon3c.watcher.roots
    (`roots.clj`: 'a checkout is identified by its watcher label … not its
    directory basename'). The scope ingester previously derived
    basename+\"-d\", which forked e.g. futon4-d/… off futon4-elisp-d/….
  - The mission component is the lowercase slug below. The watcher
    previously kept header case (futon2-d/mission/G-over-cascades vs
    futon2-d/mission/g-over-cascades), splitting 28 external-ids by case.

  Writers re-emit their entities; nobody hand-edits ids."
  (:require [clojure.string :as str])
  (:import (java.security MessageDigest)))

(defn- sha1 [s]
  (let [digest (.digest (MessageDigest/getInstance "SHA-1") (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn slug
  "Lowercase slug: non [a-z0-9] runs collapse to '-'; empty result falls
   back to a sha1 prefix so the id is still stable and non-blank.
   Byte-identical to the slug the mission-scope ingester has always used,
   so all existing lowercase keyed ids keep their names."
  [s]
  (let [base (-> (str s)
                 str/lower-case
                 (str/replace #"[^a-z0-9]+" "-")
                 (str/replace #"^-+|-+$" ""))]
    (or (not-empty base) (subs (sha1 s) 0 12))))

(defn watcher-label-for-path
  "Label of the watch root covering PATH, chosen by LONGEST path-prefix
   match over ROOTS (a seq of {:path … :label …}), or nil when no root
   covers PATH. Longest-prefix so a nested installation root wins over an
   enclosing one."
  [roots path]
  (let [p (str path)]
    (->> roots
         (filter (fn [{root-path :path}]
                   (let [r (str root-path)]
                     (or (= p r) (str/starts-with? p (str r "/"))))))
         (sort-by (comp count str :path) #(compare %2 %1))
         first
         :label)))

(defn mission-doc-id
  "Canonical mission/doc entity id: \"<watcher-label>/mission/<slug>\".
   MISSION may be a stem (\"M-formal-arxana\") or a bare id
   (\"formal-arxana\"); only the M- prefix is stripped (E-/C- stems are
   excursions/campaigns and never become mission/doc entities)."
  [label mission]
  (str label "/mission/"
       (slug (str/replace-first (str mission) #"^M-" ""))))
