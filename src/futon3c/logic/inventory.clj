(ns futon3c.logic.inventory
  "Loader and query layer for the structural-law inventory sexp.

   Parses structural-law-inventory.sexp into Clojure data, loads entries
   into core.logic pldb relations, and provides query helpers for the
   AIF head's :structural-law-compliance observation channel."
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]))

;; ---------------------------------------------------------------------------
;; Sexp parser
;; ---------------------------------------------------------------------------
;; The inventory sexp uses Lisp-style (form ...) syntax with :keyword values.
;; We convert it to nested Clojure vectors/maps by tokenizing and recursive
;; descent parsing.

(defn- tokenize
  "Split sexp text into tokens: parens, strings, and atoms."
  [text]
  (let [sb (StringBuilder.)
        tokens (transient [])
        len (count text)
        emit! (fn [t] (conj! tokens t))]
    (loop [i 0 in-string? false in-comment? false]
      (if (>= i len)
        (persistent! tokens)
        (let [c (.charAt ^String text (int i))]
          (cond
            ;; Inside a comment — skip until newline
            in-comment?
            (recur (inc i) false (not= c \newline))

            ;; Inside a string literal
            in-string?
            (if (= c \")
              (do (emit! (str (.toString sb)))
                  (.setLength sb 0)
                  (recur (inc i) false false))
              (do (.append sb c)
                  (recur (inc i) true false)))

            ;; Start of comment
            (= c \;)
            (recur (inc i) false true)

            ;; Start of string
            (= c \")
            (do (when (pos? (.length sb))
                  (emit! (.toString sb))
                  (.setLength sb 0))
                (recur (inc i) true false))

            ;; Open paren
            (= c \()
            (do (when (pos? (.length sb))
                  (emit! (.toString sb))
                  (.setLength sb 0))
                (emit! "(")
                (recur (inc i) false false))

            ;; Close paren
            (= c \))
            (do (when (pos? (.length sb))
                  (emit! (.toString sb))
                  (.setLength sb 0))
                (emit! ")")
                (recur (inc i) false false))

            ;; Whitespace
            (Character/isWhitespace c)
            (do (when (pos? (.length sb))
                  (emit! (.toString sb))
                  (.setLength sb 0))
                (recur (inc i) false false))

            ;; Regular character
            :else
            (do (.append sb c)
                (recur (inc i) false false))))))))

(defn- parse-atom
  "Convert a string token to keyword, number, or symbol string."
  [s]
  (cond
    (str/starts-with? s ":") (keyword (subs s 1))
    (re-matches #"-?\d+" s)  (Long/parseLong s)
    :else                    (symbol s)))

(defn- parse-sexp
  "Parse a flat token sequence into nested vectors (for lists) and strings."
  [tokens]
  (let [pos (volatile! 0)
        advance! (fn [] (let [t (nth tokens @pos)] (vswap! pos inc) t))]
    (letfn [(parse-one []
              (let [t (advance!)]
                (cond
                  (= t "(") (loop [acc []]
                              (if (= (nth tokens @pos) ")")
                                (do (advance!) acc)
                                (recur (conj acc (parse-one)))))
                  (string? t) (if (or (str/starts-with? t ":")
                                      (re-matches #"-?\d+" t)
                                      (not (str/includes? t " ")))
                                (parse-atom t)
                                t)
                  :else       (parse-atom t))))]
      (parse-one))))

(defn parse-sexp-string
  "Parse a sexp string into nested Clojure vectors."
  [text]
  (let [tokens (tokenize text)]
    (parse-sexp tokens)))

;; ---------------------------------------------------------------------------
;; Extraction helpers
;; ---------------------------------------------------------------------------

(defn- find-section
  "Find a top-level section by its leading symbol in the parsed sexp."
  [parsed sym]
  (when (vector? parsed)
    (some (fn [form]
            (when (and (vector? form)
                       (= (first form) sym))
              form))
          (rest parsed))))

(defn- kvs->map
  "Convert a flat sequence like [:key1 val1 :key2 val2 ...] into a map.
   Nested vectors that start with a keyword are left as-is."
  [xs]
  (loop [remaining xs acc {}]
    (if (empty? remaining)
      acc
      (let [[k & more] remaining]
        (if (keyword? k)
          (let [v (first more)]
            (recur (rest more) (assoc acc k v)))
          ;; skip non-keyword leading elements
          (recur more acc))))))

(defn- extract-invariant
  "Extract a single invariant form into a map."
  [form]
  (when (and (vector? form) (= (first form) 'invariant))
    (kvs->map (rest form))))

(defn- extract-gate
  "Extract a single gate form into a map."
  [form]
  (when (and (vector? form) (= (first form) 'gate))
    (assoc (kvs->map (rest form)) :entry-type :gate)))

(defn- extract-family
  "Extract a family form into a map."
  [form]
  (when (and (vector? form) (= (first form) 'family))
    (kvs->map (rest form))))

(defn- collect-entries-from-devmap
  "Extract all invariant/gate entries from a devmap section."
  [devmap-form]
  (let [props (kvs->map (rest devmap-form))
        devmap-id (:id props)
        extract-from (fn [section-key]
                       (when-let [section (get props section-key)]
                         (let [items (if (and (vector? section)
                                             (vector? (first section)))
                                      section
                                      [section])]
                           (keep (fn [item]
                                   (when-let [m (or (extract-invariant item)
                                                    (extract-gate item))]
                                     (assoc m :devmap devmap-id)))
                                 items))))]
    (concat (extract-from :discovered-invariants)
            (extract-from :candidate-invariants)
            (extract-from :discovered-gates))))

(defn extract-operational-families
  "Extract operational family entries from the parsed sexp."
  [parsed]
  (when-let [section (find-section parsed 'operational-families)]
    (let [families (second section)]
      (when (vector? families)
        (keep extract-family families)))))

(defn extract-candidate-families
  "Extract candidate family entries from the parsed sexp."
  [parsed]
  (when-let [section (find-section parsed 'candidate-families)]
    (let [families (second section)]
      (when (vector? families)
        (keep extract-family families)))))

(defn extract-all-families
  "Extract all families (operational + candidate) from the parsed sexp."
  [parsed]
  (concat (extract-operational-families parsed)
          (extract-candidate-families parsed)))

(defn extract-entries
  "Extract all law entries (invariants + gates) from repo-seeds devmaps."
  [parsed]
  (when-let [section (find-section parsed 'repo-seeds)]
    (let [devmaps (rest section)]
      (vec (mapcat (fn [dm]
                     (when (and (vector? dm) (= (first dm) 'devmap))
                       (collect-entries-from-devmap dm)))
                   devmaps)))))

;; ---------------------------------------------------------------------------
;; PLDB relations
;; ---------------------------------------------------------------------------

(pldb/db-rel law-entry ^:index id scope status kind family)
(pldb/db-rel law-family ^:index family-id)
(pldb/db-rel law-status ^:index id status)

(defn- entry->facts
  "Convert an entry map to a sequence of pldb fact assertions."
  [{:keys [id scope status kind family] :as entry}]
  (let [id     (or id :unknown)
        scope  (or scope :unspecified)
        status (or status :unspecified)
        kind   (or kind (:entry-type entry) :invariant)
        family (or family :unspecified)]
    [[:law-entry id scope status kind family]
     [:law-status id status]]))

(defn build-db
  "Build a pldb database from extracted entries and families."
  [entries families]
  (let [base pldb/empty-db
        ;; Add law-entry and law-status facts
        with-entries
        (reduce (fn [db entry]
                  (let [facts (entry->facts entry)
                        id     (or (:id entry) :unknown)
                        scope  (or (:scope entry) :unspecified)
                        status (or (:status entry) :unspecified)
                        kind   (or (:kind entry) (:entry-type entry) :invariant)
                        family (or (:family entry) :unspecified)]
                    (-> db
                        (pldb/db-fact law-entry id scope status kind family)
                        (pldb/db-fact law-status id status))))
                base
                entries)
        ;; Add law-family facts for distinct families
        all-family-ids (into #{} (concat
                                  (keep :family entries)
                                  (keep :id families)))
        with-families
        (reduce (fn [db fid]
                  (pldb/db-fact db law-family fid))
                with-entries
                all-family-ids)]
    with-families))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn load-inventory
  "Load and parse the structural-law inventory sexp file.

   Returns {:entries [...] :families [...] :parsed <raw> :db <pldb-database>}"
  [path]
  (let [text    (slurp (io/file path))
        parsed  (parse-sexp-string text)
        entries (extract-entries parsed)
        families (extract-all-families parsed)
        db      (build-db entries families)]
    {:entries  entries
     :families families
     :parsed   parsed
     :db       db}))

(defn laws-by-status
  "Return entries matching the given status keyword."
  [db status-kw]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [id scope kind family]
        (law-entry id scope status-kw kind family)
        (l/== q {:id id :scope scope :status status-kw :kind kind :family family})))))

(defn laws-by-family
  "Return entries matching the given family keyword."
  [db family-kw]
  (pldb/with-db db
    (l/run* [q]
      (l/fresh [id scope status kind]
        (law-entry id scope status kind family-kw)
        (l/== q {:id id :scope scope :status status :kind kind :family family-kw})))))

(def ^:private operational-statuses
  #{:operational :operational-but-bypassable :operational-when-enabled})

(def ^:private non-operational-statuses
  #{:candidate :violated})

(defn check-compliance
  "Check structural-law compliance.

   Returns {:ok true} when no violations exist, or
   {:ok false :violations [...]} listing entries with :violated status
   and entries that are candidates without operational backing."
  [db]
  (let [violated
        (pldb/with-db db
          (l/run* [q]
            (l/fresh [id scope kind family]
              (law-entry id scope :violated kind family)
              (l/== q {:id id :scope scope :kind kind :family family
                       :reason :violated-status}))))
        ;; Families that have at least one operational entry
        operational-families
        (set
         (pldb/with-db db
           (l/run* [family]
             (l/fresh [id scope status kind]
               (law-entry id scope status kind family)
               (l/project [status]
                 (if (contains? operational-statuses status)
                   l/succeed
                   l/fail))))))
        ;; Families with only candidate entries (no operational backing)
        all-families
        (set
         (pldb/with-db db
           (l/run* [fid]
             (law-family fid))))
        unbacked (remove operational-families all-families)]
    (if (and (empty? violated) (empty? unbacked))
      {:ok true}
      {:ok false
       :violations (vec violated)
       :unbacked-families (vec unbacked)})))
