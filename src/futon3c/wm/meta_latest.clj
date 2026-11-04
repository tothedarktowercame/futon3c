(ns futon3c.wm.meta-latest
  "GET /api/alpha/wm/meta-latest — serve the META field from the newest
  outer-task-selection receipt a real click retained on disk, without
  recomputing anything.

  Where the receipts live: the full-loop machinery persists each attempt's
  selection checkpoint as EDN under
    <root>/wm-contract-machinery-*-v1/attempt-*/002-selection.edn
  and the receipt is retained at [:payload :judgment :outer-task-selection]
  (futon2 full_loop_runner retain-outer-task-selection; the same file also
  carries typed absences {:status :absent :reason ...}, which are NOT
  receipts).  The files are large (tens of MB), so this namespace never
  parses a whole file: it scans the raw text for the :outer-task-selection
  key, extracts the balanced map that follows, and reads only that substring
  as EDN.  Candidates are visited newest-mtime first across the configured
  roots; the first real receipt wins. Historical disk records remain readable;
  browser projection now reports :no-task-selection-policy, as meta-preview does."
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.nio.file Files]))

(def default-roots
  "futon2's data root: the machinery cohorts (wm-full-loop-machinery-*/…) and
  the pre-machinery wm-full-loop trees (wm-outer-loop-*/…) live directly
  under it.  Only these prefixes are walked."
  "/home/joe/code/futon2/data")

(def cohort-prefixes
  "Level-one directory prefixes that hold attempt trees."
  ["wm-full-loop-machinery-" "wm-outer-loop-"])

(defn- cohort-dir?
  [^java.io.File f]
  (and (.isDirectory f)
       (some #(.startsWith (.getName f) %) cohort-prefixes)))

(defn- selection-files
  "Every attempt's 002-selection.edn under ROOT: the machinery layout is
  root/wm-full-loop-machinery-N/wm-contract-*-v1/attempt-*/002-selection.edn,
  the older layout root/wm-outer-loop-N-v1/attempt-*/002-selection.edn — a
  bounded walk to depth 4 collecting 002-selection.edn files whose parent
  directory is an attempt-*. "
  [^java.io.File root]
  (let [found (java.util.ArrayList.)]
    (letfn [(walk [^java.io.File dir depth]
              (when (and (.isDirectory dir) (pos? depth))
                (let [children (sort-by #(.getName ^java.io.File %) (.listFiles dir))]
                  (if (.startsWith (.getName dir) "attempt-")
                    (when-let [f (java.io.File. dir "002-selection.edn")]
                      (when (.isFile f) (.add found f)))
                    (doseq [^java.io.File c children] (walk c (dec depth)))))))]
      (doseq [^java.io.File c (sort-by #(.getName ^java.io.File %)
                                       (.listFiles root))]
        (when (cohort-dir? c) (walk c 4))))
    (vec found)))

(defn- newest-first
  "Files ordered by last-modified, newest first."
  [files]
  (sort-by (fn [^java.io.File f] (- (.lastModified f))) files))

(defn- match-delimiter
  "The index just past the balanced closing brace of the map that starts at
  OPEN in TEXT, skipping braces inside strings and \\char literals.  Nil when
  the file ends first (a truncated write)."
  [^String text open]
  (let [n (.length text)]
    (loop [i (inc open) depth 1 in-string false]
      (when (< i n)
        (let [c (.charAt text i)]
          (cond
            in-string (if (= c \\)
                        (recur (+ i 2) depth true)
                        (recur (inc i) depth (not= c \")))
            (= c \") (recur (inc i) depth true)
            (= c \\) (recur (+ i 2) depth false)
            (= c \{) (recur (inc i) (inc depth) false)
            (= c \}) (if (= 1 depth)
                       (inc i)
                       (recur (inc i) (dec depth) false))
            :else (recur (inc i) depth false)))))))

(defn- extract-receipt-at
  "The balanced EDN map that KEY maps to starting at offset START in TEXT
  (START points at the key), or nil when its value is not a map."
  [^String text start key-len]
  (let [k (+ start key-len)]
    (loop [k k]
      (when (< k (.length text))
        (let [c (.charAt text k)]
          (cond
            (Character/isWhitespace c) (recur (inc k))
            (= c \{) (when-let [end (match-delimiter text k)]
                       (subs text k end))
            :else nil))))))

(defn- key-occurrences
  "Every offset of KEY in TEXT."
  [^String text key]
  (loop [offsets [] from 0]
    (if-let [i (str/index-of text key from)]
      (recur (conj offsets i) (+ i (.length key)))
      offsets)))

(defn receipt-in
  "The retained receipt inside one selection checkpoint FILE's text, or nil.
  The file may mention the key in other positions (paths, pins) before the
  receipt itself, so every occurrence is tried.  A typed absence
  ({:status :absent ...}) is not a receipt."
  [^String text]
  (some (fn [start]
          (when-let [raw (extract-receipt-at text start (.length ":outer-task-selection"))]
            (try
              (let [v (edn/read-string raw)]
                (when (and (map? v)
                           (= :wm/outer-task-selection-v1 (:schema v))
                           (not= :absent (:status v)))
                  v))
              (catch Throwable _ nil))))
        (key-occurrences text ":outer-task-selection")))

(defn- attempt-of
  "The attempt directory name of FILE (attempt-042) for the :source block."
  [^java.io.File f]
  (.getName (.getParentFile f)))

(defn latest-retained-receipt
  "The newest retained outer-task-selection receipt on disk: a map
  {:receipt <raw v1 receipt> :text <its EDN text>
   :source {:path .. :attempt .. :tree .. :written-at ..}}
  or a typed absence {:absent ::no-retained-receipt :reason ... :scanned n}.
  Roots default to default-roots (a data root or a collection of them).
  No computation, no selection, one file read per candidate newest-first
  until a receipt is found."
  ([] (latest-retained-receipt default-roots))
  ([roots]
   (let [roots (if (coll? roots) roots [roots])
         files (->> (mapv #(java.io.File. (str %)) roots)
                    (filter #(.isDirectory ^java.io.File %))
                    (mapcat selection-files)
                    newest-first)]
     (loop [[^java.io.File f & more] files]
       (if (nil? f)
         {:absent ::no-retained-receipt
          :reason :no-retained-receipt
          :scanned (count files)}
         (let [text (String. (Files/readAllBytes (.toPath f)) "UTF-8")]
           (if-let [receipt (receipt-in text)]
             {:receipt receipt
              :source {:path (.getAbsolutePath f)
                       :attempt (attempt-of f)
                       :tree (.getName (.getParentFile (.getParentFile f)))
                       :written-at (str (java.time.Instant/ofEpochMilli
                                         (.lastModified f)))}}
             (recur more))))))))

(defn- selection-record-files
  [^java.io.File root]
  (let [dir (java.io.File. root "wm-runs")]
    (if (.isDirectory dir)
      (->> (.listFiles dir)
           (filter (fn [^java.io.File f]
                     (let [n (.getName f)]
                       (and (.isFile f)
                            (str/starts-with? n "selection-record-")
                            (str/ends-with? n ".edn")
                            (not (str/includes? n ".edn."))
                            (not= n "selection-record-index.edn")))))
           vec)
      [])))

(defn- instant-ms [s]
  (when (string? s)
    (try (.toEpochMilli (java.time.Instant/parse s))
         (catch Throwable _ nil))))

(defn- latest-selection-record
  [roots]
  ;; Discovery deliberately does not EDN-read every 1–13 MB record.  The
  ;; producer writes these top-level tokens once; scan them as text, retain
  ;; only metadata, then parse the single winning record below.
  (let [records (->> roots
                     (map #(java.io.File. (str %)))
                     (filter #(.isDirectory ^java.io.File %))
                     (mapcat selection-record-files)
                     (keep (fn [^java.io.File f]
                             (try
                               (let [text (String. (Files/readAllBytes (.toPath f)) "UTF-8")
                                     started-at (second (re-find #":startedAt\s+\"([^\"]+)\"" text))]
                                 (when (str/includes? text ":schema :wm/selection-record-v1")
                                   {:file f
                                    :exercised? (and (str/includes? text ":outer/selection")
                                                     (not (str/includes? text
                                                                        ":status :not-exercised")))
                                    :order-ms (or (instant-ms started-at)
                                                  (.lastModified f))}))
                               (catch Throwable _ nil))))
                     (sort-by (comp - :order-ms)))
        exercised? :exercised?
        skipped (count (take-while (complement exercised?) records))]
    (when-let [{:keys [file order-ms]} (first (filter exercised? records))]
      (let [record (edn/read-string
                    (String. (Files/readAllBytes (.toPath ^java.io.File file)) "UTF-8"))]
        {:record record
         :order-ms order-ms
         :source {:path (.getAbsolutePath ^java.io.File file)
                  :schema (:schema record)
                  :click/id (:click/id record)
                  :run/id (:run/id record)
                  :written-at (or (:startedAt record)
                                  (str (java.time.Instant/ofEpochMilli
                                        (.lastModified ^java.io.File file))))
                  :skipped-without-outer-selection skipped}}))))

(defn- target-kind [id]
  (cond
    (str/starts-with? (str id) "M-") :mission
    (str/starts-with? (str id) "E-") :excursion
    (str/starts-with? (str id) "T-") :ticket
    :else :target))

(defn- exclusion-row [stage x]
  (let [id (or (:target x) (:id x))]
    {:id id
     :kind (or (:kind x) (target-kind id))
     :source {:path (or (get-in x [:source :path])
                        (get-in x [:evidence :registry-entry :path]))}
     :ineligible-reason (or (:ineligible-reason x) (:reason x))
     :exclusion-stage stage}))

(defn- selection-record-projection [record]
  (let [outer (:outer/selection record)
        outcome (:selection-outcome record)
        selected-class (some #(when (= (:class outcome) (:id %)) %)
                             (:classes outer))]
    {:schema :wm/selection-record-v1
     :policy {:kind :outer-selection
              :meta-selection
              {:selected {:target-id (:pick outcome)
                          :policy-id (:class outcome)
                          :G (:G selected-class)}}}
     :support (mapv (fn [id] {:id id :kind (target-kind id) :source {:path nil}})
                    (:outer-field/admitted outer))
     :excluded (into (mapv #(exclusion-row :field %) (:outer-field/excluded outer))
                     (map #(exclusion-row :score %) (:outer-score/excluded outer)))}))

(defn- selection-record-frontier [record]
  (let [classes (get-in record [:outer/selection :classes])]
    (if (seq classes)
      {:status :produced
       :entries (->> classes
                     (map #(hash-map :target-id (:pick %)
                                     :policy-id (:id %)
                                     :G (:G %)))
                     (sort-by :G)
                     (map-indexed #(assoc %2 :rank (inc %1)))
                     vec)}
      {:absent :selection-record-cascade-frontier-not-retained})))

(defn- cohort-order-ms [found]
  (or (instant-ms (get-in found [:source :written-at])) 0))

(defn browser-projection
  "Return the bounded browser projection of a retained outer-selection receipt.

  Projection is intentionally local and pure: the producer that created the
  historical receipt has been retired, but the durable v1 receipt remains the
  authority.  The browser needs its identities, ordering, selected policy and
  typed exclusions, not the large comparison witnesses retained alongside
  them."
  [receipt]
  (if (= :wm/outer-task-selection-v1 (:schema receipt))
    {:schema (:schema receipt)
     :policy {:kind (get-in receipt [:policy :kind])
              :meta-selection
              (select-keys (get-in receipt [:policy :meta-selection])
                           [:schema :status :selected :reason
                            :epistemic-value-nats :ranking])}
     :support (mapv #(select-keys % [:id :kind :source]) (:support receipt))
     :excluded (mapv #(select-keys % [:id :kind :source :ineligible-reason])
                     (:excluded receipt))
     :chosen (:chosen receipt)
     :action (:action receipt)}
    {:absent :unsupported-outer-task-selection-schema
     :schema (:schema receipt)}))

(defn- cascade-at
  "Locator into the same receipt for the q4 row (under
   [:outer-policy-recomputation :q4]) holding this entry's cascade, or a
   typed absence. Reader-side only; the producer already retains the
   cascade there."
  [receipt cascade-digest]
  (if cascade-digest
    (or (when-let [rows (seq (get-in receipt [:outer-policy-recomputation :q4]))]
          (some (fn [[i row]]
                  (when (= cascade-digest (:cascade-digest row))
                    [:outer-policy-recomputation :q4 i]))
                (map vector (range) rows)))
        {:absent :cascade-not-retained})
    {:absent :cascade-not-retained}))

(defn g-frontier
  "The canonical G frontier the click scored, as a short summary: every
  scored candidate's target, rank, G and cascade digest, and the horizon.
  browser-receipt drops it; Joe asked to see how the outer loop chose."
  [receipt]
  (when-let [f (get-in receipt [:policy :canonical-g-frontier])]
    {:status (:status f)
     :horizon (get-in f [:horizon-choice :horizon])
     :entries (->> (:entries f)
                   (mapv #(assoc (select-keys % [:target-id :policy-id :status :rank :G
                                                 :cascade-digest :reason])
                                 :cascade-at (cascade-at receipt (:cascade-digest %))))
                   (sort-by (juxt #(or (:rank %) Long/MAX_VALUE) :target-id))
                   vec)}))

(defn response
  "The meta-latest HTTP response body and status for ROOTS:
  200 {:ok true :receipt <browser shape> :source {...}}, or
  404 {:ok false :error :no-retained-receipt :scanned n}."
  ([] (response default-roots))
  ([roots]
   (let [roots (if (coll? roots) roots [roots])
         cohort (latest-retained-receipt roots)
         selection (latest-selection-record roots)
         use-selection? (and selection
                             (or (:absent cohort)
                                 (> (:order-ms selection) (cohort-order-ms cohort))))
         found (if use-selection? selection cohort)]
     (if (and (not use-selection?) (:absent found))
       [404 {:ok false :error :no-retained-receipt
             :scanned (:scanned found)}]
       (if use-selection?
         [200 {:ok true
               :receipt (selection-record-projection (:record found))
               :g-frontier (selection-record-frontier (:record found))
               :source (:source found)}]
         [200 {:ok true
               :receipt (browser-projection (:receipt found))
               :g-frontier (g-frontier (:receipt found))
               :source (:source found)}])))))
