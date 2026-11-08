(ns futon3c.arxana.text-rebase
  "VERIFY spike S2 (M-formal-arxana F2): deterministic three-way text rebase
  over canonical bytes.

  Contract under test (DERIVE-F2.md, codex-21 round 9):
  - canonical bytes: UTF-8, LF newlines; revision digest = sha256 of
    canonical bytes.
  - patch identity: base revision digest, ordered codepoint hunks
    {:start :end :replacement}, author/operation id, patch digest.
  - overlap classes: :disjoint (commuting), :identical, :same-position,
    :nested, :partial-overlap. The last three NEVER silently choose a
    winner: rebase yields explicit attributable alternatives instead.
  - retained intent: after rebase both edits are addressable (hunks
    locatable in the result) or survive as explicit alternatives.

  Offsets are CODEPOINT offsets into the canonical text, never byte
  offsets, so multi-byte characters (CJK, emoji) behave like ASCII.

  Production F2 text rebase contract."
  (:require [clojure.string :as str])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util Arrays]))

(set! *warn-on-reflection* true)

;;; ---------------------------------------------------------------------
;;; Canonical bytes

(defn canonicalise
  "Canonical bytes for a revision: UTF-8, LF newlines. Accepts a String
  or a byte array (decoded as UTF-8). Returns a byte array."
  ^bytes [s]
  (let [^String s (if (string? s) s (String. ^bytes s StandardCharsets/UTF_8))
        s (-> s
              (str/replace "\r\n" "\n")
              (str/replace "\r" "\n")
              (str/replace "﻿" ""))] ; strip BOM if present
    (.getBytes s StandardCharsets/UTF_8)))

(defn canonical-str
  "Canonical text as a String (LF newlines)."
  ^String [s]
  (String. ^bytes (canonicalise s) StandardCharsets/UTF_8))

(defn sha256-hex
  ^String [^bytes bs]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") bs)]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn revision-digest
  "sha256 of the canonical bytes."
  ^String [s]
  (sha256-hex (canonicalise s)))

;;; ---------------------------------------------------------------------
;;; Codepoint helpers

(defn- cp-count
  "Number of codepoints in s."
  ^long [^String s]
  (.codePointCount s 0 (.length s)))

(defn- cp-subs
  "Substring by codepoint offsets [start end)."
  ^String [^String s ^long start ^long end]
  (let [n (cp-count s)]
    (when (or (neg? start) (< end start) (> end n))
      (throw (ex-info "codepoint range out of bounds"
                      {:start start :end end :length n})))
    (let [bi (.offsetByCodePoints s 0 (int start))
          ei (.offsetByCodePoints s 0 (int end))]
      (.substring s bi ei))))

;;; ---------------------------------------------------------------------
;;; Patches

(defn ->patch
  "Validate a patch map {:base digest :author a :op-id id :hunks [...]}.
  Hunks are codepoint spans [start end) with a replacement string,
  sorted by start, non-overlapping. Returns the patch (hunks sorted)."
  [{:keys [author op-id hunks] :as patch}]
  (when (or (nil? author) (nil? op-id))
    (throw (ex-info "patch needs :author and :op-id" {:patch patch})))
  (let [hunks (vec (sort-by (fn [{:keys [start end replacement]}]
                              [start end replacement])
                            hunks))]
    (doseq [{:keys [start end replacement] :as h} hunks]
      (when (or (not (and (integer? start) (integer? end)))
                (neg? start) (< end start)
                (not (string? replacement)))
        (throw (ex-info "malformed hunk" {:hunk h}))))
    (doseq [[h1 h2] (partition 2 1 hunks)]
      (when (and (< (:start h1) (:end h2)) (< (:start h2) (:end h1)))
        (throw (ex-info "overlapping hunks within one patch"
                        {:h1 h1 :h2 h2}))))
    (assoc patch :hunks hunks)))

(defn patch-digest
  "Deterministic digest over base digest + hunks + author/op-id."
  ^String [{:keys [base author op-id hunks]}]
  (sha256-hex (.getBytes (pr-str {:base base :author author :op-id op-id
                                  :hunks hunks})
                         StandardCharsets/UTF_8)))

;;; ---------------------------------------------------------------------
;;; Applying hunks (all hunks in base coordinates)

(defn- apply-hunks*
  "Apply sorted, non-overlapping base-coordinate hunks to canonical text s."
  ^String [^String s hunks]
  (let [n (cp-count s)]
    (loop [hs hunks, cursor 0, ^StringBuilder sb (StringBuilder.)]
      (if-let [{:keys [start end replacement]} (first hs)]
        (do
          (when (or (< start cursor) (> end n))
            (throw (ex-info "hunks overlap or out of bounds"
                            {:hunk {:start start :end end}
                             :cursor cursor :length n})))
          (.append sb ^String (cp-subs s cursor start))
          (.append sb ^String replacement)
          (recur (rest hs) (long end) sb))
        (do
          (.append sb ^String (cp-subs s cursor n))
          (.toString sb))))))

(defn apply-patch
  "Apply patch to base text (String or bytes). Verifies the base digest
  when the patch carries one. Returns canonical bytes."
  ^bytes [base patch]
  (let [bs (canonicalise base)
        s (String. bs StandardCharsets/UTF_8)]
    (when (and (:base patch) (not= (:base patch) (sha256-hex bs)))
      (throw (ex-info "base digest mismatch — stale writer"
                      {:expected (:base patch) :actual (sha256-hex bs)
                       :op-id (:op-id patch)})))
    (canonicalise (apply-hunks* s (:hunks (->patch patch))))))

;;; ---------------------------------------------------------------------
;;; Overlap classification

(defn classify-hunks
  "Classify the overlap of two hunks.
   :disjoint        — spans do not intersect (a zero-length insertion at a
                      boundary counts as disjoint; insertions order before
                      a hunk starting at the same point).
   :identical       — same span, same replacement.
   :same-position   — same span, different replacement.
   :nested          — one span strictly contains the other.
   :partial-overlap — spans intersect, neither contains the other."
  [{s1 :start e1 :end r1 :replacement} {s2 :start e2 :end r2 :replacement}]
  (cond
    (and (= s1 s2) (= e1 e2) (= r1 r2)) :identical
    (and (= s1 s2) (= e1 e2))           :same-position
    (or (<= e1 s2) (<= e2 s1))          :disjoint
    (or (and (<= s1 s2) (<= e2 e1))
        (and (<= s2 s1) (<= e1 e2)))    :nested
    :else                               :partial-overlap))

(def ^:private conflict-classes #{:same-position :nested :partial-overlap})

(defn classify
  "Classify two patches hunk-wise. Returns
  {:class overall :pairs [{:a hunk :b hunk :class c} ...]}
  where overall is :conflict if any pair is same-position/nested/
  partial-overlap, else :mergeable."
  [pa pb]
  (let [pairs (vec (for [ha (:hunks pa) hb (:hunks pb)]
                     {:a ha :b hb :class (classify-hunks ha hb)}))
        conflicts (filterv #(conflict-classes (:class %)) pairs)]
    {:class (if (seq conflicts) :conflict :mergeable)
     :pairs pairs
     :conflicts conflicts}))

;;; ---------------------------------------------------------------------
;;; Transform and compose (same-side sequencing helpers)

(defn transform
  "Patch p is against doc D; patch q (disjoint from p) has landed on D.
  Return p' against D+q with shifted offsets (base digest cleared — the
  caller knows the digest of D+q). Throws on any non-disjoint pair —
  transform never guesses."
  [p q]
  (->patch
   (assoc p
          :base nil
          :hunks
          (mapv (fn [{:keys [start end] :as h}]
                  (let [delta (reduce (fn [d qh]
                                        (let [c (classify-hunks h qh)]
                                          (when (conflict-classes c)
                                            (throw (ex-info "transform across conflicting hunk"
                                                            {:hunk h :q-hunk qh :class c})))
                                          (if (and (= :disjoint c) (<= (:start qh) start))
                                            (+ d (- (cp-count (:replacement qh))
                                                    (- (:end qh) (:start qh))))
                                            d)))
                                      0 (:hunks q))]
                    (assoc h :start (+ start delta) :end (+ end delta))))
                (:hunks p)))))

(defn compose
  "p2 is against the result of applying p1. Compose into a single patch
  against p1's base, PROVIDED p2 does not edit inside text p1 inserted
  and the merged hunks remain disjoint (otherwise throws — a client's own
  sequential edits that nest into its insertions are outside this spike's
  model and recorded as a limitation)."
  [p1 p2]
  (let [shift-back
        (fn [pos]
          (loop [hs (:hunks p1), shift 0]
            (if-let [{:keys [start end replacement]} (first hs)]
              (let [new-start (+ start shift)
                    new-end (+ new-start (cp-count replacement))]
                (cond
                  (< pos new-start) (- pos shift)
                  (< pos new-end) (throw (ex-info "sequential edit inside own insertion"
                                                  {:pos pos :hunk (first hs)}))
                  :else (recur (rest hs)
                               (long (+ shift (- (cp-count replacement) (- end start)))))))
              (- pos shift))))
        shifted (mapv (fn [{:keys [start end] :as h}]
                        (assoc h :start (shift-back start) :end (shift-back end)))
                      (:hunks p2))
        merged (vec (sort-by (fn [{:keys [start end replacement]}]
                               [start end replacement])
                             (concat (:hunks p1) shifted)))]
    (doseq [[h1 h2] (partition 2 1 merged)]
      (when (and (< (:start h1) (:end h2)) (< (:start h2) (:end h1)))
        (throw (ex-info "composed hunks overlap"
                        {:h1 h1 :h2 h2}))))
    (->patch {:base (:base p1)
              :author (:author p1)
              :op-id (str (:op-id p1) "+" (:op-id p2))
              :hunks merged})))

(defn- codepoints [^String s]
  (mapv (fn [cp] (String. (Character/toChars (int cp)))) (.toArray (.codePoints s))))

(defn- apply-symbolic
  "Apply a patch to symbolic codepoints. Replacement tokens inherit the
  provenance of what they replace as well as the current operation."
  [tokens patch deleted]
  (let [patch (->patch patch)]
    (loop [hs (reverse (:hunks patch)), tokens tokens, deleted deleted]
      (if-let [{:keys [start end replacement]} (first hs)]
        (let [removed (subvec tokens start end)
              inherited (into #{(:op-id patch)} (mapcat :sources removed))
              replacement (mapv #(hash-map :text % :origin nil :sources inherited)
                                (codepoints replacement))
              deleted (reduce (fn [m {:keys [origin sources]}]
                                (if (some? origin)
                                  (update m origin (fnil into #{})
                                          (conj (set sources) (:op-id patch)))
                                  m))
                              deleted removed)]
          (recur (rest hs)
                 (vec (concat (subvec tokens 0 start) replacement (subvec tokens end)))
                 deleted))
        [tokens deleted]))))

(defn compose-all
  "Compose valid sequential PATCHES into one patch against BASE. Unlike
  `compose`, this is total when a later patch edits text inserted by an
  earlier patch. It preserves separated hunks by using retained original
  codepoints as anchors. Each resulting hunk carries :source-op-ids for
  conflict attribution; this metadata does not affect application."
  [base patches]
  (let [base (canonical-str base)
        initial (mapv (fn [i t] {:text t :origin i :sources #{}})
                      (range) (codepoints base))
        [tokens deleted]
        (reduce (fn [[tokens deleted] patch]
                  (apply-symbolic tokens patch deleted))
                [initial {}] patches)
        n (cp-count base)
        anchors (keep-indexed (fn [i token]
                                (when (some? (:origin token)) [i token]))
                              tokens)
        steps (concat anchors [[(count tokens) {:origin n}]])
        hunks
        (loop [steps steps, final-cursor 0, original-cursor 0, out []]
          (if-let [[final-index token] (first steps)]
            (let [original-index (:origin token)
                  replacement-tokens (subvec tokens final-cursor final-index)
                  replacement (apply str (map :text replacement-tokens))
                  sources (into #{}
                                (concat (mapcat :sources replacement-tokens)
                                        (mapcat #(get deleted % #{})
                                                (range original-cursor original-index))))
                  changed? (or (< original-cursor original-index)
                               (seq replacement))
                  out (cond-> out
                        changed? (conj {:start original-cursor
                                        :end original-index
                                        :replacement replacement
                                        :source-op-ids (vec (sort sources))}))]
              (recur (rest steps) (long (inc final-index))
                     (long (inc original-index)) out))
            out))
        first-patch (first patches)]
    (->patch {:base (revision-digest base)
              :author (:author first-patch)
              :op-id (str/join "+" (map :op-id patches))
              :hunks (vec hunks)})))

;;; ---------------------------------------------------------------------
;;; Rebase

(defn rebase
  "Three-way rebase of concurrent patches pa and pb against base bytes.
  Deterministic: rebase(base, a, b) and rebase(base, b, a) return the
  same :result bytes for mergeable patches.

  Returns
    {:result bytes :merged [op-id ...] :dropped-identical n}
  when every hunk pair is disjoint or identical (identical hunks applied
  once), or
    {:alternatives [{:choice op-id :bytes bytes :retained [op-id ...]} ...]
     :conflicts [{:a hunk :b hunk :class c :authors [..] :op-ids [..]} ...]}
  when any pair conflicts. Conflicting hunks are NEVER silently resolved;
  each alternative applies one side's conflicting hunks plus all
  non-conflicting hunks from both sides."
  [base pa pb]
  (let [bs (canonicalise base)
        s (String. bs StandardCharsets/UTF_8)
        digest (sha256-hex bs)
        pa (->patch pa)
        pb (->patch pb)]
    (doseq [[label p] [["ours" pa] ["theirs" pb]]]
      (when (and (:base p) (not= (:base p) digest))
        (throw (ex-info (str label " patch is against a stale base")
                        {:label label :expected (:base p) :actual digest
                         :op-id (:op-id p)}))))
    (let [{:keys [pairs conflicts]} (classify pa pb)
          identical-b (set (map :b (filter #(= :identical (:class %)) pairs)))
          conflicting-a (set (map :a conflicts))
          conflicting-b (set (map :b conflicts))
          merged-hunks (when-not (seq conflicts)
                         (vec (sort-by (fn [{:keys [start end replacement]}]
                                         [start end replacement])
                                       (concat (map #(assoc % :op-id (:op-id pa))
                                                    (:hunks pa))
                                               (map #(assoc % :op-id (:op-id pb))
                                                    (remove identical-b
                                                            (:hunks pb)))))))]
      (if (seq conflicts)
        {:alternatives
         [{:choice (:op-id pa)
           :bytes (canonicalise
                   (apply-hunks* s (vec (sort-by (fn [{:keys [start end replacement]}]
                                                   [start end replacement])
                                                 (concat (:hunks pa)
                                                         (remove #(or (conflicting-b %)
                                                                      (identical-b %))
                                                                 (:hunks pb)))))))
           :retained [(:op-id pa) (:op-id pb)]}
          {:choice (:op-id pb)
           :bytes (canonicalise
                   (apply-hunks* s (vec (sort-by (fn [{:keys [start end replacement]}]
                                                   [start end replacement])
                                                 (concat (:hunks pb)
                                                         (remove conflicting-a
                                                                 (:hunks pa)))))))
           :retained [(:op-id pa) (:op-id pb)]}]
         :conflicts (mapv (fn [{:keys [a b class]}]
                            {:a a :b b :class class
                             :authors [(:author pa) (:author pb)]
                             :op-ids [(:op-id pa) (:op-id pb)]})
                          conflicts)}
        {:result (canonicalise
                  (apply-hunks* s merged-hunks))
         :merged [(:op-id pa) (:op-id pb)]
         :merged-hunks merged-hunks
         :dropped-identical (count identical-b)}))))


;;; ---------------------------------------------------------------------
;;; Locate

(defn- hunk-key [h]
  (select-keys h [:start :end :replacement]))

(defn locate
  "Verify that patch's hunks are addressable in result bytes, given the
  merged hunk list in base coordinates, sorted as applied (e.g. the
  :merged-hunks returned by rebase; for a single-party application, the
  patch's own hunks).

  Addressability is coordinate-based, not a content search:
  - a non-empty replacement must equal the result text at the hunk's
    computed result position;
  - a deletion is locatable when each surviving base neighbour that was
    not itself edited sits at the corresponding boundary of the
    computed result gap (a neighbour check is vacuous when another
    merged hunk occupies that boundary).

  Returns per-hunk result positions and an overall :locatable? flag."
  [result base patch merged-hunks]
  (let [rs (canonical-str result)
        bs (canonical-str base)
        rn (cp-count rs)
        bn (cp-count bs)
        merged (vec (sort-by (fn [{:keys [start end replacement]}]
                               [start end replacement])
                             merged-hunks))
        entries
        (mapv (fn [{:keys [start end replacement] :as h}]
                (if-not (some #(= (hunk-key %) (hunk-key h)) merged)
                  {:hunk h :locatable? false :reason :hunk-not-in-merged-set}
                  (let [preceding (take-while #(not= (hunk-key %) (hunk-key h))
                                              merged)
                        delta (reduce (fn [d h2]
                                        (+ d (- (cp-count (:replacement h2))
                                                (- (:end h2) (:start h2)))))
                                      0 preceding)
                        rp (+ start delta)
                        others (remove #(= (hunk-key %) (hunk-key h)) merged)]
                    (if (seq replacement)
                      (let [rend (+ rp (cp-count replacement))]
                        {:hunk h :kind :replacement
                         :position rp
                         :locatable? (and (<= 0 rp) (<= rend rn)
                                          (= replacement (cp-subs rs rp rend)))})
                      (let [left-occupied? (some (fn [h2]
                                                   (or (= (:end h2) start)
                                                       (= (:start h2) start)))
                                                 others)
                            right-occupied? (some (fn [h2]
                                                    (= (:start h2) end))
                                                  others)
                            left-ok (or (zero? start) left-occupied?
                                        (and (pos? rp)
                                             (= (cp-subs bs (dec start) start)
                                                (cp-subs rs (dec rp) rp))))
                            right-ok (or (>= end bn) right-occupied?
                                         (and (< rp rn)
                                              (= (cp-subs bs end (inc end))
                                                 (cp-subs rs rp (inc rp)))))]
                        {:hunk h :kind :deletion
                         :position rp
                         :left-checked (and (pos? start) (not left-occupied?))
                         :right-checked (and (< end bn) (not right-occupied?))
                         :locatable? (boolean (and left-ok right-ok))})))))
              (:hunks (->patch patch)))]
    {:patch (:op-id patch)
     :hunks entries
     :locatable? (every? :locatable? entries)}))

(defn bytes=
  ^Boolean [^bytes a ^bytes b]
  (Arrays/equals a b))
