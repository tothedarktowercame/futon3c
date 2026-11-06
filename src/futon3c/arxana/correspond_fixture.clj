(ns futon3c.arxana.correspond-fixture
  "Fixture loading and verification for the D4 normative fixture committed in
  futon5 (gate commit 17dc53a). Verifies revision hashes, rejects the
  contradiction-bearing negative scope table, checks the three normative
  cases (paragraph point, section range, math transformation), and builds a
  correspondence repository from the coherent positive fixture."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [futon3c.arxana.correspond :as corr]
            [futon3c.arxana.digest :as digest])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def gate-commit "17dc53aa3904a0fa2de6fe2d6b57fd1f467aa89a")

(def futon5-root
  "Fixture checkout used by the demo. An explicit environment override lets
  isolated releases avoid reading or modifying the operator's live checkout."
  (or (System/getenv "FUTON5_ROOT") "/home/joe/code/futon5"))

(def fixture-dir
  "holes/tech-notes/paper/html-build/wysiwyg/arxana-fixture")

(defn fixture-path [& segments]
  (str/join "/" (into [futon5-root fixture-dir] segments)))

(defn- sha256-hex [^bytes bs]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (.update md bs)
    (format "%064x" (java.math.BigInteger. 1 (.digest md)))))

(defn- file-bytes [path]
  (with-open [in (io/input-stream path)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn revision-id
  "Full artifact revision ID (D4: sha256 over exact bytes, no normalization)."
  [bytes]
  (str "arxana:revision:sha256:" (sha256-hex bytes)))

;;; ---------------------------------------------------------------------------
;;; Committed-gate verification: files on disk must equal the 17dc53a blobs.

(defn committed-blob-sha
  "Git blob sha of path at the gate commit, or nil."
  [rel-path]
  (let [{:keys [exit out]} (shell/sh "git" "-C" futon5-root
                                     "rev-parse" (str gate-commit ":" rel-path))]
    (when (zero? exit) (str/trim out))))

(defn live-blob-sha [rel-path]
  (let [{:keys [exit out]} (shell/sh "git" "-C" futon5-root
                                     "hash-object" (str futon5-root "/" rel-path))]
    (when (zero? exit) (str/trim out))))

(defn verify-committed!
  "Throw ex-info :fixture/not-committed-gate unless the live files equal the
  blobs committed at the gate commit."
  [rel-paths]
  (doseq [p rel-paths]
    (when-not (= (committed-blob-sha p) (live-blob-sha p))
      (throw (ex-info "fixture file differs from committed gate 17dc53a"
                      {:type :fixture/not-committed-gate :path p
                       :committed (committed-blob-sha p)
                       :live (live-blob-sha p)}))))
  true)

;;; ---------------------------------------------------------------------------
;;; Loading

(defn load-json [path]
  (json/read-str (slurp path) :key-fn keyword))

(defn load-manifest []
  (load-json (fixture-path "manifest.json")))

(defn load-scopes []
  (load-json (fixture-path "scopes.json")))

(defn load-negative-scopes []
  (load-json (fixture-path "scopes-negative-v1.json")))

;;; ---------------------------------------------------------------------------
;;; Revision verification (D4 repair steps 1–2)

(defn verify-scope-revisions
  "Check each recorded source-sha against the live bytes under src-dir.
  Returns {:ok? bool :errors [...]}; errors carry :error :fixture/<kind>."
  [scopes]
  (let [srcdir (io/file (:src-dir scopes))
        files (:files scopes)
        hashes (:source-sha scopes)
        resolved (into {}
                       (keep (fn [f]
                               (when f
                                 (let [rp (.getCanonicalFile (io/file srcdir f))]
                                   [f rp]))))
                       files)
        path-errors (for [[f rp] resolved
                          :when (not (str/starts-with? (.getPath rp)
                                                       (.getPath (.getCanonicalFile srcdir))))]
                      {:error :fixture/path-escape :file f})
        rev-errors (for [[f recorded] hashes
                         :let [f (name f)
                               rp (get resolved f)]
                         :when rp]
                     (cond
                       (not (.exists rp))
                       {:error :fixture/source-missing :file f}

                       (not= (subs (sha256-hex (file-bytes rp)) 0 16) recorded)
                       {:error :fixture/revision-mismatch :file f
                        :recorded recorded
                        :actual (subs (sha256-hex (file-bytes rp)) 0 16)}

                       :else nil))]
    {:ok? (and (empty? path-errors) (every? nil? rev-errors))
     :errors (into (vec path-errors) (remove nil? rev-errors))}))

;;; ---------------------------------------------------------------------------
;;; Normative case verification (D4 step 5)

(def ^:private sourcepos-re
  #"data-sourcepos=\"(\d+):(\d+):(\d+)-(\d+):(\d+):(\d+)\"")

(def ^:private tag-re
  #"<(p|section|h\d|math)\b([^>]*data-sourcepos=\"[^\"]*\"[^>]*)>")

(defn- parse-sourcepos [s]
  (when-let [[_ & ns] (re-find sourcepos-re s)]
    (mapv #(Long/parseLong %) ns)))

(defn verify-cases
  "Check the three normative D4 cases against scopes + page + source bytes.
  Returns {:ok? bool :errors [...] :found {...}}."
  [scopes page-path]
  (let [srcdir (io/file (:src-dir scopes))
        files (:files scopes)
        html (slurp page-path)
        lines-of (fn [f] (str/split-lines (slurp (io/file srcdir f))))
        by-name (into {} (keep-indexed (fn [i f] (when f [f i])) files))
        intro-idx (by-name "intro-generated.tex")
        main-idx (by-name "draft8.tex")
        intro-lines (lines-of "intro-generated.tex")
        main-lines (lines-of "draft8.tex")
        tags (for [[_ tag attrs] (re-seq tag-re html)]
               {:tag tag :attrs attrs :sourcepos (parse-sourcepos attrs)})
        errors (transient [])
        found (transient {})]

    ;; Case 1 — paragraph point: intro-generated.tex line 10, point anchor on
    ;; a <p> with no DOM id; leading literal quote; partial (no invented end).
    (let [p1 (first (filter (fn [{:keys [tag attrs sourcepos]}]
                              (and (= tag "p") sourcepos
                                   (= (nth sourcepos 0) intro-idx)
                                   (= (nth sourcepos 1) 10)
                                   (= (nth sourcepos 1) (nth sourcepos 4))
                                   (= (nth sourcepos 2) (nth sourcepos 5))))
                            tags))]
      (if-not p1
        (conj! errors {:error :fixture/case-paragraph-point
                       :reason "no point-anchored <p> on intro-generated.tex line 10"})
        (do
          (when (re-find #"\bid=" (:attrs p1))
            (conj! errors {:error :fixture/case-paragraph-point
                           :reason "paragraph anchor carries a DOM id; must stay partial"}))
          (let [quote "The object of study is a finite family"]
            (when-not (and (>= (count intro-lines) 10)
                           (str/starts-with? (nth intro-lines 9) quote))
              (conj! errors {:error :fixture/case-paragraph-point
                             :reason "leading literal quote not found at line 10"})))
          (assoc! found :paragraph-point p1))))

    ;; Case 2 — section range: \section{The Object} at draft8.tex:316 →
    ;; build-local DOM id S2 (locator evidence only).
    (let [s2 (first (filter #(and (= "section" (:tag %))
                                  (re-find #"\bid=\"S2\"" (:attrs %)))
                            tags))]
      (if-not s2
        (conj! errors {:error :fixture/case-section-range :reason "no element with id S2"})
        (do
          (when-not (and (:sourcepos s2)
                         (= (nth (:sourcepos s2) 0) main-idx)
                         (= (nth (:sourcepos s2) 1) 316))
            (conj! errors {:error :fixture/case-section-range
                           :reason "S2 does not map to draft8.tex line 316"}))
          (when-not (and (>= (count main-lines) 316)
                         (str/starts-with? (nth main-lines 315) "\\section{")
                         (str/includes? (nth main-lines 315) "The Object"))
            (conj! errors {:error :fixture/case-section-range
                           :reason "quote 'The Object' not at draft8.tex:316"}))
          (assoc! found :section-range s2))))

    ;; Case 3 — math transformation: $P_\sig$ → math S1.p2.m2 with
    ;; alttext P_{\sigma}. Directional, not literal.
    (let [mm (first (filter #(and (= "math" (:tag %))
                                  (re-find #"\bid=\"S1\.p2\.m2\"" (:attrs %)))
                            tags))]
      (if-not mm
        (conj! errors {:error :fixture/case-math-transformation
                       :reason "no math id S1.p2.m2"})
        (do
          (when-not (and (:sourcepos mm)
                         (= (nth (:sourcepos mm) 0) intro-idx)
                         (= (nth (:sourcepos mm) 1) 10))
            (conj! errors {:error :fixture/case-math-transformation
                           :reason "S1.p2.m2 not anchored to intro-generated.tex line 10"}))
          (when (and (:sourcepos mm) (>= (count intro-lines) 10))
            (let [sp (:sourcepos mm)
                  seg (subs (nth intro-lines 9) (dec (nth sp 2)) (nth sp 5))]
              (when-not (str/includes? seg "\\sig")
                (conj! errors {:error :fixture/case-math-transformation
                               :reason (str "source range lacks \\sig macro: " seg)}))))
          (let [alt (second (re-find #"alttext=\"([^\"]*)\"" (:attrs mm)))]
            (when-not (= alt "P_{\\sigma}")
              (conj! errors {:error :fixture/case-math-transformation
                             :reason (str "alttext is " alt)})))
          (assoc! found :math-transformation mm))))

    (let [errors* (persistent! errors)
          found* (persistent! found)]
      {:ok? (empty? errors*)
       :errors errors*
       :found found*})))

(defn verify-fixture
  "Full positive-fixture gate: committed-gate equality, revision match,
  normative cases. Throws ex-info on failure; returns the verification
  report on success."
  []
  (verify-committed! [(str fixture-dir "/manifest.json")
                      (str fixture-dir "/scopes.json")
                      (str fixture-dir "/scopes-negative-v1.json")
                      (str fixture-dir "/draft8.html")
                      "holes/tech-notes/paper/html-build/wysiwyg/sandbox/draft8.tex"
                      "holes/tech-notes/paper/html-build/wysiwyg/sandbox/intro-generated.tex"
                      "holes/tech-notes/paper/html-build/wysiwyg/sandbox/part3-exotype.tex"])
  (let [scopes (load-scopes)
        rev (verify-scope-revisions scopes)]
    (when-not (:ok? rev)
      (throw (ex-info "positive fixture failed revision verification"
                      {:type :fixture/rejected :errors (:errors rev)})))
    (let [cases (verify-cases scopes (fixture-path "draft8.html"))]
      (when-not (:ok? cases)
        (throw (ex-info "positive fixture failed normative cases"
                        {:type :fixture/rejected :errors (:errors cases)})))
      {:revisions rev :cases cases})))

;;; ---------------------------------------------------------------------------
;;; Repository construction from the positive fixture

(def fixture-authority "m-jvm-arxana/draft8-v1")
(def fixture-family "draft8")

(defn- line-offset
  "Codepoint offset (0-based) of the start of 1-based `line` in `text`."
  [text line]
  (loop [offset 0 l 1]
    (if (= l line)
      offset
      (let [nl (str/index-of text "\n" offset)]
        (recur (inc nl) (inc l))))))

(defn build-fixture-repository
  "Verify the positive fixture and construct the correspondence repository:
  manifestations (authored TeX / generated HTML, distinct IDs) and the three
  normative assertions. Returns {:repo r :manifestations {...}
  :assertions {...} :report {...}}."
  []
  (let [report (verify-fixture)
        manifest (load-manifest)
        revisions (into {}
                        (map (fn [a] [(:logical-id a) (:revision-id a)]))
                        (:artifacts manifest))
        intro-text (slurp (io/file (:src-dir (load-scopes)) "intro-generated.tex"))
        para-offset (line-offset intro-text 10)
        scopes (load-scopes)
        intro-file-index (.indexOf ^java.util.List (:files scopes) "intro-generated.tex")
        repo (corr/in-memory-repository)
        mk (fn [role-key label media-role role subject & [origin]]
             (corr/register-manifestation!
              repo
              (cond-> {:manifestation/authority fixture-authority
                       :artifact/family-id fixture-family
                       :manifestation/role-key role-key
                       :manifestation/label label
                       :manifestation/media-role media-role
                       :manifestation/role role
                       :manifestation/subject subject}
                origin (assoc :manifestation/origin origin))))
        html-origin {:origin/tool "latexml_oxide" :origin/version "0.7.5"
                     :origin/build-id (get-in manifest [:build :id])
                     :origin/source-revisions [(revisions "draft8.tex")
                                               (revisions "intro-generated.tex")
                                               (revisions "part3-exotype.tex")]}
        m-intro-src (mk "source/intro-paragraph" "Intro paragraph (authored TeX)"
                        :text/x-tex :authored "draft8/introduction")
        m-intro-html (mk "html/intro-paragraph" "Intro paragraph (generated HTML)"
                         :text/html :generated "draft8/introduction" html-origin)
        m-section-src (mk "source/section-the-object" "\\section{The Object} (authored TeX)"
                          :text/x-tex :authored "draft8/section-the-object")
        m-section-html (mk "html/section-the-object" "The Object section (generated HTML)"
                           :text/html :generated "draft8/section-the-object" html-origin)
        m-math-src (mk "source/math-p-sig" "$P_\\sig$ macro (authored TeX)"
                       :text/x-tex :authored "draft8/intro-math")
        m-math-html (mk "html/math-p-sigma" "P_\\sigma rendered math (generated HTML)"
                        :text/html :generated "draft8/intro-math" html-origin)

        ;; Case 1: paragraph point — partial, point locator, quote evidence,
        ;; no DOM id, no invented end.
        a-paragraph (corr/assert-correspondence!
                     repo
                     {:source {:manifestation/id (:manifestation/id m-intro-src)
                               :artifact/revision-id (revisions "intro-generated.tex")
                               :locator {:kind :codepoint-point :offset para-offset}}
                      :target {:manifestation/id (:manifestation/id m-intro-html)
                               :artifact/revision-id (revisions "draft8.html")
                               :locator {:kind :source-map-coordinate
                                         :file-index intro-file-index :line 10}}
                      :direction :source-to-target
                      :cardinality :one-to-one
                      :method [:quote :source-map]
                      :verification :partial
                      :evidence [{:type :quote
                                  :value "The object of study is a finite family"}]
                      :producer {:tool "latexml_oxide" :version "0.7.5"
                                 :build/id (get-in manifest [:build :id])}})

        ;; Case 2: section range — verified source-map + quote; DOM id is
        ;; locator evidence only.
        a-section (corr/assert-correspondence!
                   repo
                   {:source {:manifestation/id (:manifestation/id m-section-src)
                             :artifact/revision-id (revisions "draft8.tex")
                             :locator {:kind :quote-context
                                       :value "\\section{The Object}" :line 316}}
                    :target {:manifestation/id (:manifestation/id m-section-html)
                             :artifact/revision-id (revisions "draft8.html")
                             :locator {:kind :dom-id :value "S2"}}
                    :direction :source-to-target
                    :cardinality :one-to-one
                    :method [:source-map :quote]
                    :verification :verified
                    :evidence [{:type :quote :value "The Object"}
                               {:type :dom-id :value "S2"}]
                    :producer {:tool "latexml_oxide" :version "0.7.5"
                               :build/id (get-in manifest [:build :id])}})

        ;; Case 3: math transformation — directional; reverse literal edit
        ;; unsupported.
        a-math (corr/assert-correspondence!
                repo
                {:source {:manifestation/id (:manifestation/id m-math-src)
                          :artifact/revision-id (revisions "intro-generated.tex")
                          :locator {:kind :quote-context :value "$P_\\sig$" :line 10}}
                 :target {:manifestation/id (:manifestation/id m-math-html)
                          :artifact/revision-id (revisions "draft8.html")
                          :locator {:kind :dom-id :value "S1.p2.m2"}}
                 :direction :source-to-target
                 :cardinality :one-to-one
                 :method [:transformation]
                 :verification :verified
                 :evidence [{:type :transformation
                             :from "P_\\sig" :to "P_{\\sigma}"
                             :alttext "P_{\\sigma}"}]
                 :producer {:tool "latexml_oxide" :version "0.7.5"
                            :build/id (get-in manifest [:build :id])}})]
    {:repo repo
     :manifestations {:intro-source m-intro-src :intro-html m-intro-html
                      :section-source m-section-src :section-html m-section-html
                      :math-source m-math-src :math-html m-math-html}
     :assertions {:paragraph-point a-paragraph
                  :section-range a-section
                  :math-transformation a-math}
     :report report}))
