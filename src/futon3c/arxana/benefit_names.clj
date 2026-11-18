(ns futon3c.arxana.benefit-names
  "Source-file labels, not store facts. Parse only explicit BEN literals, language
  headings and numbered table rows; unknown codes stay unknown. Never writes a store."
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.arxana.records :as records])
  (:import [java.security MessageDigest]
           [java.nio.file Files]))

(defn sha256 [path]
  (format "%064x" (java.math.BigInteger. 1 (.digest (MessageDigest/getInstance "SHA-256")
                                                  (Files/readAllBytes (.toPath (io/file path)))))))
(defn source [path line] {:path path :line line :sha256 (sha256 path)})
(defn parse-sources
  "BEN is the Python source path, TABLES the Markdown paths. No Python executes.
  Conflicting explicit definitions refuse instead of choosing a name."
  [ben tables]
  (let [text (slurp ben)
        block (second (re-find #"(?s)\bBEN\s*=\s*\{([^}]+)\}" text))
        _ (when-not block (throw (ex-info "BEN literal missing" {:reason :missing-trunks :path ben})))
        trunks (into {} (for [[_ code label] (re-seq #"'([^']+)'\s*:\s*'([^']+)'" block)]
                          [code {:label label :source (source ben (inc (count (filter #{\newline} (subs text 0 (.indexOf text (str "'" code "'")))))))}]))
        docs (for [path tables [i line] (map-indexed vector (str/split-lines (slurp path)))]
               {:path path :line (inc i) :text line})
        trunks (reduce (fn [ts {:keys [path line text]}]
                         (if-let [[_ code label] (re-matches #"# Project language ([A-Z]): (.+?) \(draft,.*" text)]
                           (if (contains? ts code) ts (assoc ts code {:label label :source (source path line)})) ts)) trunks docs)
        base (into {} (for [[code {:keys [label source]}] trunks]
                        [code {:trunk code :trunk-name label :name label :pattern nil :gloss label
                               :source source :trunk-source source}]))]
    (reduce (fn [out {:keys [path line text]}]
              (let [cells (mapv str/trim (str/split text #"\|")) code (get cells 1)]
                (if (and code (re-matches #"[A-Z][0-9]+" code))
                  (let [harm? (str/starts-with? code "H") trunk (if harm? "harm" (subs code 0 1))
                        t (get trunks trunk) pattern (when-not harm? (get cells 2))
                        gloss (get cells (if harm? 2 3))
                        entry {:trunk trunk :trunk-name (:label t) :name (or pattern gloss)
                               :pattern pattern :gloss gloss :source (source path line) :trunk-source (:source t)}]
                    (when-not (and t (seq gloss)) (throw (ex-info "incomplete source row" {:code code :path path :line line})))
                    (when (and (get out code) (not= entry (get out code)))
                      (throw (ex-info "conflicting source rows" {:code code})))
                    (assoc out code entry)) out))) base docs)))

(defn manifest [benefits names]
  (let [rows (sort-by #(get-in % [:asserted-content :benefit/line]) benefits)
        named (filter #(get names (get-in % [:asserted-content :benefit/line])) rows)]
    {:dry-run true :named (count named)
     :unnamed (mapv #(get-in % [:asserted-content :benefit/line]) (remove (set named) rows))
     :attestations
     (mapv (fn [r]
             (let [code (get-in r [:asserted-content :benefit/line]) n (get names code)]
               (records/make-attestation
                {:subject-clusion-id (:id r) :producer {:parser "futon3c.arxana.benefit-names" :version 1}
                 :basis :parser :standing :unverified :coverage :complete
                 :method {:procedure :explicit-source-row :source (:source n) :trunk-source (:trunk-source n)}
                 :content (assoc (select-keys n [:name :pattern :gloss :trunk :trunk-name]) :benefit/line code)
                 :source-refs (vec (distinct [(:source n) (:trunk-source n)]))}))) named)}))

(def ^:private labels (delay (edn/read-string (slurp (io/resource "arxana/benefit-names.edn")))))
(defn label [code]
  (when-let [n (get @labels code)]
    (assoc n :status :source-file :notice "From source file, not yet a record")))

(defn -main
  "BEN benefits.edn names.edn manifest.edn table.md ...; benefits is a saved,
  complete records-by-type response's record vector. Local files only."
  [ben benefits-path names-path manifest-path & tables]
  (let [benefits (edn/read-string (slurp benefits-path))
        parsed (parse-sources ben tables)
        names (into (sorted-map) (select-keys parsed (map #(get-in % [:asserted-content :benefit/line]) benefits)))
        proposal (manifest benefits names)]
    (spit names-path (with-out-str (pprint/pprint names)))
    (spit manifest-path (with-out-str (pprint/pprint proposal)))
    (prn (select-keys proposal [:dry-run :named :unnamed]))))
