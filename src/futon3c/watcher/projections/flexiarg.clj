(ns futon3c.watcher.projections.flexiarg
  "Substrate-2 projector for .flexiarg pattern files.

   Reads the canonical `futon.flexiarg.projection` packet and derives the
   watcher-facing shape from it. This keeps substrate-2 aligned with the
   shared parser rather than maintaining another parallel flexiarg reader."
  (:require [clojure.string :as str]
            [futon.flexiarg.projection :as projection]))

(def src-exts #{"flexiarg" "multiarg"})

(defn- packet->header
  [packet]
  (when-let [pattern-id (:pattern/id packet)]
    (when-let [i (str/last-index-of pattern-id "/")]
      (let [ns-part (subs pattern-id 0 i)
            nm (subs pattern-id (inc i))]
        {:ns (str "flexiarg." (str/replace ns-part "/" "."))
         :name nm
         :qname (str "flexiarg." (str/replace ns-part "/" ".") "/" nm)}))))

(defn- has-toulmin-slots?
  [packet]
  (let [keys (into #{} (map :name-key) (:pattern/clauses packet))]
    (every? keys ["if" "however" "then" "because"])))

(defn- collect-symbols
  "Collect explicit pattern references from both structured references and
   inline `library/<ns>/<name>` citations inside clause text."
  [packet]
  (let [body-text (str/join "\n\n" (map :text (:pattern/clauses packet)))
        inline-refs (re-seq #"library/[\w./_-]+/[\w._-]+" body-text)
        explicit-refs (:pattern/references packet)]
    (into #{}
          (keep (fn [s]
                  (let [ref (str/replace s #"^library/" "")
                        i (str/last-index-of ref "/")]
                    (when i
                      (let [ns (subs ref 0 i)
                            nm (subs ref (inc i))]
                        (symbol (str "flexiarg."
                                     (str/replace ns "/" ".")
                                     "/"
                                     nm)))))))
          (concat explicit-refs inline-refs))))

(defn- unresolved-sigil?
  [sigil]
  (str/includes? (or sigil "") "?"))

(defn- packet-slots
  [packet]
  (mapv (fn [idx clause]
          {:slot/index idx
           :slot/name (:name clause)
           :slot/name-key (:name-key clause)
           :slot/slug (:slug clause)
           :slot/text (:text clause)})
        (range)
        (:pattern/clauses packet)))

(defn- packet->var
  [packet]
  (let [header (packet->header packet)
        directives (:pattern/directives packet)
        sigils (vec (:pattern/sigils packet))
        pending? (boolean (some unresolved-sigil? sigils))
        doc-quality (has-toulmin-slots? packet)]
    {:vertex/type :var
     :var/ns (:ns header)
     :var/name (:name header)
     :var/qname (:qname header)
     :var/kind "flexiarg"
     :var/has-doc (or (some? (:pattern/title packet))
                      (some? (:pattern/conclusion packet))
                      doc-quality)
     :var/syms (collect-symbols packet)
     :pattern/id (:pattern/id packet)
     :pattern/title (:pattern/title packet)
     :pattern/source-path (:pattern/source-path packet)
     :pattern/conclusion (:pattern/conclusion packet)
     :pattern/projection-version (:pattern/projection-version packet)
     :pattern/references (vec (:pattern/references packet))
     :pattern/keywords (vec (:pattern/keywords packet))
     :pattern/sigils-raw sigils
     :pattern/sigils-canonical (when-not pending? sigils)
     :pattern/sigil-pending pending?
     :pattern/audience (:audience directives)
     :pattern/tone (:tone directives)
     :pattern/style (:style directives)
     :pattern/factor (:factor directives)
     :pattern/energy (:energy directives)
     :pattern/pattern-ref (:pattern-ref directives)
     :pattern/slots (packet-slots packet)}))

(defn collect-file
  "Project one .flexiarg/.multiarg file into the substrate-2 metadata shape."
  [path]
  (let [packets (projection/parse-file path)
        ok-packets (filter #(= :ok (:pattern/status %)) packets)
        headers (keep packet->header ok-packets)
        first-header (first headers)]
    (when first-header
      {:ns (:ns first-header)
       :aliases {}
       :is-test? false
       :tests []
       :vars (mapv packet->var ok-packets)})))
