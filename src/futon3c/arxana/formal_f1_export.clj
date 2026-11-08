(ns futon3c.arxana.formal-f1-export
  "F1b deterministic Markdown projection and codepoint sidecar."
  (:require [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.service :as service])
  (:import [java.nio.charset StandardCharsets]))

(def exporter
  {:exporter/id "futon3c.arxana/formal-f1-conversation"
   :exporter/version 1
   :exporter/config {:document-title "Formal Arxana F1 conversation"
                     :turn-heading-level 2
                     :turn-order :fixture-order
                     :range-unit :unicode-codepoints-zero-based-end-exclusive}})

(defn codepoint-count [^String s]
  (.codePointCount s 0 (.length s)))

(defn codepoint-substring [^String s start end]
  (let [a (.offsetByCodePoints s 0 start)
        b (.offsetByCodePoints s 0 end)]
    (.substring s a b)))

(defn- shift-range [[start end] delta] [(+ delta start) (+ delta end)])

(defn export
  "Return deterministic Markdown bytes and a canonical EDN manifest. Plans
  are already in fixture order; no map iteration controls emitted order."
  [plans]
  (let [header (str "# " (get-in exporter [:exporter/config :document-title]) "\n\n")
        built
        (reduce
         (fn [{:keys [text offset entries]} plan]
           (let [heading (str "## " (:turn/id plan) "\n\n")
                 paragraph (String. ^bytes (:bytes plan) StandardCharsets/UTF_8)
                 paragraph-start (+ offset (codepoint-count heading))
                 paragraph-end (+ paragraph-start (codepoint-count paragraph))
                 claims (filterv #(= [:interpretation 1] (:type-revision %))
                                 (:claims plan))
                 attestations (:attestations plan)
                 direct-ranges (into {}
                                     (keep (fn [a]
                                             (when-let [r (:range (first (:source-refs a)))]
                                               [(:subject-clusion-id a) r])))
                                     attestations)
                 range-by-source
                 (into {} (keep (fn [claim]
                                  (when-let [r (direct-ranges (:id claim))]
                                    [(get-in claim [:endpoints 0 :target]) r])))
                                claims)
                 interpretation-ranges
                 (mapv (fn [claim]
                         {:clusion/id (:id claim)
                          :range (shift-range
                                  (or (direct-ranges (:id claim))
                                      (range-by-source (get-in claim [:endpoints 0 :target])))
                                  paragraph-start)})
                       claims)
                 mark-ranges
                 (mapv (fn [mark]
                         {:mark-occurrence/id (:id mark)
                          :glyph (:glyph mark)
                          :range (shift-range (get-in mark [:locator :codepoint-range])
                                              paragraph-start)})
                       (:marks plan))
                 block (str heading paragraph "\n\n")]
             {:text (str text block)
              :offset (+ offset (codepoint-count block))
              :entries (conj entries
                             {:turn/id (:turn/id plan)
                              :paragraph/index (count entries)
                              :range [paragraph-start paragraph-end]
                              :interpretations interpretation-ranges
                              :marks mark-ranges})}))
         {:text header :offset (codepoint-count header) :entries []}
         plans)
        markdown (:text built)
        markdown-bytes (.getBytes ^String markdown StandardCharsets/UTF_8)
        revision-id (artifact/revision-id-for-bytes markdown-bytes)
        manifest {:manifest/version 1
                  :artifact/id "formal-arxana:f1-conversation"
                  :artifact/revision-id revision-id
                  :producer exporter
                  :turns (:entries built)}
        manifest-text (str (digest/canonicalize manifest) "\n")
        manifest-bytes (.getBytes ^String manifest-text StandardCharsets/UTF_8)]
    {:markdown markdown :markdown/bytes markdown-bytes
     :markdown/revision-id revision-id
     :manifest manifest :manifest/text manifest-text
     :manifest/bytes manifest-bytes
     :manifest/revision-id (artifact/revision-id-for-bytes manifest-bytes)}))

(defn- ingest-message [message-id artifact-id text media-type]
  {:protocol/version "arxana.v1" :message/id message-id
   :operation :revision/ingest :principal/id "formal-f1-exporter"
   :body {:artifact/id artifact-id :bytes text :media-type media-type
          :expected/revision-id :absent}})

(defn ingest!
  "Ingest both projection and provenance-bearing sidecar. Content addressing
  makes an equal second export resolve to the same two revision IDs."
  [cfg exported]
  {:markdown (service/dispatch
              cfg (ingest-message "f1b:markdown:v1"
                                  "formal-arxana:f1-conversation"
                                  (:markdown exported)
                                  "text/markdown; charset=utf-8"))
   :manifest (service/dispatch
              cfg (ingest-message "f1b:manifest:v1"
                                  "formal-arxana:f1-conversation-manifest"
                                  (:manifest/text exported)
                                  "application/edn; charset=utf-8"))})
