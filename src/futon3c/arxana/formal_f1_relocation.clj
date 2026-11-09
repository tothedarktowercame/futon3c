(ns futon3c.arxana.formal-f1-relocation
  "F1b2: relocate F1 interpretation scopes across an edited projection.

  The interpretation claim is immutable.  A resolver run is appended as an
  evidence attestation whose content names both revisions and preserves the
  complete candidate set."
  (:require [futon3c.arxana.artifact :as artifact]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.formal-f1-export :as export]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.resolve.evaluator :as evaluator]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store :as store])
  (:import [java.nio.charset StandardCharsets]))

(def resolver-method {:id "arxana.resolve/anchor" :version 1})

(defn- utf8 [^String s] (.getBytes s StandardCharsets/UTF_8))

(defn- entry-text [markdown {:keys [range]}]
  (export/codepoint-substring markdown (first range) (second range)))

(defn strategy
  "Build the exact, non-fuzzy F0 anchor captured at `entry` in the first
  exported revision. Empty contexts deliberately mean that duplicated
  semantic text remains ambiguous rather than being selected by adjacency."
  [markdown source-revision-id {:keys [clusion/id range] :as entry}]
  (let [text (entry-text markdown entry)
        cfg {:anchor/content-hash (digest/sha256-hex (utf8 text))
             :anchor/content-length (export/codepoint-count text)
             :anchor/context-before ""
             :anchor/context-after ""
             :anchor/hint {:offset (first range)}}
        value {:arxana/semantics :arxana.resolution/v1
               :arxana/strategy-id (str "formal-f1/" id)
               :arxana/scope {:subject-clusion-id id
                              :source-revision-id source-revision-id}
               :arxana/cascade-mode :first-unique
               :arxana/finders [{:arxana/type :arxana.finder/anchor
                                 :arxana/on [:ambiguous :missing]
                                 :arxana/config cfg}]}]
    {:subject-clusion-id id
     :strategy/revision-id (evaluator/strategy-revision-id value)
     :strategy value}))

(defn register-strategies
  "Return the content-addressed F0 strategy registry for every interpretation
  in the export manifest. Registration is a value operation: the registry is
  supplied to the evaluator, so no generic arxana/* entity write is needed."
  [exported]
  (let [source-rid (:markdown/revision-id exported)]
    (into (sorted-map)
          (for [turn (get-in exported [:manifest :turns])
                entry (:interpretations turn)
                :let [s (strategy (:markdown exported) source-rid entry)]]
            [(:strategy/revision-id s) s]))))

(defn edit-successor
  "One deterministic edit containing the three required cases.  Prefix text
  shifts every surviving scope; `ambiguous-id` is duplicated verbatim and
  `missing-id` is deleted verbatim."
  [exported unique-id ambiguous-id missing-id]
  (let [entries (into {}
                      (for [turn (get-in exported [:manifest :turns])
                            entry (:interpretations turn)]
                        [(:clusion/id entry) entry]))
        markdown (:markdown exported)
        ambiguous-text (entry-text markdown (entries ambiguous-id))
        missing-text (entry-text markdown (entries missing-id))
        without-missing (.replace ^String markdown missing-text "")
        edited (str "F1b2 editorial preface (not part of any settled turn).\n\n"
                    without-missing
                    "\n\nF1b2 deliberate duplicate:\n\n" ambiguous-text "\n")]
    {:markdown edited
     :bytes (utf8 edited)
     :revision-id (artifact/revision-id-for-bytes (utf8 edited))
     :cases {:unique unique-id :ambiguous ambiguous-id :missing missing-id}}))

(defn ingest-successor!
  [cfg exported edited]
  (let [rid (artifact/revision-id-for-bytes (utf8 (:markdown edited)))]
    (service/dispatch
   cfg {:protocol/version "arxana.v1"
        :message/id (str "f1b2:successor:" rid)
        :operation :revision/ingest
        :principal/id "formal-f1-relocator"
        :body {:artifact/id "formal-arxana:f1-conversation"
               :bytes (:markdown edited)
               :media-type "text/markdown; charset=utf-8"
               :expected/revision-id (:markdown/revision-id exported)}})))

(defn relocate
  "Evaluate all registered strategies against target-revision-id.  The caller
  must supply the authority-observed head. A historical expected head is a
  typed stale result and no finder runs."
  [provider registry source-revision-id target-revision-id expected-head actual-head]
  (if (not= expected-head actual-head)
    {:result/status :stale
     :expected/revision-id expected-head
     :current/revision-id actual-head
     :results []}
    {:result/status :complete
     :results
     (mapv (fn [[srid {:keys [subject-clusion-id]}]]
             (let [result (evaluator/evaluate
                           {:artifact-provider provider
                            :strategy-source #(get-in registry [% :strategy])}
                           {:request/id (str "f1b2:" subject-clusion-id)
                            :artifact/revision-id target-revision-id
                            :strategy/revision-id srid
                            :subject/id subject-clusion-id
                            :options {:allow-unverified-fuzzy? false}})]
               {:subject-clusion-id subject-clusion-id
                :source-revision-id source-revision-id
                :target-revision-id target-revision-id
                :strategy/revision-id srid
                :resolution result}))
           registry)}))

(defn relocation-attestation
  "Turn one resolver result into append-only evidence about the unchanged
  interpretation claim. Candidates are never reduced to a chosen first."
  [{:keys [subject-clusion-id source-revision-id target-revision-id
           application/id application/from-revision-id application/hunks
           strategy/revision-id resolution]}]
  (let [status (:result/status resolution)
        content (cond-> {:resolution/status status
                         :source/revision-id source-revision-id
                         :target/revision-id target-revision-id
                         :candidates (:result/candidates resolution)
                         :evidence (:result/evidence resolution)}
                  from-revision-id
                  (assoc :application/from-revision-id from-revision-id)
                  id (assoc :application/id id)
                  hunks (assoc :application/hunks hunks))
        source-refs (vec (concat [{:artifact/revision-id source-revision-id}]
                                 (when from-revision-id
                                   [{:artifact/revision-id from-revision-id}])
                                 [{:artifact/revision-id target-revision-id}]
                                 (when id [{:clusion/id id}])))]
    (records/make-attestation
     {:subject-clusion-id subject-clusion-id
      :producer "futon3c.arxana.resolve"
      :basis :parser
      ;; A deleted fragment is absence, not evidence against the reading:
      ;; it stays :unverified with :coverage :none, never :contradicted.
      :standing (case status :resolved :verified :ambiguous :disputed
                      :unverified)
      :coverage (case status :resolved :complete :ambiguous :partial
                      :missing :none :partial)
      :method (assoc resolver-method :strategy/revision-id revision-id)
      :content content
      :source-refs source-refs})))

(defn append-relocations! [cfg relocation]
  (mapv (fn [r]
          (let [a (relocation-attestation r)]
            (service/dispatch
             cfg {:protocol/version "arxana.v1"
                  :message/id (str "f1b2:evidence:" (:id a))
                  :operation :evidence/append
                  :principal/id "formal-f1-relocator"
                  :body {:record a}})))
        (:results relocation)))

(defn- query! [graph-store q]
  (let [{:keys [ok error]} (store/bounded-query! graph-store q)]
    (if error (throw (ex-info "relocation store query failed" {:typed-error error})) ok)))

(defn- revision-text! [graph-store revision-id]
  (:revision/canonical-bytes
   (query! graph-store {:query/type :revision-by-id :revision-id revision-id})))

(defn relocate-application!
  "Derive F1 relocation entirely from stored revisions, manifest and the
  application record. This is called synchronously after the application is
  appended; a fresh JVM instance can repeat it idempotently."
  [cfg application]
  (if (not= "formal-arxana:f1-conversation"
            (get-in application [:asserted-content :artifact/id]))
    {:result/status :not-f1-artifact}
    (let [graph-store (:graph-store cfg)
          manifest-head (:head/revision-id
                         (query! graph-store {:query/type :artifact-head
                                              :artifact-id "formal-arxana:f1-conversation-manifest"}))
          manifest (walk/postwalk
                    #(if (symbol? %) (keyword (namespace %) (name %)) %)
                    (edn/read-string (revision-text! graph-store manifest-head)))
          source-rid (:artifact/revision-id manifest)
          first-subject (get-in manifest [:turns 0 :interpretations 0 :clusion/id])
          subject-present? (:ok (store/get-entity graph-store first-subject))]
      (if-not subject-present?
        {:result/status :skipped-no-f1-records}
        (let [source-text (revision-text! graph-store source-rid)
              from-rid (some->> (:endpoints application)
                                (filter #(= :from (:role %))) first :target
                                :artifact/revision-id)
              target-rid (some->> (:endpoints application)
                                  (filter #(= :to (:role %))) first :target
                                  :artifact/revision-id)
              target-text (revision-text! graph-store target-rid)
              provider (artifact/in-memory-provider)
              _ (artifact/ingest! provider "formal-arxana:f1-conversation"
                                  (.getBytes ^String source-text StandardCharsets/UTF_8)
                                  "text/markdown; charset=utf-8")
              _ (artifact/ingest! provider "formal-arxana:f1-conversation"
                                  (.getBytes ^String target-text StandardCharsets/UTF_8)
                                  "text/markdown; charset=utf-8")
              exported {:markdown source-text :markdown/revision-id source-rid :manifest manifest}
              registry (register-strategies exported)
              run (relocate provider registry source-rid target-rid target-rid target-rid)
              enriched (update run :results
                               (fn [results]
                                 (mapv #(assoc % :application/id (:id application)
                                                :application/from-revision-id from-rid
                                                :application/hunks
                                                (get-in application [:asserted-content :patch :hunks]))
                                       results)))
              writes (append-relocations! cfg enriched)]
          {:result/status :complete :target/revision-id target-rid
           :application/id (:id application) :writes writes
           :results (:results enriched)})))))
