(ns futon3c.arxana.resolve.legacy
  "Legacy finder classification and translation (D2-DR1, D2 legacy
  classification table).

  Legacy strategy records persist a list of five finder forms but no executor,
  fallback ordering or ambiguity semantics was ever found. They are imported
  as `declaration-only` and must be explicitly translated to a versioned
  executable strategy; translation produces a NEW strategy revision with a
  provenance link to the legacy ID. Unknown types remain visible and return
  `unsupported`; they are never dropped from the digest.

  v1 classification:
    explicit              -> executable (requires artifact revision + anchor/range)
    symbol-as-term        -> executable when configured (pinned patterns/language/normalization)
    filename-mention      -> executable when configured (pinned repo revision/root + rules)
    embedding-proximity   -> advisory-only by default; executable only fully pinned
    surface-form-hybrid   -> translated composition; the legacy tag alone is unsupported"
  (:require [futon3c.arxana.digest :as digest]
            [futon3c.arxana.envelope :as env]))

(def legacy-finder-types
  "The five legacy finder forms (P2 census)."
  #{:explicit :symbol-as-term :filename-mention :embedding-proximity
    :surface-form-hybrid})

(def classification
  "D2 v1 classification per legacy finder type."
  {:explicit :executable
   :symbol-as-term :executable-when-configured
   :filename-mention :executable-when-configured
   :embedding-proximity :advisory-only
   :surface-form-hybrid :translated-composition})

(defn classify
  "Classification of a legacy finder type keyword, or :unsupported for
  unknown types (kept visible, never dropped)."
  [finder-type]
  (get classification finder-type :unsupported))

;;; Required pins for executable-when-configured finders -------------------------

(def ^:private required-config
  {:symbol-as-term #{:patterns :language :normalization}
   :filename-mention #{:repository-revision :root :path-normalization
                       :mention-grammar}
   :embedding-proximity #{:model-id :model-digest :vector-space-version
                          :threshold :tie-rule :neighbour-input-revision}})

(defn missing-pins
  "Which required pins a config is missing for its legacy type."
  [finder-type config]
  (remove (set (keys config)) (get required-config finder-type #{})))

;;; Import / translate ------------------------------------------------------------

(defn import-legacy
  "Import a legacy strategy record as declaration-only (D2-DR1). The original
  JSON is retained verbatim; finder list order is recorded as declaration
  data, never as execution semantics."
  [{:keys [legacy/id legacy/raw-json legacy/finders]}]
  {:arxana/semantics :arxana.resolution/declaration-only
   :arxana/legacy-id id
   :arxana/legacy-json raw-json
   :arxana/declared-finders
   (vec (map-indexed
         (fn [ordinal f]
           {:legacy/ordinal ordinal
            :legacy/type (:legacy/type f)
            :legacy/config (:legacy/config f)
            :arxana/classification (classify (:legacy/type f))})
         finders))})

(defn declaration-only?
  [strategy]
  (= :arxana.resolution/declaration-only (:arxana/semantics strategy)))

(defn translate-finder
  "Translate one declared legacy finder into v1 executable finder map(s).
  Returns {:ok finder-or-finders} or {:error typed-error} with the explicit
  executable/advisory/unsupported status beneath detail."
  [legacy-id ordinal {:keys [legacy/type legacy/config] :as declared}]
  (let [base {:arxana/on [:ambiguous :missing]
              :arxana/provenance {:legacy/id legacy-id :legacy/ordinal ordinal}}]
    (case type
      :explicit
      (if (and (:anchor config)
               (or (:artifact/revision-id config)
                   (:artifact-selection config)))
        {:ok (merge base
                    {:arxana/type :arxana.finder/anchor
                     :arxana/config (:anchor config)}
                    (select-keys config
                                 [:artifact/revision-id :artifact-selection]))}
        {:error (env/error :invalid :arxana.legacy/insufficient-translation
                           {:legacy/type type :status :executable
                            :reason "offsets alone are insufficient; name an artifact revision or selection rule plus an anchor/range"}
                           [:supply-anchor-and-revision])})

      :symbol-as-term
      (let [missing (missing-pins type config)]
        (if (empty? missing)
          {:ok (merge base
                      {:arxana/type :arxana.finder/symbol-as-term
                       :arxana/config (select-keys config
                                                   [:patterns :language
                                                    :normalization :term])})}
          {:error (env/error :invalid :arxana.legacy/unpinned-configuration
                             {:legacy/type type :status :executable-when-configured
                              :missing-pins (vec (sort missing))}
                             [:pin-required-config])}))

      :filename-mention
      (let [missing (missing-pins type config)]
        (if (empty? missing)
          {:ok (merge base
                      {:arxana/type :arxana.finder/filename-mention
                       :arxana/config (select-keys config
                                                   [:repository-revision :root
                                                    :path-normalization
                                                    :mention-grammar
                                                    :include :exclude :filename])})}
          {:error (env/error :invalid :arxana.legacy/unpinned-configuration
                             {:legacy/type type :status :executable-when-configured
                              :missing-pins (vec (sort missing))}
                             [:pin-required-config])}))

      :embedding-proximity
      (let [missing (missing-pins type config)]
        (if (empty? missing)
          ;; Fully pinned: still advisory evidence, never verified (D2 table).
          {:ok (merge base
                      {:arxana/type :arxana.finder/embedding-proximity
                       :arxana/advisory? true
                       :arxana/config (select-keys config (vec required-config))})}
          {:error (env/error :unsupported :arxana.legacy/advisory-unpinned
                             {:legacy/type type :status :advisory-only
                              :missing-pins (vec (sort missing))
                              :reason "advisory-only by default; executable only with pinned model/vector-space/threshold/tie-rule/inputs"}
                             [:pin-required-config])}))

      :surface-form-hybrid
      ;; The legacy tag alone is unsupported; translation expands it into
      ;; named exact/surface/optional embedding steps with explicit cascade
      ;; mode supplied by the translator.
      (let [steps (:steps config)]
        (if (and (sequential? steps) (seq steps))
          {:ok (mapv (fn [i step]
                       (merge base
                              {:arxana/type (:arxana/type step)
                               :arxana/config (:arxana/config step)}
                              (when (:advisory? step)
                                {:arxana/advisory? true})))
                     (range) steps)}
          {:error (env/error :unsupported :arxana.legacy/hybrid-tag-alone
                             {:legacy/type type :status :unsupported
                              :reason "the legacy tag alone is unsupported; expand into named exact/surface/optional embedding steps"}
                             [:expand-composition])}))

      ;; unknown legacy types: visible, unsupported, never dropped
      {:error (env/error :unsupported :arxana.legacy/unknown-finder-type
                         {:legacy/type type :status :unsupported
                          :declared declared}
                         [:author-translation])})))

(defn translate-strategy
  "Translate a declaration-only imported strategy into a v1 executable
  strategy value (D2-DR1). Succeeds only when EVERY finder that can be
  reached under the cascade rules is executable; an unpinned embedding step
  inside a hybrid composition keeps the strategy non-executable. On success
  returns {:ok strategy} carrying the translation receipt (author, legacy
  provenance). Finder order is vector order and feeds the strategy revision
  digest (evaluator owns digest computation)."
  [{:keys [strategy-id scope cascade-mode author]} imported]
  (if-not (declaration-only? imported)
    {:error (env/error :invalid :arxana.legacy/not-declaration-only
                       {:semantics (:arxana/semantics imported)} nil)}
    (if-not (contains? #{:first-unique :accumulate :all-evidence} cascade-mode)
      {:error (env/error :invalid :arxana.legacy/implicit-fallback
                         {:cascade-mode cascade-mode
                          :reason "there is no implicit fallback mode; a translated strategy must name one"}
                         [:name-cascade-mode])}
      (let [legacy-id (:arxana/legacy-id imported)
            translated (map-indexed (fn [i d] (translate-finder legacy-id i d))
                                    (:arxana/declared-finders imported))
            failures (filter :error translated)]
        (if (seq failures)
          {:error (env/error :unsupported :arxana.legacy/translation-incomplete
                             {:legacy/id legacy-id
                              :failures (mapv :error failures)}
                             [:complete-translation])}
          (let [finders (vec (mapcat (fn [{:keys [ok]}]
                                       (if (sequential? ok) ok [ok]))
                                     translated))]
            {:ok {:arxana/semantics :arxana.resolution/v1
                  :arxana/strategy-id strategy-id
                  :arxana/scope scope
                  :arxana/cascade-mode cascade-mode
                  :arxana/finders finders
                  :arxana/translation {:legacy/id legacy-id
                                       :author author
                                       :receipt-digest
                                       (digest/canonical-digest
                                        {:legacy/id legacy-id
                                         :finders (count finders)
                                         :cascade-mode cascade-mode})}}}))))))
