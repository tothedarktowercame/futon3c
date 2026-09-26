(ns futon3c.wm.run4-infrastructure-reconciliation
  "Immutable evidence view for a started RUN4 click that failed outside the
  task-verdict vocabulary. This never closes a cohort or advances a series."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-realized-recording :as recording]))

(defn- refuse! [reason]
  (throw (ex-info "RUN4 infrastructure reconciliation refused"
                  {:error :run4-infrastructure-reconciliation-refused
                   :reason reason})))

(defn- safe-id? [value]
  (and (string? value)
       (boolean (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" value))))

(defn- admission-content-digest [identity]
  (digest/sha256 (pr-str [(:series-id identity) (:trial-id identity)
                          (:pin-sha256 identity) (:casting identity)])))

(defn- read-one! [root path]
  (let [base (.getCanonicalFile (io/file root))
        file (.getCanonicalFile (io/file path))
        prefix (str (.getPath base) java.io.File/separator)]
    (when-not (and (.isDirectory base) (.isFile file)
                   (str/starts-with? (.getPath file) prefix))
      (refuse! :artifact-outside-authority))
    (let [text (slurp file)
          value (try
                  (with-open [r (java.io.PushbackReader.
                                 (java.io.StringReader. text))]
                    (let [v (edn/read {:eof ::empty} r)]
                      (when (or (= ::empty v)
                                (not= ::end (edn/read {:eof ::end} r)))
                        (refuse! :invalid-artifact-form))
                      v))
                  (catch clojure.lang.ExceptionInfo e (throw e))
                  (catch Throwable _ (refuse! :invalid-artifact-form)))]
      {:path (.getCanonicalPath file) :sha256 (digest/sha256 text) :value value})))

(defn construct!
  "Strictly capture the exact evidence chain described by SPEC. All paths are
  server-owned and must be regular descendants of their corresponding roots."
  [{:keys [series-root cohort-root binding-root repair-root] :as roots}
   {:keys [series-id trial-id controller-attempt-id click-id cohort-id
           cohort-attempt-id wrapper-attempt-id repair-id pin-sha256
           manifest-sha256] :as identity}
   paths]
  (when-not (and (= #{:series-root :cohort-root :binding-root :repair-root}
                     (set (keys roots)))
                 (= #{:series-id :trial-id :controller-attempt-id :click-id
                      :cohort-id :cohort-attempt-id :wrapper-attempt-id :repair-id}
                    (disj (set (keys identity)) :pin-sha256 :manifest-sha256))
                 (every? #(and (string? %) (re-matches #"[0-9a-f]{64}" %))
                         [pin-sha256 manifest-sha256]))
    (refuse! :invalid-contract))
  (let [started (read-one! series-root (:started paths))
        reservation (read-one! series-root (:reservation paths))
        click (read-one! series-root (:click-result paths))
        binding (read-one! binding-root (:binding paths))
        repair (read-one! repair-root (:repair paths))
        cells (mapv #(read-one! cohort-root %) (:checkpoint-prefix paths))
        sv (:value started) rv (:value reservation) cv (:value click)
        bv (:value binding) fv (:value repair)
        cell-values (mapv :value cells)]
    (when-not (and (= 6 (count cells))
                   (every? #(and (= 1 (:event/schema-version %))
                                 (= 1 (:attempt/ordinal %))) cell-values)
                   (= (range 1 7) (map :event/sequence cell-values))
                   (= [:time-step :selection :construction :dispatch :build :adjudication]
                      (mapv :checkpoint/type cell-values))
                   (every? #(and (= cohort-id (:cohort/id %))
                                 (= cohort-attempt-id (:attempt/id %))) cell-values)
                   (every? (fn [cell]
                             (let [cp (:checkpoint/type cell)]
                               (if (contains? #{:construction :dispatch :build :adjudication} cp)
                                 (= {:outcome :agent-unavailable
                                     :kind (keyword (str "not-reached-" (name cp)))}
                                    (get-in cell [:payload :sorry]))
                                 (map? (:payload cell)))))
                           cell-values))
      (refuse! :checkpoint-prefix-not-positive))
    (when-not (and (= :wm/run4-series-started-v1 (:schema sv))
                   (= :wm/run4-attempt-reservation-v1 (:schema rv))
                   (= :wm/run4-attempt-click-result-v1 (:schema cv))
                   (= :wm-click-run-binding-v1 (:schema bv))
                   (= 3 (:repair/schema-version fv))
                   (= :open (:repair/status fv))
                   (= series-id (:series-id sv)) (= trial-id (:trial-id sv))
                   (= controller-attempt-id (:attempt-id sv)) (= click-id (:click-id sv))
                   (= pin-sha256 (:pin-sha256 sv))
                   (= manifest-sha256 (:manifest-sha256 sv))
                   (= controller-attempt-id (:attempt-id rv))
                   (= (admission-content-digest (:identity rv))
                      (:content-sha256 rv))
                   (= pin-sha256 (get-in rv [:identity :pin-sha256]))
                   (= series-id (get-in rv [:identity :series-id]))
                   (= trial-id (get-in rv [:identity :trial-id]))
                   (= controller-attempt-id (:attempt-id cv))
                   (= click-id (get-in cv [:click :click-id]))
                   (string? (get-in cv [:click :started-at]))
                   (try (java.time.Instant/parse (get-in cv [:click :started-at])) true
                        (catch Throwable _ false))
                   (= click-id (:click/id bv)) (= :incomplete (:outcome bv))
                   (= :unavailable (:binding-status bv)) (= :absent (:run-record-status bv))
                   (= wrapper-attempt-id (:attempt/id bv))
                   (= repair-id (:repair/id fv)) (= wrapper-attempt-id (:attempt-id fv))
                   (= :machine-failure (:repair/class fv))
                   (= :initialization (:failure-stage fv))
                   (= :incomplete (:failure-outcome fv))
                   (= :initialization-failed (:failure-kind fv)))
      (refuse! :identity-or-incomplete-binding-mismatch))
    {:schema :wm/run4-infrastructure-reconciliation-v1
     :state :reconciliation-required
     :task-verdict :unknown
     :redispatch-permitted? false
     ;; No retained producer artifact joins the controller click/wrapper to
     ;; the execution-cohort prefix. Preserve both captures without inventing
     ;; that missing edge.
     :cross-store-association :unknown
     :identity identity
     :evidence (mapv #(select-keys % [:path :sha256])
                     (concat [started reservation click binding repair] cells))}))

(defn publish!
  "Recapture and validate all sources at publication. Caller-supplied records
  are deliberately not an admission API."
  [root roots identity paths]
  (let [record (construct! roots identity paths)]
    (when-not (and (.isDirectory (io/file root))
                 (safe-id? (get-in record [:identity :controller-attempt-id]))
                 (= :wm/run4-infrastructure-reconciliation-v1 (:schema record))
                 (= :reconciliation-required (:state record))
                 (= :unknown (:task-verdict record))
                 (false? (:redispatch-permitted? record))
                 (= 11 (count (:evidence record)))
                 (every? #(and (string? (:path %))
                               (re-matches #"[0-9a-f]{64}" (:sha256 %)))
                         (:evidence record)))
      (refuse! :invalid-publication))
    (let [base (.getCanonicalFile (io/file root))
          file (.getCanonicalFile
                (io/file base (str (get-in record [:identity :controller-attempt-id])
                                   ".reconciliation.edn")))]
      (when-not (= (.getCanonicalPath base)
                   (.getCanonicalPath (.getParentFile file)))
        (refuse! :publication-outside-authority))
      (recording/*append-immutable!* file record)
      record)))
