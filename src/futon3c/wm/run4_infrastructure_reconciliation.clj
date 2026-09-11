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
    (when-not (and (= series-id (:series-id sv)) (= trial-id (:trial-id sv))
                   (= controller-attempt-id (:attempt-id sv)) (= click-id (:click-id sv))
                   (= pin-sha256 (:pin-sha256 sv))
                   (= manifest-sha256 (:manifest-sha256 sv))
                   (= controller-attempt-id (:attempt-id rv))
                   (= pin-sha256 (get-in rv [:identity :pin-sha256]))
                   (= series-id (get-in rv [:identity :series-id]))
                   (= trial-id (get-in rv [:identity :trial-id]))
                   (= click-id (get-in cv [:click :click-id]))
                   (= click-id (:click/id bv)) (= :incomplete (:outcome bv))
                   (= :unavailable (:binding-status bv)) (= :absent (:run-record-status bv))
                   (= wrapper-attempt-id (:attempt/id bv))
                   (= repair-id (:repair/id fv)) (= wrapper-attempt-id (:attempt-id fv))
                   (= :initialization-failed (:failure-kind fv)))
      (refuse! :identity-or-incomplete-binding-mismatch))
    {:schema :wm/run4-infrastructure-reconciliation-v1
     :state :reconciliation-required
     :task-verdict :unknown
     :redispatch-permitted? false
     :identity identity
     :evidence (mapv #(select-keys % [:path :sha256])
                     (concat [started reservation click binding repair] cells))}))

(defn publish!
  [root record]
  (when-not (and (.isDirectory (io/file root))
                 (= :wm/run4-infrastructure-reconciliation-v1 (:schema record))
                 (= :reconciliation-required (:state record))
                 (= :unknown (:task-verdict record))
                 (false? (:redispatch-permitted? record))
                 (= 11 (count (:evidence record)))
                 (every? #(and (string? (:path %))
                               (re-matches #"[0-9a-f]{64}" (:sha256 %)))
                         (:evidence record)))
    (refuse! :invalid-publication))
  (let [file (io/file root (str (get-in record [:identity :controller-attempt-id])
                                ".reconciliation.edn"))]
    (recording/*append-immutable!* file record)
    record))
