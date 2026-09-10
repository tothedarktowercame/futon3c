(ns futon3c.wm.run4-terminal-evidence
  "Read-only, fail-closed join from a RUN4 series start to durable full-loop
  terminal evidence.  This namespace classifies only producer states whose
  current persisted fields determine the series result."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.c-fold-config :as digest]))

(defn- refuse! [reason & [data]]
  (throw (ex-info "RUN4 terminal evidence refused"
                  (merge {:error :run4-terminal-evidence-refused :reason reason}
                         data))))

(defn- sha? [x]
  (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))

(defn- nonblank? [x] (and (string? x) (not (str/blank? x))))

(defn- safe-id? [x]
  (and (string? x) (boolean (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" x))))

(defn- instant? [x]
  (and (string? x)
       (try (java.time.Instant/parse x) true (catch Throwable _ false))))

(defn- parse-one-bytes [file kind]
  (let [bytes (try (java.nio.file.Files/readAllBytes (.toPath file))
                   (catch java.nio.file.NoSuchFileException _ nil)
                   (catch Throwable _ (refuse! :unreadable-artifact {:kind kind}))) ]
    (when bytes
      (let [text (String. bytes java.nio.charset.StandardCharsets/UTF_8)
            value (try
                    (with-open [r (java.io.PushbackReader.
                                   (java.io.StringReader. text))]
                      (let [v (edn/read {:eof ::empty} r)]
                        (when (= ::empty v) (refuse! :empty-artifact {:kind kind}))
                        (when-not (= ::end (edn/read {:eof ::end} r))
                          (refuse! :trailing-artifact-form {:kind kind}))
                        v))
                    (catch clojure.lang.ExceptionInfo e (throw e))
                    (catch Throwable _ (refuse! :malformed-artifact {:kind kind})))]
        {:value value :text text :sha256 (digest/sha256 text)}))))

(defn- child-file [root relative kind]
  (when-not (and (string? root) (nonblank? relative))
    (refuse! :invalid-artifact-location {:kind kind}))
  (let [base (.getCanonicalFile (io/file root))
        file (.getCanonicalFile (io/file base relative))
        prefix (str (.getPath base) java.io.File/separator)]
    (when-not (and (.isDirectory base) (str/starts-with? (.getPath file) prefix))
      (refuse! :artifact-outside-authority {:kind kind}))
    file))

(defn- declared-file [root declared kind]
  (when-not (nonblank? declared)
    (refuse! :invalid-artifact-location {:kind kind}))
  (let [base (.getCanonicalFile (io/file root))
        file (.getCanonicalFile (io/file declared))
        prefix (str (.getPath base) java.io.File/separator)]
    (when-not (and (.isDirectory base) (str/starts-with? (.getPath file) prefix))
      (refuse! :artifact-outside-authority {:kind kind}))
    file))

(defn- exact-keys? [m expected]
  (and (map? m) (= expected (set (clojure.core/keys m)))))

(defn- admission-content-digest [identity]
  (digest/sha256 (pr-str [(:series-id identity) (:trial-id identity)
                          (:pin-sha256 identity) (:casting identity)])))

(defn- read-admission! [root {:keys [attempt-id identity]} started]
  (let [reservation-file (child-file root (str attempt-id "/reservation.edn") :reservation)
        click-file (child-file root (str attempt-id "/click-result.edn") :click-result)
        reservation (:value (parse-one-bytes reservation-file :reservation))
        click-result (:value (parse-one-bytes click-file :click-result))]
    (when-not (and
               (exact-keys? reservation
                            #{:schema :attempt-id :identity :content-sha256})
               (= :wm/run4-attempt-reservation-v1 (:schema reservation))
               (= attempt-id (:attempt-id reservation))
               (= identity (:identity reservation))
               (= (admission-content-digest identity) (:content-sha256 reservation)))
      (refuse! :admission-binding-mismatch))
    (when-not (and
               (exact-keys? click-result #{:schema :attempt-id :click})
               (= :wm/run4-attempt-click-result-v1 (:schema click-result))
               (= attempt-id (:attempt-id click-result))
               (= (select-keys started [:click-id :started-at]) (:click click-result)))
      (refuse! :click-binding-mismatch))
    click-result))

(defn- binding-file [root click-id]
  (child-file root (str "click-run-binding-" click-id ".edn") :click-run-binding))

(defn- read-binding! [root started]
  (let [snapshot (parse-one-bytes (binding-file root (:click-id started))
                                  :click-run-binding)
        value (:value snapshot)
        required #{:schema :click/id :attempt/id :outcome :binding-status
                   :run-id-observation :run-record-status :recorded-at
                   :run-record :run4/terminal-projection}]
    (when-not (and (exact-keys? value required)
                   (= :wm-click-run-binding-v1 (:schema value))
                   (= (:click-id started) (:click/id value))
                   (= :verified (:binding-status value))
                   (= :present (:run-record-status value))
                   (nonblank? (:attempt/id value))
                   (instant? (:recorded-at value))
                   (= :present (get-in value [:run-id-observation :status]))
                   (nonblank? (get-in value [:run-id-observation :value]))
                   (map? (:run4/terminal-projection value)))
      (refuse! :invalid-click-run-binding))
    value))

(def projection-keys
  #{:schema :click/id :run/id :attempt/id :run4/task-pin :outcome
    :checkpoints :failure :evidence :source})

(declare projection-schema?)

(defn- read-projection! [root binding]
  (let [ref (:run4/terminal-projection binding)
        _ (when-not (exact-keys? ref #{:path :sha256 :source-sha256})
            (refuse! :invalid-projection-reference))
        file (declared-file root (:path ref) :terminal-projection)
        snapshot (parse-one-bytes file :terminal-projection)
        value (:value snapshot)]
    (when-not (and (sha? (:sha256 ref)) (sha? (:source-sha256 ref))
                   (= (:sha256 ref) (digest/sha256 (pr-str value)))
                   (projection-schema? value)
                   (= (:click/id binding) (:click/id value))
                   (= (:attempt/id binding) (:attempt/id value))
                   (= (:outcome binding) (:outcome value))
                   (= (get-in binding [:run-id-observation :value]) (:run/id value))
                   (= (:source-sha256 ref)
                      (get-in value [:source :run-record-sha256])))
      (refuse! :projection-binding-mismatch))
    value))

(defn- read-run-record! [root binding projection]
  (let [declared (get-in projection [:source :run-record])
        file (declared-file root declared :run-record)
        snapshot (parse-one-bytes file :run-record)
        value (:value snapshot)]
    (when-not (and (= (.getCanonicalPath file)
                      (.getCanonicalPath (io/file (:run-record binding))))
                   (= (:sha256 snapshot)
                      (get-in projection [:source :run-record-sha256]))
                   (exact-keys? value
                                #{:run/id :click/id :startedAt :selectorSeam
                                  :traceWritten :route :run4/task-pin})
                   (= (:click/id projection) (:click/id value))
                   (= (:run/id projection) (:run/id value))
                   (= (:run4/task-pin projection) (:run4/task-pin value)))
      (refuse! :run-record-binding-mismatch))
    value))

(def checkpoint-names #{:selection :construction :dispatch :build :adjudication})

(defn- valid-checkpoint? [cell]
  (or (and (exact-keys? cell #{:status :reason})
           (= :absent (:status cell))
           (= :checkpoint-not-returned (:reason cell)))
      (and (exact-keys? cell #{:status :judgment :ground})
           (= :present (:status cell))
           (map? (:judgment cell))
           (map? (:ground cell)))))

(defn- projection-schema? [value]
  (and (exact-keys? value projection-keys)
       (= :wm-run4-terminal-projection-v1 (:schema value))
       (keyword? (:outcome value))
       (exact-keys? (:failure value) #{:kind :stage})
       (exact-keys? (:evidence value)
                    #{:commit :author-job-id :reviewer-job-id :grounding-witness})
       (map? (get-in value [:evidence :grounding-witness]))
       (exact-keys? (:source value) #{:run-record :run-record-sha256})
       (sha? (get-in value [:source :run-record-sha256]))
       (= checkpoint-names (set (keys (:checkpoints value))))
       (every? valid-checkpoint? (vals (:checkpoints value)))
       (let [pin (:run4/task-pin value)]
         (and (map? pin) (sha? (:sha256 pin))
              (some? (:series-id pin)) (some? (:trial-id pin))))))

(defn- complete-grounded-change? [p]
  (let [pin (:run4/task-pin p)
        selection (get-in p [:checkpoints :selection])
        construction (get-in p [:checkpoints :construction])
        dispatch (get-in p [:checkpoints :dispatch])
        build (get-in p [:checkpoints :build])
        adjudication (get-in p [:checkpoints :adjudication])
        validation (get-in build [:judgment :validation])
        gate (:review-gate validation)
        evidence (:evidence p)
        witness (:grounding-witness evidence)]
    (and (= :grounded-change (:outcome p))
         (nil? (get-in p [:failure :kind]))
         (nil? (get-in p [:failure :stage]))
         (every? #(= :present (:status %))
                 [selection construction dispatch build adjudication])
         (= pin (get-in selection [:ground :run4/task-pin]))
         (= :wm-judgement (get-in selection [:ground :kind]))
         (= pin (get-in construction [:judgment :run4/task-pin]))
         (= pin (get-in construction [:ground :run4/task-pin]))
         (= :decision-pinned-construction (get-in construction [:ground :kind]))
         (contains? #{:agency-dispatch :agency-recovered-completion}
                    (get-in dispatch [:ground :kind]))
         (= :git-commit-and-independent-review (get-in build [:ground :kind]))
         (true? (:approved? validation)) (nonblank? (:review-job validation))
         (true? (:required? gate)) (true? (:executed? gate))
         (true? (:passed? gate)) (pos-int? (:tool-events gate))
         (nonblank? (:commit evidence))
         (nonblank? (:author-job-id evidence))
         (nonblank? (:reviewer-job-id evidence))
         (not= (:author-job-id evidence) (:reviewer-job-id evidence))
         (= (:review-job validation) (:reviewer-job-id evidence))
         (some #{(:commit evidence)} (get-in build [:judgment :commits]))
         (= (:commit evidence) (get-in adjudication [:judgment :build-match :commit]))
         (true? (get-in adjudication [:judgment :build-match :review-approved?]))
         (true? (get-in adjudication [:judgment :dial :moved?]))
         (= :authoritative-substrate-discharge (get-in adjudication [:ground :kind]))
         (true? (:resolved? witness)) (true? (:dial-moved? witness))
         (nonblank? (:implementation-id witness))
         (= (:implementation-id witness)
            (get-in adjudication [:judgment :dial :implementation-id])))))

(defn- safe-build-failure? [p]
  (let [pin (:run4/task-pin p)
        selection (get-in p [:checkpoints :selection])
        construction (get-in p [:checkpoints :construction])
        dispatch (get-in p [:checkpoints :dispatch])
        build (get-in p [:checkpoints :build])
        validation (get-in build [:judgment :validation])
        evidence (:evidence p)]
    (and (= :build-failed (:outcome p))
         (= :build-failed (get-in p [:failure :kind]))
         (= :reviewer-wait (get-in p [:failure :stage]))
         (every? #(= :present (:status %)) [selection construction dispatch build])
         (= pin (get-in selection [:ground :run4/task-pin]))
         (= pin (get-in construction [:judgment :run4/task-pin]))
         (= pin (get-in construction [:ground :run4/task-pin]))
         (contains? #{:agency-dispatch :agency-recovered-completion}
                    (get-in dispatch [:ground :kind]))
         (= :git-commit-and-independent-review (get-in build [:ground :kind]))
         (false? (:approved? validation))
         (true? (get-in validation [:review-gate :required?]))
         (true? (get-in validation [:review-gate :executed?]))
         (false? (get-in validation [:review-gate :passed?]))
         (= (:review-job validation) (:reviewer-job-id evidence))
         (nonblank? (:commit evidence))
         (some #{(:commit evidence)} (get-in build [:judgment :commits])))))

(def unsafe-failure-kinds
  #{:agent-unavailable :agent-readiness-failed :substrate-unavailable
    :dispatch-failed :transport-timeout :transport-unavailable
    :initialization-failed :untyped-failure})

(defn- classify [projection projection-ref]
  (let [evidence-id (:sha256 projection-ref)]
    (cond
      (complete-grounded-change? projection)
      {:task-result :succeeded :infrastructure :safe :evidence-id evidence-id}

      (safe-build-failure? projection)
      {:task-result :failed :infrastructure :safe :evidence-id evidence-id}

      (contains? unsafe-failure-kinds (get-in projection [:failure :kind]))
      {:task-result :blocked :infrastructure :unsafe :evidence-id evidence-id}

      :else nil)))

(defn read-terminal-evidence
  "Read and join the exact durable artifacts for STARTED. Missing terminal
  producer artifacts return nil; malformed or conflicting artifacts refuse.
  ROOTS requires :admission, :bindings, :projections and :run-records."
  [{:keys [admission bindings projections run-records] :as roots}
   admission-request started]
  (when-not (and (map? roots) (map? admission-request) (map? started)
                 (safe-id? (:attempt-id admission-request))
                 (map? (:identity admission-request))
                 (safe-id? (:click-id started))
                 (instant? (:started-at started)))
    (refuse! :invalid-consumer-input))
  (read-admission! admission admission-request started)
  (if-not (.exists (binding-file bindings (:click-id started)))
    nil
    (let [binding (read-binding! bindings started)
          projection (read-projection! projections binding)
          pin (:run4/task-pin projection)
          identity (:identity admission-request)]
      (when-not (and (= (:pin-sha256 identity) (:sha256 pin))
                     (= (:series-id identity) (:series-id pin))
                     (= (:trial-id identity) (:trial-id pin)))
        (refuse! :task-pin-binding-mismatch))
      (read-run-record! run-records binding projection)
      (classify projection (:run4/terminal-projection binding)))))

(defn terminal-evidence-port [roots prepared-by-ordinal]
  (fn [started]
    (let [prepared (get prepared-by-ordinal (:ordinal started))]
      (when-not prepared (refuse! :missing-prepared-trial))
      (read-terminal-evidence roots (:admission-request prepared) started))))
