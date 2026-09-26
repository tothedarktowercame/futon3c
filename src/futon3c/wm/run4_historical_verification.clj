(ns futon3c.wm.run4-historical-verification
  "Disabled server-side admission of reviewed historical qualification."
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [clojure.string :as str] [clojure.java.shell :as shell]
            [futon2.aif.c-fold-config :as digest]
            [futon2.aif.full-loop-runner :as runner]
            [futon3c.wm.run4-realized-recording :as recording]
            [futon3c.wm.run4-historical-qualification :as qualification]))

(defn- refuse! [reason] (throw (ex-info "Historical verification refused" {:reason reason})))
(defn- pin? [x] (and (string? x) (re-matches #"[0-9a-f]{64}" x)))
(defn- id? [x] (and (string? x) (re-matches #"[A-Za-z0-9][A-Za-z0-9._-]{0,127}" x)))
(defn- capture! [root path sha]
  (let [base (.getCanonicalFile (io/file root)) f (.getCanonicalFile (io/file path))]
    (when-not (and (.isDirectory base) (.isFile f)
                   (.startsWith (.toPath f) (.toPath base))) (refuse! :outside-authority))
    (let [text (slurp f)]
      (when-not (= sha (digest/sha256 text)) (refuse! :source-drift))
      {:path (.getPath f) :sha256 sha :text text})))
(defn- one! [text]
  (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
    (let [x (edn/read {:eof ::empty} r)]
      (when (or (= x ::empty) (not= ::end (edn/read {:eof ::end} r))) (refuse! :invalid-form)) x)))
(defn- ancestor? [repo a b]
  (zero? (:exit (shell/sh "git" "-C" repo "merge-base" "--is-ancestor" a b))))
(defn- head [repo] (str/trim (:out (shell/sh "git" "-C" repo "rev-parse" "HEAD"))))
(defn- output-valid? [{:keys [sha256 utf8] :as output}]
  (and (= #{:sha256 :utf8} (set (keys output))) (pin? sha256) (string? utf8) (= sha256 (digest/sha256 utf8))))

(defn admit!
  [{:keys [finding-root qualification-root qualification-source-root output-root source-repo finding-path
           finding-sha256 qualification-path qualification-sha256 expected-check-ids
           first-commit last-commit source-head verification-id author reviewer
           review-job-id review-job-reader] :as opts}]
  (when-not (and (= #{:finding-root :qualification-root :qualification-source-root
                     :output-root :source-repo :finding-path
                     :finding-sha256 :qualification-path :qualification-sha256
                     :expected-check-ids :first-commit :last-commit :source-head
                     :verification-id :author :reviewer :review-job-id :review-job-reader}
                    (set (keys opts)))
                 (every? id? [verification-id author reviewer review-job-id])
                 (not= author reviewer) (fn? review-job-reader)
                 (vector? expected-check-ids) (seq expected-check-ids)
                 (= (count expected-check-ids) (count (distinct expected-check-ids)))
                 (every? keyword? expected-check-ids) (every? pin? [finding-sha256 qualification-sha256]))
    (refuse! :invalid-contract))
  (let [finding-cap (capture! finding-root finding-path finding-sha256)
        qcap (capture! qualification-root qualification-path qualification-sha256)
        finding (one! (:text finding-cap)) q (one! (:text qcap))
        plan-cap (capture! qualification-source-root (get-in q [:manifest :path])
                           (get-in q [:manifest :sha256]))
        plan (qualification/validate-plan! (one! (:text plan-cap))) rows (:checks q) ids (mapv :id rows)
        job (review-job-reader review-job-id)
        reviewed (runner/independent-review-evidence [finding-path qualification-path] job)
        marker-re #"(?m)^HISTORICAL_VERIFICATION_SHA256: ([0-9a-f]{64})$"
        markers (mapv second (re-seq marker-re (str (:result job))))]
    (when-not (and (= #{:schema :verification-id :repair-id :manifest :sources :checks
                           :qualification-passed? :independent-review :repair-admitted?}
                        (set (keys q)))
                   (= #{:path :sha256} (set (keys (:manifest q))))
                   (vector? rows)
                   (every? #(= #{:id :argv :timeout-ms :timed-out? :exit :stdout :stderr}
                                (set (keys %))) rows)
                   (= :wm/historical-qualification-output-v1 (:schema q))
                   (= :wm/historical-qualification-plan-v1 (:schema plan))
                   (= verification-id (:verification-id q) (:verification-id plan))
                   (= (:repair/id finding) (:repair-id q))
                   (= :open (:repair/status finding))
                   (= :machine-failure (:repair/class finding))
                   (= 3 (:repair/schema-version finding))
                   (= (:repair/id finding) (:repair-id plan))
                   (= expected-check-ids ids)
                   (= expected-check-ids (mapv :id (:checks plan)))
                   (= (mapv #(select-keys % [:id :argv :timeout-ms]) rows) (:checks plan))
                   (= (:sources q)
                      (mapv (fn [{:keys [path sha256]}]
                              (select-keys (capture! qualification-source-root path sha256)
                                           [:path :sha256])) (:sources plan)))
                   (every? #(and (zero? (:exit %)) (false? (:timed-out? %))) rows)
                   (every? #(and (output-valid? (:stdout %)) (output-valid? (:stderr %))) rows)
                   (true? (:qualification-passed? q)) (= :not-performed (:independent-review q))
                   (false? (:repair-admitted? q))
                   (= review-job-id (:job-id reviewed)) (:valid? reviewed)
                   (= reviewer (:agent-id job)) (not= author (:agent-id job))
                   (= [qualification-sha256] markers)
                   (ancestor? source-repo first-commit last-commit)
                   (ancestor? source-repo last-commit source-head)
                   (= source-head (head source-repo)))
      (refuse! :evidence-not-qualified))
    (let [record {:schema :wm/historical-repair-verification-v1
                  :verification-id verification-id :repair-id (:repair/id finding)
                  :state :awaiting-validation :repair-resolved? false
                  :actors {:author author :reviewer reviewer}
                  :review (select-keys reviewed [:job-id :verdict :execution :execution-source])
                  :qualification {:path (:path qcap) :sha256 qualification-sha256
                                  :check-ids ids}
                  :finding (select-keys finding-cap [:path :sha256])
                  :implementation {:first first-commit :last last-commit :source-head source-head}}]
      (capture! finding-root finding-path finding-sha256)
      (capture! qualification-root qualification-path qualification-sha256)
      (capture! qualification-source-root (:path plan-cap) (:sha256 plan-cap))
      (doseq [{:keys [path sha256]} (:sources plan)]
        (capture! qualification-source-root path sha256))
      (when-not (= source-head (head source-repo)) (refuse! :source-head-drift))
      (let [base (.getCanonicalFile (io/file output-root))
            target (io/file base (str verification-id ".verification.edn"))]
        (when-not (and (.isDirectory base)
                       (= base (.getCanonicalFile (.getParentFile target)))
                       (not (java.nio.file.Files/isSymbolicLink (.toPath target))))
          (refuse! :invalid-output-authority))
        (recording/*append-immutable!* target record)
        record))))
