(ns futon3c.arxana.formal-f5
  "F5a Lean gate and coordinate-free unit-circle geometry records."
  (:require [clojure.java.io :as io]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.formal-f3 :as f3]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.service :as service])
  (:import [java.io ByteArrayOutputStream File]
           [java.nio.file Files]))

(def theory-version "formal-arxana/geometry-v1")
(def lean-source "FormalArxanaGeometry.lean")
(def toolchain "leanprover/lean4:v4.29.0-rc8")
(defn- run [root command]
  (let [p (.start (doto (ProcessBuilder. ^java.util.List command)
                    (.directory (io/file root)) (.redirectErrorStream true)))
        out (with-open [i (.getInputStream p) o (ByteArrayOutputStream.)]
              (.transferTo i o) (.toByteArray o)) status (.waitFor p)]
    {:exit-status status :output (String. ^bytes out "UTF-8") :output/bytes out}))
(defn- tree-metadata [root]
  (->> (file-seq (io/file root ".lake/packages/mathlib")) (filter #(.isFile ^File %))
       (map (fn [^File f] [(.getPath f) (.length f) (.lastModified f)])) sort vec
       digest/canonical-digest))
(defn sorry-free?
  "Lean exits 0 on `sorry`, so exit status alone cannot pass a run: the source
  must contain no sorry/admit token and the output no sorry warning."
  [source output]
  (not (or (re-find #"\b(sorry|admit)\b" source)
           (re-find #"declaration uses 'sorry'" output))))
(defn run-lean-gate [root]
  (let [paths ["lean-toolchain" "lakefile.toml" "lake-manifest.json" lean-source]
        inputs (mapv (fn [p] {:path p :sha256 (digest/sha256-hex
                                               (Files/readAllBytes (.toPath (io/file root p))))}) paths)
        before (tree-metadata root) command ["lake" "env" "lean" lean-source]
        result (run root command) after (tree-metadata root)]
    {:record/type :lean-kernel-run :record/version 1 :toolchain toolchain
     :command command :exit-status (:exit-status result)
     :sorry-free? (sorry-free? (slurp (io/file root lean-source)) (:output result))
     :outcome (if (and (zero? (:exit-status result))
                       (sorry-free? (slurp (io/file root lean-source)) (:output result))
                       (= before after))
                :passed :failed)
     :output (:output result) :output/sha256 (digest/sha256-hex (:output/bytes result))
     :inputs inputs :mathlib/metadata-before before :mathlib/metadata-after after
     :mathlib/unchanged? (= before after)}))

(defn entity [id kind]
  (records/make-clusion {:type-revision [:geometry/entity 1] :endpoints []
                         :asserted-content {:entity/id id :entity/kind kind
                                            :theory/version theory-version}}))
(defn endpoint [role x] {:role role :kind :clusion :target {:clusion/id (:id x)}})
(defn relation [type endpoints content]
  (records/make-clusion {:type-revision [type 1] :endpoints endpoints
                         :asserted-content (assoc content :theory/version theory-version)}))
(defn plan [kernel-attestation-id]
  (let [complex (entity :complex :space) circle (entity :circle :space)
        i (entity :i :point) neg-i (entity :neg-i :point) neg-one (entity :neg-one :point)
        fibre (entity :square-over-neg-one :fibre) rotations (entity :circle-rotations :symmetry-group)
        invariant (entity :unit-norm :invariant)
        square (relation :geometry/map [(endpoint :domain circle) (endpoint :codomain circle)]
                         {:map/id :square :map/definition :z↦z²})
        cube (relation :geometry/map [(endpoint :domain circle) (endpoint :codomain circle)]
                       {:map/id :cube :map/definition :z↦z³})
        conjugation (relation :geometry/map [(endpoint :domain circle) (endpoint :codomain circle)]
                              {:map/id :conjugation-inversion
                               :map/definition :z↦conj-z})
        symmetry (relation :geometry/symmetry [(endpoint :carrier circle) (endpoint :map conjugation)]
                            {:symmetry/id :inversion-conjugation
                             :kernel/attestation-id kernel-attestation-id
                             :lean/declaration "inversion_is_conjugation"})
        fibre-of (relation :geometry/fibre-of [(endpoint :fibre fibre) (endpoint :map square)
                                               (endpoint :value neg-one)]
                           {:kernel/attestation-id kernel-attestation-id})
        members [(relation :geometry/member [(endpoint :fibre fibre) (endpoint :member i)] {})
                 (relation :geometry/member [(endpoint :fibre fibre) (endpoint :member neg-i)] {})]
        preserves (relation :geometry/preserves [(endpoint :map conjugation) (endpoint :invariant invariant)] {})
        satisfies (mapv #(relation :geometry/satisfies [(endpoint :subject %) (endpoint :invariant invariant)] {})
                        [i neg-i neg-one])
        projection (relation :geometry/projection [(endpoint :subject circle)]
                             {:projection/id :cartesian-circle})]
    {:entities {:complex complex :circle circle :i i :neg-i neg-i :neg-one neg-one
                :fibre fibre :rotations rotations :unit-norm invariant}
     :maps {:square square :cube cube :conjugation conjugation}
     :records (vec (concat (vals {:complex complex :circle circle :i i :neg-i neg-i
                                  :neg-one neg-one :fibre fibre :rotations rotations :invariant invariant})
                           [square cube conjugation symmetry fibre-of preserves projection]
                           members satisfies))
     :fibre-of fibre-of :members members :symmetry symmetry}))
(defn- write! [cfg record]
  (service/dispatch cfg {:protocol/version "arxana.v1" :message/id (str "formal-f5a:" (:id record))
                         :operation :clusion/assert :principal/id "formal-f5a/materialiser"
                         :body {:record record}}))
(defn materialise! [cfg lean-revision run]
  (let [kernel (f3/append-kernel-run! cfg lean-revision run)
        p (plan (get-in kernel [:attestation :id]))]
    (assoc p :kernel kernel :writes (mapv #(write! cfg %) (:records p)))))
(defn -main [& [root output]]
  (let [r (run-lean-gate root)] (spit output (str (pr-str r) "\n"))
       (println (pr-str (select-keys r [:outcome :exit-status :output/sha256 :mathlib/unchanged?])))))
