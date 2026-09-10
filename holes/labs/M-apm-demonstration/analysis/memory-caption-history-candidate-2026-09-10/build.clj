(ns build
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [java.math BigInteger]
           [java.security MessageDigest]))

(def root (io/file "."))
(def audit-dir
  (io/file root "holes/labs/M-apm-demonstration/analysis/memory-audit-2026-09-10"))
(def snapshot-dir
  (io/file root "data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f211/snapshots"))
(def out-dir
  (io/file root "holes/labs/M-apm-demonstration/analysis/memory-caption-history-candidate-2026-09-10"))

(defn read-json [name]
  (json/parse-string (slurp (io/file audit-dir name)) true))

(defn sha256-file [path]
  (with-open [in (io/input-stream path)]
    (let [digest (MessageDigest/getInstance "SHA-256")
          buffer (byte-array 8192)]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur))))
      (format "%064x" (BigInteger. 1 (.digest digest))))))

(def census (read-json "census.json"))
(def fingerprint (read-json "fingerprint.json"))
(def readback (read-json "readback.json"))

(def snapshot-files
  (->> (file-seq snapshot-dir)
       (filter #(and (.isFile %) (str/ends-with? (.getName %) "-memory.edn")))
       (sort-by #(.getName %))))

(def snapshots
  (mapv (fn [f] (assoc (edn/read-string (slurp f)) ::file (.getPath f)))
        snapshot-files))

(def memories-by-id
  (->> snapshots
       (mapcat :snapshot/memories)
       (reduce (fn [acc memory] (assoc acc (:memory-id memory) memory)) {})))

(def identity-keys
  [:memory-id :content-digest :review-evidence-id :reviewer :depositor
   :provenance :name :hook :body])

(defn identity-conflicts [memories]
  (->> memories
       (group-by :memory-id)
       (keep (fn [[id rows]]
               (let [variants (set (map #(select-keys % identity-keys) rows))]
                 (when (> (count variants) 1)
                   {:memory-id id :variant-count (count variants)}))))
       vec))

(defn self-review-conflicts [memories]
  (->> memories
       (filter #(and (:depositor %) (= (:depositor %) (:reviewer %))))
       (mapv #(select-keys % [:memory-id :depositor :reviewer]))))

(def cohort-rows
  (->> (:frames census)
       (mapcat :own-memories)
       (sort-by (juxt #(get-in % [:provenance :frame-id]) :memory-id))
       vec))

(def fingerprint-by-memory
  (group-by :memory (:rows fingerprint)))

(defn positive-caption [{:keys [hook]}]
  (str "Useful when "
       (-> hook str str/trim
           (str/replace-first #"^when\s+" "")
           (str/replace-first #"^[Aa]n?\s+" ""))))

(defn source-basis [memory]
  (mapv (fn [source-id]
          {:source-ref source-id
           :source-digest nil
           :digest-status :unknown
           :locator :source-attempts
           :claim "The original independently reviewed memory cites this source-attempt identifier; this artifact does not treat an identifier substring as a verified content digest."})
        (:source-attempts memory)))

(defn observation-base [memory]
    {:schema :apm-memory-applicability-v1
   :event :memory-applicability-observation
   :memory-id (:memory-id memory)
   :memory-content-digest (:content-digest memory)
   :caption-id nil
   :caption-revision nil
   :suggested-contexts []
   :supersedes-observation-id nil
   :contradiction-refs []})

(defn later-observation [memory row]
  (merge
   (observation-base memory)
   {:context {:kind :historical-accepted-use
              :frame-id (:frame row)
              :problem-id (:problem row)
              :attempt (:attempt row)
              :source (:source row)
              :transfer-stratum (keyword (:transfer-stratum row))
              :memory-use/kind (keyword (:memory-use-kind row))}
    :epistemic-status (case (:verdict row)
                        "fingerprinted" :supported
                        :unknown)
    :task-observation {:disposition :used
                       :reason (case (:verdict row)
                                 "fingerprinted" "Rare named tokens from the memory occur in the retained attempt source."
                                 "already-in-base" "The accepted use was recorded, but the relevant tokens were already present in the base file; contribution is not attributable."
                                 "unwitnessed" "The accepted use was recorded, but this token audit found no rare-token witness."
                                 "no-source" "The accepted use was recorded, but no retained attempt source was available to this audit."
                                 "not-adjudicable-by-token" "The accepted use is regulative or otherwise not adjudicable by source-token overlap."
                                 "The accepted use exists, but its applicability status is unknown in this audit.")
                       :evidence-refs (cond-> [] (:source row) (conj (:source row)))}
    :audit-verdict (keyword (:verdict row))
    :basis (if (= "fingerprinted" (:verdict row))
             [{:source-ref (:source row)
               :source-digest nil
               :digest-status :unknown
               :locator "retained attempt source"
               :claim "The frozen fingerprint audit found rare named memory tokens in this source."}]
             [])
    :scope-limit "An accepted-use record is not by itself a causal claim; the audit verdict controls whether this observation is supported or unknown."}))

(defn cohort-record [row]
  (let [id (:memory-id row)
        memory (get memories-by-id id)
        uses (mapv #(later-observation memory %) (get fingerprint-by-memory id []))
        source-attempts (:source-attempts memory)]
    {:schema :apm-memory-caption-v1
     :event :memory-caption
     :status :draft-not-published
     :memory-id id
     :memory-content-digest (:content-digest memory)
     :memory-review-evidence-id (:review-evidence-id memory)
     :revision 1
     :previous-caption-id nil
     :useful-when (positive-caption memory)
     :epistemic-status :supported
     :basis (source-basis memory)
     :conditions []
     :suggested-contexts []
     :scope-limit "Describes the reviewed reusable move in its source context; it does not claim that the memory closed the whole source problem or that unstated prerequisites are absent."
     :contradiction-refs []
     :coverage-disposition :hook-bootstrap-review-pending
     :source {:problem-id (get-in memory [:provenance :problem-id])
              :frame-id (get-in memory [:provenance :frame-id])
              :attempt-evidence-ids (vec source-attempts)
              :attempt-evidence-status (if (seq source-attempts) :supported :unknown)}
     :independent-review {:review-evidence-id (:review-evidence-id memory)
                          :reviewer (:reviewer memory)
                          :status (if (and (:review-evidence-id memory)
                                           (:reviewer memory))
                                    :supported
                                    :unknown)}
     :applicability-observations
     (into [(merge
             (observation-base memory)
             {:context {:kind :historical-source
                        :frame-id (get-in memory [:provenance :frame-id])
                        :problem-id (get-in memory [:provenance :problem-id])
                        :source-attempt-ids (vec source-attempts)
                        :memory-review-evidence-id (:review-evidence-id memory)}
              :epistemic-status (if (seq source-attempts) :supported :unknown)
              :task-observation {:disposition :unresolved
                                 :reason "The source attempt is cited as derivation evidence for the later memory; the memory could not have been used in the earlier source attempt."
                                 :evidence-refs (vec source-attempts)}
              :basis (source-basis memory)
              :scope-limit "Historical source context only; this is not a Student search receipt and does not claim whole-problem closure."})]
           uses)
     :later-use-disposition
     (if (seq uses)
       :audited-accepted-use-records-attached
       :no-audited-accepted-use-record)
     :condition-disposition :none-authored-no-absence-inferred}))

(defn write-edn! [name value]
  (spit (io/file out-dir name)
        (binding [*print-length* nil *print-level* nil]
          (str (pr-str value) "\n"))))

(defn check! [pred message]
  (when-not pred (throw (ex-info message {}))))

(defn searchable-projection-bytes [caption]
  (count (.getBytes
          (pr-str (select-keys caption [:useful-when :epistemic-status
                                        :conditions :suggested-contexts
                                        :scope-limit]))
          "UTF-8")))

(def enriched-records
  [{:memory-id "e-apm-promotion-5fdb99169bd788313841375c797c302c"
    :coverage-disposition :source-read-enriched-review-pending
    :six-field-draft
    {:caption/schema :apm-memory-caption-v1
     :text "Useful when proving ODE uniqueness from equal initial values although derivative hypotheses hold only inside a half-interval: compare on [δ,t] with Gronwall, then send δ to 0 using continuity."
     :epistemic-status :supported
     :conditions [{:condition "Both trajectories are continuous at the initial endpoint."
                   :status :established
                   :evidence-id "f196-attempt-1-lines-85-109"}
                  {:condition "A common Lipschitz bound controls the vector field on each interior comparison interval."
                   :status :established
                   :evidence-id "f196-attempt-1-lines-56-83"}]
     :suggested-contexts [{:context "Endpoint uniqueness arguments for Volterra equations or other trajectories whose derivative law is available only away from the endpoint."
                           :status :suggested
                           :basis "74622e23 provides an analyst-validated reference adaptation, not historical Student transfer."}]
     :scope-limit "This closes the endpoint uniqueness step; it does not establish solution existence, global continuation, or convergence to equilibrium."
     :basis-evidence [{:path "/home/joe/code/apm-lean/problems/m00A05/problem.md"
                       :sha256 "1db154aae9736eec3adcf230e5d702338bc68d4473412f6c02aa963f61cbd239"}
                      {:path "data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f196/live/student-attempt-1-source/18d4b2817a50d66c328c2fbaa4c6787048987cd0-Main.lean"
                       :sha256 "7e25b70b538d870c55bdf95d1cceb866ea5e2f089731c81a284698b9890f5a20"
                       :locator "lines 49-110"}]}}
   {:memory-id "e-apm-promotion-faa280c92bd833ed990672c3b7007a78"
    :coverage-disposition :source-read-enriched-review-pending
    :memory-use/kind :regulative
    :six-field-draft
    {:caption/schema :apm-memory-caption-v1
     :text "Useful when constructing a distributional derivative or primitive and continuity into the test-function LF space is the obstacle: define the result by transposed pairings in the dual, where continuity can be checked on each fixed-support stage."
     :epistemic-status :supported
     :conditions [{:condition "The desired object can be specified by its pairing with compactly supported smooth test functions."
                   :status :established
                   :evidence-id "m02J01-problem-and-solution"}
                  {:condition "The stagewise pairing estimates needed for the chosen normalized primitive have been proved."
                   :status :unchecked
                   :evidence-id "f207-attempt-1-lines-164-245"}]
     :suggested-contexts []
     :scope-limit "This is route-selection guidance. It does not supply the normalized primitive's stagewise estimates, and its recorded accepted uses remain unknown under the token audit because the memory is regulative."
     :basis-evidence [{:path "/home/joe/code/apm-lean/problems/m02J01/problem.md"
                       :sha256 "88d3dc986e68d30b6e469aa373946f9edceeeac3febbddc361784ae44eee9c29"}
                      {:path "data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f207/live/student-attempt-1-source/24d19442e53f95299ddc1acdc6017f5599823038-Main.lean"
                       :sha256 "a0ebcd8c54a4ce9d1e1c78cd7b2b565bc5c0f12f5bed93e28dff56e5cafdf56d"
                       :locator "lines 164-245"}]}}
   {:memory-id "e-apm-promotion-442f1ab7683c3685e2232135f161830e"
    :coverage-disposition :source-read-enriched-review-pending
    :six-field-draft
    {:caption/schema :apm-memory-caption-v1
     :text "Useful when building an operator on Mathlib test functions: first compose packaged derivative, postcomposition, restriction, and stagewise-limit continuous linear maps; reserve manual LF seminorm estimates for operators such as primitives whose support changes with the input."
     :epistemic-status :supported
     :conditions [{:condition "The intended operator factors through Mathlib's packaged TestFunction continuous linear maps."
                   :status :unchecked
                   :evidence-id "operator-specific-check-required"}
                  {:condition "For a derivative operator, fderivCLM followed by evaluation supplies the required composition."
                   :status :established
                   :evidence-id "f207-attempt-2-lines-109-121"}]
     :suggested-contexts []
     :scope-limit "The packaged composition does not construct an antidifferentiation operator with varying output support; that case still requires a separately checked stagewise continuity proof."
     :basis-evidence [{:path "/home/joe/code/apm-lean/problems/m02J01/informal-solution.md"
                       :sha256 "61c70c3ac1b47b097ef7e912e37820c96bca31b88b82c620325ce9a0c780abb3"}
                      {:path "data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f207/live/student-attempt-2-source/ce045529d95cec5bc92eeb836f7c928ad51d25ef-Main.lean"
                       :sha256 "219cb4de145504585dc80d6208e06c9a1d47bd7186f410a7aa349931ea327372"
                       :locator "lines 109-121 and 184-190"}]}}])

(defn -main []
  (let [snapshot-id-sets (mapv #(set (map :memory-id (:snapshot/memories %))) snapshots)
        union-ids (apply set/union snapshot-id-sets)
        intersection-ids (apply set/intersection snapshot-id-sets)
        records (mapv cohort-record cohort-rows)
        readback-ids (set (map :memory-id (:rows readback)))
        cohort-ids (set (map :memory-id cohort-rows))
        historical
        {:schema :memory-caption-historical-census/v1
         :status :pinned-candidate-not-publication
         :scope {:campaign-id "jit-all-open-v3"
                 :terminal-observed-frame "f211"
                 :note "The 57-member f190-f213 audit cohort is the first caption-authoring slice, not the whole retained store. Older retained memories remain in scope for later slices."}
         :snapshot-pins
         (mapv (fn [snapshot]
                 {:path (::file snapshot)
                  :snapshot-id (:snapshot/id snapshot)
                  :snapshot-digest (:snapshot/digest snapshot)
                  :frame-id (:snapshot/frame-id snapshot)
                  :problem-id (:snapshot/problem-id snapshot)
                  :memory-count (count (:snapshot/memories snapshot))})
               snapshots)
         :population {:role-snapshot-union-count (count union-ids)
                      :role-snapshot-intersection-count (count intersection-ids)
                      :role-dependent-count (count (set/difference union-ids intersection-ids))
                      :union-memory-ids (vec (sort union-ids))
                      :intersection-memory-ids (vec (sort intersection-ids))}
         :bounded-slice {:cohort-frame-range ["f190" "f213"]
                         :audited-memory-count (count records)
                         :memory-ids (vec (sort cohort-ids))}}
        all-snapshot-memories (mapcat :snapshot/memories snapshots)
        conflicts (identity-conflicts all-snapshot-memories)
        self-reviews (self-review-conflicts all-snapshot-memories)
        planted-memory (first all-snapshot-memories)
        planted-identity (assoc planted-memory :body (str (:body planted-memory) " planted mismatch"))
        planted-self-review (assoc planted-memory :reviewer (:depositor planted-memory))
        coverage
        {:schema :memory-caption-coverage/v1
         :expected 57
         :accounted (count records)
         :hook-bootstrap-drafted (count records)
         :source-read-enriched 3
         :source-attempt-supported (count (filter #(= :supported (get-in % [:source :attempt-evidence-status])) records))
         :source-attempt-unknown (count (filter #(= :unknown (get-in % [:source :attempt-evidence-status])) records))
         :independent-review-supported (count (filter #(= :supported (get-in % [:independent-review :status])) records))
         :with-audited-accepted-use (count (filter #(= :audited-accepted-use-records-attached (:later-use-disposition %)) records))
         :without-audited-accepted-use (count (filter #(= :no-audited-accepted-use-record (:later-use-disposition %)) records))
         :identity-conflicts (count conflicts)
         :self-review-conflicts (count self-reviews)
         :caption-independent-review :pending
         :publication :blocked-pending-reviewed-apparatus-admission}
        output {:schema :memory-caption-cohort-candidate/v1
                :status :draft-not-published
                :coverage coverage
                :records records}]
    (check! (= 4 (count snapshots)) "Expected four terminal f211 role snapshots")
    (check! (= 57 (count records)) "Expected exactly 57 audited cohort memories")
    (check! (= 57 (count cohort-ids)) "Cohort memory IDs must be unique")
    (check! (= 3 (count enriched-records)) "Expected three source-read enrichments")
    (check! (set/subset? (set (map :memory-id enriched-records)) cohort-ids)
            "Every enrichment must name an audited cohort memory")
    (check! (set/subset? cohort-ids union-ids) "Every cohort memory must occur in the historical union")
    (check! (= cohort-ids readback-ids) "Read-back coverage must equal cohort coverage")
    (check! (empty? conflicts) "Snapshot identity/content/review variants conflict")
    (check! (empty? self-reviews) "Original memory author and reviewer must differ")
    (check! (= 1 (count (identity-conflicts [planted-memory planted-identity])))
            "Planted identity mismatch must be detected")
    (check! (= 1 (count (self-review-conflicts [planted-self-review])))
            "Planted self-review must be detected")
    (check! (every? #(str/starts-with? (:useful-when %) "Useful when ") records)
            "Every caption must use positive useful-when wording")
    (check! (every? #(<= (searchable-projection-bytes %) 1024)
                    records)
            "Every proposed searchable projection must fit 1,024 UTF-8 bytes")
    (check! (every? #(<= (searchable-projection-bytes
                           (-> (:six-field-draft %)
                               (assoc :useful-when
                                      (get-in % [:six-field-draft :text]))))
                          1024)
                    enriched-records)
            "Every source-read searchable projection must fit the implemented 1,024-byte limit")
    (check! (every? #(= :supported (get-in % [:independent-review :status])) records)
            "Every cohort memory must retain independent-review evidence")
    (doseq [record enriched-records
            evidence (get-in record [:six-field-draft :basis-evidence])]
      (check! (= (:sha256 evidence) (sha256-file (:path evidence)))
              (str "Source digest mismatch: " (:path evidence))))
    (write-edn! "historical-census.edn" historical)
    (write-edn! "cohort-57.edn" output)
    (write-edn! "source-read-enrichments.edn"
                {:schema :memory-caption-source-read-enrichment/v1
                 :status :draft-caption-review-pending
                 :records enriched-records})
    (write-edn! "coverage.edn" coverage)
    (println (pr-str coverage))))

(-main)
