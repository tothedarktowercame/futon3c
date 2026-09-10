(ns build
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def root (io/file "."))
(def audit-dir
  (io/file root "holes/labs/M-apm-demonstration/analysis/memory-audit-2026-09-10"))
(def snapshot-dir
  (io/file root "data/apm-campaigns/jit-all-open-v3/jit-all-open-v3-f211/snapshots"))
(def out-dir
  (io/file root "holes/labs/M-apm-demonstration/analysis/memory-caption-history-candidate-2026-09-10"))

(defn read-json [name]
  (json/parse-string (slurp (io/file audit-dir name)) true))

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

(defn later-observation [row]
  {:observation/type :accepted-use
   :target {:frame-id (:frame row)
            :problem-id (:problem row)
            :attempt (:attempt row)
            :source (:source row)}
   :transfer-stratum (keyword (:transfer-stratum row))
   :memory-use/kind (keyword (:memory-use-kind row))
   :epistemic-status (case (:verdict row)
                       "fingerprinted" :supported
                       :unknown)
   :audit-verdict (keyword (:verdict row))
   :evidence-note (case (:verdict row)
                    "fingerprinted" "Rare named tokens from the memory occur in the retained attempt source."
                    "already-in-base" "The accepted use was recorded, but the relevant tokens were already present in the base file; contribution is not attributable."
                    "unwitnessed" "The accepted use was recorded, but this token audit found no rare-token witness."
                    "no-source" "The accepted use was recorded, but no retained attempt source was available to this audit."
                    "not-adjudicable-by-token" "The accepted use is regulative or otherwise not adjudicable by source-token overlap."
                    "The accepted use exists, but its applicability status is unknown in this audit.")})

(defn cohort-record [row]
  (let [id (:memory-id row)
        memory (get memories-by-id id)
        uses (mapv later-observation (get fingerprint-by-memory id []))
        source-attempts (:source-attempts memory)]
    {:schema :memory-caption-candidate/v1
     :status :draft-not-published
     :memory-id id
     :original-content-digest (:content-digest memory)
     :caption {:text (positive-caption memory)
               :epistemic-status :supported
               :basis :original-reviewed-memory}
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
     (into [{:observation/type :source-use
             :target {:frame-id (get-in memory [:provenance :frame-id])
                      :problem-id (get-in memory [:provenance :problem-id])
                      :attempt-evidence-ids (vec source-attempts)}
             :epistemic-status (if (seq source-attempts) :supported :unknown)
             :useful-when (positive-caption memory)
             :evidence-note "The reviewed memory and its source-attempt links support this source-context applicability; this does not claim that the memory closed the whole source problem."}]
           uses)
     :later-use-disposition
     (if (seq uses)
       :audited-accepted-use-records-attached
       :no-audited-accepted-use-record)
     :condition-claims
     [{:text "No additional prerequisite claim was inferred beyond the original reviewed hook and body."
       :epistemic-status :unknown}]}))

(defn write-edn! [name value]
  (spit (io/file out-dir name)
        (binding [*print-length* nil *print-level* nil]
          (str (pr-str value) "\n"))))

(defn check! [pred message]
  (when-not pred (throw (ex-info message {}))))

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
        coverage
        {:schema :memory-caption-coverage/v1
         :expected 57
         :accounted (count records)
         :caption-drafted (count records)
         :source-attempt-supported (count (filter #(= :supported (get-in % [:source :attempt-evidence-status])) records))
         :source-attempt-unknown (count (filter #(= :unknown (get-in % [:source :attempt-evidence-status])) records))
         :independent-review-supported (count (filter #(= :supported (get-in % [:independent-review :status])) records))
         :with-audited-accepted-use (count (filter #(= :audited-accepted-use-records-attached (:later-use-disposition %)) records))
         :without-audited-accepted-use (count (filter #(= :no-audited-accepted-use-record (:later-use-disposition %)) records))
         :conflicts 0
         :publication :blocked-pending-reviewed-apparatus-admission}
        output {:schema :memory-caption-cohort-candidate/v1
                :status :draft-not-published
                :coverage coverage
                :records records}]
    (check! (= 4 (count snapshots)) "Expected four terminal f211 role snapshots")
    (check! (= 57 (count records)) "Expected exactly 57 audited cohort memories")
    (check! (= 57 (count cohort-ids)) "Cohort memory IDs must be unique")
    (check! (set/subset? cohort-ids union-ids) "Every cohort memory must occur in the historical union")
    (check! (= cohort-ids readback-ids) "Read-back coverage must equal cohort coverage")
    (check! (every? #(str/starts-with? (get-in % [:caption :text]) "Useful when ") records)
            "Every caption must use positive useful-when wording")
    (check! (every? #(= :supported (get-in % [:independent-review :status])) records)
            "Every cohort memory must retain independent-review evidence")
    (write-edn! "historical-census.edn" historical)
    (write-edn! "cohort-57.edn" output)
    (write-edn! "coverage.edn" coverage)
    (println (pr-str coverage))))

(-main)
