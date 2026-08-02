(require '[cheshire.core :as json]
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def coding-path "holes/labs/M-memory-retrieval/coding-sections-20260731.json")
(def receipts-path "holes/labs/M-memory-retrieval/receipts-export-20260731-all-authors.edn")
(def output-path "holes/labs/M-memory-retrieval/load-bearing-candidates-20260731.jsonl")

(def coding (json/parse-string (slurp coding-path)))
(def receipts (:entries (edn/read-string (slurp receipts-path))))

;; The export is newest-first. Retain the newest recorded classification if a
;; job has more than one outcome receipt (the known job-448 duplicate).
(def outcome-by-job
  (reduce
   (fn [m entry]
     (let [body (:evidence/body entry)
           job (:job-id body)
           outcome (:outcome body)
           recorded (or (:classification outcome) (:result outcome))]
       (if (and job recorded (not (contains? m job)))
         (assoc m job (name recorded))
         m)))
   {}
   receipts))

(def prose-only
  {"invoke-1785467054646-455-b8371b8f"
   ["e-1ac936fb-04e8-460e-a710-37fac474401c"]
   "invoke-1785469907154-467-a2928a5f"
   ["e-codexpilot-descend-through-eLpNorm-rpow-before-lintegral-rewrite"]
   "invoke-1785470457961-468-ad547147"
   ["e-codexpilot-derive-integrable-from-nonzero-bochner-integral"
    "e-codexpilot-distinguish-ContDiff-top-analytic-from-ContDiff-infinity-smooth"]
   "invoke-1785471377736-471-acbcd206"
   ["e-codexpilot-bound-polynomial-sum-degree-by-a-common-summand-bound"]
   "invoke-1785473298737-474-6e1af56a"
   ["e-codexpilot-reduce-probability-kernel-L2-contraction-to-young"]})

(def legacy-lines
  ;; This early report names memories by title rather than evidence ID.
  {"e-9751e537-f5b7-4c40-a857-0c0b699b93a2" "inventory-assembly-dependencies-before-polishing-leaves"
   "e-dfea2de9-8979-4f8f-9343-caabb48487e6" "stop-research-after-repeated-young-api-miss"})

(defn usage-lines [section]
  (let [lines (str/split-lines section)]
    (vec (take-while #(not (re-find #"^(## Final Summary|Completed |Blocked |Compiling partial |Committed )" %)) lines))))

(defn verbatim-for [section memory-id]
  (let [lines (usage-lines section)
        needles (cond-> [memory-id] (legacy-lines memory-id) (conj (legacy-lines memory-id)))
        indexes (keep-indexed #(when (some (fn [needle] (str/includes? %2 needle)) needles) %1) lines)]
    (when-not (= 1 (count indexes))
      (throw (ex-info "Memory reference did not resolve uniquely"
                      {:memory-id memory-id :matches (count indexes)})))
    (let [i (first indexes)
          line (lines i)
          ;; A few reports use a plural lead followed by bare nested ID bullets.
          ;; Preserve the surrounding verbatim sentences in those cases.
          bare? (boolean (re-matches #"\s*-\s*`[^`]+`\s*" line))
          selected (if bare?
                     (filterv some? [(get lines (dec i)) line (get lines (inc i))])
                     [line])]
      (str/join "\n" selected))))

(def rows
  (->> coding
       (mapcat
        (fn [row]
          (let [job (get row "job-id")
                ids (get row "used-ids")
                source (if (seq ids) "used-ids" "prose-only")
                ids (if (seq ids) ids (get prose-only job []))]
            (for [memory-id ids]
              (array-map
               "receipt / job id" job
               "problem id" (get row "problem")
               "memory id" memory-id
               "runner verbatim" (verbatim-for (get row "section") memory-id)
               "recorded outcome" (or (get outcome-by-job job)
                                       (throw (ex-info "No recorded outcome" {:job job})))
               "source" source)))))
       (sort-by (juxt #(get % "receipt / job id") #(get % "memory id")))))

(when-not (= 49 (count rows))
  (throw (ex-info "Unexpected population size" {:count (count rows)})))
(when-not (= {"used-ids" 43 "prose-only" 6} (frequencies (map #(get % "source") rows)))
  (throw (ex-info "Unexpected source split" {})))

(spit output-path (str (str/join "\n" (map json/generate-string rows)) "\n"))
(println (count rows) (frequencies (map #(get % "source") rows)) output-path)
