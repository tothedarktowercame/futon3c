(ns futon3c.giraffe.second-reading
  "Separate repair lane for corpus records with completion criteria mistaken for dependencies."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3c.giraffe.corpus-generation :as generation]
            [futon3c.giraffe.reread-worker :as worker])
  (:import [java.time LocalDate ZoneOffset]))

(def default-pointer
  "/home/joe/notes/wm-next/giraffe-reads-2026-11-04/corpus")
(def default-readers ["kimi-2" "kimi-3"])
(def default-outcomes-dir "/home/joe/code/storage/giraffe-second-reading")
(defonce ^:private outcomes-lock (Object.))

(defn- json-lines [file]
  (when (.isFile (io/file file))
    (keep #(when-not (str/blank? %) (json/parse-string % true))
          (str/split-lines (slurp file)))))

(defn- append-outcome! [file outcome]
  (locking outcomes-lock
    (spit file (str (json/generate-string outcome) "\n") :append true))
  outcome)

(defn current-member-and-record [pointer target-id]
  (let [resolved (generation/resolve-generation pointer)
        root (io/file (:path resolved))
        member (some #(when (= target-id (:id %)) %)
                     (json/parse-string (slurp (io/file root "members.json")) true))
        record (some #(when (= target-id (:id %)) %)
                     (mapcat json-lines
                             (filter #(and (.isFile %) (str/ends-with? (.getName %) ".jsonl"))
                                     (.listFiles root))))]
    (when-not (and member record)
      (throw (ex-info "Second-reading target absent from current generation"
                      {:kind :giraffe-second-reading/target-absent :target-id target-id})))
    {:generation resolved :member member :record record
     :checker (io/file root "check_edges.py")}))

(defn repair-prompt [target-id member record checker]
  (let [request {:target_id target-id :source_path (:path member)
                 :source_sha (:sha256 member)}]
    (str "Repair one existing Giraffe reading. Return ONLY the full corrected JSON object.\n\n"
         "Known defect: a first reader recorded this item's own unmet completion criteria, "
         "acceptance checks, or 'not met' items as unresolved depends-on/unblocks entries. "
         "Giraffe interprets those as missing prerequisites and incorrectly makes the item "
         "undetermined. Remove those entries. Keep every genuine link to OTHER work; do not "
         "drop real prerequisites. Fix nothing else unless the checker requires it.\n\n"
         "Target: " target-id "\nCurrent record verbatim:\n"
         (json/generate-string record) "\n\n"
         (worker/checker-contract checker) "\n\n"
         "Self-check: write the full record to " (worker/scratch-path request)
         " and run `python3 -I " (.getAbsolutePath (io/file checker)) " <scratch-file>` repeatedly "
         "until it prints 0 problems. Source path and sha must remain " (:path member) " and "
         (:sha256 member) ". Return the JSON record only; do not bell or whistle anyone.")))

(defn- links [record] (concat (:edges record) (:unresolved record)))
(defn- removed-links [before after]
  (let [kept (frequencies (links after))]
    (first
     (reduce (fn [[removed remaining] entry]
               (if (pos? (get remaining entry 0))
                 [removed (update remaining entry dec)]
                 [(conj removed (select-keys entry [:raw_target :reason])) remaining]))
             [[] kept] (links before)))))

(defn process-one!
  [{:keys [pointer generations-dir contact-fn outcomes-file max-attempts]
    :or {max-attempts 2}} seat target-id]
  (loop [attempt 1]
    (let [{:keys [member record checker]} (current-member-and-record pointer target-id)
          current-sha (generation/file-sha (:path member))
          base {:id target-id :seat seat :attempt attempt
                :before_edges (count (:edges record))
                :before_unresolved (count (:unresolved record))}]
      (if (not= current-sha (:sha256 member))
        (let [out (assoc base :publication_status "skipped"
                         :publication_reason "source-changed")]
          (append-outcome! outcomes-file out))
        (let [request {:target_id target-id :source_path (:path member)
                       :source_sha (:sha256 member)}
              response (contact-fn seat request (repair-prompt target-id member record checker))]
          (if-not (:ok response)
            (if (< attempt max-attempts)
              (recur (inc attempt))
              (let [out (assoc base :publication_status "failed"
                               :publication_reason (name (:reason response)))]
                (append-outcome! outcomes-file out)))
            (let [corrected (:record response)
                  result (generation/publish-generation!
                          {:pointer pointer :generations-dir generations-dir
                           :claim {:target-id target-id :source-path (:path member)
                                   :source-sha (:sha256 member) :old-sha (:sha256 member)}
                           :replacement corrected})
                  out (assoc base
                             :after_edges (count (:edges corrected))
                             :after_unresolved (count (:unresolved corrected))
                             :removed (removed-links record corrected)
                             :publication_status (name (:status result))
                             :publication_reason (some-> (:reason result) name))]
              (if (and (= :rejected (:status result)) (< attempt max-attempts))
                (recur (inc attempt))
                (append-outcome! outcomes-file out)))))))))

(defn run-second-readings!
  [{:keys [ids readers pointer outcomes-file contact-fn generations-dir]
    :or {readers default-readers pointer default-pointer}}]
  (let [assignments (map-indexed (fn [i id] [(nth readers (mod i (count readers))) id]) ids)
        per-seat (group-by first assignments)]
    (->> per-seat
         (mapv (fn [[seat jobs]]
                 (future (mapv (fn [[_ id]]
                                 (process-one! {:pointer pointer
                                                :generations-dir generations-dir
                                                :contact-fn contact-fn
                                                :outcomes-file outcomes-file}
                                               seat id)) jobs))))
         (mapcat deref)
         vec)))

(defn- arg-value [args flag]
  (second (drop-while #(not= flag %) args)))

(defn -main [& args]
  (let [ids-file (or (arg-value args "--ids")
                     (throw (ex-info "--ids is required" {:kind :ids-required})))
        readers (some-> (arg-value args "--readers") (str/split #",") seq vec)
        readers (or readers default-readers)
        pointer (or (arg-value args "--pointer") default-pointer)
        generations-dir (str (.getAbsolutePath (io/file pointer)) ".generations")
        outcomes-file (or (arg-value args "--outcomes")
                          (str default-outcomes-dir "/outcomes-"
                               (LocalDate/now ZoneOffset/UTC) ".jsonl"))
        kimi-repo (System/getenv "KIMI_TASK_REPO")]
    (when-not kimi-repo
      (throw (ex-info "KIMI_TASK_REPO is required" {:kind :kimi-task-repo-required})))
    (.mkdirs (.getParentFile (io/file outcomes-file)))
    (let [contact (fn [seat request prompt]
                    (worker/kimi-contact {:reader-seat seat :kimi-task-repo kimi-repo
                                          :caller "giraffe-second-reading"}
                                         request prompt))
          ids (->> (str/split-lines (slurp ids-file)) (map str/trim)
                   (remove #(or (str/blank? %) (str/starts-with? % "#"))) vec)]
      (doseq [result (run-second-readings! {:ids ids :readers readers :pointer pointer
                                            :generations-dir generations-dir
                                            :outcomes-file outcomes-file :contact-fn contact})]
        (println (json/generate-string result))))))
