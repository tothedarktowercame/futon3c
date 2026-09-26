(ns futon3c.apm.coined-pattern
  "Publish newly coined candidate patterns so an independent reviewer can
  inspect them. Publication makes a pattern proposed, never reviewed."
  (:require [clojure.string :as str]
            [futon3c.watcher.file-ingest :as file-ingest]))

(defn- wire-key [value]
  (if (keyword? value) (subs (str value) 1) value))

(defn pattern-entities
  [{:keys [depositor candidates new-pattern-rationales]}]
  (let [rationales (into {} (map (fn [[id rationale]]
                                   [(wire-key id) rationale]))
                         (or new-pattern-rationales {}))
        witnesses (reduce (fn [by-pattern candidate]
                            (reduce #(update %1 %2 (fnil conj [])
                                             (:memory-id candidate))
                                    by-pattern (:pattern-ids candidate)))
                          {} candidates)
        unwitnessed (->> (keys rationales)
                         (remove #(seq (get witnesses %)))
                         sort vec)
        invalid-rationales (->> rationales
                                (keep (fn [[id rationale]]
                                        (when-not (and (string? rationale)
                                                       (not (str/blank? rationale)))
                                          id)))
                                sort vec)]
    (cond
      (seq unwitnessed)
      {:ok false :findings [:pattern-without-witness]
       :pattern-ids unwitnessed}

      (seq invalid-rationales)
      {:ok false :findings [:pattern-rationale-invalid]
       :pattern-ids invalid-rationales}

      :else
      {:ok true
       :entities
       (mapv (fn [[id rationale]]
               {:id id :name id :type "pattern/library" :external-id id
                :source rationale
                :props {"attachment-status" "proposed"
                        "pattern/coiner" depositor
                        "pattern/rationale" rationale
                        "pattern/witness-memory-ids" (vec (get witnesses id))}})
             (sort-by key rationales))})))

(defn publish!
  [deposit]
  (let [{:keys [ok entities] :as planned} (pattern-entities deposit)]
    (cond
      (not ok) planned
      (empty? entities) {:ok true :entities []}
      :else
      (try
        (let [result (file-ingest/post-entities-batch! entities)]
          {:ok true :entities (:entities result)})
        (catch Exception error
          {:ok false :error/code :coined-pattern-publication-failed
           :error/message (.getMessage error)})))))

(defn file-pattern-entities
  "Parse an explicit coined-pattern deposit file. The file itself is the
  durable witness for these historical patterns."
  [path coiner]
  (let [text (slurp path)
        sections (rest (str/split text #"(?m)^##\s+"))]
    (mapv
     (fn [section]
       (let [[heading & body-lines] (str/split-lines section)
             id (str/trim heading)
             rationale (str/trim (str/join "\n" body-lines))]
         {:id id :name id :type "pattern/library" :external-id id
          :source rationale
          :props {"attachment-status" "proposed"
                  "pattern/coiner" coiner
                  "pattern/rationale" rationale
                  "pattern/witness-deposits" [path]}}))
     sections)))

(defn publish-file!
  [path coiner]
  (let [entities (file-pattern-entities path coiner)
        result (file-ingest/post-entities-batch! entities)]
    {:ok true :count (:count result) :entities (:entities result)}))
