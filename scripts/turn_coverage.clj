;; M-repl-circus invariant-2 report (read-only). Every operator turn gets a 象
;; record: lists operator turns (futon1b evidence, author joe, event
;; "chat-turn", role "user") with no covering record in
;; ~/.emacs-graph/session-turn-analysis/. Writes nothing.
;;
;;   clojure -M scripts/turn_coverage.clj \
;;     --agents claude-13,claude-9 --since 2026-11-10T16:00:00Z \
;;     [--grace-min 10] [--json]
;;
;; Fetches the evidence API newest-first in 200-entry pages with a pause
;; between pages, backing off on 503 (the fetch_since.py discipline), until
;; the page is older than --since. The records directory (tens of thousands
;; of files) is indexed once per run. Harness markers (park wakes, clock
;; reminders) come from the shared list
;; resources/futon3c/turn-coverage/harness-markers.txt (also read by
;; scripts/operator_turn_capture.py).

(require '[futon3c.logic.turn-coverage :as tc]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.java.io :as io])
(import '[java.net URI HttpURLConnection]
        '[java.time Instant])

(def F1B "http://127.0.0.1:7073")
(def RECORDS (str (System/getProperty "user.home")
                  "/.emacs-graph/session-turn-analysis"))
(def PAGE-SIZE 200)
(def PAGE-PAUSE-MS 2000)

;; ---------------------------------------------------------------------------
;; Args

(defn parse-args [args]
  (loop [m {:grace-min 10 :json false} [a & more] args]
    (if-not a m
      (case a
        "--agents" (recur (assoc m :agents (str/split (first more) #",")) (rest more))
        "--since" (recur (assoc m :since (first more)) (rest more))
        "--grace-min" (recur (assoc m :grace-min (Long/parseLong (first more))) (rest more))
        "--json" (recur (assoc m :json true) more)
        (do (binding [*out* *err*] (println "unknown arg:" a))
            (System/exit 2))))))

;; ---------------------------------------------------------------------------
;; Evidence fetch (GET only, 503 backoff)

(defn get-json [url]
  (loop [attempt 0]
    (let [^HttpURLConnection
          conn (doto (.openConnection (.toURL (URI/create url)))
                 (.setRequestProperty "Accept" "application/json")
                 (.setConnectTimeout 60000)
                 (.setReadTimeout 120000))
          code (.getResponseCode conn)]
      (if (and (= code 503) (< attempt 5))
        (do (binding [*out* *err*]
              (println (str "503, waiting " (* 60 (inc attempt)) "s")))
            (Thread/sleep (* 60000 (inc attempt)))
            (recur (inc attempt)))
        (if (= code 200)
          (json/parse-stream (io/reader (.getInputStream conn)))
          (throw (ex-info (str "evidence GET failed: " code)
                          {:url url :status code})))))))

(defn fetch-turns
  "Operator-relevant chat turns since SINCE (ISO string), for AGENTS."
  [agents since]
  (let [agents (set agents)]
    (loop [before nil out []]
      (let [url (str F1B "/api/alpha/evidence?author=joe&limit=" PAGE-SIZE
                     (when before (str "&before=" before)))
            page (get-json url)
            entries (get page "entries")
            rows (keep (fn [e]
                         (let [body (get e "evidence/body")]
                           (when (and (= "chat-turn" (get body "event"))
                                      (= "user" (get body "role")))
                             (let [turn-id (get body "turn-id")]
                               {:id (get e "evidence/id")
                                :turn-id turn-id
                                :session (get e "evidence/session-id")
                                :agent (some-> turn-id (str/split #"-turn-" 2) first)
                                :at (get e "evidence/at")
                                :text (get body "text")}))))
                       entries)
            ats (keep #(get % "evidence/at") entries)]
        (when (empty? ats)
          (throw (ex-info "empty evidence page" {:url url})))
        (let [oldest (reduce #(if (neg? (compare %1 %2)) %1 %2) ats)
              in-window (filter #(and (:turn-id %) (:at %) (agents (:agent %))) rows)
              stop? (and since (neg? (compare oldest since)))]
          (binding [*out* *err*]
            (println (str "page " (count entries) " oldest " oldest)))
          (if (or stop? (< (count entries) PAGE-SIZE))
            ;; stop: keep only entries at-or-after since
            (into out (filter #(not (neg? (compare (:at %) since))) in-window))
            (do (Thread/sleep PAGE-PAUSE-MS)
                (recur oldest (into out in-window)))))))))

;; ---------------------------------------------------------------------------
;; Record index (one pass over the directory)

(defn record-index []
  (let [dir (io/file RECORDS)
        files (filter #(re-matches #"turn-[^.]+\.json" (.getName ^java.io.File %))
                      (.listFiles dir))]
    (binding [*out* *err*] (println (str "indexing " (count files) " record files")))
    (into []
          (keep (fn [^java.io.File f]
                  (try (let [d (json/parse-stream (io/reader f))]
                         {:evidence-id (get d "evidence_id")
                          :turn-id (get d "turn_id")
                          :session-id (get d "session_id")})
                       (catch Exception _ nil))))
          files)))

;; ---------------------------------------------------------------------------
;; Main

(defn -main [& args]
  (let [{:keys [agents since grace-min json]} (parse-args args)
        _ (when-not (and agents since)
            (binding [*out* *err*]
              (println "usage: --agents a,b --since <iso> [--grace-min 10] [--json]"))
            (System/exit 2))
        as-of (Instant/now)
        turns (fetch-turns agents since)
        records (record-index)
        harness (into #{} (comp (filter #(tc/harness-text? (:text %)))
                                (map :id))
                      turns)
        db (tc/db {:turns (map #(dissoc % :text) turns)
                   :harness harness
                   :records records})
        opts {:agents agents :as-of as-of :grace-ms (* grace-min 60 1000)}
        summary (tc/coverage-summary db opts)
        uncovered (tc/uncovered db opts)]
    (if json
      (println (json/generate-string
                {:as-of (str as-of) :since since :grace-min grace-min
                 :agents agents :summary summary
                 :uncovered uncovered}
                {:pretty true}))
      (do
        (println (str "turn coverage as of " as-of
                      " (grace " grace-min " min, since " since ")"))
        (doseq [[agent s] summary]
          (println (format "  %-12s turns %4d  covered %4d  uncovered %3d%s"
                           agent (:turns s) (:covered s) (:uncovered s)
                           (if (:newest-uncovered s)
                             (str "  newest uncovered " (:newest-uncovered s))
                             ""))))
        (when (seq uncovered)
          (println "uncovered (oldest first):")
          (doseq [u uncovered]
            (println (format "  %s  %-24s %s  %s"
                             (:at u) (:agent u) (:turn-id u) (:id u)))))))))

(apply -main *command-line-args*)
