(ns futon3c.inbox-zero.infer-adapters
  "Fail-closed IO adapters for pure inbox-zero attribution inference.

  This slice normalizes only current inbox-zero state, bounded substrate
  session evidence, and filesystem mtime. Git and transcript adapters remain
  deliberately absent. Every path join uses worktree id plus path."
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as inbox-state])
  (:import [java.net URLEncoder]
           [java.time Instant]
           [java.util Date]))

(def default-window-ms 7200000)

(defn- epoch-ms [value]
  (try
    (cond
      (integer? value) (long value)
      (instance? Date value) (.getTime ^Date value)
      (instance? Instant value) (.toEpochMilli ^Instant value)
      (string? value) (.toEpochMilli (Instant/parse value))
      :else nil)
    (catch Exception _ nil)))

(defn- iso [milliseconds]
  (.toString (Instant/ofEpochMilli milliseconds)))

(defn- normalized-time [value]
  (some-> (epoch-ms value) Date.))

(defn- encoded [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn- seat-identity [seat-id]
  (when-let [[_ agent session] (and (string? seat-id)
                                    (re-matches #"seat:([^:]+):(.+)" seat-id))]
    {:agent-id agent :session-id session}))

(defn- default-stat [absolute-path]
  (let [file (io/file absolute-path)]
    (when (.exists file)
      {:at (Date. (.lastModified file))
       :source/id (str "stat:" absolute-path)})))

(defn- valid-stat [value]
  (when (and (map? value) (epoch-ms (:at value))
             (string? (:source/id value)) (not (str/blank? (:source/id value))))
    (assoc value :at (normalized-time (:at value)))))

(defn- body-names-path? [body path]
  (and (string? path)
       (str/includes? (if (string? body) body (pr-str body)) path)))

(defn- evidence-author [entry]
  (some-> (:evidence/author entry) str))

(def ^:private max-evidence-pages
  "Safety stop for cursor following. 1000 entries a page, so this bounds one
  session window at 20k entries rather than looping on a pathological cursor."
  20)

(defn- fetch-session-evidence
  "Every evidence entry for SESSION-ID in the window, following the cursor.

  The substrate serves ONE BOUNDED PAGE per request and returns :next-cursor
  when more remain (futon1b `query-evidence-response`: \"Continue with the
  returned :next-cursor map as cursor-at/cursor-id\"). This ignored it and kept
  only the first page, so any session with more than `limit` entries in the
  window was silently truncated -- and truncation here is not a smaller answer
  but a wrong one, because `session-adaptation` derives its activity window
  from the first and last entry. A dropped tail moves :to to a time the session
  did not end at.

  Paging is followed only when the substrate says there is more. Measured
  2026-09-07 against a 640-entry window: limit=1000 returns all 640 with no
  cursor, so the ordinary case is exactly one request, as before. That matters
  because each request costs a fixed ~850ms floor at futon1b regardless of
  size, and futon1b's permit pool is two -- shared with the promotion's
  visibility reads, whose per-read bound is 5000ms of wall clock including
  queue wait. Paging unconditionally at limit=100 would have been ~7 requests
  and ~4x the gate occupancy for the same rows, which is why the page size
  stays at the maximum and the cursor is followed only when it appears."
  [http-get-fn substrate-url session-id since-ms before-ms]
  (try
    (let [base (str (str/replace substrate-url #"/$" "")
                    "/api/alpha/evidence?session-id=" (encoded session-id)
                    "&since=" (encoded (iso since-ms))
                    "&before=" (encoded (iso before-ms))
                    "&limit=1000")]
      (loop [cursor nil pages 0 acc []]
        (let [url (if cursor
                    (str base "&cursor-at=" (encoded (:at cursor))
                         "&cursor-id=" (encoded (:id cursor)))
                    base)
              response (http-get-fn url {:headers {"Accept" "application/edn"}
                                         :throw false})]
          (if-not (= 200 (:status response))
            ;; A failed page is not a short answer: return nil so the caller
            ;; treats it as no evidence rather than as a truncated window.
            (when (seq acc) nil)
            (let [parsed (when (string? (:body response))
                           (edn/read-string (:body response)))
                  entries (:entries parsed)
                  next-cursor (:next-cursor parsed)
                  acc (if (sequential? entries) (into acc entries) acc)]
              (if (and (map? next-cursor) (:at next-cursor) (:id next-cursor)
                       (< (inc pages) max-evidence-pages)
                       (sequential? entries) (seq entries))
                (recur next-cursor (inc pages) acc)
                (->> acc
                     (filter (fn [entry]
                               (when-let [at-ms (epoch-ms (:evidence/at entry))]
                                 (<= since-ms at-ms before-ms))))
                     vec)))))))
    (catch Exception _ nil)))

(defn- session-adaptation
  [path-fact http-get-fn substrate-url since-ms before-ms seat-id]
  (let [{:keys [agent-id session-id]} (seat-identity seat-id)
        entries (when (and agent-id session-id)
                  (fetch-session-evidence http-get-fn substrate-url session-id
                                          since-ms before-ms))]
    (if-not (seq entries)
      {:mentions [] :windows []}
      (let [ordered (sort-by (comp epoch-ms :evidence/at) entries)
            window-id (str "substrate-session-window:" session-id ":"
                           (:evidence/at (first ordered)) ":"
                           (:evidence/at (last ordered)))
            window {:seat/id seat-id
                    :worktree/id (:worktree/id path-fact)
                    :from (normalized-time (:evidence/at (first ordered)))
                    :to (normalized-time (:evidence/at (last ordered)))
                    :source/id window-id}
            mentions
            (->> ordered
                 (filter #(body-names-path? (:evidence/body %) (:path path-fact)))
                 (mapv (fn [entry]
                         {:seat/id seat-id
                          :worktree/id (:worktree/id path-fact)
                          :path (:path path-fact)
                          :at (normalized-time (:evidence/at entry))
                          :source/id (:evidence/id entry)
                          :candidate-authored? (= agent-id (evidence-author entry))
                          ;; Substrate evidence bodies are turn-level text
                          ;; (prompt, result preview): a path named there is
                          ;; context, not a write the seat issued. Never
                          ;; attested; the inferrer therefore treats it as
                          ;; :weak at most. Attested mentions will come from
                          ;; tool-input adapters (shell-write witnessing).
                          :attested? false
                          :evidence/kind :turn-text})))]
        {:mentions mentions :windows [window]}))))

(defn build-evidence-bundle
  "Build normalized inference evidence for PATH-FACT.

  Substrate reads are bounded to ±WINDOW-MS around the observed filesystem
  mtime. Without that temporal anchor they are skipped. Missing, malformed, or
  unavailable sources contribute empty vectors and never escape as errors."
  [path-fact options]
  (let [load-state-fn (or (:load-state-fn options) inbox-state/load-state)
        stat-fn (or (:stat-fn options) default-stat)
        http-get-fn (or (:http-get-fn options) http/get)
        state-path (:state-path options)
        substrate-url (or (:substrate-url options)
                          (System/getenv "FUTON_SUBSTRATE_URL")
                          "http://127.0.0.1:7073")
        window-ms (or (:window-ms options) default-window-ms)
        empty-bundle {:same-worktree-claims [] :substrate-mentions []
                      :activity-windows [] :mtimes []}]
    (try
      (let [state (load-state-fn state-path)
            observation (get (projection/current-observations state)
                             [(:worktree/id path-fact) (:path path-fact)])]
        (if-not observation
          empty-bundle
          (let [claims (->> (projection/current-claims state)
                            vals
                            (filter #(and (= :active (:state %))
                                          (= (:worktree/id path-fact)
                                             (:worktree/id %))))
                            (sort-by (juxt :seat/id :last-observed-at :claim/id))
                            vec)
                normalized-claims
                (mapv (fn [claim]
                         {:seat/id (:seat/id claim)
                         :worktree/id (:worktree/id claim)
                         :at (normalized-time (:last-observed-at claim))
                         :source/id (:claim/id claim)})
                      claims)
                absolute-path (.getPath (io/file (:repo/root observation)
                                                  (:path path-fact)))
                stat (try (valid-stat (stat-fn absolute-path))
                          (catch Exception _ nil))
                mtimes (if stat
                         [(assoc stat :worktree/id (:worktree/id path-fact)
                                      :path (:path path-fact))]
                         [])
                mtime-ms (some-> stat :at epoch-ms)
                adaptations
                (if-not mtime-ms
                  []
                  (let [since-ms (- mtime-ms window-ms)
                        before-ms (+ mtime-ms window-ms)]
                    (mapv #(session-adaptation path-fact http-get-fn substrate-url
                                               since-ms before-ms %)
                          (sort (distinct (map :seat/id claims))))))]
            {:same-worktree-claims normalized-claims
             :substrate-mentions (->> adaptations (mapcat :mentions)
                                         (sort-by (juxt :seat/id :at :source/id)) vec)
             :activity-windows (->> adaptations (mapcat :windows)
                                     (sort-by (juxt :seat/id :from :source/id)) vec)
             :mtimes mtimes})))
      (catch Exception _ empty-bundle))))
