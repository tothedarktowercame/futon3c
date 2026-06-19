(ns scripts.backfill-block-mana
  "One-shot backfill: walk the 14-repo manifest's recent commits, find
   every commit with a `Block:` trailer, attribute it to a session via
   the timestamp-window heuristic, and call
   `nonstarter.db/credit-block-mana!` (idempotent on commit-sha).

   Run via Drawbridge eval (preferred — uses the running JVM's classpath
   and existing nonstarter ds), e.g.:

     curl -sS -H \"x-admin-token: $TOKEN\" \\
          --data '(load-file \"/home/joe/code/futon3c/scripts/backfill_block_mana.clj\")
                  (scripts.backfill-block-mana/run! {:days 30})' \\
          http://localhost:6768/eval

   Or pass {:dry-run? true} to see what would be credited without
   touching the ledger.

   Strict idempotency: re-running is safe — credits are keyed on full
   commit sha and the second attempt for any sha is a no-op. So this
   script can be re-run safely as the source of truth as the wire-up
   matures."
  (:require [clojure.string :as str]
            [futon3c.watcher.commit-ingest :as ci]))

(def ^:private repos
  "Same manifest as futon2.report.war-machine + the bb mana-snapshot
   script. Kept here as a literal so the backfill is self-contained."
  (let [home (System/getProperty "user.home")]
    [{:label "futon0"  :path (str home "/code/futon0")}
     {:label "futon1"  :path (str home "/code/futon1")}
     {:label "futon1a" :path (str home "/code/futon1a")}
     {:label "futon2"  :path (str home "/code/futon2")}
     {:label "futon3"  :path (str home "/code/futon3")}
     {:label "futon3a" :path (str home "/code/futon3a")}
     {:label "futon3b" :path (str home "/code/futon3b")}
     {:label "futon3c" :path (str home "/code/futon3c")}
     {:label "futon4"  :path (str home "/code/futon4")}
     {:label "futon5"  :path (str home "/code/futon5")}
     {:label "futon5a" :path (str home "/code/futon5a")}
     {:label "futon6"  :path (str home "/code/futon6")}
     {:label "futon7"  :path (str home "/code/futon7")}
     {:label "futon7a" :path (str home "/code/futon7a")}]))

(defn- since-iso [days]
  (-> (java.time.Instant/now)
      (.minus (java.time.Duration/ofDays (long days)))
      str))

(defn- collect-block-commits
  "Walk all repos and return a single seq of {:repo :sha :ts :block} maps
   for every commit in the last DAYS days that carries a Block: trailer.
   Sorted by timestamp ascending so credits land in chronological order."
  [days]
  (let [since (since-iso days)]
    (->> repos
         (filter #(.exists ^java.io.File (java.io.File. ^String (:path %))))
         (mapcat (fn [{:keys [label path]}]
                   (let [text (apply ci/run-git path
                                     ["log" "--all" "--no-merges"
                                      (str "--since=" since)
                                      ;; field-sep U+001F, record-sep U+001E
                                      (str "--format=%H" "\u001f" "%at" "\u001f" "%B" "\u001e")])]
                     (->> (str/split (or text "") (re-pattern "\u001e"))
                          (map str/trim)
                          (remove str/blank?)
                          (keep (fn [chunk]
                                  (let [[sha ts body] (str/split chunk
                                                                  (re-pattern "\u001f")
                                                                  3)]
                                    (when-let [block (ci/parse-block-trailer body)]
                                      {:repo label
                                       :sha sha
                                       :ts (parse-long (or ts "0"))
                                       :block block}))))))))
         (sort-by :ts)
         vec)))

(defn run!
  "Backfill block mana for the last DAYS days. Default 30.

   opts: {:days 30 :dry-run? false}.

   Returns {:total-blocks N
            :credited [{:sha :session-id :balance ...} ...]
            :skipped  [{:sha :reason ...} ...]}."
  ([] (run! {}))
  ([{:keys [days dry-run?] :or {days 30 dry-run? false}}]
   (let [blocks (collect-block-commits days)
         results (atom {:credited [] :skipped []})]
     (println (str "[backfill] " (count blocks) " Block-footered commits in last " days "d"))
     (doseq [{:keys [repo sha ts block]} blocks]
       (let [r (if dry-run?
                 {:credited :dry-run
                  :session-id (when ts
                                (ci/resolve-session-for-commit (* 1000 (long ts))))
                  :commit-sha sha
                  :block block
                  :repo repo}
                 (ci/credit-block-mana-for-commit!
                  {:sha sha :ts ts :block block}
                  :verbose? true))]
         (cond
           (:credited r)
           (swap! results update :credited conj
                  {:sha sha :repo repo :session-id (:session-id r)
                   :balance (:balance r) :block-tag (:tag block)})
           :else
           (swap! results update :skipped conj
                  {:sha sha :repo repo :reason (:reason r)
                   :block-tag (:tag block)}))))
     (let [{:keys [credited skipped]} @results]
       (println (str "[backfill] credited: " (count credited)
                     "  skipped: " (count skipped)))
       (println (str "[backfill] skip reasons: "
                     (frequencies (map :reason skipped))))
       {:total-blocks (count blocks)
        :credited credited
        :skipped skipped}))))

(defn dry-run!
  "Convenience: see what would happen without touching the ledger."
  ([] (dry-run! {}))
  ([opts] (run! (assoc opts :dry-run? true))))
