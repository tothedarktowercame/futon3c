#!/usr/bin/env bb
;; Read-only fixed cohort census. Run from futon3c; stdout is sanitized JSON.
;; Does not query live services, execute agents, or copy packet credentials.
(require '[clojure.edn :as edn] '[clojure.java.io :as io]
         '[clojure.string :as str] '[cheshire.core :as json])
(def root "data/apm-campaigns/jit-all-open-v3")
(def pins (atom (sorted-map)))
(defn bytes-at [f]
  (let [b (java.nio.file.Files/readAllBytes (.toPath (io/file f)))
        h (.digest (java.security.MessageDigest/getInstance "SHA-256") b)]
    (swap! pins assoc (str f) (format "%064x" (java.math.BigInteger. 1 h)))
    (String. b "UTF-8")))
(defn read-at [f] (when (.isFile (io/file f)) (edn/read-string (bytes-at f))))
(defn files-at [dir re]
  (sort-by str (filter #(and (.isFile %) (re-matches re (.getName %)))
                      (or (.listFiles (io/file dir)) []))))
(defn attempt [f]
  (let [a (read-at f) q (:request a) r (:receipt a)
        c (:memory-cascade q) u (:receipt/memory-use r)
        packet (str/replace (str f) #"\.edn$" "-packet.txt")
        packet-text (when (.isFile (io/file packet)) (bytes-at packet))
        offered (mapv :memory-id (:offers c)) used (:used-ids u)]
    {:path (str f) :state (:state/type a) :role (:role q)
     :frame (:frame-id q) :problem (:problem-id q)
     :attempt (:attempt-ordinal q) :outcome (:receipt/outcome r)
     :job-id (get-in a [:ticket :job-id])
     :packet-present (boolean packet-text)
     :offers-in-packet (when packet-text (every? #(str/includes? packet-text %) offered))
     :search-instruction (boolean (and packet-text (str/includes? packet-text "apm-search-memory.py")))
     :accessible (count (get-in q [:memory-snapshot :accessible-memory-ids]))
     :cascade-outcome (:outcome c) :offers (count offered) :offered-ids offered
     :offer-names (count (filter :offer/name (:offers c)))
     :offer-hooks (count (filter :offer/hook (:offers c)))
     :offer-routes (frequencies (map :route (:offers c)))
     :offer-body-present (count (filter #(not (str/blank? (:body %))) (:offers c)))
     :cascade-ms (:expansion-ms c) :truncated (:truncated? c)
     :used (vec used) :surfaced (count (:surfaced-ids u))
     :used-outside-surfaced (vec (remove (set (:surfaced-ids u)) used))
     :queries (count (:queries u))
     :cascade-used (vec (get-in r [:receipt/memory-cascade :used-via-cascade]))
     :solver-canary (boolean (:solver-shelf-canary q))
     :source-files (mapv (fn [sf] (bytes-at sf) (str sf))
                         (files-at (str/replace (str f) #"\.edn$" "-source") #".*\.lean"))}))
(defn frame [n]
  (let [id (str "f" n) dir (str root "/jit-all-open-v3-" id)
        snapshots (mapv read-at (files-at (str dir "/snapshots") #".*\.edn"))
        memories (mapcat :snapshot/memories snapshots)
        own (filter #(= id (get-in % [:provenance :frame-id])) memories)
        close (read-at (str dir "/live/close-frame.edn"))]
    {:frame id :close-state (:state/type close)
     :close-result (get-in close [:receipt :receipt/result])
     :snapshot-count (count snapshots)
     :snapshot-kinds (frequencies (mapcat #(map :memory-use/kind (:snapshot/memories %)) snapshots))
     :own-memories (vec (vals (into (sorted-map)
                               (map (fn [m] [(:memory-id m)
                                 (select-keys m [:memory-id :provenance :depositor :reviewer
                                                :review-evidence-id :materialization :memory-use/kind])]) own))))
     :attempts (mapv attempt (files-at (str dir "/live") #"student-attempt-\d+\.edn"))
     :solver (when (.isFile (io/file (str dir "/live/solve.edn")))
               (attempt (io/file (str dir "/live/solve.edn"))))}))
(def frames (mapv frame (range 190 214)))
(def searches
  (->> (files-at "data/apm-role-memory-searches/receipts" #".*\.edn")
       (keep (fn [f]
               ;; Non-cohort files are inspected for selection only, not basis pins.
               (let [r (edn/read-string (slurp f))]
                 (when (contains? (set (map :frame frames)) (:frame-id r))
                   (bytes-at f)
                   (assoc (select-keys r [:receipt/id :frame-id :problem-id :job-id :role :phase])
                          :results (count (:result-ids r))))))) vec))
(def after-stable
  (every? (fn [[f h]]
            (= h (format "%064x" (java.math.BigInteger. 1
                  (.digest (java.security.MessageDigest/getInstance "SHA-256")
                           (java.nio.file.Files/readAllBytes (.toPath (io/file f)))))))) @pins))
(println (json/generate-string {:schema "apm-memory-audit/v1" :cohort [190 213]
                               :frames frames :searches searches
                               :basis-stable after-stable :source-sha256 @pins} {:pretty true}))
(when-not after-stable (System/exit 2))
