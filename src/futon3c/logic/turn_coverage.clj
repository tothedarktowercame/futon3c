(ns futon3c.logic.turn-coverage
  "Turn-coverage invariant in core.logic: every operator turn gets a 象 record.

   M-repl-circus, section \"Two invariants for the Circus\" (Joe, 2026-11-11),
   invariant 2: \"the REPL circus runs on all turns whether or not there is a
   stepper attached ... enforced with core.logic\". Witness case: 34 of Joe's
   turns to claude-13 between 2026-11-10 17:39 and 2026-11-11 02:00 got no
   象 record when an Emacs display change switched capture off (fixed in
   futon4 1e786e2dd); nothing noticed for eight hours.

   A flat fact base over two read-only sources, in the style of xiang.clj:

   - Operator turns (futon1b evidence, author joe, body event \"chat-turn\",
     role \"user\"):  turn id turn-id session agent at
       id      the evidence id
       turn-id e.g. \"claude-13-turn-287\"; the agent is derived from it by
               the fact producer (operator_turn_capture.py's rule:
               turn-id.rsplit(\"-turn-\", 1)[0])
       at      ISO-8601 string (valid time; parsed to Instant for compares)
   - harness id         a turn the harness posted in Joe's name (park wake,
                        clock reminder; markers live in
                        resources/futon3c/turn-coverage/harness-markers.txt).
                        Not an operator turn; no record is expected.
   - record evidence-id a 象 turn record
                        (~/.emacs-graph/session-turn-analysis/turn-*.json)
                        naming the turn's evidence id
   - record-turn turn-id  a record naming the turn-id (fallback cover, as
                        operator_turn_capture.py's skip rule does)

   The core relation is `uncoveredo`: an operator turn, not harness, in the
   agent scope, older than GRACE at the as-of time, with no covering record.
   Violations are listed, never silently dropped."
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer [== fresh run* conde]]
            [clojure.core.logic.pldb :as pldb]
            [clojure.string]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.time Instant Duration]))

;; ---------------------------------------------------------------------------
;; Facts

(pldb/db-rel turn ^:index id turn-id session agent at)
(pldb/db-rel harness ^:index id)
(pldb/db-rel record ^:index evidence-id)
(pldb/db-rel record-turn ^:index turn-id)

(def default-grace-ms
  "A turn younger than this at the as-of time is not yet a violation: capture
   and reconcile get a chance first."
  (* 10 60 1000))

;; ---------------------------------------------------------------------------
;; Fact base

(defn harness-markers
  "The shared marker list (resources/futon3c/turn-coverage/harness-markers.txt),
   one marker per line, '#' comments dropped. A turn whose text contains any
   marker is the harness speaking in Joe's name, not an operator turn."
  []
  (->> (line-seq (io/reader (io/resource "futon3c/turn-coverage/harness-markers.txt")))
       (remove #(or (clojure.string/blank? %) (clojure.string/starts-with? % "#")))
       vec))

(defn harness-text?
  "True when TEXT carries a harness marker."
  [text]
  (boolean (and text (some #(clojure.string/includes? text %) (harness-markers)))))

(defn facts
  "Fact vectors from maps.
   TURNS:   {:id :turn-id :session :agent :at}     operator + harness turns
   HARNESS: set/seq of evidence ids that are harness turns
   RECORDS: {:evidence-id :turn-id}                象 turn records"
  [{:keys [turns records] :as m}]
  (let [harness-ids (set (:harness m))]
    (into []
          (concat
           (for [{:keys [id turn-id session agent at]} turns]
             [turn id turn-id session agent at])
           (for [id harness-ids] [harness id])
           (for [{:keys [evidence-id]} records :when evidence-id]
             [record evidence-id])
           (for [{:keys [turn-id]} records :when turn-id]
             [record-turn turn-id])))))

(defn db
  "A pldb database from a {:turns .. :harness .. :records ..} map (or from
   fact vectors, as load-fixture yields)."
  [fact-source]
  (apply pldb/db
         (if (map? fact-source) (facts fact-source) fact-source)))

(defn load-fixture
  "An EDN fixture: {:name .. :as-of .. :agents [..] :turns [..] :harness [..]
                    :records [..] :expect {..}}."
  [path]
  (edn/read-string (slurp (io/file path))))

;; ---------------------------------------------------------------------------
;; Relations

(defn operator-turno
  "ID is an operator turn (in the fact base; harness turns are excluded by
   uncoveredo, not here)."
  [id]
  (fresh [tid session agent at]
    (turn id tid session agent at)))

(defn coveredo
  "Turn ID has a 象 record: a record names its evidence id, or (fallback, as
   operator_turn_capture.py's skip rule) a record names its turn-id."
  [id]
  (fresh [tid session agent at]
    (turn id tid session agent at)
    (conde
     [(record id)]
     [(record-turn tid)])))

(defn old-enougho
  "Turn ID was done at or before AS-OF minus GRACE-MS."
  [id as-of grace-ms]
  (fresh [tid session agent at]
    (turn id tid session agent at)
    (l/project [at as-of grace-ms]
      (== true (not (.isAfter (Instant/parse at)
                              (.minus ^Instant as-of
                                      (Duration/ofMillis grace-ms))))))))

(defn uncoveredo
  "ID is an operator turn, not the harness speaking in Joe's name, in AGENTS
   scope, older than GRACE-MS at AS-OF, with no covering 象 record."
  [id as-of grace-ms agents]
  (fresh [tid session agent at]
    (turn id tid session agent at)
    (l/membero agent (seq agents))
    (l/nafc harness id)
    (old-enougho id as-of grace-ms)
    (l/nafc coveredo id)))

;; ---------------------------------------------------------------------------
;; Query API

(defn- turn-rows
  "All turns of AGENTS (harness turns included; callers exclude them) as maps."
  [db agents]
  (pldb/with-db db
    (run* [q]
      (fresh [id tid session agent at]
        (turn id tid session agent at)
        (l/membero agent (seq agents))
        (== q {:id id :turn-id tid :session session :agent agent :at at})))))

(defn uncovered
  "Operator turns of AGENTS with no 象 record, older than GRACE-MS (default
   10 min) at AS-OF (an Instant, or an ISO string). Oldest first."
  [db {:keys [agents as-of grace-ms]
       :or {grace-ms default-grace-ms}}]
  (let [as-of (if (string? as-of) (Instant/parse as-of) as-of)
        rows (pldb/with-db db
               (run* [q]
                 (fresh [id tid session agent at]
                   (uncoveredo id as-of grace-ms (set agents))
                   (turn id tid session agent at)
                   (== q {:id id :turn-id tid :session session
                          :agent agent :at at}))))]
    (vec (sort-by :at rows))))

(defn coverage-summary
  "Per agent: operator turns in scope (harness turns excluded), how many have
   a 象 record, how many are uncovered (see `uncovered`), and the newest
   uncovered turn."
  [db {:keys [agents] :as opts}]
  (let [agents (set agents)
        all (turn-rows db agents)
        harness-ids (set (pldb/with-db db (run* [id] (harness id))))
        turns (remove #(harness-ids (:id %)) all)
        covered-ids (set (pldb/with-db db
                           (run* [id]
                             (fresh [tid session agent at]
                               (turn id tid session agent at)
                               (l/membero agent (seq agents))
                               (coveredo id)))))
        uncovered (uncovered db opts)
        uncovered-by-agent (group-by :agent uncovered)]
    (into {}
          (for [agent (sort agents)]
            (let [ts (filter #(= agent (:agent %)) turns)
                  unc (get uncovered-by-agent agent [])
                  newest (when (seq unc) (:at (last unc)))]
              [agent {:turns (count ts)
                      :covered (count (filter #(covered-ids (:id %)) ts))
                      :uncovered (count unc)
                      :newest-uncovered newest}])))))
