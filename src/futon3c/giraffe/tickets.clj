(ns futon3c.giraffe.tickets
  "M-giraffe stage 1, packet 2: the holes/tickets source adapter.

  Turns the tickets index (holes/tickets/tickets-index.edn) plus an
  edges file (the VERIFY edges.json shape: tickets / edges / rejected)
  into the tracker-neutral pass input (futon3c.giraffe.pass).

  §DERIVE (2026-10-27): every edge records how it was obtained.  All
  edges this adapter reads are :source :prose-reading, so each carries
  its verbatim quote with file and line, and a computed check that the
  quote occurs at that line (the cited line or the next five,
  whitespace-normalised — as VERIFY's check_edges.py).  An edge whose
  quote fails the check is rejected (:edge/quote-not-at-line), never
  passed on.

  Status -> state group, as pilot 1 mapped, plus the statuses added
  since: :open, :scoped, :blocked -> :unstarted (a :blocked ticket
  keeps its declared status on the item as :declared-status, because
  \"blocked\" is what the pass computes and the two must be
  comparable).  An unknown status is not guessed: the item gets
  :state-group :unstarted plus :status-unmapped, and is listed in the
  result.

  Edge kinds: \"needs\" only; \"precedes\" is out of scope for stage 1
  (:edge/precedes-not-in-stage-1).  to_kind \"cluster\" means a ticket
  target (must be among the items, else :edge/dangling-target);
  anything else is an external need, carried as
  {:kind :external :to <string>} and unmet."
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private status->group
  {:done-in-fact :completed
   :partial :started
   :superseded :cancelled
   :still-open :unstarted
   :parked :backlog
   :watch :backlog
   :open :unstarted
   :scoped :unstarted
   :blocked :unstarted})

(defn- ticket-id [filename]
  (keyword (str/replace filename #"\.md$" "")))

(defn- read-index [index-file]
  (with-open [r (java.io.PushbackReader. (io/reader index-file))]
    (let [rows (edn/read r)]
      (when-not (vector? rows)
        (throw (ex-info "tickets index is not a vector"
                        {:kind :giraffe/index-not-vector
                         :path (str index-file)})))
      (vec (keep (fn [row] (when (map? row)
                             (when-let [t (:ticket row)]
                               {:id (ticket-id t)
                                :filename t
                                :status (:status row)})))
                 rows)))))

(defn- normalize-only [only]
  (when only
    (set (map (fn [x] (keyword (str/replace (name (keyword x)) #"\.md$" ""))) only))))

(defn- items-from-index
  "One item per index row that survives :only.  :source-order is the
  row's position in the index.  Missing ticket files are typed rejects
  (:index/missing-file), not items."
  [rows only dir]
  (reduce
   (fn [{:keys [items rejects unmapped]} [order {:keys [id filename status]}]]
     (let [f (io/file dir filename)]
       (cond
         (and only (not (contains? only id)))
         {:items items :rejects rejects :unmapped unmapped}

         (not (.isFile f))
         {:items items
          :rejects (conj rejects {:reject :index/missing-file
                                  :ticket filename :status status})
          :unmapped unmapped}

         :else
         (let [group (get status->group status)]
           {:items (conj items (cond-> {:id id
                                        :state-group (or group :unstarted)
                                        :source-order order
                                        :declared-status status}
                                 (nil? group) (assoc :status-unmapped status)))
            :rejects rejects
            :unmapped (if (nil? group)
                        (conj unmapped {:id id :status-unmapped status})
                        unmapped)}))))
   {:items [] :rejects [] :unmapped []}
   (map-indexed vector rows)))

(defn- read-lines [dir file]
  (let [f (io/file dir file)]
    (when (.isFile f)
      (str/split-lines (slurp f)))))

(defn- whitespace-normalize [s]
  (str/join " " (str/split s #"\s+")))

(defn external-key
  "Identifier-safe key for an external target.  The pass builds one
  need variable per edge from (name :to) inside whitespace-split
  structural equations (scm/parse-equation), so the key must contain
  no whitespace.  The raw target text is kept on the edge as
  :external-target; nothing is lost.  Public: the missions adapter
  (packet 6) reuses it."
  [target]
  (str/replace (str target) #"\s+" "-"))

(defn quote-at-line?
  "The quote occurs verbatim in the cited line or the next five lines;
  line-wrap whitespace differences are allowed (check_edges.py).  A blank
  quote never passes: every window contains the empty string.  Public:
  the missions adapter (packet 6) reuses this check rather than
  copying it."
  [lines line quote]
  (boolean
   (when (and (vector? lines) (pos-int? line) (<= line (count lines))
              (string? quote) (not (str/blank? quote)))
     (let [window (str/join "\n" (subvec lines (dec line)
                                          (min (count lines) (+ line 5))))
           wnorm (whitespace-normalize window)]
       (or (str/includes? window quote)
           (str/includes? wnorm (whitespace-normalize quote)))))))

(defn- edge-class [e]
  (if (contains? e :class)
    (keyword (str/replace (str (:class e)) #"^:" ""))
    :closure))

(defn- edge->pass-edge
  "Convert one JSON edge against the item id set.  Returns either
  {:edge ...} or {:reject ...}."
  [dir item-ids e]
  (let [from (keyword (:from e))
        to (keyword (:to e))
        file (:file e) line (:line e) quote (:quote e)]
    (cond
      (not= "needs" (:kind e))
      {:reject {:reject :edge/precedes-not-in-stage-1 :edge e}}

      (not (contains? item-ids from))
      {:reject {:reject :edge/dangling-source :edge e}}

      (not (#{:closure :artifact :alternative} (edge-class e)))
      {:reject {:reject :edge/unknown-class :edge e}}

      (= "cluster" (:to_kind e))
      (if (contains? item-ids to)
        (if (quote-at-line? (read-lines dir file) line quote)
          {:edge {:from from :to to
                  :kind :item :class (edge-class e)
                  :source :prose-reading
                  :quote quote :file file :line line
                  :note (:note e)}}
          {:reject {:reject :edge/quote-not-at-line :edge e}})
        {:reject {:reject :edge/dangling-target :edge e}})

      :else
      (if (quote-at-line? (read-lines dir file) line quote)
        {:edge {:from from :to (external-key (:to e))
                :kind :external :class (edge-class e)
                :source :prose-reading :met false
                :external-target (str (:to e))
                :quote quote :file file :line line
                :note (:note e)}}
        {:reject {:reject :edge/quote-not-at-line :edge e}}))))

(defn tickets->input
  "Read the tickets index and an edges file; return
  {:input <pass input map> :rejected [...] :status-mapping [...]
   :unmapped-statuses [...]}.

  :only, when supplied, is a collection of ticket ids (strings or
  keywords, with or without .md); items outside it are dropped and
  edges into or out of it are rejected as dangling."
  [{:keys [tickets-dir index-file edges-file only]}]
  (let [dir (str tickets-dir)
        rows (read-index index-file)
        {:keys [items rejects unmapped]} (items-from-index rows (normalize-only only) dir)
        item-ids (set (map :id items))
        edges-json (json/parse-string (slurp (str edges-file)) keyword)
        converted (map (partial edge->pass-edge dir item-ids) (:edges edges-json))
        edges (vec (keep :edge converted))
        edge-rejects (vec (keep :reject converted))
        carried (mapv (fn [e] (assoc e :reject :edge/rejected-at-reading))
                      (:rejected edges-json))
        item-statuses (distinct (keep :declared-status items))
        status-mapping (vec (for [s item-statuses]
                              {:status s
                               :group (get status->group s)
                               :mapped? (contains? status->group s)}))]
    {:input {:as-of (str (java.time.LocalDate/now))
             :items (vec items)
             :edges edges}
     :rejected (vec (concat rejects edge-rejects carried))
     :status-mapping status-mapping
     :unmapped-statuses unmapped}))
