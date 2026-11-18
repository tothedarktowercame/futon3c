(ns futon3c.circus.navigability
  "Executable certificate for Circus.Navigable: schema moves versus ※ moves.

  S is the declared Arxana semantic graph plus bounded, read-only relations
  that live in the turn/Agency stores. U is derived from the same files as
  mojira-anykey-check.py. Global ※ codes are literal moves from every known
  kind; the local ※ graph is empty until context-sensitive codes exist."
  (:require [cheshire.core :as json]
            [clojure.set :as set]
            [clojure.string :as str]
            [futon3c.agency.registry :as agency]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.type-registry :as types]))

(def ^:private codes-path "/home/joe/code/futon4/dev/mojira-codes.json")
(def ^:private allowlist-path "/home/joe/code/futon0/system/zone/code-allowlist/allowlist.tsv")

(def outside-arxana
  "Real query relations whose records do not live in the Arxana registry."
  [{:from :turn :to :mission
    :query "turn record :mission_id"
    :evidence {:path "src/futon3c/circus/routes.clj" :line 502}}
   {:from :mission :to :turn
    :query "turn_store/mission-index; GET /api/alpha/circus/mission-session-patterns"
    :evidence {:path "src/futon3c/circus/routes.clj" :line 494}}
   {:from :turn :to :session
    :query "turn record :session_id"
    :evidence {:path "src/futon3c/circus/routes.clj" :line 291}}
   {:from :session :to :turn
    :query "turn_store/session-head + predecessor walk; GET /api/alpha/circus/strand"
    :evidence {:path "src/futon3c/circus/routes.clj" :line 329}}
   {:from :mission :to :pattern
    :query "GET /api/alpha/circus/mission-pattern-scopes"
    :evidence {:path "src/futon3c/circus/routes.clj" :line 521}}
   {:from :agent :to :session
    :query "GET /api/alpha/agent-clock"
    :evidence {:path "src/futon3c/transport/http.clj" :line 8138}}
   {:from :agent :to :mission
    :query "GET /api/alpha/agent-clock"
    :evidence {:path "src/futon3c/transport/http.clj" :line 8154}}])

(defn schema-graph
  "All schema edges, retaining the declarations/queries that realise them."
  []
  (let [arxana (for [[[from to] labels] (types/schema-kind-graph)]
                 {:from from :to to :source :arxana :realisers labels})]
    (->> (concat arxana (map #(assoc % :source :outside-arxana) outside-arxana))
         (sort-by (juxt :from :to :source)) vec)))

(def ^:private fixed-code-kinds
  {"p" :pattern
   "v" :file
   ;; Reserved so the certificate starts exposing it as soon as the shared
   ;; table registers the proposed mission-pattern shortcut.
   "m p" :pattern})

(defn- table [] (json/parse-string (slurp codes-path) true))

(defn- repo-keys []
  (for [line (str/split-lines (slurp allowlist-path))
        :let [[name kind] (str/split line #"\t")]
        :when (= "repo" kind)
        key (cond-> [(str "r " name)]
              (re-matches #"futon[0-9][0-9a-z]*" name)
              (conj (subs name 5)))]
    key))

(defn- agent-codes [table]
  (let [letter (into {} (map (juxt :kind :key) (:agents table)))
        rows (for [agent-id (keys (:agents (agency/registry-status)))
                   :let [[_ kind n] (re-matches #"([a-z]+)-([0-9]+)" (name agent-id))]
                   :when (and kind (letter kind))]
               {:kind kind :code (str (letter kind) " " n)})]
    (concat (map :code rows)
            (for [kind (distinct (map :kind rows))]
              (str (letter kind) " l")))))

(defn code-set
  "The complete-code families relevant to Emacs ※, with explicit destinations.
  Mojira-only fixed codes are excluded because Emacs deliberately refuses them."
  []
  (let [t (table)
        fixed (for [{:keys [code emacs]} (:codes t)
                    :when (and emacs (fixed-code-kinds code))]
                {:code code :kind (fixed-code-kinds code) :family :fixed})
        kinds (map :key (:kinds t))
        repos (repo-keys)
        repo-codes (mapcat (fn [repo]
                             (concat [{:code repo :kind :repo :family :repo}]
                                     [{:code (str repo " l") :kind :file :family :repo-file}]
                                     (for [kind kinds]
                                       {:code (str repo " " kind) :kind :file :family :repo-file})))
                           repos)
        agents (map (fn [code] {:code code :kind :agent :family :agent}) (agent-codes t))]
    (->> (concat fixed repo-codes agents
                 [{:code "?" :kind :help :family :prompt}
                  {:code "l" :kind :buffer :family :buffer}])
         (distinct)
         (sort-by (juxt :code :kind)) vec)))

(defn kinds
  "All S endpoint kinds plus ※-only destination kinds."
  [s codes]
  (set (concat (map :from s) (map :to s) (map :kind codes))))

(defn global-ui
  "※ global moves represented literally from every certificate kind."
  [ks codes]
  (set (for [from ks, {:keys [kind]} codes] [from kind])))

(defn closure
  "BFS closure and shortest distances from START through directed edge set U."
  [u start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         distances {start 0}]
    (if (empty? queue)
      distances
      (let [from (peek queue)
            queue (pop queue)
            nexts (->> u (keep (fn [[a b]] (when (= a from) b)))
                       (remove distances) distinct)]
        (recur (into queue nexts)
               (reduce #(assoc %1 %2 (inc (distances from))) distances nexts))))))

(defn check-witness!
  "Check the three hypotheses used by Navigable.gap_of_closed."
  [u from to witness]
  (let [witness (set witness)
        escaped (seq (for [[a b] u :when (and (witness a) (not (witness b)))] [a b]))]
    (when-not (and (witness from) (not (witness to)) (nil? escaped))
      (throw (ex-info "invalid navigability closure witness"
                      {:from from :to to :witness witness :escaping-ui-edges escaped})))
    true))

(defn analyse
  "Classify each S edge against U; unreachable results carry a checked closure."
  [s u]
  (mapv (fn [{:keys [from to] :as edge}]
          (let [distances (closure u from)]
            (if-some [distance (get distances to)]
              {:edge edge :status :reachable :distance distance}
              (let [witness (set (keys distances))]
                (check-witness! u from to witness)
                {:edge edge :status :gap :witness witness}))))
        s))

(defn- summary [results]
  {:edges (count results)
   :reachable (count (filter #(= :reachable (:status %)) results))
   :gaps (count (filter #(= :gap (:status %)) results))})

(defonce ^:private !cache (atom nil))

(defn certificate
  "Return S and both current ※ interpretations with Lean-style gap witnesses."
  []
  (let [s (schema-graph)
        codes (code-set)
        cache-key (digest/canonical-digest
                   {:registry (mapv (juxt key (comp :type/digest val)) types/registry-v1)
                    :codes codes})]
    (if (= cache-key (:key @!cache))
      (:value @!cache)
      (let [ks (kinds s codes)
            global (global-ui ks codes)
            local #{}
            global-results (analyse s global)
            local-results (analyse s local)
            value {:kinds ks
                   :S s
                   :U {:global global :local local
                       :codes codes
                       :ui-only-kinds (set/difference (set (map :kind codes))
                                                      (set (mapcat (juxt :from :to) s)))}
                   :results {:global global-results :local local-results}
                   :summary {:global (summary global-results)
                             :local (summary local-results)}}]
        (reset! !cache {:key cache-key :value value})
        value))))

(defn response []
  {:status 200 :body (certificate)})
