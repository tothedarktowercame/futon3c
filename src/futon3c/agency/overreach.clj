(ns futon3c.agency.overreach
  "Pure retrospective grant scanner.

   Each act is a map with :act/id, :act/kind, :act/rule-id,
   :act/executor, :act/at, and :act/authority. Authority is either a grant
   act id, a typed non-grant reference such as {:kind :interpretation}, or
   absent. Grants are stored :grant/record hyperedges; direct grant maps with
   :act/id are also accepted to keep the pure boundary convenient.

   scan returns at most one finding per act and performs no I/O."
  (:import [java.time Instant]))

(defn- props [grant]
  (if (= :grant/record (:hx/type grant))
    (dissoc (:hx/props grant) :grant/schema :act/harness)
    grant))

(defn- grant-id [grant]
  (or (:hx/id grant) (:act/id grant)))

(defn- instant [stamp]
  (try
    (when stamp (Instant/parse stamp))
    (catch Exception _ nil)))

(defn- time-reason [grant at]
  (let [{:keys [from until]} (:grant/interval grant)
        t (instant at)
        start (instant from)
        end (instant until)]
    (cond
      (or (nil? t) (nil? start)) :grant-not-yet-valid
      (.isBefore ^Instant t ^Instant start) :grant-not-yet-valid
      (and end (not (.isBefore ^Instant t ^Instant end))) :grant-expired)))

(defn- scope-result [grant act]
  (let [{:keys [description act-kinds rule-ids]} (:grant/scope grant)
        checkable? (or (seq act-kinds) (seq rule-ids))]
    (cond
      (not checkable?) {:covered? false
                        :detail {:scope description
                                 :explanation :description-only-scope}}
      (or (contains? (set act-kinds) (:act/kind act))
          (contains? (set rule-ids) (:act/rule-id act)))
      {:covered? true}
      :else {:covered? false
             :detail {:act-kind (:act/kind act)
                      :act/rule-id (:act/rule-id act)}})))

(defn- finding [act reason detail]
  {:finding/act-id (:act/id act)
   :finding/reason reason
   :finding/detail detail})

(defn- direct-failure [act grant]
  (let [g (props grant)
        tr (time-reason g (:act/at act))
        sr (scope-result g act)]
    (cond
      (not= :explicit (:grant/basis g))
      [:interpretation-as-grant {:authority (grant-id grant)
                                 :basis (:grant/basis g)}]

      (not= (:act/executor act) (:grant/grantee g))
      [:wrong-grantee {:authority (grant-id grant)
                       :executor (:act/executor act)
                       :grantee (:grant/grantee g)}]

      tr [tr {:authority (grant-id grant)
              :at (:act/at act)
              :interval (:grant/interval g)}]

      (not (:covered? sr))
      [:out-of-scope (assoc (:detail sr) :authority (grant-id grant))])))

(defn- chain-failure [act leaf by-id]
  (loop [child leaf, seen #{(grant-id leaf)}]
    (let [c (props child)
          parent-id (:grant/parent c)]
      (if-not parent-id
        (when-not (= "joe" (:grant/grantor c))
          {:authority (grant-id leaf)
           :grant (grant-id child)
           :explanation :root-grantor-not-operator})
        (cond
          (contains? seen parent-id)
          {:authority (grant-id leaf) :grant parent-id :explanation :parent-cycle}

          (nil? (get by-id parent-id))
          {:authority (grant-id leaf) :grant parent-id :explanation :missing-parent}

          :else
          (let [parent (get by-id parent-id)
                p (props parent)
                tr (time-reason p (:act/at act))
                sr (scope-result p act)]
            (cond
              (not= :explicit (:grant/basis p))
              {:authority (grant-id leaf) :grant parent-id
               :explanation :parent-not-explicit}

              (not= (:grant/grantor c) (:grant/grantee p))
              {:authority (grant-id leaf) :grant parent-id
               :explanation :delegation-identity-mismatch}

              tr
              {:authority (grant-id leaf) :grant parent-id
               :explanation tr :interval (:grant/interval p)}

              (not (:covered? sr))
              (merge {:authority (grant-id leaf) :grant parent-id
                      :explanation :parent-out-of-scope}
                     (:detail sr))

              :else (recur parent (conj seen parent-id)))))))))

(defn- scan-act [act by-id]
  (let [authority (:act/authority act)]
    (cond
      (nil? authority)
      (finding act :no-grant {:explanation :authority-absent})

      (map? authority)
      (if (= :interpretation (:kind authority))
        (finding act :interpretation-as-grant {:authority authority})
        (finding act :no-grant {:authority authority
                                :explanation :authority-is-not-a-grant-id}))

      (nil? (get by-id authority))
      (finding act :no-grant {:authority authority
                              :explanation :grant-not-found})

      :else
      (let [leaf (get by-id authority)]
        (if-let [[reason detail] (direct-failure act leaf)]
          (finding act reason detail)
          (when-let [detail (chain-failure act leaf by-id)]
            (finding act :broken-delegation detail)))))))

(defn scan
  "Return one finding for every act not covered by its claimed explicit grant."
  [acts grants]
  (let [by-id (into {} (keep (fn [grant]
                               (when-let [id (grant-id grant)] [id grant]))) grants)]
    (into [] (keep #(scan-act % by-id)) acts)))
