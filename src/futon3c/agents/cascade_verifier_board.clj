(ns futon3c.agents.cascade-verifier-board
  "The N1 verifier board (NOTE-agent-needs-from-issue-board.md): work the
  Cascade Live issue board's verification debt. Modeled on the inbox-zero
  board — same runtime, same two-wire discipline — with one constitutional
  difference: NO :zap chip. N1 agents prepare and surface; they never act
  on the register (the board's own accepts-nothing clause).

  Flow per cycle: SMELL the backlog for verification debt (items in
  :needs-verification, or freshness :dated-not-revalidated /
  :stale-authority); LOOK the first item's evidence pointer; emit a typed
  :verify-request proposal (the standing query the freshness column
  implies); SING the meter; YIELD. The verify-request is a proposal for
  the established pipeline / an N3 lane to execute — R9: proposal is not
  witness."
  (:require [clojure.edn :as edn]
            [futon3c.agents.chip-board :as board]))

(board/register-verb!
 :smell-backlog
 (fn [state _args inputs]
   (let [debt (vec (:verification-debt inputs))]
     (assoc (board/verbs-branch (if (seq debt) :true :false))
            :effects [[:observe {:channel :issue-board/verification-debt
                                 :ids (mapv :id debt)
                                 :count (count debt)}]]
            :state' (assoc state :shelf-debt debt)))))

(board/register-verb!
 :look-debt
 (fn [state args _inputs]
   (let [id (:id args)
         item (some #(when (= id (:id %)) %) (:shelf-debt state))]
     (if item
       ;; evidential distance v0: basis-count as the read-back distance —
       ;; how many bases must be re-walked to revalidate this item.
       (assoc (board/verbs-branch :true)
              :effects [[:verify-request
                         {:id id
                          :pointer (or (get-in item [:component-context :pointer])
                                       (-> item :basis first :pointer))
                          :basis-sha256 (-> item :basis first :sha256)
                          :freshness (get-in item [:freshness :state])
                          :basis-count (get-in item [:freshness :basis-count])}]]
              :state' (assoc state :range-finder
                             (get-in item [:freshness :basis-count] 0)))
       (assoc (board/verbs-branch :false)
              :effects [[:typed-none {:channel :issue-board/verification-debt
                                      :id id
                                      :reason :item-absent-from-backlog}]]
              :state' state)))))

(def board-v0
  {:board/id "b-cascade-verifier-0"
   :board/version 1
   :entry :ck/smell
   :constants {:fuel-budget 8}
   :provenance {:author "joe+zai-7"
                :spec "NOTE-agent-needs-from-issue-board.md (N1)"}
   :chips
   [{:chip/id :ck/smell :verb :smell-backlog
     :wires {:true :ck/look-first :false :ck/sing-clear}}
    {:chip/id :ck/look-first :verb :look-debt
     :args {:id :first-debt}
     :wires {:true :ck/sing-debt :false :ck/yield}}
    {:chip/id :ck/sing-clear :verb :sing
     :wires {:true :ck/yield}}
    {:chip/id :ck/sing-debt :verb :sing
     :wires {:true :ck/yield}}
    {:chip/id :ck/yield :verb :yield}]})

(defn- first-debt-id [inputs]
  (-> (:verification-debt inputs) first :id))

(defn- resolve-args [inputs]
  (let [target (first-debt-id inputs)]
    (update board-v0 :chips
            (fn [chips]
              (mapv #(if (= :first-debt (get-in % [:args :id]))
                       (assoc-in % [:args :id] target)
                       %)
                    chips)))))

(defn observation-packet
  "Build the R2 packet from issue rows. A row qualifies as verification
  debt when :column is :needs-verification, or its freshness state is
  :dated-not-revalidated / :stale-authority (the standing-query reading of
  the freshness column)."
  [issues]
  {:verification-debt
   (vec (filter (fn [i]
                  (or (= :needs-verification (:column i))
                      (contains? #{:dated-not-revalidated :stale-authority}
                                 (get-in i [:freshness :state]))))
                issues))})

(defn run
  "Run one cycle. EFFECT-HANDLER performs effects; the default prints."
  ([inputs] (run inputs (fn [e] (prn {:effect (first e)}))))
  ([inputs effect-handler]
   (let [b (resolve-args inputs)
         run (board/run-board b inputs effect-handler)]
     (assoc run
            :certificate {:cert/type :chip-board/run-v0
                          :board/id (:board/id b)
                          :board/digest (board/board-digest b)
                          :verified? (board/verify-trace b inputs run)
                          :lean/status :pending}))))

;; ------------------------------------------------------- live adapter

(defn- parse-issue-board
  "Read an issue-board.edn projection. The board is a map with per-issue
  entries under :subjects/:issues as rendered; we accept the raw EDN and
  walk it for maps carrying :column and :id (kind-agnostic)."
  [path]
  (let [edn-data (edn/read-string (slurp path))]
    (if (contains? edn-data :issues)
      (:issues edn-data)
      ;; projection format: issues appear as top-level sequence values in
      ;; :subjects; keep it simple and typed — absent is typed, not guessed
      (throw (ex-info "issue-board projection lacks :issues key"
                      {:keys (keys edn-data)})))))

(defn run-live!
  "One live cycle against the served issue-board.edn projection."
  ([board-path] (run-live! board-path (fn [e] (prn {:effect (first e)
                                                    :payload (second e)}))))
  ([board-path effect-handler]
   (run (observation-packet (parse-issue-board board-path)) effect-handler)))
