(ns futon3c.inbox-zero.confirm-intake
  "Server-owned re-inference and immutable confirmation intake.

  Client evidence is never accepted. The server rebuilds and reranks evidence,
  then permits only the fresh rank-1 exact seat to confirm. Confirmation uses
  first-confirmation-wins provenance: a later acknowledgement of the same
  immutable attribution identity returns the original claim without rewriting
  its response id or observation time."
  (:require [clojure.string :as str]
            [futon3.inbox-zero.confirm :as confirm]
            [futon3.inbox-zero.infer :as infer]
            [futon3.inbox-zero.projection :as projection]
            [futon3.inbox-zero.state :as inbox-state]
            [futon3c.inbox-zero.infer-adapters :as infer-adapters]
            [futon3c.inbox-zero.witness :as witness])
  (:import [java.net InetAddress]
           [java.util Date]))

(def ^:private identity-keys
  [:seat/id :repo/id :worktree/id :path :relation :witness/type :state
   :attribution/evidence])

(defn- non-blank? [value]
  (and (string? value) (not (str/blank? value))))

(defn- invalid! [message data]
  (throw (ex-info message (assoc data :error/type :inbox-zero/invalid-confirmation-input))))

(defn- validate-input! [{:keys [agent session path-key response-id]}]
  (when-not (and (non-blank? agent) (non-blank? session) (non-blank? response-id)
                 (map? path-key)
                 (every? #(non-blank? (get path-key %))
                         [:repo/id :worktree/id :path]))
    (invalid! "Confirmation requires agent, session, response-id, and path-key"
              {:agent agent :session session :path-key path-key
               :response-id response-id})))

(defn- same-attribution? [left right]
  (= (select-keys left identity-keys) (select-keys right identity-keys)))

(defn confirm-attribution!
  "Re-infer and, only for the fresh exact-seat winner, publish seat plus claim."
  [{:keys [agent session path-key response-id] :as input} options]
  (validate-input! input)
  (let [seat-id (str "seat:" agent ":" session)
        state-path (or (:state-path options)
                       (System/getenv "FUTON3C_INBOX_ZERO_STATE_PATH")
                       "/home/joe/code/storage/inbox-zero/state.edn")
        witness-dir (or (:witness-dir options)
                        (System/getenv "FUTON3_INBOX_ZERO_WITNESS_DIR"))
        build-bundle-fn (or (:build-bundle-fn options)
                            infer-adapters/build-evidence-bundle)
        infer-fn (or (:infer-fn options) infer/infer-attribution)
        mint-fn (or (:mint-fn options) confirm/confirmation-claim)
        publish-fn (or (:publish-fn options) witness/publish-immutable-batch!)
        load-state-fn (or (:load-state-fn options) inbox-state/load-state)
        now-fn (or (:now-fn options) #(Date.))
        bundle (build-bundle-fn path-key (assoc options :state-path state-path))
        result (infer-fn path-key bundle)
        candidate (first (:candidates result))]
    (cond
      (not= :propose (:verdict result))
      {:ok false :refused true :verdict (:verdict result)}

      (not= seat-id (:seat/id candidate))
      {:ok false :refused :not-your-attribution}

      :else
      (let [state (load-state-fn state-path)
            observation (get (projection/current-observations state)
                             [(:worktree/id path-key) (:path path-key)])
            workspace-root (:repo/root observation)]
        (when-not (non-blank? workspace-root)
          (throw (ex-info "Confirmation path has no current observed workspace root"
                          {:error/type :inbox-zero/confirmation-observation-missing
                           :path-key path-key})))
        (when-not (non-blank? witness-dir)
          (throw (ex-info "Confirmation witness directory is required"
                          {:error/type :inbox-zero/confirmation-witness-dir-missing})))
        (let [at (now-fn)
              proposal {:path/key path-key :candidate candidate}
              claim (mint-fn proposal {:confirmed-by seat-id :at at
                                       :response/id response-id})
              existing-claim (witness/published-record witness-dir (:claim/id claim))]
          (if existing-claim
            (if (same-attribution? claim existing-claim)
              {:ok true :claim/id (:claim/id existing-claim) :already? true}
              (throw (ex-info "Existing confirmation claim has different attribution"
                              {:error/type :inbox-zero/confirmation-claim-conflict
                               :claim/id (:claim/id claim)})))
            (let [existing-seat (get-in state [:records seat-id])
                  seat (or (when (= :inbox-zero/session-seat
                                    (:record/type existing-seat))
                             existing-seat)
                           {:record/type :inbox-zero/session-seat
                            :seat/id seat-id :agent/id agent :session/id session
                            :surface :attribution-confirmation
                            :host/id (.getHostName (InetAddress/getLocalHost))
                            :workspace/root workspace-root :observed-at at
                            :registry-witness
                            {:endpoint :inbox-zero/confirm-attribution
                             :session/id session}})
                  publication (publish-fn witness-dir [seat claim])
                  claim-result (first (filter #(= (:claim/id claim) (:id %))
                                              publication))]
              {:ok true :claim/id (:claim/id claim)
               :already? (:already? claim-result)})))))))
