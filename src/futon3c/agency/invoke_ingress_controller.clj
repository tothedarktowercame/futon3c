(ns futon3c.agency.invoke-ingress-controller
  "Prospective invoke intake/drain state machine. This namespace is not wired
  into the serving HTTP process. One controller owns its generation and token;
  request candidates cannot supply either."
  (:import (java.net InetAddress)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.util.concurrent.locks ReentrantLock)))

(defn- refuse! [code data]
  (throw (ex-info (name code) (assoc data :refusal code))))

(defmacro ^:private with-controller-lock [c & body]
  `(let [^ReentrantLock lock# (:lock ~c)]
     (.lock lock#)
     (try ~@body (finally (.unlock lock#)))))

(defn controller
  "Construct an isolated controller. PERSIST! must durably replace the deferred
  resume state before a transition becomes visible. AUTH-TOKEN is provisioned
  by the local operator boundary, never taken from an invoke request."
  [{:keys [auth-token persist!] :or {persist! (fn [_] true)}}]
  (when-not (and (string? auth-token) (not-empty auth-token))
    (refuse! :ingress/auth-token-missing {}))
  {:lock (ReentrantLock.)
   :auth-token auth-token
   :persist! persist!
   :state (atom {:mode :open :generation 0 :waiting-writer 0 :entrant-tokens #{}
                 :accepted-queued #{} :executing #{} :final-delivery #{}
                 :deferred-order [] :deferred {}})})

(defn- persist-state! [c next-state]
  (when-not (true? ((:persist! c) next-state))
    (refuse! :ingress/deferred-persistence-failed {}))
  (reset! (:state c) next-state))

(defn begin-creation!
  "Register an entrant before it waits for the invoke-jobs writer lock. Returns
  an opaque generation ticket, or refuses after intake closes."
  [c]
  (with-controller-lock c
    (let [s @(:state c)]
      (when-not (= :open (:mode s))
        (refuse! :ingress/intake-closed {:generation (:generation s)}))
      (let [token (Object.)]
        (swap! (:state c) #(-> % (update :waiting-writer inc)
                                  (update :entrant-tokens conj token)))
        {:generation (:generation s) :token token}))))

(defn creation-finished!
  "Move a waiting entrant to the accepted queue, or merely release it when its
  ledger operation failed. Must run in finally around the writer-lock section."
  [c ticket accepted-job-id]
  (with-controller-lock c
    (let [s @(:state c)]
      (when-not (contains? (:entrant-tokens s) (:token ticket))
        (refuse! :ingress/entrant-ticket-invalid {}))
      (swap! (:state c)
             (fn [x] (cond-> (-> x (update :waiting-writer dec)
                                    (update :entrant-tokens disj (:token ticket)))
                       accepted-job-id (update :accepted-queued conj (str accepted-job-id))))))))

(defn start-execution! [c job-id]
  (with-controller-lock c
    (let [id (str job-id)]
      (when-not (contains? (:accepted-queued @(:state c)) id)
        (refuse! :ingress/job-not-queued {:job-id id}))
      (swap! (:state c) #(-> % (update :accepted-queued disj id)
                              (update :executing conj id))))))

(defn finish-execution! [c job-id]
  (with-controller-lock c
    (let [id (str job-id)]
      (when-not (contains? (:executing @(:state c)) id)
        (refuse! :ingress/job-not-executing {:job-id id}))
      (swap! (:state c) #(-> % (update :executing disj id)
                              (update :final-delivery conj id))))))

(defn finish-delivery! [c job-id]
  (with-controller-lock c
    (let [id (str job-id)]
      (when-not (contains? (:final-delivery @(:state c)) id)
        (refuse! :ingress/job-not-delivering {:job-id id}))
      (swap! (:state c) update :final-delivery disj id))))

(defn close-intake! [c]
  (with-controller-lock c
    (swap! (:state c) #(if (= :open (:mode %))
                         (-> % (assoc :mode :closed) (update :generation inc)) %))
    @(:state c)))

(defn drained? [c]
  (let [{:keys [waiting-writer accepted-queued executing final-delivery]} @(:state c)]
    (and (zero? waiting-writer) (empty? accepted-queued)
         (empty? executing) (empty? final-delivery))))

(defn defer-resume!
  "Durably retain an internal parked completion/deadline resume while closed.
  Repeating the identical id/payload is idempotent; mutation refuses."
  [c resume-id payload]
  (with-controller-lock c
    (let [id (str resume-id) s @(:state c) old (get-in s [:deferred id])]
      (when (= :open (:mode s))
        (refuse! :ingress/defer-while-open {:resume-id id}))
      (when (and old (not= payload (:payload old)))
        (refuse! :ingress/deferred-resume-conflict {:resume-id id}))
      (when-not old
        (persist-state! c (-> s
                              (update :deferred-order conj id)
                              (assoc-in [:deferred id] {:payload payload :status :pending}))))
      id)))

(defn reopen! [c]
  (with-controller-lock c
    (when-not (drained? c)
      (refuse! :ingress/not-drained {}))
    (swap! (:state c) #(-> % (assoc :mode :open) (update :generation inc)))
    (mapv (fn [id] [id (get-in @(:state c) [:deferred id :payload])])
          (:deferred-order @(:state c)))))

(defn acknowledge-resume!
  "Remove a deferred resume only after downstream acceptance under its stable
  requested job id. A retry before acknowledgement replays the same pair."
  [c resume-id]
  (with-controller-lock c
    (let [id (str resume-id) s @(:state c)]
      (when-not (contains? (:deferred s) id)
        (refuse! :ingress/deferred-resume-missing {:resume-id id}))
      (persist-state! c (-> s
                            (update :deferred dissoc id)
                            (update :deferred-order #(vec (remove #{id} %))))))))

(defn- constant-time= [a b]
  (MessageDigest/isEqual (.getBytes (str a) StandardCharsets/UTF_8)
                         (.getBytes (str b) StandardCharsets/UTF_8)))

(defn verification-snapshot
  "Bounded local verification API. Integration must expose it on a distinct
  loopback-only listener; ordinary Agency HTTP routes must not dispatch here."
  [c {:keys [remote-addr auth-token]}]
  (let [local? (try (.isLoopbackAddress (InetAddress/getByName (str remote-addr)))
                    (catch Throwable _ false))]
    (when-not local? (refuse! :ingress/verification-not-loopback {}))
    (when-not (constant-time= (:auth-token c) auth-token)
      (refuse! :ingress/verification-unauthorized {}))
    (let [s @(:state c)]
      (assoc (select-keys s [:mode :generation :waiting-writer])
             :accepted-queued (count (:accepted-queued s))
             :executing (count (:executing s))
             :final-delivery (count (:final-delivery s))
             :deferred (count (:deferred s))
             :drained? (drained? c)))))
