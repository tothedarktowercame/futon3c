(ns futon3c.apm.pattern-revision-review
  "V4 outer-loop review through V3 Agency and typed-submission authorities.
  Review receipts do not publish patterns or certify mathematics."
  (:require [clojure.string :as str]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.job-port :as jobs]
            [futon3c.apm.live-preflight-runtime :as runtime]
            [futon3c.apm.typed-role-submission :as submission])
  (:import [java.nio.file Files Path LinkOption]
           [java.security MessageDigest]))

(defn- text? [x] (and (string? x) (not (str/blank? x))))
(defn- fail! [code] (throw (ex-info (name code) {:error/code code})))
(defn- need! [condition code] (when-not condition (fail! code)))
(defn- sha? [x] (and (string? x) (boolean (re-matches #"[0-9a-f]{64}" x))))
(defn- guarded [f]
  (try (f)
       (catch Exception e
         {:ok false :error/code (or (:error/code (ex-data e)) :pattern-review-read-failed)})))

(defn file-pin
  "Retain the supplied root/path AND resolved target. Every recheck traverses
  the supplied path again, detecting retargeted root or file symlinks."
  [root relative-path]
  (need! (and (text? root) (text? relative-path)) :pattern-review-path-missing)
  (let [supplied-root (.toAbsolutePath (Path/of root (make-array String 0)))
        base (.toRealPath supplied-root (make-array LinkOption 0))
        path (.toRealPath (.resolve base relative-path) (make-array LinkOption 0))]
    (need! (.startsWith path base) :pattern-review-path-escape)
    (need! (Files/isRegularFile path (make-array LinkOption 0)) :pattern-review-not-file)
    (let [bytes (Files/readAllBytes path)
          hash (.digest (MessageDigest/getInstance "SHA-256") bytes)]
      {:root (str supplied-root) :path relative-path
       :resolved/root (str base) :resolved/path (str path)
       :sha256 (apply str (map #(format "%02x" (bit-and 255 %)) hash))})))

(defn- executed-submission
  ([http base job-id] (executed-submission job-id (jobs/observe http base job-id)))
  ([job-id job]
  (let [typed (submission/submitted job-id)
        auth (:authority typed)
        authenticated (when typed
                        (submission/authenticated-completion auth {:job-id job-id}))]
    (need! (and (:ok job) (= :done (:state job)) (= job-id (:job-id job)))
           :pattern-review-job-not-successful)
    (need! (and (:ok authenticated)
                (= typed (:submission authenticated))
                (= (:submission/id typed)
                   (machine/ledger-digest [(dissoc typed :submission/id)])))
           :pattern-review-submission-unverified)
    (need! (and (= job-id (:job-id auth)) (text? (:agent-id auth))
                (= (:agent-id auth) (:agent-id job)) (text? (:session-id job)))
           :pattern-review-executed-identity-mismatch)
    (when (contains? auth :session-id)
      (need! (and (text? (:session-id auth)) (= (:session-id auth) (:session-id job)))
             :pattern-review-registered-session-mismatch))
    {:job job :typed typed})))

(defn- author-completion [typed]
  (let [payload (:payload typed)
        exit (:command-own-exit payload)
        outcome (submission/wire-keyword (:outcome payload))]
    {:observation (select-keys payload [:command-own-exit :outcome :failure-account])
     :status (cond
               (or (and (int? exit) (not= 0 exit))
                   (contains? #{:failed :failure :error} outcome)) :failed
               (and (= 0 exit) (contains? #{:complete :success} outcome)) :successful
               :else :unknown)}))

(defn prepare!
  "Read an executed TA proposal, pin both files under configured roots, and
  resolve an independent reviewer session. No dispatch or library mutation."
  [{:keys [author-job-id reviewer-agent-id library-root candidate-root
           agency-base http-fn]
    :or {agency-base "http://127.0.0.1:7070" http-fn runtime/http-json}}]
  (guarded
   (fn []
     (need! (and (text? reviewer-agent-id)
                 (re-matches #"[A-Za-z0-9_.:-]+" reviewer-agent-id))
            :pattern-review-agent-id-invalid)
     (let [{:keys [job typed]} (executed-submission http-fn agency-base author-job-id)
           proposed (get-in typed [:payload :evidence :pattern-revision-proposal])
           trigger (:trigger proposed)
           _ (need! (map? proposed) :pattern-review-proposal-missing)
           source (file-pin library-root (:source/path proposed))
           candidate (file-pin candidate-root (:candidate/path proposed))
           resolved (http-fn "GET" (str agency-base "/api/alpha/agents/" reviewer-agent-id) nil)
           session-id (get-in resolved [:agent :session-id])]
       (need! (and (text? (:pattern/id proposed))
                   (contains? #{:retrieval :applicability :execution}
                              (submission/wire-keyword (:diagnosis/kind proposed)))
                   (every? text? ((juxt :problem/id :node/id :evidence/id) trigger))
                   (sha? (:cascade/digest trigger))
                   (= (:problem/id trigger) (get-in typed [:authority :problem-id]))
                   (text? (:expected-change proposed)))
              :pattern-review-proposal-invalid)
       (need! (and (= (:sha256 source) (:source/sha256 proposed))
                   (= (:sha256 candidate) (:candidate/sha256 proposed))
                   (not= (:sha256 source) (:sha256 candidate)))
              :pattern-review-byte-pin-mismatch)
       (need! (and (= 200 (:http/status resolved)) (:ok resolved)
                   (= reviewer-agent-id (:agent-id resolved)) (text? session-id))
              :pattern-review-reviewer-unavailable)
       (need! (and (not= reviewer-agent-id (:agent-id job))
                   (not= session-id (:session-id job))) :pattern-review-not-independent)
       (let [authority {:proposal/id (machine/ledger-digest [proposed])
                        :proposal proposed :source source :candidate candidate
                        :author/job-id author-job-id :author/agent-id (:agent-id job)
                        :author/session-id (:session-id job)
                        :author/session-binding
                        (cond
                          (contains? (:authority typed) :session-id) :registered-and-matched
                          (= 2 (get-in typed [:authority :submission/authority-version])) :unpinned
                          :else :legacy-unpinned)
                        :author/completion (author-completion typed)
                        :author/submission-id (:submission/id typed)
                        :reviewer/session-id session-id :review/budget-ms 900000}
             request (-> {:agent-id reviewer-agent-id
                          :frame-id (get-in typed [:authority :frame-id])
                          :problem-id (:problem/id trigger)
                          :phase :pattern-revision-review :role :pattern-reviewer
                          :v4/revision-review authority
                          :dispatch/id (machine/ledger-digest [authority reviewer-agent-id])}
                         submission/prepare-request submission/with-job-authority)]
         {:ok true :request request})))))

(defn- pins-current! [request]
  (need! (and (= :pattern-revision-review (:phase request))
              (= :pattern-reviewer (:role request))
              (= (:dispatch/id request)
                 (machine/ledger-digest [(:v4/revision-review request) (:agent-id request)]))
              (= (:submission/job-id request) (submission/canonical-job-id request)))
         :pattern-review-request-invalid)
  (doseq [pin ((juxt :source :candidate) (:v4/revision-review request))]
    (need! (= pin (file-pin (:root pin) (:path pin))) :pattern-review-source-drift)))

(defn dispatch!
  "Announce, register immutable authority, then activate through V3 ports.
  A completed job is collected, never reactivated."
  [{:keys [request agency-base http-fn]
    :or {agency-base "http://127.0.0.1:7070" http-fn runtime/http-json}}]
  (guarded
   (fn []
     (pins-current! request)
     (let [job-id (:submission/job-id request)
           _ (need! (= job-id (submission/canonical-job-id request)) :pattern-review-job-id-mismatch)
           prompt (str "V4 PATTERN REVISION REVIEW: inspect the exact source and candidate files. "
                       "Review the diagnosed gap, conditions, construction and failure contrasts. "
                       "Do not modify or publish either file, close a frame, or treat a citation "
                       "as mathematical proof. Return accept, reject or cannot-judge with reasons. "
                       "Use the typed completion command below. Put your result under "
                       ":evidence :revision-review with :proposal/id, :candidate/sha256, "
                       ":verdict, :reason and :residual. Authority: "
                       (pr-str (:v4/revision-review request)) "\n"
                       (submission/command (assoc request :agency-base agency-base) {:job-id job-id}))
           dispatch {:agent-id (:agent-id request) :job-id job-id :surface "bell"
                     :caller "apm-v4-library-review" :prompt prompt
                     :timeout-ms (get-in request [:v4/revision-review :review/budget-ms])}
           announced (jobs/announce! http-fn agency-base dispatch)]
       (need! (and (:ok announced) (= job-id (:job-id announced)))
              :pattern-review-announce-failed)
       (let [registered (submission/register! request {:job-id job-id})]
         (need! (:ok registered) :pattern-review-registration-failed))
       (if (= :terminal (jobs/classify-state (:state announced)))
         {:ok true :status :already-terminal :job-id job-id}
         (let [activated (jobs/activate! http-fn agency-base dispatch)]
           (need! (:ok activated) :pattern-review-activation-failed)
           {:ok true :status :review-dispatched :job-id job-id}))))))

(defn collect!
  "Produce a revision-bound review receipt from actual executed job authority.
  No canonical publication or mathematical validity follows from this receipt."
  [{:keys [request agency-base http-fn]
    :or {agency-base "http://127.0.0.1:7070" http-fn runtime/http-json}}]
  (guarded
   (fn []
     (pins-current! request)
     (let [job-id (:submission/job-id request)
           observed (jobs/observe http-fn agency-base job-id)]
       (if (and (:ok observed) (contains? #{:active :settling} (:state/class observed)))
         {:ok true :status :awaiting-review :job-id job-id}
         (let [{:keys [job typed]} (executed-submission job-id observed)
           checked (submission/authenticated-completion request {:job-id job-id})
           authority (:v4/revision-review request)
           review (get-in typed [:payload :evidence :revision-review])
           verdict (submission/wire-keyword (:verdict review))]
       (need! (and (:ok checked) (= typed (:submission checked))
                   (= (:reviewer/session-id authority) (:session-id job))
                   (not= (:author/session-id authority) (:session-id job))
                   (not= (:author/agent-id authority) (:agent-id job)))
              :pattern-review-authority-mismatch)
       (need! (and (= 0 (get-in typed [:payload :command-own-exit]))
                   (contains? #{:complete :success}
                              (submission/wire-keyword (get-in typed [:payload :outcome])))
                   (= (:proposal/id authority) (:proposal/id review))
                   (= (get-in authority [:candidate :sha256]) (:candidate/sha256 review))
                   (contains? #{:accept :reject :cannot-judge} verdict)
                   (text? (:reason review)) (text? (:residual review)))
              :pattern-review-verdict-invalid)
       (let [body {:receipt/type :apm-pattern-revision-review :authority authority
                   :review/job-id job-id :review/agent-id (:agent-id job)
                   :review/session-id (:session-id job) :submission/id (:submission/id typed)
                   :verdict verdict :reason (:reason review) :residual (:residual review)
                   :publication/authorized? false :mathematics/verified? false}]
         {:ok true :receipt (assoc body :receipt/id (machine/ledger-digest [body]))})))))))
