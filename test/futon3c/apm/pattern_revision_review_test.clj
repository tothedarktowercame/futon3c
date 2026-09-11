(ns futon3c.apm.pattern-revision-review-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.pattern-revision-review :as sut]
            [futon3c.apm.typed-role-submission :as submission])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn with-fixture [f]
  (let [root (.toFile (Files/createTempDirectory "v4-review" (make-array FileAttribute 0)))
        library (io/file root "library") candidates (io/file root "candidates")
        jobs (atom {"author-job" {:job-id "author-job" :agent-id "ta" :session-id "ta-session" :state "done"}})
        reviewer-session (atom "reviewer-session") calls (atom [])]
    (try
      (.mkdir library) (.mkdir candidates)
      (spit (io/file library "method.flexiarg") "original test bytes")
      (spit (io/file candidates "method.flexiarg") "candidate test bytes")
      (binding [submission/*submission-root* (str (io/file root "submissions"))]
        (let [http (fn [method url body]
                     (swap! calls conj [method url body])
                     (cond
                       (.contains url "/agents/")
                       {:http/status 200 :ok true :agent-id "reviewer" :agent {:session-id @reviewer-session}}
                       (= method "GET")
                       {:http/status 200 :job (get @jobs (last (str/split url #"/")))}
                       (.endsWith url "/announce")
                       (let [id (:job-id body)]
                         (swap! jobs #(if (contains? % id) %
                                         (assoc % id {:job-id id :agent-id "reviewer"
                                                      :session-id @reviewer-session :state "queued"})))
                         {:http/status 202 :ok true :accepted true :job-id id
                          :state (get-in @jobs [id :state])})
                       (.endsWith url "/activate")
                       (do (swap! jobs assoc-in [(:job-id body) :state] "running")
                           {:http/status 202 :ok true :accepted true})))
              author (submission/prepare-request
                      {:agent-id "ta" :frame-id "development-frame" :problem-id "source"
                       :phase :promote-solver :role :solver :dispatch/id "author-dispatch"})
              proposal {:pattern/id "method" :source/path "method.flexiarg"
                        :source/sha256 (:sha256 (sut/file-pin (str library) "method.flexiarg"))
                        :candidate/path "method.flexiarg"
                        :candidate/sha256 (:sha256 (sut/file-pin (str candidates) "method.flexiarg"))
                        :diagnosis/kind :applicability :expected-change "explicit prerequisites"
                        :trigger {:problem/id "source" :node/id "B2" :evidence/id "source-evidence"
                                  :cascade/digest (apply str (repeat 64 "a"))}}
              options {:author-job-id "author-job" :reviewer-agent-id "reviewer"
                       :library-root (str library) :candidate-root (str candidates)
                       :http-fn http :agency-base "http://test"}]
          (is (:ok (submission/register! author {:job-id "author-job"})))
          (is (:ok (submission/submit! "author-job" (:submission/token author)
                                      {:command-own-exit 0 :outcome "complete" :failure-account []
                                       :evidence {:pattern-revision-proposal proposal}})))
          (f {:root root :library library :candidates candidates :jobs jobs
              :reviewer-session reviewer-session :calls calls :options options})))
      (finally (doseq [file (reverse (file-seq root))] (.delete file))))))

(defn submit-review! [request jobs changes]
  (let [id (:submission/job-id request)
        auth (:v4/revision-review request)
        payload {:command-own-exit 0 :outcome "complete" :failure-account []
                 :evidence {:revision-review
                            (merge {:proposal/id (:proposal/id auth)
                                    :candidate/sha256 (get-in auth [:candidate :sha256])
                                    :verdict "accept" :reason "fixture review" :residual "not a proof"}
                                   changes)}}]
    (let [result (submission/submit! id (:submission/token request) payload)]
      (is (= (not (contains? changes :candidate/sha256)) (:ok result))))
    (swap! jobs assoc-in [id :state] "done")))

(deftest executed-review-binds-bytes-identity-and-completion-without-publication
  (with-fixture
    (fn [{:keys [options jobs calls]}]
      (let [prepared (sut/prepare! options) request (:request prepared)
            opts (assoc options :request request)]
        (is (:ok prepared))
        (is (:ok (sut/dispatch! opts)))
        (is (= :awaiting-review (:status (sut/collect! opts))))
        (submit-review! request jobs {})
        (let [result (sut/collect! opts)]
          (is (:ok result))
          (is (= :accept (get-in result [:receipt :verdict])))
          (is (false? (get-in result [:receipt :publication/authorized?])))
          (is (false? (get-in result [:receipt :mathematics/verified?])))
          (is (= result (sut/collect! opts))))
        (let [activations (count (filter #(.endsWith (second %) "/activate") @calls))]
          (is (= :already-terminal (:status (sut/dispatch! opts))))
          (is (= activations (count (filter #(.endsWith (second %) "/activate") @calls)))))))))

(deftest author-must-have-an-executed-authenticated-proposal
  (with-fixture
    (fn [{:keys [options jobs root]}]
      (swap! jobs assoc-in ["author-job" :state] "running")
      (is (= :pattern-review-job-not-successful (:error/code (sut/prepare! options))))
      (swap! jobs assoc-in ["author-job" :state] "done")
      (let [path (io/file root "submissions/author-job.edn") record (edn/read-string (slurp path))]
        (spit path (pr-str (assoc-in record [:submission :submission/id] "forged")))
        (is (= :pattern-review-submission-unverified (:error/code (sut/prepare! options))))))))

(deftest alias-with-same-session-is-not-independent
  (with-fixture
    (fn [{:keys [options reviewer-session]}]
      (reset! reviewer-session "ta-session")
      (is (= :pattern-review-not-independent (:error/code (sut/prepare! options)))))))

(deftest source-drift-before-dispatch-does-not-activate
  (with-fixture
    (fn [{:keys [options library calls]}]
      (let [request (:request (sut/prepare! options))]
        (spit (io/file library "method.flexiarg") "changed")
        (is (= :pattern-review-source-drift
               (:error/code (sut/dispatch! (assoc options :request request))))))
      (is (not-any? #(.endsWith (second %) "/activate") @calls)))))

(deftest wrong-candidate-and-recycled-session-refuse-review
  (doseq [fault [:candidate :session :terminal]]
    (with-fixture
      (fn [{:keys [options jobs]}]
        (let [request (:request (sut/prepare! options)) opts (assoc options :request request)]
          (sut/dispatch! opts)
          (submit-review! request jobs (if (= fault :candidate) {:candidate/sha256 "wrong"} {}))
          (when (= fault :session)
            (swap! jobs assoc-in [(:submission/job-id request) :session-id] "new-session"))
          (when (= fault :terminal)
            (swap! jobs assoc-in [(:submission/job-id request) :state] "failed"))
          (is (false? (:ok (sut/collect! opts)))))))))

(deftest outside-root-and-symlink-escape-are-refused-before-reading
  (with-fixture
    (fn [{:keys [root library]}]
      (spit (io/file root "outside") "not library content")
      (is (thrown? clojure.lang.ExceptionInfo (sut/file-pin (str library) "../outside")))
      (Files/createSymbolicLink (.toPath (io/file library "link"))
                               (.toPath (io/file root "outside")) (make-array FileAttribute 0))
      (is (thrown? clojure.lang.ExceptionInfo (sut/file-pin (str library) "link"))))))

(deftest invalid-review-envelope-does-not-consume-immutable-slot
  (with-fixture
    (fn [{:keys [options jobs]}]
      (let [request (:request (sut/prepare! options)) opts (assoc options :request request)
            id (:submission/job-id request) auth (:v4/revision-review request)
            valid {:command-own-exit 0 :outcome "complete" :failure-account []
                   :evidence {:revision-review {:proposal/id (:proposal/id auth)
                                               :candidate/sha256 (get-in auth [:candidate :sha256])
                                               :verdict :accept :reason "fixture" :residual "unverified"}}}]
        (sut/dispatch! opts)
        (doseq [bad [(assoc valid :outcome nil) (assoc valid :outcome "failed")
                     (assoc valid :command-own-exit 1) (assoc valid :failure-account nil)]]
          (is (false? (:ok (submission/submit! id (:submission/token request) bad))))
          (is (nil? (submission/submitted id))))
        (submit-review! request jobs {})
        (is (:ok (sut/collect! opts)))))))

(deftest symlink-retarget-is-drift-at-dispatch-and-collection
  (doseq [when-retarget [:before-dispatch :before-collection]]
    (with-fixture
      (fn [{:keys [options library jobs]}]
        (let [alias (.toPath (io/file library "method.flexiarg"))
              original (.toPath (io/file library "original.flexiarg"))
              replacement (.toPath (io/file library "replacement.flexiarg"))]
          (Files/move alias original (make-array java.nio.file.CopyOption 0))
          (Files/createSymbolicLink alias original (make-array FileAttribute 0))
          ;; Equal bytes still must not conceal a changed resolution.
          (spit (.toFile replacement) (slurp (.toFile original)))
          (let [request (:request (sut/prepare! options)) opts (assoc options :request request)]
            (when (= when-retarget :before-collection)
              (is (:ok (sut/dispatch! opts)))
              (submit-review! request jobs {}))
            (Files/delete alias)
            (Files/createSymbolicLink alias replacement (make-array FileAttribute 0))
            (is (= :pattern-review-source-drift
                   (:error/code ((if (= when-retarget :before-dispatch) sut/dispatch! sut/collect!) opts))))))))))

(deftest registered-author-session-and-failed-attempt-policy
  (doseq [case [:matched :mismatched :legacy :failed]]
    (with-fixture
      (fn [{:keys [options jobs]}]
        (let [id (str "author-" (name case))
              request (submission/prepare-request
                       (cond-> {:agent-id "ta" :frame-id "development-frame" :problem-id "source"
                                :phase :promote-solver :role :solver :dispatch/id id}
                         (= case :legacy) (assoc :submission/token "retained-v1-token")
                         (not= case :legacy)
                         (assoc :session-id (if (= case :mismatched) "wrong-session" "ta-session"))))
              payload (:payload (submission/submitted "author-job"))
              payload (if (= case :failed) (assoc payload :outcome "failed" :command-own-exit 1) payload)]
          (is (:ok (submission/register! request {:job-id id})))
          (is (:ok (submission/submit! id (:submission/token request) payload)))
          (swap! jobs assoc id {:job-id id :agent-id "ta" :session-id "ta-session" :state "done"})
          (let [result (sut/prepare! (assoc options :author-job-id id))
                authority (get-in result [:request :v4/revision-review])]
            (if (= case :mismatched)
              (is (= :pattern-review-registered-session-mismatch (:error/code result)))
              (do
                (is (:ok result))
                (is (= (if (= case :legacy) :legacy-unpinned :registered-and-matched)
                       (:author/session-binding authority)))
                (is (= (if (= case :failed) :failed :successful)
                       (get-in authority [:author/completion :status])))
                (is (= payload (assoc (:observation (:author/completion authority))
                                      :evidence (:evidence payload))))))))))))

(deftest retargeted-library-root-is-not-hidden-by-a-resolved-pin
  (with-fixture
    (fn [{:keys [options root library]}]
      (let [alias (.toPath (io/file root "library-alias"))
            replacement (io/file root "replacement-library")]
        (.mkdir replacement)
        (spit (io/file replacement "method.flexiarg") (slurp (io/file library "method.flexiarg")))
        (Files/createSymbolicLink alias (.toPath library) (make-array FileAttribute 0))
        (let [opts (assoc options :library-root (str alias))
              request (:request (sut/prepare! opts))]
          (is (some? request))
          (Files/delete alias)
          (Files/createSymbolicLink alias (.toPath replacement) (make-array FileAttribute 0))
          (is (= :pattern-review-source-drift
                 (:error/code (sut/dispatch! (assoc opts :request request))))))))))
