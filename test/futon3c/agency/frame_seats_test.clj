(ns futon3c.agency.frame-seats-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.frame-seats :as frame-seats]
            [futon3c.agency.registry :as registry]
            [futon3c.agents.zai-api :as zai-api]
            [futon3c.transport.http :as http]
            [jsonista.core :as json]))

(use-fixtures :each
  (fn [f]
    (registry/reset-registry!)
    (try (f) (finally (registry/reset-registry!)))))

(defn- ready-seat [{:keys [agent-id agent-type]}]
  {:invoke-fn (fn [_prompt _session-id]
                {:result (str "fresh " agent-id) :session-id nil})
   :metadata {:fixture-type agent-type}})

(deftest mint-registers-five-fresh-invoke-ready-seats
  (let [calls (atom [])
        result (frame-seats/mint-seats!
                {:prepare-seat-fn (fn [seat]
                                    (swap! calls conj seat)
                                    (ready-seat seat))}
                "frame-17")
        expected {:reg/solver-seat "frame-17-solver"
                  :reg/student-seat "frame-17-student"
                  :reg/guide-seat "frame-17-guide"
                  :reg/proctor-seat "frame-17-proctor"
                  :reg/scribe-seat "frame-17-scribe"}]
    (is (:ok result))
    (is (= expected (:seats result)))
    (is (= 5 (count @calls)))
    (doseq [[seat-key agent-id] expected]
      (let [agent (registry/get-agent agent-id)
            roster (get-in (registry/registry-status) [:agents agent-id])]
        (is (some? agent) (name seat-key))
        (is (nil? (:agent/session-id agent)) (name seat-key))
        (is (true? (:invoke-ready? roster)) (name seat-key))
        (is (true? (get-in agent [:agent/metadata :fresh-session?])) (name seat-key))))))

(deftest mint-is-idempotent-per-frame
  (let [calls (atom 0)
        opts {:prepare-seat-fn (fn [seat]
                                (swap! calls inc)
                                (ready-seat seat))}
        first-result (frame-seats/mint-seats! opts "same-frame")
        second-result (frame-seats/mint-seats! opts "same-frame")]
    (is (= first-result second-result))
    (is (= 5 @calls))
    (is (= 5 (count (registry/registered-agents))))))

(deftest mint-registers-tenure-scoped-analyst
  (let [calls (atom [])
        result (frame-seats/mint-analyst!
                {:prepare-seat-fn (fn [seat]
                                    (swap! calls conj seat)
                                    (ready-seat seat))}
                1)
        agent (registry/get-agent "analyst-1")
        roster (get-in (registry/registry-status) [:agents "analyst-1"])]
    (is (= {:ok true :tenure 1 :analyst-seat "analyst-1"} result))
    (is (= [{:agent-id "analyst-1" :agent-type :claude}] @calls))
    (is (= :claude (:agent/type agent)))
    (is (true? (:invoke-ready? roster)))
    (is (true? (get-in agent [:agent/metadata :fresh-session?])))))

(deftest analyst-remint-preserves-live-session
  (let [calls (atom 0)
        resets (atom 0)
        opts {:prepare-seat-fn
              (fn [seat]
                (swap! calls inc)
                (assoc (ready-seat seat)
                       :session-reset-fn #(swap! resets inc)))}
        first-result (frame-seats/mint-analyst! opts 7)]
    (registry/update-agent! "analyst-7"
                            {:agent/session-id "live-tenure-session"
                             :agent/status :invoking})
    (let [second-result (frame-seats/mint-analyst! opts 7)
          analyst (registry/get-agent "analyst-7")]
      (is (= first-result second-result))
      (is (= 1 @calls))
      (is (zero? @resets))
      (is (= "live-tenure-session" (:agent/session-id analyst)))
      (is (= :invoking (:agent/status analyst))))))

(deftest non-invocable-seat-is-a-structured-finding
  (let [result
        (frame-seats/mint-seats!
         {:prepare-seat-fn
          (fn [{:keys [agent-type] :as seat}]
            (if (= :zai agent-type)
              {:invoke-fn nil :reason :adapter-unavailable}
              (ready-seat seat)))}
         "broken-frame")]
    (is (false? (:ok result)))
    (is (= :seat-mint-incomplete (:error result)))
    (is (some #(and (= :seat-not-invoke-ready (:finding %))
                    (= :reg/student-seat (:seat %)))
              (:findings result)))
    (is (nil? (:seats result)))))

(deftest mint-seats-http-route
  (let [handler (http/make-handler {:frame-seat-prepare-fn ready-seat})
        response (handler {:request-method :post
                           :uri "/api/alpha/frames/mint-seats"
                           :body (java.io.ByteArrayInputStream.
                                  (.getBytes (json/write-value-as-string
                                              {:frame-id "http-frame"})
                                             "UTF-8"))})
        body (json/read-value (:body response) json/keyword-keys-object-mapper)]
    (is (= 200 (:status response)))
    (is (true? (:ok body)))
    (is (= "http-frame-solver" (get-in body [:seats :reg/solver-seat])))
    (is (= 5 (count (:seats body))))))

(defn- post-seat-mint [handler payload]
  (let [response (handler
                  {:request-method :post
                   :uri "/api/alpha/frames/mint-seats"
                   :body (java.io.ByteArrayInputStream.
                          (.getBytes (json/write-value-as-string payload)
                                     "UTF-8"))})]
    [(:status response)
     (json/read-value (:body response) json/keyword-keys-object-mapper)]))

(deftest absent-cast-preserves-default-seat-types-and-identities
  (let [result (frame-seats/mint-seats! {:prepare-seat-fn ready-seat}
                                         "default-cast")
        expected-types {:solver :codex
                        :student :zai
                        :guide :claude
                        :proctor :codex
                        :scribe :codex}]
    (is (:ok result))
    (doseq [[suffix agent-type] expected-types]
      (let [agent-id (str "default-cast-" (name suffix))]
        (is (= agent-id (get (:seats result)
                             (keyword "reg" (str (name suffix) "-seat")))))
        (is (= agent-type (:agent/type (registry/get-agent agent-id))))
        (is (= {:agent-type agent-type}
               (get-in result [:casting (name suffix)])))))))

(deftest per-seat-cast-overrides-guide-and-scribe-only
  (let [prepared (atom [])
        result (frame-seats/mint-seats!
                {:prepare-seat-fn (fn [seat]
                                    (swap! prepared conj seat)
                                    (ready-seat seat))
                 :model "global-model"
                 :cast {:guide {:type "zai" :model "glm-5.3"}
                        :scribe {:type "zai" :model "glm-5.3"}}}
                "recast")
        by-id (into {} (map (juxt :agent-id identity)) @prepared)]
    (is (:ok result))
    (is (= :zai (:agent-type (get by-id "recast-guide"))))
    (is (= :zai (:agent-type (get by-id "recast-scribe"))))
    (is (= :codex (:agent-type (get by-id "recast-solver"))))
    (is (= :zai (:agent-type (get by-id "recast-student"))))
    (is (= :codex (:agent-type (get by-id "recast-proctor"))))
    (is (= "glm-5.3" (get-in result [:casting "guide" :model])))
    (is (= "global-model" (get-in result [:casting "solver" :model])))
    (is (not-any? (fn [[_ casting]]
                    (and (contains? casting :model) (nil? (:model casting))))
                  (:casting result)))
    (is (not-any? #(and (contains? % :model) (nil? (:model %))) @prepared))))

(deftest frame-seat-cast-refuses-unknown-seat-and-type
  (let [handler (http/make-handler {:frame-seat-prepare-fn ready-seat})
        [seat-status seat-body]
        (post-seat-mint handler {:frame-id "bad-seat"
                                 :cast {:guid {:type "zai"}}})
        [type-status type-body]
        (post-seat-mint handler {:frame-id "bad-type"
                                 :cast {:guide {:type "unknown-vendor"}}})]
    (is (= 400 seat-status))
    (is (= "guid" (get-in seat-body [:findings 0 :seat])))
    (is (= #{"guide" "proctor" "scribe" "solver" "student"}
           (set (get-in seat-body [:findings 0 :accepted-seats]))))
    (is (= 400 type-status))
    (is (= "unknown-vendor"
           (get-in type-body [:findings 0 :agent-type])))
    (is (= #{"claude" "codex" "zai"}
           (set (get-in type-body [:findings 0 :accepted-types]))))))

(deftest frame-seat-cast-refuses-typos-rather-than-dropping-them
  ;; A misspelled override key or a non-string type must not be accepted and
  ;; silently dropped. That is the ?tag= / ?df= shape: 200 with a plausible
  ;; result, and a caller who asked for a Zai guide gets a Claude one.
  (let [handler (http/make-handler {:frame-seat-prepare-fn ready-seat})
        [typo-status typo-body]
        (post-seat-mint handler {:frame-id "typo-key"
                                 :cast {:guide {:tpye "zai"}}})
        [model-status model-body]
        (post-seat-mint handler {:frame-id "typo-model"
                                 :cast {:guide {:modle "glm-5.3"}}})
        [numeric-status numeric-body]
        (post-seat-mint handler {:frame-id "numeric-type"
                                 :cast {:guide {:type 5}}})]
    (is (= 400 typo-status))
    (is (= :unknown-override-key
           (keyword (get-in typo-body [:findings 0 :finding]))))
    (is (= ["tpye"] (get-in typo-body [:findings 0 :keys])))
    (is (= #{"model" "type"}
           (set (get-in typo-body [:findings 0 :accepted-keys]))))
    (is (= 400 model-status))
    (is (= ["modle"] (get-in model-body [:findings 0 :keys])))
    (is (= 400 numeric-status))
    (is (= :invalid-agent-type
           (keyword (get-in numeric-body [:findings 0 :finding]))))))

(deftest mint-analyst-http-route
  (let [handler (http/make-handler {:frame-seat-prepare-fn ready-seat})
        response (handler {:request-method :post
                           :uri "/api/alpha/frames/mint-analyst"
                           :body (java.io.ByteArrayInputStream.
                                  (.getBytes (json/write-value-as-string
                                              {:tenure 3})
                                             "UTF-8"))})
        body (json/read-value (:body response) json/keyword-keys-object-mapper)
        roster (get-in (registry/registry-status) [:agents "analyst-3"])]
    (is (= 200 (:status response)))
    (is (= "analyst-3" (:analyst-seat body)))
    (is (= :claude (:type roster)))
    (is (true? (:invoke-ready? roster)))))

(deftest mint-http-routes-thread-optional-model-without-changing-absent-input
  (let [prepared (atom [])
        prepare-seat (fn [seat]
                       (swap! prepared conj seat)
                       {:invoke-fn (fn [_prompt _session-id]
                                     {:result (:model seat) :session-id nil})})
        handler (http/make-handler {:frame-seat-prepare-fn prepare-seat})
        post! (fn [uri payload]
                (handler {:request-method :post
                          :uri uri
                          :body (java.io.ByteArrayInputStream.
                                 (.getBytes (json/write-value-as-string payload)
                                            "UTF-8"))}))]
    (is (= 200 (:status (post! "/api/alpha/frames/mint-seats"
                               {:frame-id "model-frame"
                                :model "requested-model"}))))
    (is (= 200 (:status (post! "/api/alpha/frames/mint-seats"
                               {:frame-id "default-frame"}))))
    (is (= 200 (:status (post! "/api/alpha/frames/mint-analyst"
                               {:tenure 11 :model "requested-model"}))))
    (is (= 200 (:status (post! "/api/alpha/frames/mint-analyst"
                               {:tenure 12}))))
    (let [by-id (into {} (map (juxt :agent-id identity)) @prepared)]
      (is (= "requested-model" (:model (get by-id "model-frame-guide"))))
      (is (= "requested-model" (:model (get by-id "analyst-11"))))
      (is (= {:agent-id "default-frame-guide" :agent-type :claude}
             (get by-id "default-frame-guide")))
      (is (= {:agent-id "analyst-12" :agent-type :claude}
             (get by-id "analyst-12")))
      (is (= "requested-model"
             (:result ((:agent/invoke-fn (registry/get-agent "model-frame-guide"))
                       "probe" nil))))
      (is (nil? (:result ((:agent/invoke-fn (registry/get-agent
                                             "default-frame-guide"))
                          "probe" nil)))))))

;; Added by claude-2 (ground control) during review of 2bf90753.
;;
;; The test above injects its own :frame-seat-prepare-fn, so mint-frame-seats!
;; takes the (or (:frame-seat-prepare-fn config) ...) override branch and the
;; REAL prepare-frame-seat is never exercised. Disabling the model threading
;; inside prepare-frame-seat therefore left the whole namespace green — which
;; is precisely the D1 regression it is supposed to catch.
;;
;; This covers the production preparer directly: it is the only line that puts
;; :model into the invoke-fn opts, so if it regresses every minted claude seat
;; silently falls back to the CLI default.
(deftest prepare-frame-seat-threads-model-into-invoke-opts
  (let [captured (atom [])
        prepare (var-get #'futon3c.transport.http/prepare-frame-seat)]
    (with-redefs [futon3c.transport.http/make-local-agent-invoke-fn
                  (fn [agent-type opts]
                    (swap! captured conj [agent-type opts])
                    (fn [_prompt _session-id] {:result :stub :session-id nil}))]
      (prepare {} {:agent-id "pfs-with-model"
                   :agent-type :claude
                   :model "claude-opus-5"})
      (prepare {} {:agent-id "pfs-no-model" :agent-type :claude}))
    (let [[[_ with-opts] [_ without-opts]] @captured]
      ;; the model reaches the invoke constructor
      (is (= "claude-opus-5" (:model with-opts)))
      ;; and is ABSENT — not nil-valued — when unrequested, so the CLI default
      ;; is untouched for every seat minted the way frames 2..10 minted theirs
      (is (not (contains? without-opts :model))))))

(deftest local-zai-seat-pins-turn-timeout-separately-from-request-timeout
  (let [captured (atom nil)
        make-local (var-get #'futon3c.transport.http/make-local-agent-invoke-fn)]
    (with-redefs [zai-api/make-invoke-fn
                  (fn [opts]
                    (reset! captured opts)
                    (fn [_prompt _session-id] {:result :stub :session-id nil}))]
      (make-local :zai {:agent-id "timeout-frame-guide"
                        :evidence-store (atom {})}))
    (is (= zai-api/default-turn-timeout-ms (:turn-timeout-ms @captured)))
    (is (not (contains? @captured :request-timeout-ms)))
    (is (not (contains? @captured :timeout-ms)))))

(deftest production-frame-mint-gives-only-student-mathematics-memory-domain
  (let [captured (atom [])]
    (with-redefs [futon3c.transport.http/make-local-agent-invoke-fn
                  (fn [agent-type opts]
                    (swap! captured conj [agent-type opts])
                    (fn [_prompt _session-id] {:result :stub :session-id nil}))]
      (is (:ok (http/mint-frame-seats! {} "domain-frame"))))
    (let [by-id (into {} (map (fn [[agent-type opts]]
                                [(:agent-id opts) [agent-type opts]]))
                      @captured)
          [student-type student-opts] (get by-id "domain-frame-student")]
      (is (= :zai student-type))
      (is (= :mathematics (:memory-domain student-opts)))
      (doseq [agent-id ["domain-frame-solver" "domain-frame-guide"
                        "domain-frame-proctor" "domain-frame-scribe"]]
        (is (not (contains? (second (get by-id agent-id)) :memory-domain))
            agent-id)))))
