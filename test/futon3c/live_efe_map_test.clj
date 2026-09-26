(ns futon3c.live-efe-map-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.clock-store :as clock-store]
            [futon3c.live-efe-map :as live]))

(defn- private-var [sym]
  (or (ns-resolve 'futon3c.live-efe-map sym)
      (throw (ex-info "Missing live-map var" {:symbol sym}))))

(deftest capability-zones-static-projection-is-served
  (let [zones ((var-get (private-var 'capability-zones)))]
    (testing "the version-pinned static artifact is complete"
      (is (= "pca3-v1" (get zones "reduction-version")))
      (is (= 14 (count (get zones "legend"))))
      (is (= 269 (count (get zones "items")))))
    (testing "build-response exposes the projection without changing live layers"
      (with-redefs-fn
        {(private-var 'coordinate-map) (constantly {})
         (private-var 'mission-doc-index) (constantly {})
         (private-var 'durable-clock-by-agent) (constantly {})
         (private-var 'wm-ticks) (fn [_ _] [])
         (private-var 'recent-coordination) (constantly {:count 0 :items []})}
        #(let [response (live/build-response
                         {:registry {:agents {}}
                          :invoke-jobs [] :evidence-store nil :wm-limit 0})]
           (is (= zones (:capability-zones response)))
           (is (nil? (:ship response)))
           (is (= [] (get-in response [:agents :items]))))))))

;; Warrant: turn-c12-saucers F3-diagnose-the-false-attachment agent/state-is-hypothesis,
;; F5-prune-the-inactive code-coherence/dead-code-hygiene + hygiene/exempt-the-in-use,
;; HOLE-1 [196,368] — attachment shown on a mission must be witnessed per-agent;
;; non-active roster entries are pruned and their stale attachment is downgraded to
;; a recorded, distinguishable hypothesis rather than rendered as presence.
(deftest attachment-requires-active-witness
  (let [agent-row (var-get (private-var 'agent-row))
        durable {"codex-1" {:target "M-the-perfect-crime" :last-clock-ms 1}
                 "kimi-9" {:target "M-the-perfect-crime" :last-clock-ms 7}}]
    (with-redefs-fn
      {#'clock-store/current-state
       (fn [agent-id _session-id]
         (if (= agent-id "claude-8")
           {:clock {:mission-id "M-wm-wiring"}}
           {:clock {}}))}
      (fn []
        (let [active-witnessed (agent-row {} {} {} durable
                                          ["claude-8" {:status "invoking"
                                                       :session-id "s1"}])
              active-durable (agent-row {} {} {} durable
                                        ["codex-1" {:status "idle"
                                                    :session-id "s2"}])
              restored-stale (agent-row {} {} {} durable
                                        ["kimi-9" {:status "restored"
                                                   :session-id "s3"}])]
          (testing "F2: a live clock on an active agent stays attached and witnessed"
            (is (= "M-wm-wiring" (:mission-id active-witnessed)))
            (is (= {:witnessed true :basis :live-clock-store}
                   (:attachment active-witnessed))))
          (testing "F4: durable lineage on an active agent stays, marked unwitnessed"
            (is (= "M-the-perfect-crime" (:mission-id active-durable)))
            (is (= {:witnessed false :basis :durable-clock-lineage}
                   (:attachment active-durable))))
          (testing "F5/HOLE-1: a non-active agent is pruned, attachment downgraded"
            (is (nil? (:mission-id restored-stale)))
            (is (nil? (:placement restored-stale)))
            (is (= {:witnessed false
                    :pruned true
                    :pruned-mission-id "M-the-perfect-crime"
                    :reason :agent-not-active-this-server-epoch}
                   (:attachment restored-stale)))))))))
