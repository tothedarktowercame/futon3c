(ns futon3c.agency.federation-test
  "Federation unit tests — peer announcement, proxy invoke, hook wiring."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [cheshire.core :as json]
            [futon3c.agency.registry :as reg]
            [futon3c.agency.federation :as fed]
            [futon3c.social.test-fixtures :as fix]
            [org.httpkit.client :as http]))

(use-fixtures
  :each
  (fn [f]
    (reg/reset-registry!)
    (fed/configure! {:peers [] :self-url nil})
    (fed/remove-hook!)
    (f)))

;; =============================================================================
;; Configuration
;; =============================================================================

(deftest configure-sets-peers-and-self-url
  (testing "configure! stores peers and self-url"
    (fed/configure! {:peers ["http://host-a:7070" "http://host-b:7070"]
                     :self-url "http://me:7070"})
    (is (= ["http://host-a:7070" "http://host-b:7070"] (fed/peers)))
    (is (= "http://me:7070" (fed/self-url)))))

(deftest configure-filters-blank-peers
  (testing "configure! removes blank peer entries"
    (fed/configure! {:peers ["http://host-a:7070" "" "  " "http://host-b:7070"]
                     :self-url "http://me:7070"})
    (is (= ["http://host-a:7070" "http://host-b:7070"] (fed/peers)))))

(deftest configure-nil-peers-defaults-to-empty
  (testing "configure! with nil peers defaults to []"
    (fed/configure! {:peers nil :self-url nil})
    (is (= [] (fed/peers)))
    (is (nil? (fed/self-url)))))

;; =============================================================================
;; Proxy invoke-fn
;; =============================================================================

(deftest make-proxy-invoke-fn-returns-fn
  (testing "make-proxy-invoke-fn returns a 2-arity function"
    (let [f (fed/make-proxy-invoke-fn "http://remote:7070" "codex-1")]
      (is (fn? f)))))

(deftest proxy-invoke-fn-handles-unreachable-host
  (testing "proxy invoke-fn returns error map when host is unreachable"
    (let [f (fed/make-proxy-invoke-fn "http://127.0.0.1:19999" "codex-1")
          result (f "hello" "sess-1")]
      (is (map? result))
      (is (string? (:error result))))))

(deftest proxy-invoke-fn-forwards-to-invoke-endpoint
  (testing "proxy invoke calls /api/alpha/invoke and returns result/session"
    (let [calls (atom [])
          f (fed/make-proxy-invoke-fn "http://remote:7070" "codex-1")]
      (with-redefs [http/post (fn [url opts]
                                (swap! calls conj {:url url :opts opts})
                                (doto (promise)
                                  (deliver {:status 200
                                            :body "{\"ok\":true,\"result\":\"pong\",\"session-id\":\"sid-2\"}"})))]
        (let [result (f "hello" "sess-1")
              call (first @calls)
              payload (json/parse-string (get-in call [:opts :body]) true)]
          (is (= "pong" (:result result)))
          (is (= "sid-2" (:session-id result)))
          (is (= "http://remote:7070/api/alpha/invoke" (:url call)))
          (is (= "codex-1" (:agent-id payload)))
          (is (= "hello" (:prompt payload)))
          (is (= "federation-proxy" (:caller payload))))))))

;; =============================================================================
;; Announcement — skips when not configured
;; =============================================================================

(deftest announce-returns-nil-without-peers
  (testing "announce! returns nil when no peers configured"
    (fed/configure! {:peers [] :self-url "http://me:7070"})
    (let [agent-record {:agent/id {:id/value "codex-1" :id/type :continuity}
                        :agent/type :codex
                        :agent/capabilities [:edit]
                        :agent/metadata {}}]
      (is (nil? (fed/announce! agent-record))))))

(deftest announce-returns-nil-without-self-url
  (testing "announce! returns nil when no self-url configured"
    (fed/configure! {:peers ["http://host-a:7070"] :self-url nil})
    (let [agent-record {:agent/id {:id/value "codex-1" :id/type :continuity}
                        :agent/type :codex
                        :agent/capabilities [:edit]
                        :agent/metadata {}}]
      (is (nil? (fed/announce! agent-record))))))

(deftest announce-skips-proxy-agents
  (testing "announce! skips agents marked as proxy (prevents loops)"
    (fed/configure! {:peers ["http://host-a:7070"] :self-url "http://me:7070"})
    (let [agent-record {:agent/id {:id/value "codex-1" :id/type :continuity}
                        :agent/type :codex
                        :agent/capabilities [:edit]
                        :agent/metadata {:proxy? true}}]
      (is (nil? (fed/announce! agent-record))))))

(deftest announce-skips-ws-remote-bridge-agents
  (testing "announce! skips agents marked to avoid proxy federation"
    (fed/configure! {:peers ["http://host-a:7070"] :self-url "http://me:7070"})
    (let [agent-record {:agent/id {:id/value "codex-1" :id/type :continuity}
                        :agent/type :codex
                        :agent/capabilities [:edit]
                        :agent/metadata {:skip-federation-proxy? true}}]
      (is (nil? (fed/announce! agent-record))))))

(deftest announce-attempts-post-to-each-peer
  (testing "announce! attempts POST to each configured peer"
    (fed/configure! {:peers ["http://127.0.0.1:19998" "http://127.0.0.1:19999"]
                     :self-url "http://me:7070"})
    (let [agent-record {:agent/id {:id/value "codex-1" :id/type :continuity}
                        :agent/type :codex
                        :agent/capabilities [:edit]
                        :agent/metadata {}}
          results (fed/announce! agent-record)]
      ;; Both peers are unreachable, but we get a result per peer
      (is (= 2 (count results)))
      (is (every? #(not (:ok %)) results))
      (is (= "http://127.0.0.1:19998" (:peer (first results))))
      (is (= "http://127.0.0.1:19999" (:peer (second results)))))))

(deftest announce-preserves-namespaced-capabilities
  (testing "announce serializes namespaced capabilities with full keyword string"
    (fed/configure! {:peers ["http://peer:7070"] :self-url "http://me:7070"})
    (let [calls (atom [])
          agent-record {:agent/id {:id/value "codex-1" :id/type :continuity}
                        :agent/type :codex
                        :agent/capabilities [:edit :coordination/execute]
                        :agent/metadata {}}]
      (with-redefs [http/post (fn [url opts]
                                (swap! calls conj {:url url :opts opts})
                                (doto (promise) (deliver {:status 201 :body "{}"})))]
        (let [results (fed/announce! agent-record)
              payload (json/parse-string (get-in (first @calls) [:opts :body]) true)]
          (is (= 1 (count results)))
          (is (= ["edit" "coordination/execute"] (:capabilities payload))))))))

;; =============================================================================
;; Hook wiring
;; =============================================================================

(deftest install-hook-fires-on-registration
  (testing "install-hook! causes announce! to fire when agent registers"
    (let [announced (atom [])]
      ;; Install a test hook instead of the real one
      (reg/set-on-register! (fn [record] (swap! announced conj record)))
      (reg/register-agent!
       {:agent-id {:id/value "hook-test-1" :id/type :continuity}
        :type :codex
        :invoke-fn (fn [_ _] {:result "ok"})
        :capabilities [:edit]})
      ;; Give the async future time to fire
      (Thread/sleep 100)
      (is (= 1 (count @announced)))
      (is (= "hook-test-1" (get-in (first @announced) [:agent/id :id/value]))))))

(deftest hook-does-not-fire-on-duplicate
  (testing "on-register hook does not fire for duplicate registrations"
    (let [announced (atom [])]
      (reg/set-on-register! (fn [record] (swap! announced conj record)))
      (reg/register-agent!
       {:agent-id {:id/value "dup-test-1" :id/type :continuity}
        :type :codex
        :invoke-fn (fn [_ _] {:result "ok"})
        :capabilities [:edit]})
      ;; Register same ID again
      (reg/register-agent!
       {:agent-id {:id/value "dup-test-1" :id/type :continuity}
        :type :codex
        :invoke-fn (fn [_ _] {:result "ok"})
        :capabilities [:edit]})
      (Thread/sleep 100)
      (is (= 1 (count @announced))))))

(deftest remove-hook-stops-announcements
  (testing "remove-hook! stops the on-register callback"
    (let [announced (atom [])]
      (reg/set-on-register! (fn [record] (swap! announced conj record)))
      (fed/remove-hook!)
      (reg/register-agent!
       {:agent-id {:id/value "no-hook-1" :id/type :continuity}
        :type :codex
        :invoke-fn (fn [_ _] {:result "ok"})
        :capabilities [:edit]})
      (Thread/sleep 100)
      (is (= 0 (count @announced))))))
