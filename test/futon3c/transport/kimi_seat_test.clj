(ns futon3c.transport.kimi-seat-test
  "The :kimi agent type as the Agency sees it: capabilities, session file,
   timeout policy, bellback membership, and that a kimi seat is built by the
   Kimi provider rather than by Z.AI's."
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agents.kimi-api :as kimi-api]
            [futon3c.agents.zai-api :as zai-api]
            [futon3c.transport.http :as http]))

(def ^:private session-file-for
  #'http/default-session-file-for-agent)

(def ^:private timeout-policy
  #'http/frame-seat-timeout-policy)

(def ^:private make-local-invoke-fn
  #'http/make-local-agent-invoke-fn)

(deftest kimi-seats-have-their-own-session-file-namespace
  (is (= "/tmp/futon-kimi-session-id-kimi-1" (session-file-for :kimi "kimi-1")))
  ;; Colliding with the zai path would let a kimi seat inherit a zai session.
  (is (not= (session-file-for :zai "kimi-1") (session-file-for :kimi "kimi-1"))))

(deftest kimi-seats-advertise-the-harness-capabilities
  (is (= [:explore :edit :test :coordination/execute]
         (get @#'http/default-capabilities :kimi))))

(deftest kimi-completions-bell-the-caller-back
  ;; Same harness as zai, so the same silent-completion hazard.
  (is (contains? http/auto-bellback-recipient-types :kimi)))

(deftest kimi-frame-seats-carry-the-harness-request-envelope
  (let [policy (timeout-policy "f1-guide" :kimi)]
    (is (= zai-api/default-request-timeout-ms (:request-timeout-ms policy)))
    (is (= :zai-api/default-request-timeout-ms (:request/source policy)))
    (is (= zai-api/default-turn-timeout-ms (:turn-timeout-ms policy)))))

(deftest a-kimi-seat-is-built-by-the-kimi-provider
  (let [captured (atom nil)]
    (with-redefs [kimi-api/make-invoke-fn (fn [opts]
                                            (reset! captured opts)
                                            (fn [_ _] {:result "ok"}))
                  zai-api/make-invoke-fn (fn [_]
                                           (throw (ex-info "zai must not build a kimi seat" {})))]
      (let [invoke-fn (make-local-invoke-fn
                       :kimi
                       {:agent-id "kimi-1"
                        :session-file (session-file-for :kimi "kimi-1")
                        :evidence-store (atom {:entries {} :order []})
                        :model "kimi-for-coding"})]
        (is (fn? invoke-fn))
        (is (= "kimi-1" (:agent-id @captured)))
        (is (= "kimi-for-coding" (:model @captured)))
        (is (= "/tmp/futon-kimi-session-id-kimi-1" (:session-file @captured)))
        (is (= zai-api/default-request-timeout-ms (:request-timeout-ms @captured)))
        (is (= zai-api/default-turn-timeout-ms (:turn-timeout-ms @captured)))))))
