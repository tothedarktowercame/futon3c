(ns futon3c.agents.kimi-api-test
  "Kimi shares zai-api's agent loop, so what needs proving is the provider
   seam: the endpoint, the model, the session-id prefix, the key resolver and
   the sampling block Kimi accepts."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.kimi-api :as kimi]
            [futon3c.agents.zai-api :as zai]))

(defn- text-response [text]
  {:choices [{:message {:role "assistant" :content text}}]})

(defn- make-invoke [opts]
  (kimi/make-invoke-fn
   (merge {:agent-id "kimi-test"
           :api-key "test-key"
           :initial-session-id "kimi-sid-test"
           :evidence-store (atom {:entries {} :order []})
           :memory-mode :none
           :cwd "/tmp"}
          opts)))

(defn- capture-opts
  "Run one turn and return the opts map zai-api handed to chat!."
  [invoke]
  (let [seen (atom nil)]
    (with-redefs [zai/chat! (fn [_client opts _messages]
                              (reset! seen opts)
                              (text-response "done"))]
      (invoke "work" nil))
    @seen))

(deftest kimi-turns-address-the-kimi-coding-endpoint
  (let [opts (capture-opts (make-invoke {}))]
    (is (= kimi/default-base-url (:base-url opts)))
    (is (= kimi/default-model (:model opts)))
    (is (= "KIMI" (:env-prefix opts)))))

(deftest kimi-omits-temperature-because-kimi-refuses-it
  ;; Live 2026-09-23: any explicit temperature returns
  ;; "invalid temperature: only 0.6 is allowed for this model". nil here means
  ;; the field is left out of the request body altogether.
  (let [opts (capture-opts (make-invoke {}))]
    (is (nil? (get-in opts [:sampling :temperature])))
    (is (contains? (:sampling opts) :temperature))
    (is (nil? (get-in opts [:sampling :thinking])))
    (is (= "low" (get-in opts [:sampling :reasoning-effort])))))

(deftest zai-sampling-is-unchanged-by-the-provider-seam
  (let [invoke (zai/make-invoke-fn {:agent-id "zai-test"
                                    :api-key "test-key"
                                    :initial-session-id "zai-sid-test"
                                    :evidence-store (atom {:entries {} :order []})
                                    :memory-mode :none
                                    :cwd "/tmp"})
        opts (capture-opts invoke)]
    (is (= 0.2 (get-in opts [:sampling :temperature])))
    (is (= {:type "disabled"} (get-in opts [:sampling :thinking])))
    (is (= "none" (get-in opts [:sampling :reasoning-effort])))))

(deftest caller-supplied-model-wins-over-the-kimi-default
  (let [opts (capture-opts (make-invoke {:model "kimi-for-coding"}))]
    (is (= "kimi-for-coding" (:model opts)))))

(deftest fresh-kimi-sessions-are-prefixed-kimi
  (let [session-id (atom "kimi-old")
        invoke (make-invoke {:initial-session-id "kimi-old"
                             :session-id-atom session-id})]
    (with-redefs [zai/chat! (fn [_ _ _] (text-response "done"))]
      (let [first-result (invoke "first" nil)]
        (reset! session-id nil)
        (let [second-result (invoke "second" nil)]
          (is (= "kimi-old" (:session-id first-result)))
          (is (str/starts-with? (:session-id second-result) "kimi-"))
          (is (not= (:session-id first-result) (:session-id second-result))))))))

(deftest a-keyless-kimi-seat-refuses-instead-of-borrowing-the-zai-key
  ;; The hazard this guards: falling through to zai-api/resolve-api-key would
  ;; run a Kimi seat on the Z.AI subscription and look like it worked.
  ;; The key is resolved at construction as well as per turn, so the redefs
  ;; have to cover BOTH — a seat built outside them reads the real ~/.kimikey
  ;; and calls the live endpoint.
  (with-redefs [kimi/resolve-api-key (constantly nil)
                zai/resolve-api-key (constantly "zai-key-must-not-be-used")]
    (let [invoke (kimi/make-invoke-fn
                  {:agent-id "kimi-test"
                   :initial-session-id "kimi-sid-test"
                   :evidence-store (atom {:entries {} :order []})
                   :memory-mode :none
                   :cwd "/tmp"})
          result (invoke "work" nil)]
      (is (nil? (:result result)))
      (is (= kimi/api-key-hint (:error result))))))
