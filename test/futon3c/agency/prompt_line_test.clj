(ns futon3c.agency.prompt-line-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.agency.prompt-line :as prompt-line]
            [futon3c.xiang.turn-store :as turn-store]))

(def now "2026-09-27T20:00:00Z")

(defn registration
  ([id provider f] (registration id provider f 100))
  ([id provider f budget]
   {:segment/id id :provider provider :fn f :budget-ms budget}))

(defn segment [id provider extra]
  (merge {:segment/id id :segment/provider provider
          :segment/observed-at now
          :segment/basis {:evidence-ref "e-test" :scope {}}}
         extra))

(use-fixtures :each (fn [f] (prompt-line/reset-registry!) (f)))

(deftest pure-render-composes-pattern-and-ordered-markers
  (is (= "> " (:prompt (prompt-line/render {:render-at now} []))))
  (let [providers [(registration :pattern "pattern"
                                 (fn [_] (segment :pattern "pattern"
                                                  {:segment/value "~象/诺必践"})))
                   (registration :first "first"
                                 (fn [_] (segment :first "first"
                                                  {:segment/marker "*"})))
                   (registration :inbox-zero "inbox"
                                 (fn [_] (segment :inbox-zero "inbox"
                                                  {:segment/marker "?"
                                                   :segment/header "unattributed"})))]
        result (prompt-line/render {:render-at now} providers)]
    (is (= "$~象/诺必践*?> " (:prompt result)))
    (is (= [:pattern :first :inbox-zero] (mapv :segment/id (:segments result))))))

(deftest provider-omissions-are-typed-and-bounded
  (let [providers [(registration :slow "slow" (fn [_] (Thread/sleep 2000)) 25)
                   (registration :throwing "throwing" (fn [_] (throw (ex-info "no" {}))))
                   (registration :nothing "nothing" (constantly nil))
                   (registration :invalid "invalid" (constantly {:segment/id :invalid}))]
        started (System/nanoTime)
        result (prompt-line/render {:render-at now} providers)
        elapsed-ms (/ (- (System/nanoTime) started) 1000000.0)]
    (is (< elapsed-ms 250.0) (str "render took " elapsed-ms " ms"))
    (is (= [{:segment/id :slow :reason :timeout}
            {:segment/id :throwing :reason :error}
            {:segment/id :nothing :reason :nil}
            {:segment/id :invalid :reason :invalid}]
           (:omitted result)))
    (is (= "> " (:prompt result)))))

(deftest duplicate-provider-is-refused
  (prompt-line/register-provider! (registration :one "a" (constantly nil)))
  (is (= "a" (:provider (prompt-line/register-provider!
                          (registration :one "a" (constantly nil))))))
  (is (= :duplicate-segment-provider
         (:error/code (ex-data
                       (try
                         (prompt-line/register-provider!
                          (registration :one "b" (constantly nil)))
                         (catch Exception e e)))))))

(deftest render-bang-remembers-exact-seat
  (prompt-line/register-provider!
   (registration :inbox-zero "inbox"
                 (fn [_] (segment :inbox-zero "inbox" {:segment/marker "?"}))))
  (let [result (prompt-line/render! {:agent-id "claude-17" :session-id "s1"
                                     :render-at now})]
    (is (= result (prompt-line/last-render "claude-17" "s1")))
    (is (nil? (prompt-line/last-render "claude-17" "s2")))))

;; --- turn-record writes (E-象-repl-cycle packet 1b) --------------------------
;; render! writes the :pattern segment(s) onto the turn record named by
;; :turn-record-id: the render's own target, never last-render or [agent
;; session] keying.

(defn- temp-turn-store []
  (let [dir (java.nio.file.Files/createTempDirectory
             "xiang-prompt-record-" (make-array java.nio.file.attribute.FileAttribute 0))]
    (turn-store/store (str dir))))

(deftest turn-record-write-targets-the-render-s-own-record
  (let [store (temp-turn-store)
        pattern (atom "~p1")]
    (turn-store/write-record-id! store "turn-aaaaaa" {:turn_id "t1"})
    (turn-store/write-record-id! store "turn-bbbbbb" {:turn_id "t2"})
    (prompt-line/register-provider!
     (registration :pattern "t" (fn [_] (segment :pattern "t"
                                                 {:segment/value @pattern}))))
    (binding [prompt-line/*turn-record-store* store]
      ;; Same agent, same session, consecutive renders: only the target
      ;; record id differs (the same-seat mid-turn bell shape).
      (prompt-line/render! {:agent-id "claude-17" :session-id "s1" :render-at now
                            :turn-record-id "turn-aaaaaa"})
      (reset! pattern "~p2")
      (prompt-line/render! {:agent-id "claude-17" :session-id "s1" :render-at now
                            :turn-record-id "turn-bbbbbb"}))
    (is (= ["~p1"] (:proposed_patterns (turn-store/read-record store "turn-aaaaaa")))
        "record A keeps its own render; a later render for B must not touch it")
    (is (= ["~p2"] (:proposed_patterns (turn-store/read-record store "turn-bbbbbb"))))
    (is (= now (:proposed_patterns_at (turn-store/read-record store "turn-bbbbbb")))
        "the pattern segment's observed-at, not the render clock")))

(deftest turn-record-write-with-no-pattern-segment-is-present-and-empty
  (let [store (temp-turn-store)]
    (turn-store/write-record-id! store "turn-cccccc" {:turn_id "t3"})
    (prompt-line/register-provider!
     (registration :first "t" (fn [_] (segment :first "t" {:segment/marker "*"}))))
    (binding [prompt-line/*turn-record-store* store]
      (prompt-line/render! {:agent-id "claude-17" :session-id "s1" :render-at now
                            :turn-record-id "turn-cccccc"}))
    (let [record (turn-store/read-record store "turn-cccccc")]
      (is (= [] (:proposed_patterns record))
          "rendered, no pattern: present and empty, distinct from absent")
      (is (= now (:proposed_patterns_at record))
          "no segment: the render's rendered-at"))))

(deftest turn-record-write-skips-unknown-and-malformed-ids
  (let [store (temp-turn-store)]
    (prompt-line/register-provider!
     (registration :pattern "t" (fn [_] (segment :pattern "t"
                                                 {:segment/value "~p1"}))))
    (binding [prompt-line/*turn-record-store* store]
      (let [unknown (prompt-line/render! {:agent-id "claude-17" :session-id "s1"
                                          :render-at now :turn-record-id "turn-zzzzzz"})
            malformed (prompt-line/render! {:agent-id "claude-17" :session-id "s1"
                                            :render-at now :turn-record-id "not an id"})
            plain (prompt-line/render! {:agent-id "claude-17" :session-id "s1"
                                        :render-at now})]
        (is (nil? (turn-store/read-record store "turn-zzzzzz")))
        (is (= (:prompt plain) (:prompt unknown) (:prompt malformed))
            "the render result itself is unchanged by a bad target")
        (is (= (:segments plain) (:segments unknown)))))))
