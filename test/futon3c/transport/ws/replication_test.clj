(ns futon3c.transport.ws.replication-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.transport.ws.replication :as repl]))

(defn- mk-entry
  [id at & {:keys [tags]
            :or {tags []}}]
  {:evidence/id id
   :evidence/subject {:ref/type :mission :ref/id "M-test"}
   :evidence/type :reflection
   :evidence/claim-type :observation
   :evidence/author "codex-1"
   :evidence/at at
   :evidence/body {:text id}
   :evidence/tags tags})

(defn- mk-store
  [entries]
  (atom {:entries (into {} (map (juxt :evidence/id identity) entries))
         :order (mapv :evidence/id entries)}))

(defn- sent-ids
  [sent*]
  (mapv #(get-in % ["entry" :evidence/id]) @sent*))

(deftest polling-produces-evidence-frames
  (testing "poll sends one evidence frame per pending entry"
    (let [store (mk-store [(mk-entry "e-1" "2026-02-23T10:00:00Z")
                           (mk-entry "e-2" "2026-02-23T10:01:00Z")])
          state* (repl/make-state)
          sent* (atom [])
          result (repl/poll-once! state* store #(swap! sent* conj %))]
      (is (= 2 (:sent result)))
      (is (= ["e-1" "e-2"] (sent-ids sent*)))
      (is (= #{"e-1" "e-2"} (set (keys (:pending @state*))))))))

(deftest polling-skips-replicated-entries
  (testing "entries tagged :replicated are never sent"
    (let [store (mk-store [(mk-entry "e-1" "2026-02-23T10:00:00Z" :tags [:replicated])
                           (mk-entry "e-2" "2026-02-23T10:01:00Z" :tags ["replicated"])
                           (mk-entry "e-3" "2026-02-23T10:02:00Z" :tags [:local])])
          sent* (atom [])]
      (repl/poll-once! (repl/make-state) store #(swap! sent* conj %))
      (is (= ["e-3"] (sent-ids sent*))))))

(deftest high-water-mark-advances-on-ack
  (testing "acked entries are not resent; later entries are sent"
    (let [store (mk-store [(mk-entry "e-1" "2026-02-23T10:00:00Z")
                           (mk-entry "e-2" "2026-02-23T10:01:00Z")
                           (mk-entry "e-3" "2026-02-23T10:02:00Z")])
          state* (repl/make-state)
          sent* (atom [])]
      ;; Initial send: all 3 entries are pending.
      (repl/poll-once! state* store #(swap! sent* conj %))
      (is (= ["e-1" "e-2" "e-3"] (sent-ids sent*)))

      ;; Ack first two only; high-water mark reaches T1.
      (repl/handle-frame! state* {:type "evidence_ack" :evidence_id "e-1" :ok true})
      (repl/handle-frame! state* {:type "evidence_ack" :evidence_id "e-2" :ok true})
      (is (= "2026-02-23T10:01:00Z" (:last-acked-at @state*)))

      ;; Simulate reconnect: pending queue is reset and sender resumes from last acked.
      (repl/reset-connection! state*)
      (reset! sent* [])
      (repl/poll-once! state* store #(swap! sent* conj %))
      (is (= ["e-3"] (sent-ids sent*))))))

(deftest duplicate-id-error-is-idempotent
  (testing "duplicate-id errors are handled without crash/retry loops"
    (let [store (mk-store [(mk-entry "e-1" "2026-02-23T10:00:00Z")])
          state* (repl/make-state)
          sent* (atom [])]
      (repl/poll-once! state* store #(swap! sent* conj %))
      (is (= ["e-1"] (sent-ids sent*)))

      ;; Server duplicate-id error (no structured evidence_id).
      (let [res (repl/handle-frame! state* {:type "error"
                                            :code "duplicate-id"
                                            :message "Evidence id already exists"})]
        (is (= :duplicate-id (:handled res)))
        (is (true? (:ok res))))

      ;; After handling duplicate-id, next poll should not resend e-1.
      (reset! sent* [])
      (repl/poll-once! state* store #(swap! sent* conj %))
      (is (empty? @sent*)))))

(deftest empty-store-sends-nothing
  (testing "polling empty store emits no frames"
    (let [store (mk-store [])
          sent* (atom [])
          result (repl/poll-once! (repl/make-state) store #(swap! sent* conj %))]
      (is (= 0 (:sent result)))
      (is (empty? @sent*)))))

(deftest reconnect-resumes-from-last-acked-position
  (testing "after reconnect reset, sender resumes from last acked timestamp"
    (let [store (mk-store [(mk-entry "e-1" "2026-02-23T10:00:00Z")
                           (mk-entry "e-2" "2026-02-23T10:01:00Z")])
          ;; Use make-state + poll-once! directly to avoid the background
          ;; future race in start! (future can poll between set-send-fn!
          ;; and the manual poll!, duplicating entries).
          state* (repl/make-state)
          sent* (atom [])]
      (repl/poll-once! state* store #(swap! sent* conj %))
      (is (= ["e-1" "e-2"] (sent-ids sent*)))

      ;; Ack first entry only.
      (repl/handle-frame! state* {:type "evidence_ack"
                                  :evidence_id "e-1"
                                  :ok true})

      ;; Reconnect: clear pending and continue from last acked point.
      (repl/reset-connection! state*)
      (reset! sent* [])
      (repl/poll-once! state* store #(swap! sent* conj %))
      (is (= ["e-2"] (sent-ids sent*))))))
