(ns futon3c.dev.context-retrieval-delivery-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.dev :as dev]))

(def base-opts
  {:agent-id "claude-test"
   :session-id "session-test"
   :prompt-str "--- CURRENT TURN ---\nSurface: emacs-repl\n\nUser message:\nquestion"
   :response-text "answer"
   :turn-id "turn-test"
   :turn-counter (atom 0)
   :bb-opts nil})

(defn private-var [sym]
  (or (ns-resolve 'futon3c.dev sym)
      (throw (ex-info "missing private test seam" {:symbol sym}))))

(deftest legacy-worker-and-terminal-wait-share-one-search
  (let [runs (atom 0)
        context! (private-var 'context-retrieval!)
        state (private-var '!context-retrieval-runs)]
    (reset! @state {})
    (with-redefs-fn
      {(private-var 'perform-context-retrieval!)
       (fn [{:keys [publish-ready!]}]
         (swap! runs inc)
         (Thread/sleep 20)
         (publish-ready! "$~this-turn> "))}
      (fn []
        (let [legacy (future (context! base-opts))
              prompt (dev/context-retrieval-for-delivery! base-opts)]
          @legacy
          (is (= "$~this-turn> " prompt))
          (is (= 1 @runs)))))))

(deftest terminal-omits-a-stalled-search-within-bound
  (let [state (private-var '!context-retrieval-runs)
        opts (assoc base-opts :response-text "different answer")]
    (reset! @state {})
    (with-redefs-fn
      {(private-var 'perform-context-retrieval!)
       (fn [{:keys [publish-ready!]}]
         (Thread/sleep 1000)
         (publish-ready! "$~late> "))}
      (fn []
        (let [started (System/nanoTime)
              prompt (dev/context-retrieval-for-delivery! opts)
              elapsed-ms (/ (- (System/nanoTime) started) 1000000.0)]
          (is (nil? prompt))
          (is (< elapsed-ms 280.0) (str "terminal wait was " elapsed-ms "ms")))))))

(deftest analysis-seat-does-not-start-delivery-search
  (let [runs (atom 0)]
    (with-redefs-fn
      {(private-var 'perform-context-retrieval!) (fn [_] (swap! runs inc))}
      (fn []
        (is (nil? (dev/context-retrieval-for-delivery!
                   (assoc base-opts :agent-id "象-sonnet"))))
        (is (zero? @runs))))))
