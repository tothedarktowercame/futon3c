(ns futon3c.xiang.frames-stream-test
  (:require [cheshire.core :as json]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.xiang.frames-cache :as cache]
            [futon3c.xiang.frames-stream :as stream]))

(use-fixtures :each (fn [f] (cache/reset!) (try (f) (finally (cache/reset!)))))

(defn- harness [runner]
  (let [scheduled (atom [])]
    {:scheduled scheduled
     :opts {:runner runner
            :schedule! (fn [delay thunk] (swap! scheduled conj [delay thunk]))}}))

(defn- run-next! [h]
  (let [[_ thunk] (first @(:scheduled h))]
    (swap! (:scheduled h) subvec 1)
    (thunk)))

(defn- event-data [event]
  (json/parse-string (second (re-find #"(?m)^data: (.*)$" event)) true))

(deftest cached-subscription-sends-current-newest-frames
  (let [h (harness (fn [_] {:ok true :frames (mapv #(hash-map :n %) (range 5))}))
        _ (cache/frames! "s" (:opts h))
        sent (atom [])
        stop (stream/subscribe! "s" {:limit 2 :send! #(swap! sent conj %)})]
    (is (= 1 (count @sent)))
    (is (str/includes? (first @sent) "event: frames\nid: 1\n"))
    (is (= [{:n 3} {:n 4}] (:frames (event-data (first @sent)))))
    (stop)))

(deftest matching-last-event-id-sends-nothing-on-connect
  (let [h (harness (fn [_] {:ok true :frames [{:n 1}]}))
        entry (cache/frames! "s" (:opts h)) sent (atom [])
        stop (stream/subscribe! "s" {:last-event-id (:version entry)
                                      :send! #(swap! sent conj %)})]
    (is (empty? @sent))
    (stop)))

(deftest coalesced-rebuild-sends-one-new-version
  (let [calls (atom 0)
        h (harness (fn [_] {:ok true :frames [{:n (swap! calls inc)}]}))
        _ (cache/frames! "s" (:opts h)) sent (atom [])
        stop (stream/subscribe! "s" {:last-event-id 1 :send! #(swap! sent conj %)})]
    (dotimes [_ 3] (cache/invalidate! "s" (:opts h)))
    (is (= 1 (count @(:scheduled h))))
    (run-next! h)
    (is (= 2 @calls))
    (is (= 1 (count @sent)))
    (is (str/includes? (first @sent) "id: 2\n"))
    (stop)))

(deftest failed-rebuild-sends-status-and-keeps-frames-quiet
  (let [fail? (atom false)
        h (harness (fn [_] (if @fail? {:ok false :reason :broken :err "no"}
                               {:ok true :frames [{:n 1}]})))
        _ (cache/frames! "s" (:opts h)) sent (atom [])
        stop (stream/subscribe! "s" {:last-event-id 1 :send! #(swap! sent conj %)})]
    (reset! fail? true)
    (cache/invalidate! "s" (:opts h))
    (run-next! h)
    (is (= 1 (count @sent)))
    (is (str/starts-with? (first @sent) "event: status\n"))
    (is (= "rebuild-failed" (:state (event-data (first @sent)))))
    (is (not (str/includes? (first @sent) "event: frames")))
    (stop)))

(deftest missing-session-announces-building-and-delivers-first-build
  (let [sent (atom []) missing (atom 0)
        h (harness (fn [_] {:ok true :frames [{:n 1}]}))
        stop (stream/subscribe! "s" {:send! #(swap! sent conj %)
                                      :on-missing #(swap! missing inc)})]
    (is (= 1 @missing))
    (is (= "building" (:state (event-data (first @sent)))))
    (cache/frames! "s" (:opts h))
    (is (= 2 (count @sent)))
    (is (str/starts-with? (second @sent) "event: frames\n"))
    (stop)))

(deftest unsubscribe-removes-watch-and-stops-events
  (let [h (harness (fn [_] {:ok true :frames [{:n 1}]}))
        _ (cache/frames! "s" (:opts h)) sent (atom [])
        before (set (keys (.getWatches cache/!cache)))
        stop (stream/subscribe! "s" {:last-event-id 1 :send! #(swap! sent conj %)})
        added (set/difference (set (keys (.getWatches cache/!cache))) before)]
    (is (= 1 (count added)))
    (stop)
    (is (empty? (set/intersection added (set (keys (.getWatches cache/!cache))))))
    (cache/invalidate! "s" (:opts h))
    (run-next! h)
    (is (empty? @sent))))

(deftest throwing-send-unsubscribes
  (let [h (harness (fn [_] {:ok true :frames [{:n 1}]}))
        _ (cache/frames! "s" (:opts h)) attempts (atom 0)]
    (stream/subscribe! "s" {:send! (fn [_] (swap! attempts inc) (throw (ex-info "closed" {})))})
    (is (= 1 @attempts))
    (cache/invalidate! "s" (:opts h))
    (run-next! h)
    (is (= 1 @attempts))))

(deftest sse-framing-is-exact-and-json-newlines-stay-escaped
  (is (= "event: frames\nid: 3\ndata: {\"ok\":true}\n\n"
         (stream/sse-event "frames" 3 {:ok true})))
  (let [event (stream/sse-event "status" nil {:message "a\nb"})]
    (is (= 1 (count (filter #(str/starts-with? % "data:") (str/split-lines event)))))
    (is (str/includes? event "a\\nb"))))
