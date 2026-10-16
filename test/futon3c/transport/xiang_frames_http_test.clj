(ns futon3c.transport.xiang-frames-http-test
  "GET /api/alpha/xiang/frames: the stepper's frames over HTTP.

   The process plumbing is tested against real python3 processes (a script
   that prints JSON, one that exits 3, one that outlives its timeout); the
   route's shaping is tested with the runner redefined, so no futon1b is
   needed."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [futon3c.social.test-fixtures :as fix]
            [futon3c.transport.http :as http]
            [futon3c.xiang.frames-cache :as frames-cache]))

(use-fixtures :each (fn [f] (frames-cache/reset!)
                      (try (f) (finally (frames-cache/reset!)))))

(defn- handler []
  (http/make-handler {:registry (fix/mock-registry) :patterns (fix/mock-patterns)}))

(defn- get-frames [query]
  (let [resp ((handler) {:request-method :get
                         :uri "/api/alpha/xiang/frames"
                         :query-string query})]
    [(:status resp) (json/parse-string (:body resp) true)]))

(deftest argv-names-the-session-and-the-limit
  (is (= ["python3" "-I" "/x/turn_frames.py" "s-1"]
         (#'http/xiang-frames-argv "/x/turn_frames.py" "s-1" nil)))
  (is (= ["python3" "-I" "/x/turn_frames.py" "s-1" "--limit" "12"]
         (#'http/xiang-frames-argv "/x/turn_frames.py" "s-1" 12))))

(deftest runner-against-real-processes
  (testing "a script that prints JSON"
    (let [r (#'http/run-turn-frames! ["python3" "-c" "print('[{\"turn\": {}}]')"] 10)]
      (is (:ok r))
      (is (= [{"turn" {}}] (json/parse-string (:out r))))))
  (testing "a script that fails carries its exit code and the stderr tail"
    (let [r (#'http/run-turn-frames! ["python3" "-c" "import sys; print('boom', file=sys.stderr); sys.exit(3)"] 10)]
      (is (false? (:ok r)))
      (is (= :script-failed (:reason r)))
      (is (= 3 (:exit r)))
      (is (= "boom" (:err r)))))
  (testing "a script that outlives the bound is killed and reported as a timeout"
    (let [t0 (System/nanoTime)
          r (#'http/run-turn-frames! ["python3" "-c" "import time; time.sleep(30)"] 1)
          secs (/ (- (System/nanoTime) t0) 1e9)]
      (is (= :timeout (:reason r)))
      (is (< secs 10) "the wait ended at the bound, not at the sleep"))))

(deftest route-refuses-a-missing-or-odd-session
  (let [[status body] (get-frames nil)]
    (is (= 400 status))
    (is (= "missing-field" (:reason body))))
  (let [[status body] (get-frames "session=a%20b;rm")]
    (is (= 400 status))
    (is (= "bad-session" (:reason body)))))

(deftest dead-channel-detection-reads-the-selection-key
  ;; http-kit 2.8.1 never calls :on-close for a client-closed HTTP channel
  ;; (see async-channel-key-field in http.clj); the keepalive tick relies on
  ;; this check instead, so pin that the reflection finds the key.
  (let [selector (java.nio.channels.Selector/open)
        pipe (java.nio.channels.Pipe/open)
        source (doto (.source pipe) (.configureBlocking false))
        key (.register source selector java.nio.channels.SelectionKey/OP_READ)
        channel (org.httpkit.server.AsyncChannel. key nil)
        dead? #'http/async-channel-dead?]
    (try
      (is (false? (dead? channel)))
      (.cancel key)
      (is (true? (dead? channel)))
      (finally
        (.close source) (.close (.sink pipe)) (.close selector)))))

(deftest stream-route-refuses-a-missing-session-before-upgrade
  (let [resp ((handler) {:request-method :get
                         :uri "/api/alpha/xiang/frames/stream"})
        body (json/parse-string (:body resp) true)]
    (is (= 400 (:status resp)))
    (is (= "missing-field" (:reason body)))
    (is (= "session" (:field body)))))

(deftest route-wraps-the-frames-and-the-script-warnings
  (with-redefs-fn {#'http/run-turn-frames!
                   (fn [argv _]
                     (is (= "00e653e3-28f7-4add-8304-47ca0475b5c2" (last argv)))
                     {:ok true
                      :out (json/generate-string [{:turn {:at "2026-10-11T04:24:22Z" :text "hi"}
                                                   :parse {:status "analyzed" :fragments []}}])
                      :err "evidence fetch failed: timed out\n"})}
    (fn []
      (let [[status body] (get-frames "session=00e653e3-28f7-4add-8304-47ca0475b5c2&limit=3")]
        (is (= 200 status))
        (is (true? (:ok body)))
        (is (= 1 (:count body)))
        (is (= "hi" (get-in body [:frames 0 :turn :text])))
        (is (= 3 (:limit body)))
        (is (false? (:cached body)))
        (is (= "evidence fetch failed: timed out" (:warnings body)))))))

(deftest route-caches-whole-session-and-limits-newest-frames
  (let [calls (atom 0)]
    (with-redefs-fn {#'http/run-turn-frames!
                     (fn [_ _]
                       (swap! calls inc)
                       {:ok true :out (json/generate-string
                                       (mapv (fn [n] {:turn {:text (str n)}}) (range 5)))
                        :err ""})}
      (fn []
        (let [[s1 b1] (get-frames "session=s-cache&limit=2")
              [s2 b2] (get-frames "session=s-cache&limit=2")]
          (is (= [200 200] [s1 s2]))
          (is (= 1 @calls))
          (is (false? (:cached b1)))
          (is (true? (:cached b2)))
          (is (= ["3" "4"] (mapv #(get-in % [:turn :text]) (:frames b2))))
          (is (= (:version b1) (:version b2) 1)))))))

(deftest route-reports-a-timeout-and-bad-output-as-such
  (with-redefs-fn {#'http/run-turn-frames! (fn [_ _] {:ok false :reason :timeout :timeout-s 90})}
    (fn []
      (let [[status body] (get-frames "session=s1")]
        (is (= 504 status))
        (is (= "timeout" (:reason body))))))
  (with-redefs-fn {#'http/run-turn-frames! (fn [_ _] {:ok true :out "not json" :err ""})}
    (fn []
      (let [[status body] (get-frames "session=s1")]
        (is (= 502 status))
        (is (= "bad-json" (:reason body)))))))
