(ns futon3c.wm.r10-commission-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.wm.r10-commission :as r10])
  (:import (java.nio.file Files)
           (java.nio.charset StandardCharsets)))

(defn- fixture [dir]
  (let [path (.resolve dir "authority.edn")
        record {:schema :wm/r10-click-commission-v1
                :commission/id "commission-1" :commission/issuer "operator-config"
                :commission/source-pin (apply str (repeat 64 "a"))
                :commission/scope {:service :wm-click}}
        bytes (.getBytes (str (pr-str record) "\n") StandardCharsets/UTF_8)]
    (Files/write path bytes (make-array java.nio.file.OpenOption 0))
    (r10/load-authorized-commission {:authority-path (str path)
                                     :authority-sha256 (r10/sha256-bytes bytes)})))

(deftest authority-and-reservation-controls
  (let [dir (Files/createTempDirectory "r10-test" (make-array java.nio.file.attribute.FileAttribute 0))
        commission (fixture dir)
        root (str (.resolve dir "reservations"))
        calls (atom 0)
        dispatch (fn [c] (swap! calls inc)
                   {:node :R10 :commission/id (:commission/id c)
                    :dispatch/id "click-1" :click/id "click-1"})]
    (testing "valid dispatch reserves first and exact click identity advances state"
      (is (:ok (r10/dispatch-reserved! {:reservation-root root :commission commission
                                        :dispatch-fn dispatch :now "2026-09-14T00:00:00Z"})))
      (is (= 1 @calls))
      (is (= :dispatched (:recovery-state
                           (r10/read-reservation {:reservation-root root
                                                  :commission-id "commission-1"}))))
      (is (= :recorded (:state (r10/mark-recorded! {:reservation-root root
                                                     :commission-id "commission-1"
                                                     :evidence-id "e-1"})))))
    (testing "duplicate refuses before dispatch"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"R10 commission refused"
                            (r10/dispatch-reserved! {:reservation-root root
                                                     :commission commission
                                                     :dispatch-fn dispatch})))
      (is (= 1 @calls)))
    (testing "unavailable store refuses before dispatch"
      (let [blocker (.resolve dir "not-a-directory")]
        (Files/write blocker (.getBytes "x") (make-array java.nio.file.OpenOption 0))
        (is (thrown? clojure.lang.ExceptionInfo
                     (r10/dispatch-reserved! {:reservation-root (str (.resolve blocker "child"))
                                              :commission (assoc commission :commission/id "commission-2")
                                              :dispatch-fn dispatch})))
        (is (= 1 @calls))))))

(deftest after-dispatch-and-dangling-controls
  (let [dir (Files/createTempDirectory "r10-dangling" (make-array java.nio.file.attribute.FileAttribute 0))
        commission (fixture dir)
        root (str (.resolve dir "reservations"))
        calls (atom 0)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (r10/dispatch-reserved! {:reservation-root root :commission commission
                                          :dispatch-fn (fn [_] (swap! calls inc) {:click/id "click-1"})})))
    (is (= 1 @calls))
    (is (= :dangling (:recovery-state
                       (r10/read-reservation {:reservation-root root
                                              :commission-id "commission-1"}))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (r10/dispatch-reserved! {:reservation-root root :commission commission
                                          :dispatch-fn #(throw (Exception. (str %)))})))
    (is (= 1 @calls))))

(deftest invalid-authority-and-commission-controls
  (let [dir (Files/createTempDirectory "r10-authority" (make-array java.nio.file.attribute.FileAttribute 0))
        commission (fixture dir)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (r10/load-authorized-commission {:authority-path (:authority/path commission)
                                                  :authority-sha256 (apply str (repeat 64 "0"))})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (r10/dispatch-reserved! {:reservation-root (str dir)
                                          :commission (dissoc commission :commission/id)
                                          :dispatch-fn identity})))))
