(ns futon3c.wm.run4-attempt-admission-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.wm.run4-attempt-admission :as sut]))

(def attempt-identity
  {:series-id "RUN4-2026-09-10" :trial-id :outer-loop-successor
   :pin-sha256 (apply str (repeat 64 "a"))
   :casting {:author "codex-10" :reviewer "codex-17"
             :repair-reviewer "codex-1"}})

(def request {:attempt-id "attempt-1" :identity attempt-identity})

(defn delete-tree! [root]
  (doseq [f (reverse (file-seq root))] (io/delete-file f true)))

(defn with-store [f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "run4-admission"
                       (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try (f (.getPath root)) (finally (delete-tree! root)))))

(deftest reserves-once-and-reopens-as-indeterminate
  (with-store
    (fn [root]
      (let [first (sut/reserve! root request)
            reopened (sut/reserve! (str root) request)]
        (is (:new? first))
        (is (= :reconciliation-required (get-in first [:admission :state])))
        (is (false? (:new? reopened)))
        (is (true? (get-in reopened [:admission :duplicate?])))
        (is (= :reconciliation-required (get-in reopened [:admission :state])))))))

(deftest concurrent-identical-reservations-admit-exactly-once
  (with-store
    (fn [root]
      (let [start (promise)
            calls (doall (repeatedly 12
                                     #(future @start (sut/reserve! root request))))]
        (deliver start true)
        (let [results (mapv deref calls)]
          (is (= 1 (count (filter :new? results))))
          (is (= 11 (count (remove :new? results)))))))))

(deftest same-attempt-with-different-content-refuses
  (with-store
    (fn [root]
      (is (:ok (sut/reserve! root request)))
      (let [conflict (sut/reserve! root
                                   (assoc-in request [:identity :trial-id]
                                             :different-trial))]
        (is (= 409 (:status conflict)))
        (is (= :run4-attempt-content-conflict (:error conflict)))))))

(deftest durable-click-result-is-returned-to-duplicates
  (with-store
    (fn [root]
      (sut/reserve! root request)
      (let [recorded (sut/record-click! root "attempt-1"
                                        {:started true :click-id "click-1"
                                         :secret "not-persisted"})
            duplicate (sut/reserve! root request)]
        (is (= :click-recorded (:state recorded)))
        (is (= {:started true :click-id "click-1"}
               (get-in duplicate [:admission :result :click])))
        (is (nil? (get-in duplicate [:admission :result :click :secret])))))))

(deftest failed-reservation-write-is-not-an-admission
  (with-store
    (fn [root]
      (binding [sut/*atomic-write!*
                (fn [& _] (throw (ex-info "disk failed" {:committed? false})))]
        (is (thrown? clojure.lang.ExceptionInfo (sut/reserve! root request))))
      (is (:new? (sut/reserve! root request))))))
