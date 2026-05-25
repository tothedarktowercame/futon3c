(ns futon3c.watcher.multi-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.cyder :as cyder]
            [futon3c.watcher.multi :as sut]))

(deftest run-cycle-skips-post-file-work-when-stop-requested
  (testing "shutdown gate suppresses heartbeat and commit ingest"
    (let [heartbeat-called? (atom false)
          commit-called? (atom false)
          cache (atom {})]
      (with-redefs [sut/build-plan (fn [_ _]
                                     {:root "/tmp/repo"
                                      :label "demo"
                                      :snapshot []
                                      :cache {}
                                      :moves {:renamed [] :deleted [] :added []}
                                      :ingest-paths []
                                      :first-cycle? false})
                    sut/detect-cross-root-moves (constantly [])
                    sut/heartbeat! (fn [& _] (reset! heartbeat-called? true))
                    sut/ingest-new-commits-for-root! (fn [& _] (reset! commit-called? true))]
        (reset! sut/!state {:stopping? true})
        (try
          (sut/run-cycle! {:roots [{:path "/tmp/repo" :label "demo"}]
                           :per-root-cache cache
                           :run-id 1
                           :event-n (atom 0)
                           :cycle-n (atom 0)
                           :cold-scan? false})
          (finally
            (reset! sut/!state nil)))
        (is (false? @heartbeat-called?))
        (is (false? @commit-called?))))))

(deftest watched-recognizes-stack-mission-docs
  (testing "watcher mission docs live under stack repos' holes/missions paths"
    (is (true? (sut/watched? "/home/joe/code/futon7/holes/missions/M-self-documenting-stack.md")))
    (is (false? (sut/watched? "/home/joe/npt/missions/M-ukrns-wp.md")))
    (is (false? (sut/watched? "/home/joe/code/futon7/holes/missions/notes.md")))))

(deftest run-cycle-can-skip-commit-ingest
  (testing "file-event cycles stay live when commit catch-up is disabled"
    (let [commit-called? (atom false)
          cache (atom {})]
      (with-redefs [sut/build-plan (fn [_ _]
                                     {:root "/tmp/repo"
                                      :label "demo"
                                      :snapshot []
                                      :cache {}
                                      :moves {:renamed [] :deleted [] :added []}
                                      :ingest-paths []
                                      :first-cycle? false})
                    sut/detect-cross-root-moves (constantly [])
                    sut/heartbeat! (fn [& _] nil)
                    sut/ingest-new-commits-for-root! (fn [& _] (reset! commit-called? true))]
        (reset! sut/!state {:stopping? false})
        (try
          (sut/run-cycle! {:roots [{:path "/tmp/repo" :label "demo"}]
                           :per-root-cache cache
                           :run-id 1
                           :event-n (atom 0)
                           :cycle-n (atom 0)
                           :cold-scan? false
                           :commit-ingest? false})
          (finally
            (reset! sut/!state nil)))
        (is (false? @commit-called?))))))

(deftest safe-cycle-updates-heartbeat-state
  (testing "successful cycles stamp timestamps and touch CYDER"
    (let [touched? (atom false)]
      (with-redefs [sut/run-cycle! (fn [_] :ok)
                    cyder/touch! (fn [id]
                                   (when (= "multi-watcher" id)
                                     (reset! touched? true)))]
        (reset! sut/!state {:stopping? false
                            :last-cycle-started-at nil
                            :last-cycle-finished-at nil
                            :last-error "old"
                            :last-subtask nil})
        (try
          (#'futon3c.watcher.multi/safe-cycle! {})
          (is (some? (:last-cycle-started-at @sut/!state)))
          (is (some? (:last-cycle-finished-at @sut/!state)))
          (is (nil? (:last-error @sut/!state)))
          (is (true? @touched?))
          (finally
            (reset! sut/!state nil)))))))
