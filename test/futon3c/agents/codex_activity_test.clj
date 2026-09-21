(ns futon3c.agents.codex-activity-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agents.codex-activity :as activity]))

(def receipt
  {:type "item.completed"
   :item {:id "patch-1" :type "file_change" :status "completed"
          :changes [{:path "src/work.clj" :kind "update"}]}})

(deftest only-completed-file-receipts-are-writes
  (is (= [{:path "/tmp/project/src/work.clj" :item-id "patch-1" :session-id nil}]
         (activity/file-writes receipt "/tmp/project")))
  (doseq [event [(assoc receipt :type "item.started")
                 (assoc-in receipt [:item :status] "failed")
                 (assoc-in receipt [:item :type] "command_execution")
                 (assoc-in receipt [:item :id] nil)
                 {:type "response_item"
                  :payload {:type "custom_tool_call" :name "apply_patch"
                            :input "*** Update File: src/work.clj"}}]]
    (is (empty? (activity/file-writes event "/tmp/project")))))

(deftest rollout-map-paths-and-moves-preserve-the-entire-path
  (let [event {:type "event_msg"
               :payload {:type "item_completed" :thread_id "session"
                         :item {:id "nested-exec" :type "FileChange" :status "completed"
                                :changes {(keyword "/tmp/old/work.clj")
                                          {:type "update" :move_path "/tmp/new/work.clj"}}}}}]
    (is (= #{"/tmp/old/work.clj" "/tmp/new/work.clj"}
           (set (map :path (activity/file-writes event "/tmp/elsewhere")))))
    (is (every? #(= "session" (:session-id %))
                (activity/file-writes event "/tmp/elsewhere")))))
