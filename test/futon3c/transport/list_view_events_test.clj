(ns futon3c.transport.list-view-events-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.transport.http :as h]))

(deftest list-view-drops-only-tool-output
  (let [j {:job-id "j" :events [{:type "tool_use" :tools ["bash"] :previews ["bash ls"] :output ["big"]}
                                {:type :tool_use :previews ["p"] :output "x"}
                                {:type "text" :text "hi" :output "keep"}]}]
    (is (= [{:type "tool_use" :tools ["bash"] :previews ["bash ls"]}
            {:type :tool_use :previews ["p"]}
            {:type "text" :text "hi" :output "keep"}]
           (:events (h/list-view-events j))))
    (is (= {:job-id "j"} (h/list-view-events {:job-id "j"})))))
