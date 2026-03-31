(ns futon3c.dev.apm-frames-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.dev.apm-frames :as apm-frames]))

(deftest prompt-context-exposes-frame-local-paths
  (let [workspace {:frame/workspace-root "/tmp/ws"
                   :frame/module-root "ApmCanaries.Frames.A01J06.Run1"
                   :frame/lean-root "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1"
                   :frame/shared-extension-root "/home/joe/code/apm-lean/ApmCanaries/Local"
                   :artifacts {:proof-plan "/tmp/ws/proof-plan.edn"
                               :changelog "/tmp/ws/changelog.edn"
                               :execute-notes "/tmp/ws/execute.md"
                               :workspace-readme "/tmp/ws/README.md"
                               :workspace-metadata "/tmp/ws/workspace.json"
                               :lean-main "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Main.lean"
                               :lean-scratch "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Scratch.lean"}}
        ctx (apm-frames/prompt-context workspace)]
    (is (= "/tmp/ws" (:workspace-root ctx)))
    (is (= "ApmCanaries.Frames.A01J06.Run1" (:module-root ctx)))
    (is (= "/tmp/ws/proof-plan.edn" (:proof-plan-path ctx)))
    (is (= "/home/joe/code/apm-lean/ApmCanaries/Frames/A01J06/Run1/Main.lean"
           (:lean-main-path ctx)))))

(deftest frame-id-includes-conductor-tag-and-problem
  (let [fid (apm-frames/frame-id "apm-v2" "a02J01")]
    (is (.startsWith fid "apm-v2-a02J01-"))))
