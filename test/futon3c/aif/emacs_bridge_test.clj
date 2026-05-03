(ns futon3c.aif.emacs-bridge-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.aif.emacs-bridge :as bridge]))

(deftest resolve-workspace-file-stays-inside-workspace
  (testing "known strategy devmap resolves under ~/code"
    (let [f (bridge/resolve-workspace-file
             "futon3/holes/strategy/globe1-market-interface.devmap")]
      (is (some? f))
      (is (.exists f))))

  (testing "path traversal is rejected"
    (is (nil? (bridge/resolve-workspace-file "../etc/passwd")))
    (is (nil? (bridge/resolve-workspace-file "/tmp/elsewhere")))))

(deftest resolve-story-file-finds-strategic-sorry-pages
  (testing "new globe story leaves resolve under futon5a/holes/stories"
    (let [f (bridge/resolve-story-file "globe3-peer-eval-artifact")]
      (is (some? f))
      (is (.exists f)))))

(deftest open-target-dispatches-by-kind
  (testing "workspace files use find-file"
    (with-redefs [bridge/run-emacsclient
                  (fn [form] {:ok? true :form form})]
      (let [result (bridge/open-target
                    {:kind "workspace-file"
                     :path "futon3/holes/strategy/globe1-market-interface.devmap"})]
        (is (:ok? result))
        (is (= "workspace-file" (:kind result)))
        (is (.contains (:form result) "find-file")))))

  (testing "VSATARCS stories keep the existing reader path"
    (with-redefs [bridge/run-emacsclient
                  (fn [form] {:ok? true :form form})]
      (let [result (bridge/open-target
                    {:kind "vsatarcs-story"
                     :leaf "globe3-peer-eval-artifact"
                     :scene-anchor "overview"})]
        (is (:ok? result))
        (is (= "vsatarcs-story" (:kind result)))
        (is (.contains (:form result) "arxana-vsatarcs-open-file"))
        (is (.contains (:form result) "arxana-vsatarcs-goto"))))))
