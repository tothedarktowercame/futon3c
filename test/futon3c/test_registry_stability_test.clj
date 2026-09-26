(ns futon3c.test-registry-stability-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry-test :as fixture]))

(deftest unrelated-commit-keeps-warrant-and-both-heads
  (fixture/fixture
   (fn [{:keys [backend current options]}]
     (let [execute registry/run-process!
           run (with-redefs [registry/run-process!
                             (fn [& args]
                               (let [result (apply execute args)]
                                 (swap! current assoc :git-head "unrelated-commit")
                                 result))]
                 (registry/register-run! backend options))
           payload (:payload run)]
       (is (true? (:warrant? payload)))
       (is (= "head" (:git-head payload)))
       (is (= "unrelated-commit" (get-in payload [:execution/post-code :git-head])))
       (is (= {:status :matched} (:postcheck payload)))
       (is (:warrant? (registry/check-record! backend (fixture/check-options run))))))))

(deftest declared-file-change-still-refuses-and-names-the-drift
  (fixture/fixture
   (fn [{:keys [backend current options]}]
     (let [execute registry/run-process!
           run (with-redefs [registry/run-process!
                             (fn [& args]
                               (let [result (apply execute args)]
                                 (swap! current assoc :git-head "scoped-commit"
                                        :code-sha "changed-code"
                                        :code-files {"src/demo.clj" "changed-blob"})
                                 result))]
                 (registry/register-run! backend options))
           payload (:payload run)]
       (is (false? (:warrant? payload)))
       (is (= :inputs-changed-during-run (get-in payload [:postcheck :reason])))
       (is (= "changed-blob"
              (get-in payload [:postcheck :details :code :code-files "src/demo.clj"])))
       (is (= ["src/demo.clj"]
              (registry/closure-diff (:code-files payload)
                                     (get-in payload [:execution/post-code :code-files]))))
       (is (= 3 (get-in payload [:results :tests])))))))

(deftest environment-change-still-refuses
  (fixture/fixture
   (fn [{:keys [backend env options]}]
     (let [execute registry/run-process!
           run (with-redefs [registry/run-process!
                             (fn [& args]
                               (let [result (apply execute args)]
                                 (reset! env {:sha256 "changed-environment"})
                                 result))]
                 (registry/register-run! backend options))]
       (is (false? (get-in run [:payload :warrant?])))
       (is (= :inputs-changed-during-run (get-in run [:payload :postcheck :reason])))
       (is (= {:sha256 "changed-environment"}
              (get-in run [:payload :postcheck :details :env])))))))
