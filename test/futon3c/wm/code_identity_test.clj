(ns futon3c.wm.code-identity-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.wm.code-identity :as identity]))

(use-fixtures :each (fn [f] (identity/reset-for-test!) (f)))

(deftest absence-is-unavailable-not-current-disk-guess
  (is (= {:schema 1
          :required-file "src/futon2/aif/full_loop_runner.clj"
          :availability :unavailable
          :reason :not-recorded-in-this-process-image
          :identity nil}
         (identity/status))))

(deftest recorded-runner-identity-is-returned-verbatim
  (let [record {:schema 1 :git-head "abc" :dirty? false :stable? true}]
    (identity/install-for-test! record)
    (is (= :available (:availability (identity/status))))
    (is (= record (:identity (identity/status))))))
