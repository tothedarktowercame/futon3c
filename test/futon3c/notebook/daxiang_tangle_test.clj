(ns futon3c.notebook.daxiang-tangle-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.notebook.render :as render]
            [futon3c.xiang.daxiang :as dx]))

(deftest daxiang-source-is-the-notebook
  ;; 大象 lives in notebooks/daxiang_live.clj; the src file is its tangle.
  ;; Edit the notebook, then: -m futon3c.notebook.render tangle notebooks/daxiang_live.clj
  (is (= (slurp "src/futon3c/xiang/daxiang.clj" :encoding "UTF-8")
         (get (render/tangled (slurp "notebooks/daxiang_live.clj" :encoding "UTF-8"))
              "src/futon3c/xiang/daxiang.clj"))))

(deftest asking-the-operator-is-read-from-the-marks
  (is (dx/asks-operator? "㊥ (gist) Done.\n\n🈸 (next) Shall I start?"))
  (is (dx/asks-operator? "㊥ x\n\n🈯 (one question) Which file?"))
  (is (not (dx/asks-operator? "㊥ x\n\n🈸: (your decision) Started, as you asked."))
      "a 🈸: pointer answers Joe's ask; it asks nothing")
  (is (not (dx/asks-operator? "㊥ x\n\n㊭ (next) I will do it.")))
  (is (not (dx/asks-operator? nil))))
