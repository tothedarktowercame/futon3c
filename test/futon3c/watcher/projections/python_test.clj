(ns futon3c.watcher.projections.python-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.watcher.projections.python :as sut]))

(defn- write-script!
  [name body]
  (let [f (java.io.File/createTempFile name ".sh")]
    (spit f body)
    (.setExecutable f true)
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(deftest collect-files-projects-helper-output
  (testing "helper output is parsed into the shared projection shape"
    (let [path "/tmp/demo.py"
          script (write-script!
                  "python-helper-ok"
                  (str "#!/usr/bin/env bash\n"
                       "cat >/dev/null\n"
                       "printf '%s\\n' '{\"path\" \"" path "\" "
                       "\"module\" \"demo.mod\" "
                       "\"imports\" [[\"np\" \"numpy\"]] "
                       "\"defs\" [{\"name\" \"f\" \"kind\" \"def\" \"has-doc\" false \"body-syms\" [\"x\"]}] "
                       "\"tests\" [] "
                       "\"is-test?\" false}'\n"))]
      (with-redefs [futon3c.watcher.projections.python/python-bin "bash"
                    futon3c.watcher.projections.python/helper-path (delay script)]
        (is (= {path
                {:ns "demo.mod"
                 :aliases {'np 'numpy}
                 :vars [{:vertex/type :var
                         :var/ns "demo.mod"
                         :var/name "f"
                         :var/qname "demo.mod/f"
                         :var/kind "def"
                         :var/has-doc false
                         :var/syms #{'x}}]
                 :tests []
                 :is-test? false}}
               (sut/collect-files [path])))))))

(deftest collect-files-times-out-hung-helper
  (testing "hung helper is killed and surfaced as a timeout"
    (let [script (write-script!
                  "python-helper-hang"
                  (str "#!/usr/bin/env bash\n"
                       "cat >/dev/null\n"
                       "sleep 5\n"))]
      (with-redefs [futon3c.watcher.projections.python/python-bin "bash"
                    futon3c.watcher.projections.python/helper-path (delay script)
                    futon3c.watcher.projections.python/helper-timeout-ms 100]
        (let [ex (try
                   (sut/collect-files ["/tmp/hang.py"])
                   nil
                   (catch clojure.lang.ExceptionInfo e
                     e))]
          (is (instance? clojure.lang.ExceptionInfo ex))
          (is (= "python_ast_helper.py timed out" (ex-message ex)))
          (is (= 100 (:timeout-ms (ex-data ex)))))))))
