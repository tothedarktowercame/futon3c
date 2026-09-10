(ns futon3c.wm.run4-trusted-entry-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon2.aif.c-fold-config :as digest]
            [futon3c.wm.run4-trusted-entry :as sut]))

(def token "0123456789abcdef0123456789abcdef")

(defn fixture []
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "run4-auth" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (spit (io/file root "pin.edn") "{:pin 1}")
    (spit (io/file root "source.md") "source")
    {:root root
     :config {:run4 {:enabled? true :bearer-token token :operator "Joe"
                     :pin-root (.getPath root) :pin-allowlist #{"pin.edn"}
                     :source-root (.getPath root) :source-allowlist #{"source.md"}
                     :resolve-mission identity :action-admissible? (constantly true)}}}))

(deftest authenticates-and-mints-one-use-digest-bound-context
  (let [{:keys [config]} (fixture)
        result (sut/prepare config {"authorization" (str "Bearer " token)}
                            {:run4-pin-ref "pin.edn"})
        opts (:opts result)
        trust (:run4-trusted-boundary-fn opts)
        sha (get (trust {:pin-digest (digest/sha256 "{:pin 1}")
                         :operator-selection {:operator "Joe"}}) :pin-sha256)]
    (is (:ok result))
    (is (= "source" ((get-in opts [:run4-task-pin-ports :read-text]) "source.md")))
    (is (string? sha))
    (is (thrown? clojure.lang.ExceptionInfo
                 (trust {:pin-digest sha :operator-selection {:operator "Joe"}})))))

(deftest refuses-untrusted-and-client-injected-authority
  (let [{:keys [config]} (fixture)]
    (is (= :run4-authentication-failed
           (:error (sut/prepare config {} {:run4-pin-ref "pin.edn"}))))
    (is (= :run4-request-key-forbidden
           (:error (sut/prepare config {"authorization" (str "Bearer " token)}
                                {:run4-pin-ref "pin.edn" :authenticated true}))))
    (is (= :run4-pin-reference-refused
           (:error (sut/prepare config {"authorization" (str "Bearer " token)}
                                {:run4-pin-ref "../pin.edn"}))))
    (is (= :run4-disabled
           (:error (sut/prepare {} {} {:run4-pin-ref "pin.edn"}))))
    (is (= :run4-credential-configuration-invalid
           (:error (sut/prepare {:run4 {:enabled? true :operator "Joe"
                                        :bearer-token "change-me"}}
                                {} {:run4-pin-ref "pin.edn"}))))))
