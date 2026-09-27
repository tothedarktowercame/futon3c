(ns futon3c.wm.click-harness-http-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon3c.transport.http :as http]
            [futon3c.wm.r10-commission :as commission]
            [futon3c.wm.r10-commission-binding :as binding]
            [futon3c.wm.runner-service :as runner])
  (:import (java.nio.file Files)))

(defn request []
  {:request-method :post :uri "/api/alpha/wm/click" :headers {}
   :body (java.io.ByteArrayInputStream.
          (.getBytes (json/generate-string {:r10-commissioned true}) "UTF-8"))})

(deftest commissioned-route-stamps-only-accepted-receipts
  (let [dir (.toFile (Files/createTempDirectory "click-harness" (make-array java.nio.file.attribute.FileAttribute 0)))
        db (atom {:entries {} :order []}) calls (atom 0)
        selected (atom nil)
        handler (http/make-handler {:evidence-store db})]
    (try
      ;; Only authority lookup and the execution port are replaced. Actual
      ;; route, commission reservations, adapter, ledger and AtomBackend run.
      (with-redefs [binding/authorized-commission #(deref selected)
                    binding/reservation-root (str (io/file dir "reservations"))
                    runner/status (constantly {:running? false})
                    runner/click! (fn [_] {:click-id (str "wm-click-test-" (swap! calls inc))})]
        (doseq [n [1 2]]
          (let [file (io/file dir (str n ".edn"))
                data {:schema :wm/r10-click-commission-v1 :commission/id (str "c" n)
                      :commission/issuer "joe" :commission/source-pin (apply str (repeat 64 "a"))
                      :commission/scope {:node :R10 :test-only true}}]
            (spit file (pr-str data))
            (reset! selected (commission/load-authorized-commission
                              {:authority-path (str file)
                               :authority-sha256 (commission/sha256-bytes
                                                  (Files/readAllBytes (.toPath file)))}))
            (is (= 200 (:status (handler (request)))))))
        (let [rows (mapv #(get-in @db [:entries %]) (:order @db))]
          (is (= 2 (count rows)))
          (is (= ["wm-click-test-1" "wm-click-test-2"]
                 (mapv #(get-in % [:evidence/harness :execution-id]) rows)))
          (doseq [row rows]
            (is (= {:kind :war-machine :basis :producer-context
                    :execution-id (get-in row [:evidence/body :dispatch/receipt :click/id])}
                   (:evidence/harness row)))
            ;; Existing ledger origin is unknown, not operator inferred from
            ;; commission issuer; this packet must not invent an origin grant.
            (is (= :unknown (get-in row [:evidence/origin :kind])))
            (is (= :unknown (get-in row [:evidence/origin :authorization])))))
        (let [before @db]
          ;; Reusing the consumed commission fails actual admission.
          (is (= 500 (:status (handler (request)))))
          (is (= before @db))
          (is (= 2 @calls))))
      (finally
        (doseq [f (reverse (file-seq dir))] (io/delete-file f))))))
