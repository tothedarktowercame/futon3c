(ns futon3c.wm.r10-click-adapter-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.evidence.backend :as backend]
            [futon3c.evidence.store :as store]
            [futon3c.wm.r10-click-adapter :as adapter]
            [futon3c.wm.r10-commission :as commission]
            [futon3c.wm.r10-commission-binding :as binding]
            [futon3c.wm.runner-service :as runner])
  (:import (java.nio.charset StandardCharsets)
           (java.nio.file Files StandardOpenOption)))

(defn- fixture [dir id]
  (let [path (.resolve dir (str id ".authority.edn"))
        record {:schema :wm/r10-click-commission-v1
                :commission/id id :commission/issuer "test-operator"
                :commission/source-pin (apply str (repeat 64 "a"))
                :commission/scope {:node :R10 :test-only true}}
        bytes (.getBytes (str (pr-str record) "\n") StandardCharsets/UTF_8)]
    (Files/write path bytes (into-array StandardOpenOption
                                        [StandardOpenOption/CREATE_NEW
                                         StandardOpenOption/WRITE]))
    (commission/load-authorized-commission
     {:authority-path (str path) :authority-sha256 (commission/sha256-bytes bytes)})))

(defn- invoke [commission root evidence-store status click-fn]
  (with-redefs [binding/authorized-commission (constantly commission)
                binding/reservation-root root
                runner/status (constantly status)]
    (adapter/commissioned-click! {:config {:evidence-store evidence-store}
                                  :click-fn click-fn})))

(defn- code-of [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:error/code (ex-data e)))))

(deftest commissioned-click-happy-and-duplicate
  (let [dir (Files/createTempDirectory "r10-adapter" (make-array java.nio.file.attribute.FileAttribute 0))
        root (str (.resolve dir "reservations"))
        c (fixture dir "adapter-happy")
        evidence (atom {:entries {} :order []})
        calls (atom 0)
        click (fn [_] (swap! calls inc) {:click-id "click-happy" :started-at "now"})
        result (invoke c root evidence {:running? false} click)]
    (is (:ok result))
    (is (= "click-happy" (get-in result [:receipt :dispatch/id])))
    (is (= "click-happy" (get-in result [:receipt :click/id])))
    (is (= :recorded (:recovery-state
                       (commission/read-reservation {:reservation-root root
                                                     :commission-id "adapter-happy"}))))
    (is (= 1 (count (store/query* evidence {:query/type :coordination
                                            :query/limit 10}))))
    (is (= :r10/duplicate-commission
           (code-of #(invoke c root evidence {:running? false} click))))
    (is (= 1 @calls))))

(deftest pre-reservation-refusals-do-not-click-or-burn
  (let [dir (Files/createTempDirectory "r10-pre" (make-array java.nio.file.attribute.FileAttribute 0))
        c (fixture dir "adapter-pre") calls (atom 0) click #(swap! calls inc)]
    (with-redefs [binding/authorized-commission (constantly c)
                  binding/reservation-root (str (.resolve dir "missing-store-root"))
                  runner/status (constantly {:running? false})]
      (is (= :r10/evidence-store-missing
             (code-of #(adapter/commissioned-click! {:config {} :click-fn click})))))
    (is (= 0 @calls))
    (is (not (Files/exists (.resolve dir "missing-store-root") (make-array java.nio.file.LinkOption 0))))
    (is (= :r10/runner-busy
           (code-of #(invoke c (str (.resolve dir "busy-root")) (atom {:entries {} :order []})
                             {:running? true} click))))
    (is (= 0 @calls))
    (is (not (Files/exists (.resolve dir "busy-root") (make-array java.nio.file.LinkOption 0))))))

(deftest thrown-and-raced-clicks-leave-dangling-and-cannot-retry
  (doseq [[id click expected]
          [["adapter-throw" (fn [_] (throw (ex-info "boom" {:boom true})) ) nil]
           ["adapter-race" (constantly {:rejected :already-running :click-id "other"})
            :r10/click-rejected]]]
    (let [dir (Files/createTempDirectory id (make-array java.nio.file.attribute.FileAttribute 0))
          c (fixture dir id) root (str (.resolve dir "reservations"))
          evidence (atom {:entries {} :order []}) calls (atom 0)
          counted (fn [opts] (swap! calls inc) (click opts))
          observed (code-of #(invoke c root evidence {:running? false} counted))]
      (when expected (is (= expected observed)))
      (is (= 1 @calls))
      (is (= :dangling (:recovery-state
                         (commission/read-reservation {:reservation-root root
                                                       :commission-id id}))))
      (is (= :r10/duplicate-commission
             (code-of #(invoke c root evidence {:running? false} counted))))
      (is (= 1 @calls)))))

(deftest recording-failure-is-after-click-and-leaves-dispatched
  (let [dir (Files/createTempDirectory "r10-record-fail" (make-array java.nio.file.attribute.FileAttribute 0))
        c (fixture dir "adapter-record-fail") root (str (.resolve dir "reservations"))
        calls (atom 0)
        refusing (reify backend/EvidenceBackend
                    (-append [_ _] {:ok false :error/code :test-refusal :error/message "no"})
                    (-get [_ _] nil) (-exists? [_ _] false) (-query [_ _] [])
                    (-count [_ _] 0) (-forks-of [_ _] []) (-delete! [_ _] nil) (-all [_] []))
        click (fn [_] (swap! calls inc) {:click-id "click-record-fail"})]
    (is (= :r10/recording-failed
           (code-of #(invoke c root refusing {:running? false} click))))
    (is (= 1 @calls))
    (is (= :dispatched (:recovery-state
                         (commission/read-reservation {:reservation-root root
                                                       :commission-id "adapter-record-fail"}))))
    (is (= :r10/duplicate-commission
           (code-of #(invoke c root refusing {:running? false} click))))
    (is (= 1 @calls))))

(deftest corrupt-reservation-has-reservation-vocabulary
  (let [dir (Files/createTempDirectory "r10-corrupt" (make-array java.nio.file.attribute.FileAttribute 0))
        path (.resolve dir "corrupt.edn")]
    (Files/write path (.getBytes "{:broken" StandardCharsets/UTF_8)
                 (into-array StandardOpenOption [StandardOpenOption/CREATE_NEW StandardOpenOption/WRITE]))
    (is (= :r10/reservation-invalid
           (code-of #(commission/read-reservation {:reservation-root (str dir)
                                                    :commission-id "corrupt"}))))))

(deftest issuer-provenance-is-descriptive-and-survives-commission
  (let [dir (Files/createTempDirectory "r10-issuer" (make-array java.nio.file.attribute.FileAttribute 0))
        c (fixture dir "issuer")
        provenance {:status :present :identity "commissioner" :source :wm-click-http-boundary}
        observed (atom nil)]
    (with-redefs [binding/authorized-commission (constantly c)
                  binding/reservation-root (str (.resolve dir "reservations"))
                  runner/status (constantly {:running? false})]
      (is (:ok (adapter/commissioned-click!
                {:config {:evidence-store (atom {:entries {} :order []})}
                 :issuer-provenance provenance
                 :click-fn (fn [opts] (reset! observed opts)
                             {:click-id "test-issuer" :started-at "now"})}))))
    (is (= {:issuer-provenance provenance} @observed))
    (let [broken (dissoc @observed :issuer-provenance)]
      (is (not= @observed broken))
      (is (not= provenance (:issuer-provenance broken))))))
