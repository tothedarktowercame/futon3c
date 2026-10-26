(ns futon3c.wm.parent-child-verdict-test
  "R11 contract: a retained requirements record updates only its exact,
  completed retry child and becomes the parent's projected verdict."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon3c.transport.http :as http]
            [futon3c.wm.machinery-execution-cohort :as cohort]
            [futon3c.wm.runner-service :as service]
            [futon3c.wm.runner-service-test :as fixture]))

(use-fixtures :each #'fixture/reset-service!
  (fn [f] (with-redefs [cohort/apply-binding identity] (f))))

(def parent-id "wm-click-parent-verdict-r11")
(def run-record "/fixture/r11-child-run.edn")
(def initial-verdict {:status :not-checked :reason :requirements-not-run})

(defn parent-state [completion]
  {:schema :wm/click-state-v1
   :click-id parent-id
   :status :completed
   :child {:schema :wm/click-child-v1
           :click-id "wm-retry-r11"
           :completion completion}})

(defn seed! [state]
  (#'service/persist-click-state! parent-id (constantly state)))

(defn complete-state []
  (parent-state {:status :complete :run-record run-record
                 :verdict initial-verdict}))

(defn verdict-file! [value]
  (let [file (java.io.File/createTempFile
              "r11-child-verdict-" ".edn"
              (.getParentFile (io/file service/*click-acceptance-dir*)))]
    (spit file (str (pr-str value) "\n"))
    file))

(defn body-stream [value]
  (java.io.ByteArrayInputStream.
   (.getBytes (if (string? value) value (json/generate-string value)) "UTF-8")))

(deftest passed-requirements-land-and-project
  (seed! (complete-state))
  (let [file (verdict-file! {:run-record run-record :conforms true
                             :checked-at "checked" :verdicts {:Q1 :holds}})]
    (try
      (let [result (service/record-child-verdict! parent-id (.getPath file))
            expected {:status :passed :verdicts {:Q1 :holds}
                      :checked-at "checked" :verdict-file (.getCanonicalPath file)}]
        (is (= expected (:child-verdict result)))
        (is (= expected (:parent-verdict result)))
        (is (nil? (:verdict-refusal result)))
        (is (= expected (:parent-verdict (service/click-status parent-id)))))
      (finally (.delete file)))))

(deftest producer-form-with-quoted-keywords-lands
  ;; wm_run_alert.py's edn() quotes every string, keywords included; the
  ;; file on disk is `{":run-record" "..." ":conforms" false ...}`.
  (seed! (complete-state))
  (let [file (java.io.File/createTempFile
              "r11-child-verdict-" ".edn"
              (.getParentFile (io/file service/*click-acceptance-dir*)))]
    (spit file (str "{\":run-record\" " (pr-str run-record)
                    " \":checked-at\" \"2026-10-26T08:00:00+00:00\""
                    " \":conforms\" false"
                    " \":verdicts\" {\":Q1\" \":holds\" \":Q2\" \":unverifiable\"}"
                    " \":critical-parameters\" {\":Q1\" \"784 / 784\"}}\n"))
    (try
      (let [result (service/record-child-verdict! parent-id (.getPath file))]
        (is (nil? (:verdict-refusal result)))
        (is (= :failed (get-in result [:child-verdict :status])))
        (is (= {:Q1 :holds :Q2 :unverifiable}
               (get-in result [:child-verdict :verdicts])))
        (is (= "2026-10-26T08:00:00+00:00"
               (get-in (service/click-status parent-id)
                       [:parent-verdict :checked-at]))))
      (finally (.delete file)))))

(deftest failed-requirements-land-verbatim
  (seed! (complete-state))
  (let [verdicts {:Q1 :holds :Q2 :fails}
        file (verdict-file! {:run-record run-record :conforms false
                             :checked-at "checked" :verdicts verdicts})]
    (try
      (let [result (service/record-child-verdict! parent-id (.getPath file))]
        (is (= :failed (get-in result [:child-verdict :status])))
        (is (= verdicts (get-in result [:child-verdict :verdicts]))))
      (finally (.delete file)))))

(deftest wrong-run-record-is-refused-without-overwrite
  (seed! (complete-state))
  (let [file (verdict-file! {:run-record "/fixture/foreign.edn"
                             :conforms true :verdicts {:Q1 :holds}})]
    (try
      (let [result (service/record-child-verdict! parent-id (.getPath file))]
        (is (= :verdict-run-record-mismatch
               (get-in result [:verdict-refusal :reason])))
        (is (= initial-verdict
               (get-in (service/click-status parent-id)
                       [:child :completion :verdict])))
        (is (= initial-verdict (:parent-verdict result))))
      (finally (.delete file)))))

(deftest pending-child-is-refused
  (seed! (parent-state {:status :pending}))
  (let [file (verdict-file! {:run-record run-record :conforms true})]
    (try
      (let [result (service/record-child-verdict! parent-id (.getPath file))]
        (is (= {:reason :child-not-complete :completion-status :pending}
               (:verdict-refusal result)))
        (is (nil? (get-in (service/click-status parent-id)
                          [:child :completion :verdict]))))
      (finally (.delete file)))))

(deftest recorded-verdict-is-never-overwritten
  (seed! (complete-state))
  (let [passed (verdict-file! {:run-record run-record :conforms true})
        failed (verdict-file! {:run-record run-record :conforms false})]
    (try
      (service/record-child-verdict! parent-id (.getPath passed))
      (let [result (service/record-child-verdict! parent-id (.getPath failed))]
        (is (= :verdict-already-recorded
               (get-in result [:verdict-refusal :reason])))
        (is (= :passed (get-in (service/click-status parent-id)
                               [:child :completion :verdict :status]))))
      (finally (.delete passed) (.delete failed)))))

(deftest absent-and-torn-verdict-files-are-typed-refusals
  (doseq [kind [:absent :torn]]
    (seed! (complete-state))
    (let [file (verdict-file! {:placeholder true})]
      (try
        (if (= kind :absent) (.delete file) (spit file "{:run-record"))
        (let [result (service/record-child-verdict! parent-id (.getPath file))]
          (is (= :verdict-file-unreadable
                 (get-in result [:verdict-refusal :reason])))
          (is (= initial-verdict
                 (get-in (service/click-status parent-id)
                         [:child :completion :verdict]))))
        (finally (.delete file))))))

(deftest ordinary-state-is-byte-identical-on-no-child-refusal
  (seed! {:schema :wm/click-state-v1 :click-id parent-id :status :completed})
  (let [file (verdict-file! {:run-record run-record :conforms true})
        acceptance (#'service/acceptance-file parent-id)
        before (slurp acceptance)]
    (try
      (let [result (service/record-child-verdict! parent-id (.getPath file))]
        (is (= :no-child (get-in result [:verdict-refusal :reason])))
        (is (= before (slurp acceptance))))
      (finally (.delete file)))))

(deftest http-route-records-and-reads-parent-verdict
  (seed! (complete-state))
  (let [file (verdict-file! {:run-record run-record :conforms true
                             :checked-at "checked" :verdicts {:Q1 :holds}})
        handler (http/make-handler {})]
    (try
      (let [response (handler {:request-method :post
                               :uri "/api/alpha/wm/click/child-verdict"
                               :body (body-stream {:click-id parent-id
                                                   :verdict-file (.getPath file)} )})
            response-body (json/parse-string (:body response) true)
            status-response (handler {:request-method :get
                                      :uri "/api/alpha/wm/click"
                                      :query-string (str "click-id=" parent-id)})
            status-body (json/parse-string (:body status-response) true)]
        (is (= 200 (:status response)))
        (is (= "passed" (get-in response-body [:parent-verdict :status])))
        (is (= "passed" (get-in status-body [:parent-verdict :status]))))
      (let [invalid (handler {:request-method :post
                              :uri "/api/alpha/wm/click/child-verdict"
                              :body (body-stream "not json")})
            missing (handler {:request-method :post
                              :uri "/api/alpha/wm/click/child-verdict"
                              :body (body-stream {:click-id parent-id})})]
        (is (= 400 (:status invalid)))
        (is (= "invalid-json" (:error (json/parse-string (:body invalid) true))))
        (is (= 400 (:status missing)))
        (is (= "missing-field" (:error (json/parse-string (:body missing) true)))))
      (finally (.delete file)))))
