(ns futon3c.diagramprover.wm-wire-flight-cast-click-start-reviewer-test
  "Wire [:flight-cast :click-start :reviewer]: the click's reviewer seat, as
  click-cast writes it, reaching the click endpoint
  (futon3c.transport.http/handle-wm-click-start), whose legacy-opts read
  :reviewer when it is a nonblank string.

  No live record carries both ends: the eighth flight's tick run record
  carries the endpoint's received view ([:participants :roles
  :configured-reviewer] {:status :present :identity claude-13}) but not click-cast's output, and
  its flight record carries click-cast's output (the click entry's :cast,
  and the plan's :resolved-steps :cast) but nothing the endpoint received
  (see live-records-read, each pinned). So the wire is
  WITNESSED-HERMETICALLY: click-cast (the writer's var) is called, the
  POST body is built exactly as http-click-fn builds it (only the cast's
  string values go on the body), and handle-wm-click-start (the reader's
  var) is called with that body, with runner-service/click! and
  cast-preflight-refusal redefed so the read is observed from the opts the
  endpoint hands the click. The seat values are the eighth flight's own:
  reviewer claude-13 (author claude-6 rides beside it)."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is]]
            [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.transport.http :as http]
            [futon3c.wm.runner-service :as runner-service]))

(def seats {:author "claude-6" :reviewer "claude-13"})

(defn observe
  "click-cast of CAST-OPTS, posted as http-click-fn posts it, read by
  handle-wm-click-start: {:writer the cast's :reviewer, :reader the :reviewer
  the endpoint's legacy-opts read (nil when the body carried no :reviewer),
  :response-status}."
  [cast-opts]
  (let [cast (fr/click-cast cast-opts)
        captured (atom nil)
        body (merge {:flight-edn (pr-str {:target "M-wire" :wants []})
                     :run-id "wire-run" :issuing-caller "wm-flight"
                     :trigger "duree-click-on-demand"}
                    (into {} (filter (comp string? val)) cast))
        resp (with-redefs [runner-service/click! (fn [opts] (reset! captured opts) {:click-id "wire-click-1"})
                           runner-service/cast-preflight-refusal (fn [_] nil)]
               (@#'http/handle-wm-click-start {:body (json/generate-string body)} {}))]
    {:writer (:reviewer cast)
     :reader (:reviewer @captured)
     :response-status (:status resp)}))

(defn check [] (observe seats))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn")
      :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970"
      :why "the reader's end only: [:participants :roles :configured-reviewer] is {:status :present :identity claude-13}, what the endpoint received; click-cast's output is not on the run record"}
     {:path (p "flight-ada87008/flight-ada87008.edn")
      :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
      :why "the writer's end only: the click entry's :cast :reviewer is claude-13 (and the plan's :resolved-steps :cast agrees); nothing the endpoint received is on the flight record"}
     {:path (p "flight-278b6988.edn")
      :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
      :why "neither end: the seventh flight predates WM-CAST-I, its click entry carries no :cast"}]))

(def wire
  {:wire [:flight-cast :click-start :reviewer]
   :kind :witnessed-hermetically
   :test `the-reviewer-seat-reaches-the-click-endpoint
   :check check
   :live-records-read live-records-read})

(deftest the-reviewer-seat-reaches-the-click-endpoint
  (let [o (check)]
    (is (= 200 (:response-status o)) "the endpoint accepted the click")
    (is (= "claude-13" (:writer o)))
    (is (w/received? o))))

(deftest an-absent-reviewer-never-crosses-and-fails-the-wire
  ;; click-cast types the absence; http-click-fn posts only string values,
  ;; so the endpoint reads nothing — the wire's bad case is the live one
  ;; (the eighth flight's repair-reviewer crossed exactly this way)
  (let [o (observe {})]
    (is (= {:absent :no-reviewer-given} (:writer o)))
    (is (nil? (:reader o)) "no :reviewer on the body, none read")
    (is (not (w/received? o))))
  (is (not (w/received? (assoc (check) :reader {:absent :no-reviewer-given}))))
  (is (not (w/received? (assoc (check) :reader {:status :absent :reason :no-reviewer-given})))))

(deftest a-different-reviewer-fails-the-wire
  (let [o (check)
        other (observe {:author "claude-6" :reviewer "claude-6"})]
    (is (= "claude-6" (:reader other)))
    (is (not= (:writer o) (:reader other)))
    (is (not (w/received? (assoc o :reader (:reader other)))))))

(deftest the-live-records-carry-one-end-each
  (let [[run flight seventh] live-records-read]
    (doseq [{:keys [path sha256]} [run flight seventh]]
      (is (= sha256 (w/sha256-file path)) path))
    (is (= {:status :present :identity "claude-13"}
           (get-in (w/read-record (:path run)) [:participants :roles :configured-reviewer])))
    (let [r (w/read-record (:path flight))]
      (is (= "claude-13" (get-in r [:flight :clicks 0 :cast :reviewer])))
      (is (= "claude-13" (get-in r [:plan :resolved-steps :cast :reviewer]))))
    (is (not-any? :cast (:clicks (:flight (w/read-record (:path seventh))))))))
