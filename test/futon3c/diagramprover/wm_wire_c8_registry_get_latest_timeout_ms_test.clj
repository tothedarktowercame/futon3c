(ns futon3c.diagramprover.wm-wire-c8-registry-get-latest-timeout-ms-test
  "Wire [:c8-registry-get :c8-latest :timeout-ms]: the registry GET's read
  timeout reaching fetch-latest-for's refusal.

  flight-ffcd772b's two C8 refusals are this reader's live reads, recorded
  before WM-SPIKE-FIX-II D as bare :unreachable — the second flight's two
  C8 refusals were a 5.66 s endpoint read against the 5 s timeout, recorded
  with no :timeout-ms (registry-get's docstring), which is the defect this
  field exists to repair. No live record carries the field, so the wire is
  WITNESSED-HERMETICALLY: a server that accepts and never answers makes
  registry-get (writer, private, captured mid-call) time out under a bound
  *registry-timeout-ms*, and fetch-latest-for (reader, through its public
  wrapper fetch-latest-for-namespace) merges :timeout-ms into the refusal's
  :data by select-keys (observation_checks.clj:259)."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.observation-checks :as oc]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-c8-registry-get-entry-message-test :as msg-wire]))

(def ^:private registry-get-var
  (ns-resolve 'futon2.aif.observation-checks 'registry-get))

(defn- hanging-server []
  (let [s (java.net.ServerSocket. 0)
        f (future (try (let [c (.accept s)] (Thread/sleep 3000) (.close c))
                       (catch Exception _ nil)))]
    [s f]))

(defn observe
  "fetch-latest-for (reader) against a server that never answers, with
  *registry-timeout-ms* bound to 50 and registry-get (writer) captured
  mid-call. TAMPER edits the reader's refusal before the field is read
  (the bad cases). {:writer the writer's :timeout-ms, :reader the
  refusal's [:data :timeout-ms] (a typed absence when not carried)}."
  ([] (observe identity))
  ([tamper]
   (let [[srv f] (hanging-server)
         captured (atom nil)
         orig @registry-get-var]
     (try
       (let [refusal (binding [oc/*registry-timeout-ms* 50]
                       (with-redefs-fn {registry-get-var (fn [url] (let [r (orig url)] (reset! captured r) r))}
                         #(oc/fetch-latest-for-namespace (str "http://127.0.0.1:" (.getLocalPort srv)) "some.ns")))]
         {:writer (:timeout-ms @captured)
          :refusal refusal
          :reader (let [r (tamper refusal)]
                    (get-in r [:data :timeout-ms] {:absent :field-not-carried}))})
       (finally (.close srv) (future-cancel f))))))

(defn check [] (observe))

(def live-records-read (:live-records-read msg-wire/wire))

(def wire
  {:wire [:c8-registry-get :c8-latest :timeout-ms]
   :kind :witnessed-hermetically
   :test `the-timeout-reaches-the-latest-refusal
   :check check
   :live-records-read live-records-read})

(deftest the-timeout-reaches-the-latest-refusal
  (let [o (check)]
    (is (= :registry-unreadable (get-in o [:refusal :kind])))
    (is (= :unreachable (get-in o [:refusal :data :status])))
    (is (= 50 (:writer o)) "the writer recorded the bound timeout")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(update % :data dissoc :timeout-ms))]
    (is (= {:absent :field-not-carried} (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-timeout-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % [:data :timeout-ms] 5000))]
    (is (some? (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))
