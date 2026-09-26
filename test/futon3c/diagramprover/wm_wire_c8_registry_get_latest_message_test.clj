(ns futon3c.diagramprover.wm-wire-c8-registry-get-latest-message-test
  "Wire [:c8-registry-get :c8-latest :message]: the registry GET's exception
  message reaching fetch-latest-for's refusal.

  flight-ffcd772b carries this reader's two live refusals (the command and
  namespace latest-lookups), both recorded before WM-SPIKE-FIX-II D's
  writer as bare :unreachable with no :message, so no live record carries
  the field (live-records-read). So the wire is WITNESSED-HERMETICALLY:
  fetch-latest-for-namespace (the public wrapper of the reader's var,
  fetch-latest-for) runs against a malformed base (an illegal scheme
  character, so no network is touched) with registry-get (writer, private)
  captured mid-call; the writer's value is the :message
  registry-get returned, the reader's the :message of the refusal's :data
  (merged by select-keys at observation_checks.clj:259)."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.observation-checks :as oc]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-c8-registry-get-entry-message-test :as msg-wire]))

(def ^:private registry-get-var
  (ns-resolve 'futon2.aif.observation-checks 'registry-get))

(defn observe
  "fetch-latest-for (reader, through fetch-latest-for-namespace) against a
  malformed base, registry-get (writer) captured mid-call. TAMPER edits the
  reader's refusal before the field is read (the bad cases). {:writer the
  writer's :message, :reader the refusal's [:data :message] (a typed
  absence when not carried)}."
  ([] (observe identity))
  ([tamper]
   (let [captured (atom nil)
         orig @registry-get-var
         refusal (with-redefs-fn {registry-get-var (fn [url] (let [r (orig url)] (reset! captured r) r))}
                   #(oc/fetch-latest-for-namespace "ht tp://bad host" "some.ns"))]
     {:writer (:message @captured)
      :refusal refusal
      :reader (let [r (tamper refusal)]
                (get-in r [:data :message] {:absent :field-not-carried}))})))

(defn check [] (observe))

;; flight-ffcd772b's two refusals ARE this reader's (both are
;; test-registry/latest lookups): the same pinned record, the same finding
(def live-records-read (:live-records-read msg-wire/wire))

(def wire
  {:wire [:c8-registry-get :c8-latest :message]
   :kind :witnessed-hermetically
   :test `the-exception-message-reaches-the-latest-refusal
   :check check
   :live-records-read live-records-read})

(deftest the-exception-message-reaches-the-latest-refusal
  (let [o (check)]
    (is (= :registry-unreadable (get-in o [:refusal :kind])))
    (is (= :unreachable (get-in o [:refusal :data :status])))
    (is (string? (:writer o)) "the writer wrote the exception's message")
    (is (w/received? o))))

(deftest a-typed-absence-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % [:data :message] {:absent :no-message-written}))]
    (is (w/typed-absence? (:reader o)))
    (is (not (w/received? o)))))

(deftest a-different-message-at-the-reader-fails-the-wire
  (let [o (observe #(assoc-in % [:data :message] "some other message"))]
    (is (some? (:reader o)))
    (is (not= (:writer o) (:reader o)))
    (is (not (w/received? o)))))
