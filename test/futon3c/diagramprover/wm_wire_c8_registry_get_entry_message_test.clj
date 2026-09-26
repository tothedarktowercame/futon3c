(ns futon3c.diagramprover.wm-wire-c8-registry-get-entry-message-test
  "Wire [:c8-registry-get :c8-entry :message]: the registry GET's exception
  message reaching fetch-registry-entry's refusal.

  No live record carries either end: the only C8 refusals on record
  (flight-ffcd772b's two latest-lookups, 2026-09-25) were recorded before
  WM-SPIKE-FIX-II D's writer, as bare :unreachable with no :message, and no
  record hits fetch-registry-entry's /api/alpha/evidence/ endpoint at all
  (live-records-read). So the wire is WITNESSED-HERMETICALLY: the writer's
  var (observation-checks/registry-get, private) is captured mid-call while
  the reader's var (fetch-registry-entry) runs against a malformed base
  (an illegal scheme character, so no network is touched); the writer's
  value is the :message registry-get returned, the reader's the
  :message of the refusal's :data (merged by select-keys at
  observation_checks.clj:230)."
  (:require [clojure.test :refer [deftest is]]
            [futon2.aif.observation-checks :as oc]
            [futon3c.diagramprover.wm-wire :as w]))

(def ^:private registry-get-var
  (ns-resolve 'futon2.aif.observation-checks 'registry-get))

(defn observe
  "fetch-registry-entry (reader) against a malformed base — an illegal
  scheme character, so registry-get's try/catch fires with no network at
  all — with registry-get (writer) wrapped so its return is captured from
  the same call. TAMPER
  edits the reader's refusal before the field is read (the bad cases).
  {:writer the writer's :message, :reader the refusal's [:data :message]
  (a typed absence when the key is not carried)}."
  ([] (observe identity))
  ([tamper]
   (let [captured (atom nil)
         orig @registry-get-var
         refusal (with-redefs-fn {registry-get-var (fn [url] (let [r (orig url)] (reset! captured r) r))}
                   #(oc/fetch-registry-entry "ht tp://bad host" "e-wire"))]
     {:writer (:message @captured)
      :refusal refusal
      :reader (let [r (tamper refusal)]
                (get-in r [:data :message] {:absent :field-not-carried}))})))

(defn check [] (observe))

(def live-records-read
  (let [p #(str w/spike-dir "/" %)]
    [{:path (p "flight-ffcd772b.edn")
      :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575"
      :why "its two C8 refusals (the command and namespace latest-lookups, fetch-latest-for's reads) carry {:status :missing :kind :registry-unreadable :data {:status :unreachable}} — neither :message nor :timeout-ms: the flight (2026-09-25) predates WM-SPIKE-FIX-II D's writer, so the record shows the field's read site with nothing written"}
     {:paths (mapv p ["tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn"
                      "tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
                      "tick-run-record-2026-09-26-flight-7f89646a-click-1.edn"
                      "tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn"
                      "flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn"])
      :why "no run record carries a C8 refusal with :message, and none hits fetch-registry-entry's endpoint: grep of the spike records finds no /api/alpha/evidence/ URL"}
     {:paths ["holes/labs/M-futon-seams/exemplar/click-001.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
              "holes/labs/M-futon-seams/exemplar/click-001-outcome.edn"]
      :why "hand-authored exemplar records; no registry read in any"}]))

(def wire
  {:wire [:c8-registry-get :c8-entry :message]
   :kind :witnessed-hermetically
   :test `the-exception-message-reaches-the-entry-refusal
   :check check
   :live-records-read live-records-read})

(deftest the-exception-message-reaches-the-entry-refusal
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

(deftest the-live-records-carry-no-message
  (let [{:keys [path sha256]} (first live-records-read)
        r (w/read-record path)
        refusals (atom [])]
    (is (= sha256 (w/sha256-file path)))
    (letfn [(walk [x]
              (when (map? x)
                (when (= :registry-unreadable (:kind x)) (swap! refusals conj x))
                (run! walk (vals x)))
              (when (vector? x) (run! walk x)))]
      (walk r))
    (is (= 2 (count @refusals)) "the two latest-lookup refusals")
    (is (every? #(= :unreachable (get-in % [:data :status])) @refusals))
    (is (not-any? #(contains? (:data %) :message) @refusals))
    (is (not-any? #(contains? (:data %) :timeout-ms) @refusals))))
