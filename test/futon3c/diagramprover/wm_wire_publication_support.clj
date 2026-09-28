(ns futon3c.diagramprover.wm-wire-publication-support
  "Shared support for the PROOF-2a <2>3 lane-A wire tests: the publication
  and enactment-carry group. Each function drives the REAL writer var from
  its map site through the REAL reader var; TAMPER/MUTATE rewrites the
  carrier between them (the bad cases: a typed absence at the field, and a
  different value). The reader's value is what the reader produced (or
  received at its door), never a restated literal.

  Live records: no record under holes/labs/M-wm-wiring/spike/ carries both
  ends of any of these six wires (see each test namespace's
  live-records-read, each pinned and read), so all six are witnessed
  hermetically or left unverified with the reason."
  (:require [clojure.java.io :as io]
            [clojure.test :as t]
            [futon2.aif.cascade-problems :as cp]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.mission-reading :as mr]
            [futon2.aif.observation-rates :as rates]
            [futon2.aif.want-interpretation :as wi]
             [futon2.aif.wm.cascade-decision :as wm-cd]
            [futon2.aif.flight-enact-test :as enact-test]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-enact-driver :as driver]))

(defn cleanup [root]
  (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f)))

;; ---------------------------------------------------------------------------
;; The pinned live records these lanes read (shas verified by the tests).

(def tick-278b6988
  {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
   :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"})

(def tick-e70b4baf
  {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn"
   :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"})

(def flight-278b6988
  {:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn"
   :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"})

(def flight-ada87008
  {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/flight-ada87008.edn"
   :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"})

(def click-001-enactment
  {:path "holes/labs/M-futon-seams/exemplar/click-001-enactment.edn"
   :sha256 "e51063896e2a42096718d902e0b4dfe0e4652323de0b42848c2c0cf318bf6c89"})

;; ---------------------------------------------------------------------------
;; Labels, the way flight_conditioning_step_test's produced-measured-a
;; builds them: PRESENT admitted :present labels of class CLS (MISSED of
;; them recorded false) and ABSENT admitted :absent labels (REPORTED of
;; them recorded true) — false-neg MISSED/PRESENT, false-pos
;; REPORTED/ABSENT.

(defn labels [cls present missed absent reported]
  (vec (concat (for [i (range present)] {:token-class cls :admitted :present :recorded (>= i missed)})
               (for [i (range absent)] {:token-class cls :admitted :absent :recorded (< i reported)}))))

;; ---------------------------------------------------------------------------
;; Wire [:r6-sourced-rates :r9-measured-a-version :measurement]

(def measurement-target "M-wire-measurement")
(def measurement-token :t/wanted)

(defn measurement-observe
  "The real sourced-rates (writer, its :sourced return's :measurement)
  driven through the real measured-a-version (reader) over admitted
  labels, exactly as flight_conditioning_step_test's produced-measured-a
  does: ten labels of one class, one locator of that class. TAMPER
  rewrites the writer's :measurement (keyed token) before the reader
  qualifies it into its own :measurement keyed [target token]."
  [tamper]
  (let [source rates/sourced-rates
        written (atom nil)
        ls (labels :C4 10 1 5 1)
        problems [{:target measurement-target
                   :cascade-problem {:locators {measurement-token {:class :C4}}}}]
        ma (with-redefs [rates/sourced-rates
                         (fn [& args]
                           (let [r (apply source args)]
                             (reset! written (:measurement r))
                             (update r :measurement tamper)))]
             (wm-cd/measured-a-version
              problems
              {measurement-target {:labels ls
                                   :subjects (frequencies (map :token-class ls))}}))]
    {:writer (get @written measurement-token)
     :reader (get-in ma [:measurement [measurement-target measurement-token]])
     :measured-a ma}))

;; ---------------------------------------------------------------------------
;; Wire [:r2-store-locators :r9-measured-a-version :locators]

(def locator-target :wm-wire-locators)
(def locator-token :t/wanted)
(def flight-source-locator {:class :C3 :repo "futon2" :sha "HEAD"
                            :path "src/futon2/aif/flight.clj"})

(defn publish-locator
  "The real r2 writer chain: issue the locator request, validate the
  seat's answer with the real observation (C3: the file at the resolved
  commit, observed by observation-checks/observe through git), and publish
  into a temp store with the real publish-locator!. Returns {:store dir
  :published {token locator} read back from the store}."
  []
  (let [store (w/tmp-dir "wire-locator-store-")
        criterion {:token locator-token
                   :stated "- The wanted token is mechanically observed."}
        issued (wi/issue! store (mr/locator-request locator-target "M-wire-locators" criterion))
        response {:locator flight-source-locator
                  :cue {:quote "wanted token is mechanically observed"}
                  :reading "the flight source exists at the resolved commit"}
        validated (mr/validate-locator issued response)]
    (when-not (= :valid (:status validated))
      (throw (ex-info "the fixture locator did not validate" validated)))
    (mr/publish-locator! store issued response validated "kimi-1")
    {:store store :published (mr/published-locators store locator-target)}))

(defn locator-sources [locators]
  {:universes {locator-target {locator-token false}}
   :interpretations {locator-target {:patterns {:p/make {:guard {:needs #{} :forbids #{}}
                                                         :produces #{locator-token}}}}}
   :wants {locator-target [locator-token]}
   :candidates {locator-target [{:precedence [:p/make]
                                 :construction-receipt {:kind :construction-receipt
                                                        :moves [:interpret :order]
                                                        :family-searched 1 :coverage 1}}]}
   :locators {locator-target locators}
   :horizon-steps 1
   :beta-by-context {:tick {:beta 1}}
   :context-of (fn [_] :tick)})

(defn locators-observe
  "The store's published locators assembled into a real cascade problem by
  the real cascade-problems/assemble, then read by the real
  measured-a-version. The writer's value is the {token locator} map read
  back from the store; the reader's value is the locators map the real
  sourced-rates received from measured-a-version (captured at its door).
  MUTATE rewrites the assembled problem before the reader runs."
  [mutate]
  (let [{:keys [store published]} (publish-locator)
        source rates/sourced-rates
        received (atom nil)
        ls (labels :C3 10 1 5 1)]
    (try
      (let [{:keys [problems refusals]} (cp/assemble {:targets [locator-target]
                                                      :sources (locator-sources published)})]
        (when (seq refusals)
          (throw (ex-info "the fixture problem was refused" {:refusals refusals})))
        (let [problem (mutate (first problems))
              ma (with-redefs [rates/sourced-rates
                               (fn [& args]
                                 (reset! received (nth args 3))
                                 (apply source args))]
                   (wm-cd/measured-a-version
                    [problem]
                    {locator-target {:labels ls
                                     :subjects (frequencies (map :token-class ls))}}))]
          {:writer published
           :reader @received
           :classes (:classes ma)
           :measurement (get-in ma [:measurement [locator-target locator-token]])}))
      (finally (cleanup store)))))

;; ---------------------------------------------------------------------------
;; Wire [:r7-flight-call :flight-run :increment]

(defn increment-observe
  "A real one-click flight/run!: the real enact-fn over the lane-8
  driver's exemplar-backed fixture (the pinned click-001 grains and
  candidate), and the real wc-verdict-fn (the real checker on the hermetic
  enactment record, the real enactment-habit/increment on its verdict).
  TAMPER rewrites the verdict fn's returned :increment before run! merges
  it onto the enactments entry."
  [tamper]
  (let [dir (w/tmp-dir "wire-increment-")
        real-wc (fr/wc-verdict-fn {:checker driver/checker-path
                                   :click-record-path (constantly (:path driver/click-001))})
        written (atom nil)
        enact (fr/enact-fn {:dispatch-step! (fn [step]
                                              (if (= :plan (:phase step))
                                                {:grain (driver/role-grain)}
                                                {:commit (str "c-" (name (:pattern step)))
                                                 :produced (first (get-in step [:interpretation :produces]))
                                                 :check {:class :fixture}}))
                            :check-fn (constantly {:observed true})
                            :interpretations (constantly (driver/wc-interps))
                            :record-dir dir})]
    (try
      (let [f (flight/run! (flight/start {:target "M-t" :chosen-because {:kind :requested}}
                                         {:kind :a-exits :repo "futon2" :path "p" :read-text (fn [& _] "")}
                                         {:id "flight-wire-increment"})
                           {:click-fn (constantly {:click-id "click-1"
                                                   :chosen {:candidate driver/wc-candidate
                                                            :precedence driver/wc-precedence}})
                            :enact-fn enact
                            :wc-fn (fn [fl e]
                                     (let [r (real-wc fl e)]
                                       (reset! written (:increment r))
                                       (update r :increment tamper)))
                            :observe-fn (fn [_ _] {})
                            :sources-fn (constantly {})
                            :max-clicks 1})]
        {:writer @written
         :reader (get-in f [:enactments 0 :increment])})
      (finally (cleanup dir)))))

;; ---------------------------------------------------------------------------
;; Wire [:run-record-publication :r10-observe-publication :repair/publication]

(def publication-entries
  [{:status :receipt-committed :repair/id "occ-wire" :repair/discharged? true}])

(defn publication-observe
  "The real persist-run-record! writes a run record carrying
  :repair/publication into a temp dir; the real observe-publication-fn
  reads (:repair/publication record) off it. TAMPER rewrites the record
  between the write and the read. The writer's value is the
  :repair/publication on the record the writer wrote; the reader's value
  is the :repair/publication on the record the reader actually received,
  plus the reader's own :publication-observed product."
  [tamper]
  (let [dir (w/tmp-dir "wire-run-record-")]
    (try
      (let [saved (#'runner/persist-run-record!
                   {:run-record-dir dir
                    ;; the runner has no built-in renderer; this wire does
                    ;; not read scan output
                    :scan-render-fn (fn [& _] nil)}
                   "wire-pub" "2026-09-26T00:00:00Z"
                   {:outcome :offline-no-selection
                    :repair/publication publication-entries})
            file (:run-record saved)
            writer (:repair/publication (w/read-record file))
            read (atom nil)
            obs ((fr/observe-publication-fn
                  {:fetch-run-record (fn [_]
                                       (let [r (tamper (w/read-record file))]
                                         (reset! read (:repair/publication r))
                                         r))
                   :repair-id-fn (constantly "occ-wire")})
                 {:target "T-wire"} {:click-id "wire-pub"})]
        {:writer writer :reader @read :observation (:publication-observed obs)})
      (finally (cleanup dir)))))

;; ---------------------------------------------------------------------------
;; Wire [:r10-observe-publication :r0-enact-step :publication-observed]

(defn- wrap-publication-writer
  "with-redefs binding for fr/observe-publication-fn: call the REAL var to
  build the observation fn, capture its {:publication-observed v} (the
  writer's value), and hand TAMPER's rewrite to the caller instead."
  [real written tamper]
  (fn [opts]
    (let [f (real opts)]
      (fn [flight click]
        (let [r (f flight click)]
          (reset! written (:publication-observed r))
          {:publication-observed (tamper (:publication-observed r))})))))

(defn enact-observe
  "The real enact-fn, built with no :publication-observation override, so
  its step-12 read is the real observe-publication-fn (the writer); the
  enactment it returns carries the copy (the reader's value). TAMPER
  rewrites the observation between them."
  [tamper]
  (let [dir (w/tmp-dir "wire-enact-pub-")
        real fr/observe-publication-fn
        written (atom nil)
        enact (fr/enact-fn {:dispatch-step! (fn [_] {:commit "c" :produced :t/a :check {:class :fixture}})
                            :check-fn (constantly {:observed true})
                            :interpretations (constantly {:p/a {:produces #{:t/a}}})
                            :fetch-run-record (constantly {:repair/publication publication-entries})
                            :repair-id-fn (constantly "occ-wire")
                            :record-dir dir})]
    (try
      (let [{:keys [enactment]}
            (with-redefs [fr/observe-publication-fn (wrap-publication-writer real written tamper)]
              (enact {:flight/id "flight-wire" :target "M-t"}
                     {:click-id "click-1" :chosen {:candidate :cand/x :precedence [:p/a]}}))]
        {:writer @written :reader (:publication-observed enactment)})
      (finally (cleanup dir)))))

;; ---------------------------------------------------------------------------
;; Wire [:r10-observe-publication :r0-test :publication-observed]
;; flight_enact_test's a-discharged-repair-obligation-is-observed-as-published
;; (futon2 6d98d37c): its enact-fn passes :repair-id-fn and a real
;; persist-run-record!-written run record carrying the discharge, with no
;; :publication-observation override, so the test box drives the real
;; observe-publication-fn and a PRESENT value crosses.

(defn r0-test-observe
  "Run flight_enact_test's a-discharged-repair-obligation-is-observed-as-published
  deftest with the real observe-publication-fn wrapped (TAMPER rewrites
  its value) and clojure.test's report captured; the reader's value is
  the actual value the test's :publication-observed assertion consumed,
  read off the report."
  [tamper]
  (let [real fr/observe-publication-fn
        written (atom nil)
        reports (atom [])]
    (with-redefs [fr/observe-publication-fn (wrap-publication-writer real written tamper)
                  t/report #(swap! reports conj %)]
      ((var futon2.aif.flight-enact-test/a-discharged-repair-obligation-is-observed-as-published)))
    (let [report (first (filter #(and (#{:pass :fail} (:type %))
                                      (re-find #"publication-observed" (pr-str (:expected %))))
                                @reports))
          form (:actual report)
          equality (if (= 'not (first form)) (second form) form)]
      {:writer @written
       :reader (last equality)
       :report-type (:type report)})))
