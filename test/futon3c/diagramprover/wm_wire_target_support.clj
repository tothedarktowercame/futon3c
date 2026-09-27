(ns futon3c.diagramprover.wm-wire-target-support
  "Target handoffs: resolve-target -> start -> real readers. TAMPER changes
  only the flight record's :target before the reader. IO ports are isolated."
  (:require [clojure.java.io :as io]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-driver :as driver]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.interpretation-request :as ireq]
            [futon2.aif.want-interpretation :as wi]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as r9]))

(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn" :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/flight-6cda5ee8.edn" :sha256 "b26d4c3cc98009b1f7a828cd2355f6b01bb03feaeda8f03bf43a8a94f0292e34"
   :why "Plan placement and completed flight target; ask refusals sometimes retain target. Click summaries omit target; enactments do not dispatch; reading entries omit issued target."}
  {:path "holes/labs/M-wm-wiring/spike/flight-d00574c8.edn" :sha256 "68e531b462d535983ea5114a8da370daec943a3b4283b6b87831e63ecf9eadd8"
   :why "Plan placement and completed flight target; ask refusals sometimes retain target. Click summaries omit target; enactments do not dispatch; reading entries omit issued target."}
  {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn" :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/001-time-step.edn" :sha256 "17fd3dde3352f281b21e214c8b5f468e29001686a23da9d3b3c3aa5f681cbefc"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/003-construction.edn" :sha256 "cb60510eff60e2441250fdf6b3b54aedf9fec2e7b5702596c8fc20f4ab0cdeea"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/004-dispatch.edn" :sha256 "11fbf36986a6fecc436d127600f475718704707771cacc5362518f55c4261c8e"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/005-build.edn" :sha256 "2d405ac2294f5bfc079e4948eea9385ffcce4315f77fa3212488869bd4d60e0c"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/route-attestation.edn" :sha256 "331c2802bbb19f8a33177f5d8cc30f6f82f602d016a0e17ff0a49b2c9dec11d3"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/002-selection.edn" :sha256 "ea021f12b2a8835e107805285bcde3394a887d9c9db6466b0fb7bfefcd59f37a"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/006-adjudication.edn" :sha256 "87c49e95ec0cc31059c8353833ae9dd124253e8edc3be5b61ea95a79a84c079e"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/morning-brief-item.edn" :sha256 "43cbb4978457a0ec16f86b547482ecfbaa0d9632b7cdf9589e1e92ce34a8f350"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/b-update.edn" :sha256 "15921381e1b12379bd30aa1961f2fdfab59c006eee9290182f5500142fc46194"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/007-closed.edn" :sha256 "e5c545ba48faaa11bffe46e9832859a87bcca95aca4cb98207be6a8c09767c37"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/flight-7f89646a.edn" :sha256 "d782b3830a040dca8cfe080440869ab4a08ede22fbfd9650eb488d5f7449cc26"
   :why "Plan placement and completed flight target; ask refusals sometimes retain target. Click summaries omit target; enactments do not dispatch; reading entries omit issued target."}
  {:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn" :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
   :why "Plan placement and completed flight target; ask refusals sometimes retain target. Click summaries omit target; enactments do not dispatch; reading entries omit issued target."}
  {:path "holes/labs/M-wm-wiring/spike/flight-7f89646a-repair-finding.edn" :sha256 "758eaeb64b3c0f0ef3733a1725c9a8a54f8a23701698a26373fc44b29279e264"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/driver-error-report-flight-74325007.edn" :sha256 "255280c785c17ac2e7efcd861e31b4bdc6d77cb758b32844e09308d7faaba89f"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/flight-e70b4baf.edn" :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c"
   :why "Plan placement and completed flight target; ask refusals sometimes retain target. Click summaries omit target; enactments do not dispatch; reading entries omit issued target."}
  {:path "holes/labs/M-wm-wiring/spike/flight-ffcd772b.edn" :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575"
   :why "Plan placement and completed flight target; ask refusals sometimes retain target. Click summaries omit target; enactments do not dispatch; reading entries omit issued target."}
  {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn" :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/flight-278b6988-repair-finding.edn" :sha256 "1a9b134ab9ab0dc2b1e385872015069c15cacbf8e5f2c52908363bde92571ed8"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn" :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/flight-ada87008.edn" :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
   :why "Plan placement and completed flight target; ask refusals sometimes retain target. Click summaries omit target; enactments do not dispatch; reading entries omit issued target."}
  {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn" :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}
  {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/repair-occ-036463620c0c032c9e46aa44b6a6d6b35ed3e0ceb27dc58030f07d8fc2e6c747.edn" :sha256 "95bbb9c5476aedaadc50125068fff5f0a7e773c29a7a7494d84be58a57d893ad"
   :why "Tick/checkpoint/finding values are downstream decisions, not recorded judge options, read requests, enact dispatches or publication target observations; no same-record flight placement pair."}])

(defn record-pin [name]
  (first (filter #(.endsWith ^String (:path %) name) live-records-read)))

(defn pinned [{:keys [path sha256]}]
  (when-not (= sha256 (w/sha256-file path))
    (throw (ex-info "Target record pin moved" {:path path})))
  (w/read-record path))

(def run-pin
  (assoc (record-pin "/flight-278b6988.edn")
         :writer-path [:plan :placement :target] :reader-path [:flight :target]))
(def ask-pin
  (assoc (record-pin "/flight-d00574c8.edn")
         :writer-path [:plan :placement :target]
         :reader-path [:flight :asks 0 :asked 0 :reasons 0 :refusal :target]))
(def tick-pin (record-pin "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"))

(defn live-check [kind]
  (let [r (pinned (if (= kind :flight-run) run-pin ask-pin))]
    {:writer (get-in r [:plan :placement :target])
     :reader (get-in r (if (= kind :flight-run) [:flight :target]
                          [:flight :asks 0 :asked 0 :reasons 0 :refusal :target]))}))

(defn new-flight []
  (let [placement (driver/resolve-target (get-in (pinned run-pin) [:plan :placement]))]
    (flight/start (assoc placement :chosen-because {:kind :requested})
                  {:kind :operator-declared :wants [:done] :declared-by "wire-test"}
                  {:id "wire-target"})))

(defn click-reader [f]
  (let [seen (atom nil) summary fr/record-summary]
    (with-redefs [fr/record-summary (fn [target id record]
                                    (reset! seen target)
                                    (summary target id record))]
      ((fr/http-click-fn {:today (constantly "fixture")
                         :post! (fn [_] {:status 200 :body {:click-id "fixture"}})
                         :get-status! (constantly {:running? false})
                         :read-record! (fn [_] (pinned tick-pin))})
       {:flight f}))
    @seen))

(defn run-reader [f]
  (let [seen (atom nil)]
    (flight/run! f {:max-clicks 1 :sources-fn (constantly {})
                    :observe-fn (fn [t _] (reset! seen t) {:done false})
                    :click-fn (fn [_] {:click-id "fixture" :unreached-wants []})})
    @seen))

(defn read-reader [f]
  (let [seen (atom nil) store (w/tmp-dir "target-read-")
        text "# Target\nThe owner has not listed acceptance criteria.\n"
        f (assoc f :want-source {:kind :a-exits :repo "fixture" :path "mission.md"
                                 :store store :read-text (fn [& _] text)})]
    ((fr/read-fn {:store store :read-text (fn [& _] text)
                  :answer-fn (fn [issued] (reset! seen (:target issued))
                               {:state "pending" :seat "fixture"})}) f {})
    @seen))

(defn ask-reader [f]
  (let [seen (atom nil) root (w/tmp-dir "target-ask-")
        mission (io/file root "mission.md") code (io/file root "retriever.py")
        index (io/file root "index.json") request wi/request!
        _ (spit mission "# Target\nBuild the artifact.\n")
        _ (spit code "# hermetic retrieval\n") _ (spit index "[]")
        options {:resolve-fn (fn [action] {:id (:target action) :path (str mission)})
                 :revision-fn (constantly "fixture-revision") :library-fn (constantly [])
                 :retrieve-fn (fn [_] [{:pattern "fixture/pattern" :score 1}])
                 :retriever-specs (mapv #(assoc % :implementation (str code) :index (str index))
                                        ireq/retrievers)}]
    (with-redefs [wi/request! (fn [m path opts]
                               (reset! seen (:target m))
                               (request m path opts))]
      ((fr/ask-fn {:store root :request-options options
                   :answer-fn (fn [_] {:state "pending"})})
       f {:wants [:done] :source {:criteria-by-token
                                 {:done {:stated "Build the artifact." :line 2 :lines [2 2]}}}}
       {:beta-by-context {:WM {:beta 1}}}))
    @seen))

(defn close-reader [f]
  ;; Keep the existing isolated runner fixture, inject the carrier BEFORE
  ;; the real core. The judge exception deliberately has no :target, so
  ;; judge-refusal must use the target run-opportunity-core! reads from opts.
  (let [run runner/run-opportunity!
        result (with-redefs [runner/run-opportunity!
                             (fn [opts] (run (assoc opts :flight f)))]
                 (r9/run-tick (ex-info "cascade decision refused" {:kind :live-c-stale})))]
    (get-in result [:record :decision :abstention :targets 0 :target])))

(defn enact-reader [f]
  (let [seen (atom nil)
        chosen (get-in (pinned tick-pin) [:decision :chosen])]
    ((fr/enact-fn {:dispatch-step! (fn [step]
                                    (reset! seen (:target step))
                                    {:failed {:reason :fixture-no-enactment}})})
     f {:click-id "fixture" :chosen chosen})
    @seen))

(defn observe [kind tamper]
  (let [written (new-flight) carrier (update written :target tamper)]
    (try
      {:writer (:target written)
       :reader (case kind
                 :flight-click (click-reader carrier)
                 :flight-judge-opts (get-in (flight/judge-opts carrier (flight/click-wants written {}))
                                            [:flight :target])
                 :flight-run (run-reader carrier)
                 :r2-flight-read (read-reader carrier)
                 :flight-ask-fn (ask-reader carrier)
                 :tick-flight-assembly (first (:targets (wm/flight-assembly-input carrier {})))
                 :r9-close-cause (close-reader carrier)
                 :r0-enact-step (enact-reader carrier)
                 :r10-observe-publication (get-in ((fr/observe-publication-fn {}) carrier {})
                                                  [:publication-observed :target]))}
      (catch Exception e
        ;; A malformed target may be rejected before the downstream port.
        ;; Preserve the exception for diagnosis; positive cases must receive.
        {:writer (:target written) :reader nil :error (ex-message e)}))))
