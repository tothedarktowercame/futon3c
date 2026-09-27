(ns futon3c.diagramprover.wm-wire-measured-support
  "Real labels -> decision -> persisted tick -> conditioning. Mutations touch
  the persisted carrier before its real reader; no measured-A producer stub."
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.enactment-fold-source :as folds]
            [futon2.aif.observation-rates :as rates]
            [futon2.report.war-machine :as wm]
            [futon2.report.observation-labels-consume-test :as fixture]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-r9-support :as r9]))

(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn" :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-6cda5ee8.edn" :sha256 "b26d4c3cc98009b1f7a828cd2355f6b01bb03feaeda8f03bf43a8a94f0292e34" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-d00574c8.edn" :sha256 "68e531b462d535983ea5114a8da370daec943a3b4283b6b87831e63ecf9eadd8" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn" :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/001-time-step.edn" :sha256 "17fd3dde3352f281b21e214c8b5f468e29001686a23da9d3b3c3aa5f681cbefc" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/003-construction.edn" :sha256 "cb60510eff60e2441250fdf6b3b54aedf9fec2e7b5702596c8fc20f4ab0cdeea" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/004-dispatch.edn" :sha256 "11fbf36986a6fecc436d127600f475718704707771cacc5362518f55c4261c8e" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/005-build.edn" :sha256 "2d405ac2294f5bfc079e4948eea9385ffcce4315f77fa3212488869bd4d60e0c" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/route-attestation.edn" :sha256 "331c2802bbb19f8a33177f5d8cc30f6f82f602d016a0e17ff0a49b2c9dec11d3" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/002-selection.edn" :sha256 "ea021f12b2a8835e107805285bcde3394a887d9c9db6466b0fb7bfefcd59f37a" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/006-adjudication.edn" :sha256 "87c49e95ec0cc31059c8353833ae9dd124253e8edc3be5b61ea95a79a84c079e" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/morning-brief-item.edn" :sha256 "43cbb4978457a0ec16f86b547482ecfbaa0d9632b7cdf9589e1e92ce34a8f350" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/b-update.edn" :sha256 "15921381e1b12379bd30aa1961f2fdfab59c006eee9290182f5500142fc46194" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/007-closed.edn" :sha256 "e5c545ba48faaa11bffe46e9832859a87bcca95aca4cb98207be6a8c09767c37" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-7f89646a.edn" :sha256 "d782b3830a040dca8cfe080440869ab4a08ede22fbfd9650eb488d5f7449cc26" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn" :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-7f89646a-repair-finding.edn" :sha256 "758eaeb64b3c0f0ef3733a1725c9a8a54f8a23701698a26373fc44b29279e264" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/driver-error-report-flight-74325007.edn" :sha256 "255280c785c17ac2e7efcd861e31b4bdc6d77cb758b32844e09308d7faaba89f" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-e70b4baf.edn" :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-ffcd772b.edn" :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn" :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-278b6988-repair-finding.edn" :sha256 "1a9b134ab9ab0dc2b1e385872015069c15cacbf8e5f2c52908363bde92571ed8" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn" :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/flight-ada87008.edn" :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn" :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}
   {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/repair-occ-036463620c0c032c9e46aa44b6a6d6b35ed3e0ceb27dc58030f07d8fc2e6c747.edn" :sha256 "95bbb9c5476aedaadc50125068fff5f0a7e773c29a7a7494d84be58a57d893ad" :why "No :measured-a, :rates-sha or :step: predates measured-A persistence and conditioning enactments. A refusal kind may occur, but no [:decision :kind]."}])

(defn cleanup [root]
  (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f)))

(def produced
  (delay
    (let [root (io/file (w/tmp-dir "measured-labels-")) statuses (atom [])
          measured wm/measured-a-version sourced rates/sourced-rates]
      (try
        (binding [fixture/*dir* root]
          (#'fixture/fill! 5)
          (with-redefs [wm/measured-a-version
                        (fn [problems labels]
                          (with-redefs [rates/sourced-rates
                                        (fn [& args]
                                          (let [r (apply sourced args)]
                                            (swap! statuses conj (:status r)) r))]
                            (measured problems labels)))]
            {:decision (:decision (#'fixture/decision {:observation-labels-path (#'fixture/path)}))
             :sourced-statuses @statuses}))
        (finally (cleanup root))))))

(defn with-tick [f]
  (let [root (w/tmp-dir "measured-tick-")]
    (try
      (let [saved (#'runner/persist-run-record!
                   {:run-record-dir root} "offline-wire" "2026-09-26T00:00:00Z"
                   {:outcome :offline-no-selection
                    :checkpoints {:selection {:judgment {:controller-decision (:decision @produced)}}}})
            file (:run-record saved) record (edn/read-string (slurp file))]
        (f root file record))
      (finally (cleanup root)))))

(defn local-values [target qualified]
  (into {} (for [[[t token] value] qualified :when (= t target)] [token value])))

(defn inputs [record]
  (let [chosen (get-in record [:decision :chosen]) t (:target chosen)
        local (local-values t (get-in record [:decision :measured-a :rates]))
        checked (set (keys local))]
    {:run-record record :target t :flight-id "offline-measured" :click-id "offline-wire"
     :policy-key [t (:precedence chosen) {}] :precedence (:precedence chosen) :enactments []
     :observation {:status :observed :o checked :checked checked
                   :channel (zipmap checked (repeat :C3))}}))

(defn tick-observe [field mutation]
  (with-tick
    (fn [_ file record]
      (let [ma (get-in record [:decision :measured-a]) in (inputs record)
            t (:target in) checked (get-in in [:observation :checked])
            qualified [t (first (sort-by str checked))]
            changed (case mutation
                      :none record
                      :missing (update record :decision dissoc :measured-a)
                      :absent (case field
                                :measurement (assoc-in record [:decision :measured-a :measurement qualified] :absent)
                                :rates (assoc-in record [:decision :measured-a :rates] {:absent :not-carried})
                                (assoc-in record [:decision :measured-a] {:status :absent :reason :not-carried}))
                      :different
                      (if (= field :measurement)
                        (update-in record [:decision :measured-a :measurement]
                                   #(update-vals % (fn [_] {:false-neg {:numerator 1 :denominator 5}
                                                           :false-pos {:numerator 1 :denominator 5}})))
                        (update-in record [:decision :measured-a :rates]
                                   #(update-vals % (fn [_] {:false-neg 1/3 :false-pos 1/4})))))
            _ (spit file (pr-str changed))
            ;; This port reads the persisted tick, as run!'s fetcher does.
            fetch-run-record (fn [_] (edn/read-string (slurp file)))
            localized (atom []) real-local @#'flight/target-local
            step (with-redefs-fn {#'flight/target-local
                                  (fn [target m]
                                    (let [v (real-local target m)] (swap! localized conj v) v))}
                   #(flight/conditioning-step (assoc in :run-record (fetch-run-record "offline-wire"))))
            writer (case field
                     :rates (local-values t (:rates ma))
                     :measurement (local-values t (:measurement ma))
                     (assoc (select-keys ma [:rates-sha :classes]) :rates (local-values t (:rates ma))))
            reader (case field
                     :rates (get-in step [:measured-a :rates])
                     ;; The real consumer's target-local result, not a copy
                     ;; read from the carrier. The step retains no measurement.
                     :measurement (second @localized)
                     (:measured-a step))]
        {:writer writer :reader reader :step step :token (second qualified)
         :rates (local-values t (:rates ma)) :measurement (local-values t (:measurement ma))
         :carrier file}))))

(defn step-observe [mutation]
  (with-tick
    (fn [root file record]
      (let [in (inputs record) t (:target in) checked (get-in in [:observation :checked])
            chosen (get-in record [:decision :chosen])
            f (flight/start {:target t :chosen-because {:kind :requested}}
                            {:kind :operator-declared :wants (vec checked) :declared-by "wire-test"}
                            {:id "offline-measured"})
            observed (atom 0)
            enact (fr/enact-fn {:dispatch-step! (fn [_] {:commit "offline-fixture" :produced (first checked)
                                                        :check {:class :C3}})
                                :check-fn (constantly {:observed true})})
            result (flight/run! f {:sources-fn (constantly {}) :max-clicks 1
                                   :click-fn (fn [_] {:click-id "offline-wire" :chosen chosen})
                                   :enact-fn enact
                                   :wc-fn (fn [& _] {:increment {:policy-key (:policy-key in)}})
                                   :observe-fn (fn [& _] (zipmap checked (repeat (> (swap! observed inc) 1))))
                                   :fetch-run-record (fn [_] (edn/read-string (slurp file)))})
            written (get-in result [:enactments 0 :step])
            flights (io/file root "flights") _ (.mkdirs flights)
            carrier (io/file flights "offline-measured.edn")
            changed (case mutation :none written :absent {:status :absent :reason :not-carried}
                          :different (update written :f inc))]
        (spit carrier (pr-str {:flight result}))
        (let [disk (edn/read-string (slurp carrier))]
          (spit carrier (pr-str (assoc-in disk [:flight :enactments 0 :step] changed))))
        {:writer written :reader (get-in (folds/conditioning-steps flights) [:steps 0 :step])
         :carrier (str carrier)}))))

(defn status-observe []
  (let [click ((fr/http-click-fn {:post! (fn [_] {:status 409 :body {:error "fixture"}})
                                 :today (constantly "offline")})
               {:flight {:target "M-wire" :flight/id "offline" :click 1}})]
    {:writer (get-in click [:abstention :status])
     :reader (first (:sourced-statuses @produced))}))

(defn kind-observe [mutation]
  (let [result (r9/run-tick (ex-info "cascade decision refused" {:kind :live-c-stale :target "M-wire"}))
        record (:record result)
        writer (get-in record [:decision :abstention :targets 0 :kind])
        click (fr/record-summary "M-wire" "offline-kind" record)
        carrier (update-in click [:abstention :kind]
                           (fn [v] (case mutation :none v :absent {:absent :not-carried} :different :other-refusal)))
        f (flight/start {:target "M-wire" :chosen-because {:kind :requested}}
                        {:kind :operator-declared :wants [:done] :declared-by "wire-test"} {:id "offline-kind"})
        read (flight/record-click f (merge carrier {:wants [:done] :before {:done false} :after {:done false}}))]
    {:writer writer :reader (get-in read [:clicks 0 :abstention :kind])}))

(def two-record-live-pair
  {:kind :two-record-live-pair
   :flight {:path "holes/labs/M-wm-wiring/spike/flight-ffcd772b.edn"
            :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575"
            :reader-path [:flight :clicks 0 :abstention :kind]}
   :tick {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn"
          :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
          :writer-path [:decision :abstention :targets] :target "M-autoclock-in"}
   :why "Same click, :universe-not-admitted at both ends; neither has [:decision :kind]. Two records do not meet the one-record VERIFIED definition."})

(defn live-kind-pair []
  (let [read-pin (fn [{:keys [path sha256]}]
                   (when-not (= sha256 (w/sha256-file path)) (throw (ex-info "Moved pin" {:path path})))
                   (w/read-record path))
        f (read-pin (:flight two-record-live-pair)) r (read-pin (:tick two-record-live-pair))
        target (get-in f [:flight :target])]
    (when-not (= (get-in f [:flight :clicks 0 :click-id]) (:run/id r))
      (throw (ex-info "Different clicks" {})))
    {:writer (:kind (first (filter #(= target (:target %)) (get-in r [:decision :abstention :targets]))))
     :reader (get-in f [:flight :clicks 0 :abstention :kind])}))
