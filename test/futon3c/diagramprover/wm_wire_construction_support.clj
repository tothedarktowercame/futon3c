(ns futon3c.diagramprover.wm-wire-construction-support
  "Construction through actual assembly and decision readers. No source-attribution
  port is stubbed: interception observes real calls and preserves their results."
  (:require [futon2.aif.cascade-problems :as cp]
            [futon2.aif.interpretation-construction :as construction]
            [futon2.aif.locator-fixtures :as loc]
            [futon2.aif.cascade-policy :as policy]
            [futon2.aif.cascade-model-manifest :as manifest]
            [futon2.aif.candidate-derivations :as derivations]
            [futon2.aif.cascade-equivalence :as equivalence]
            [futon2.aif.efe :as efe]
            [futon2.report.cascade-decision-test :as fixture]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire :as w]))

(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "Selected action retains the whole receipt, not a separate receipt-derived decision product. Derivation payload digest commits the nested machine receipt (VERIFIED); historical construction metadata predates the nested-receipt fix. Domain declaration records want but no beta or horizon; no assembled cascade-spec retained beside kernel input."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn" :sha256 "a8e04fb97e58808e8fabdb4ab771f3c414b4181ef82dac336729dd472a18d816"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-6cda5ee8.edn" :sha256 "b26d4c3cc98009b1f7a828cd2355f6b01bb03feaeda8f03bf43a8a94f0292e34"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-d00574c8.edn" :sha256 "68e531b462d535983ea5114a8da370daec943a3b4283b6b87831e63ecf9eadd8"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/001-time-step.edn" :sha256 "17fd3dde3352f281b21e214c8b5f468e29001686a23da9d3b3c3aa5f681cbefc"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/003-construction.edn" :sha256 "cb60510eff60e2441250fdf6b3b54aedf9fec2e7b5702596c8fc20f4ab0cdeea"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/004-dispatch.edn" :sha256 "11fbf36986a6fecc436d127600f475718704707771cacc5362518f55c4261c8e"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/005-build.edn" :sha256 "2d405ac2294f5bfc079e4948eea9385ffcce4315f77fa3212488869bd4d60e0c"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/route-attestation.edn" :sha256 "331c2802bbb19f8a33177f5d8cc30f6f82f602d016a0e17ff0a49b2c9dec11d3"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/002-selection.edn" :sha256 "ea021f12b2a8835e107805285bcde3394a887d9c9db6466b0fb7bfefcd59f37a"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/006-adjudication.edn" :sha256 "87c49e95ec0cc31059c8353833ae9dd124253e8edc3be5b61ea95a79a84c079e"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/morning-brief-item.edn" :sha256 "43cbb4978457a0ec16f86b547482ecfbaa0d9632b7cdf9589e1e92ce34a8f350"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/b-update.edn" :sha256 "15921381e1b12379bd30aa1961f2fdfab59c006eee9290182f5500142fc46194"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/attempt-001-flight-e70b4baf/007-closed.edn" :sha256 "e5c545ba48faaa11bffe46e9832859a87bcca95aca4cb98207be6a8c09767c37"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-7f89646a.edn" :sha256 "d782b3830a040dca8cfe080440869ab4a08ede22fbfd9650eb488d5f7449cc26"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn" :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-7f89646a-repair-finding.edn" :sha256 "758eaeb64b3c0f0ef3733a1725c9a8a54f8a23701698a26373fc44b29279e264"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/driver-error-report-flight-74325007.edn" :sha256 "255280c785c17ac2e7efcd861e31b4bdc6d77cb758b32844e09308d7faaba89f"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-e70b4baf.edn" :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-ffcd772b.edn" :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-25-flight-ffcd772b-click-1.edn" :sha256 "8ab0db5d770085f17bb341293a92424149bf2388ae7a6dd3e724887c9a44eba2"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-278b6988-repair-finding.edn" :sha256 "1a9b134ab9ab0dc2b1e385872015069c15cacbf8e5f2c52908363bde92571ed8"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-e70b4baf-click-1.edn" :sha256 "241b10a024020344eba5d444c12fb33ad6afe107724dec95c81229b422bc5feb"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/flight-ada87008.edn" :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/tick-run-record-2026-09-26-flight-ada87008-click-1.edn" :sha256 "df01831c24a7042d66b6ef2c38d82cdfbd0994a03b5539f3112db7dc41894970"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}
   {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/repair-occ-036463620c0c032c9e46aa44b6a6d6b35ed3e0ceb27dc58030f07d8fc2e6c747.edn" :sha256 "95bbb9c5476aedaadc50125068fff5f0a7e773c29a7a7494d84be58a57d893ad"
    :why "No candidate-derivations or token-belief domain declaration at the decision certificate; no family beta/horizon or assembled cascade-spec beside a reader-produced value."}])

(defn live-record []
  (let [{:keys [path sha256]} (first live-records-read)]
    (assert (= sha256 (w/sha256-file path)) "live pin changed")
    (w/read-record path)))

(def assembled
  (delay
    (cp/assemble
      {:targets [fixture/tick-1-target]
       :sources (-> fixture/tick-1-sources
                    (assoc :candidates {})
                    (assoc :construction {:construct construction/construct
                                          :budget {:max-moves 20 :max-expansions 1000}
                                          :move-cost 0 :evaluate-g wm/constructed-candidate-g})
                    loc/locate-all)})))

(defn change [field value mutation]
  (case mutation
    :none value
    :absent {:absent :not-carried}
    :different (case field
                 :construction-receipt (assoc value :unreached-wants [{:token :different-want :reason :no-producer}])
                 :want [:test-covers-missing-total-repos]
                 :beta (+ 2 value)
                 :horizon-steps (inc value))))

(defn carrier [field mutation]
  (if (= field :construction-receipt)
    (update-in @assembled [:problems 0 :constructed-candidates]
               #(mapv (fn [c] (update c field (fn [v] (change field v mutation)))) %))
    (update-in @assembled [:problems 0 :cascade-problem field]
               #(change field % mutation))))

(defn family [field mutation]
  (let [a (carrier field mutation)
        r (#'wm/cascade-family-parameters (:problems a))]
    {:writer (get-in @assembled [:problems 0 :cascade-problem field])
     :reader (get r field) :family r}))

(defn decision [field mutation]
  (let [a (carrier field mutation)
        observed (atom nil) rank efe/rank-actions
        result (try
                 (with-redefs [efe/rank-actions
                               (fn [state candidates opts]
                                 (reset! observed {:want (get-in opts [:cascade-spec :want])
                                                   :receipts (mapv :construction-receipt candidates)})
                                 (rank state candidates opts))]
                   (wm/cascade-decision a fixture/live-c-opts))
                 (catch clojure.lang.ExceptionInfo e {:refusal (ex-data e)}))
        receipt? (= field :construction-receipt)
        writer (if receipt?
                 (get-in @assembled [:problems 0 :constructed-candidates 0 field])
                 (set (get-in @assembled [:problems 0 :cascade-problem field])))
        reader (if receipt?
                 (first (:receipts @observed))
                 (when (:want @observed)
                   (set (map second (:want @observed)))))]
    {:writer writer :reader reader :result result}))

(def receipt-path [:decision :selection-certificate :candidates 0 :id :construction-receipt])
(def digest-path [:decision :selection-certificate :candidate-derivations :C1 :candidate-payload-sha256])

(defn derivation [mutation]
  (let [r (live-record)
        candidate (get-in r [:decision :selection-certificate :candidates 0])
        changed (update-in candidate [:id :construction-receipt]
                           #(change :construction-receipt % mutation))
        entry (#'derivations/entry changed nil {})]
    {:writer (equivalence/canonical-sha256 (:id candidate))
     :reader (:candidate-payload-sha256 entry)
     :recorded (get-in r digest-path)
     :receipt (get-in r receipt-path)
     :entry entry :nested-receipt (get-in changed [:id :construction-receipt])
     :outer-receipt (:construction-receipt changed)}))

(defn live-digest []
  (let [o (derivation :none)]
    (assoc o :reader (:recorded o) :recomputed (:reader o))))

(defn kernel [mutation]
  (let [p (get-in @assembled [:problems 0 :cascade-problem])
        pair (get-in @assembled [:problems 0 :constructed-candidates 0])
        candidate {:kind :cascade-candidate :id (:candidate-id pair)
                   :precedence (mapv #(policy/token-interpretation % (get-in p [:interpretations %]))
                                     (:precedence pair))
                   :construction-receipt (:construction-receipt pair)}
        spec (update (:cascade-spec p) :want #(change :want % mutation))
        cert manifest/horizon-g-sparse-cert read-want (atom nil)
        r (with-redefs [manifest/horizon-g-sparse-cert
                        (fn [in]
                          (reset! read-want (get-in in [:spec :want]))
                          (cert in))]
            (efe/rank-cascade-actions
              {:cascade-belief (manifest/observed-belief (set (for [[k v] (:facts p) :when (true? v)] k)))}
              [candidate] {:cascade-spec spec :horizon-steps (:horizon-steps p)}))]
    {:writer (get-in p [:cascade-spec :want]) :reader @read-want :ranked r}))
