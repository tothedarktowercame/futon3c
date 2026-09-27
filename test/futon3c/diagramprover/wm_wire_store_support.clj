(ns futon3c.diagramprover.wm-wire-store-support
  "Real mission-reading publications and read-back, with mutations of the
  actual <temporary-store>/M-store-wire.edn carrier between the two calls."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.mission-reading :as reading]
            [futon2.aif.want-interpretation :as wi]
            [futon3c.diagramprover.wm-wire :as w]))

(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/flight-6cda5ee8.edn", :sha256 "b26d4c3cc98009b1f7a828cd2355f6b01bb03feaeda8f03bf43a8a94f0292e34" :why "Reading entries record request outcomes, not published payloads. Criteria in click want-source lack a matching publication; asks contain no store read-back value. No constraints-read, locator-declines or locator-questions pair."}
   {:path "holes/labs/M-wm-wiring/spike/flight-d00574c8.edn", :sha256 "68e531b462d535983ea5114a8da370daec943a3b4283b6b87831e63ecf9eadd8" :why "Reading entries record request outcomes, not published payloads. Criteria in click want-source lack a matching publication; asks contain no store read-back value. No constraints-read, locator-declines or locator-questions pair."}
   {:path "holes/labs/M-wm-wiring/spike/flight-7f89646a.edn", :sha256 "d782b3830a040dca8cfe080440869ab4a08ede22fbfd9650eb488d5f7449cc26" :why "Reading entries record request outcomes, not published payloads. Criteria in click want-source lack a matching publication; asks contain no store read-back value. No constraints-read, locator-declines or locator-questions pair."}
   {:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn", :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212" :why "Reading entries record request outcomes, not published payloads. Criteria in click want-source lack a matching publication; asks contain no store read-back value. No constraints-read, locator-declines or locator-questions pair."}
   {:path "holes/labs/M-wm-wiring/spike/flight-e70b4baf.edn", :sha256 "10ac7cd76b2b18b40c4044bb55f0e54bc210cd908d9c4deef74875fc9d2bc88c" :why "Reading entries record request outcomes, not published payloads. Criteria in click want-source lack a matching publication; asks contain no store read-back value. No constraints-read, locator-declines or locator-questions pair."}
   {:path "holes/labs/M-wm-wiring/spike/flight-ffcd772b.edn", :sha256 "998565fb0a341077ae5b9341d977a9c5bf6db6ea0cf33fead1e00c630f4e2575" :why "Reading entries record request outcomes, not published payloads. Criteria in click want-source lack a matching publication; asks contain no store read-back value. No constraints-read, locator-declines or locator-questions pair."}
   {:path "holes/labs/M-wm-wiring/spike/flight-ada87008/flight-ada87008.edn", :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de" :why "Reading entries record request outcomes, not published payloads. Criteria in click want-source lack a matching publication; asks contain no store read-back value. No constraints-read, locator-declines or locator-questions pair."}])

(def target "M-store-wire")
(def text "Build the artifact.\nPublish after building.\n")
(def mission-sha (reading/text-sha text))
(def cue {:lines [1 1] :quote "Build the artifact."})

(defn publish [root k variant]
  (let [issued (wi/issue! root {:target target :want {:token :done :mission-sha mission-sha}
                                :criterion {:stated (:quote cue)}
                                :known [{:token :done} {:token :built}]})
        question {:question (str "Which artifact " variant "?") :span cue
                  :alternatives ["First artifact" "Second artifact"]}
        response (case k
                   :criteria {:criteria [{:statement (str "Artifact " variant) :cue cue}]}
                   :coverage {:scope-outs [{:statement (str "Excluded " variant) :cue cue}]}
                   :locators {:locator {:class :C4 :repo "fixture" :sha "fixture"
                                        :path (str variant ".clj") :decl "(def artifact"}
                              :cue cue :reading "The declaration names the artifact."}
                   :locator-questions {:questions [question]}
                   :locator-declines {:reason (keyword variant)}
                   :constraints-read {:constraints [{:want (if (= variant "first") :done :built)
                                                     :requires (if (= variant "first") :built :done)
                                                     :cue cue}]})
        validated (case k
                    :criteria (reading/validate-criteria issued response text)
                    :coverage (reading/validate-coverage issued response text)
                    :constraints-read (reading/validate-constraints issued response text)
                    :locators (reading/validate-locator issued response
                                {:observe (fn [_] {:observed #{} :refused {}}) :text text})
                    :locator-questions (reading/validate-locator issued response {:text text})
                    :locator-declines nil)
        who {:seat "fixture" :job-id variant}]
    (case k
      :criteria (reading/publish-criteria! root issued response validated who mission-sha)
      :coverage (reading/publish-coverage! root issued response validated who)
      :locators (reading/publish-locator! root issued response validated who)
      :locator-questions (reading/publish-locator-questions! root issued response validated who)
      :locator-declines (reading/record-locator-decline! root issued response who mission-sha)
      :constraints-read (reading/publish-constraints! root issued response validated who))))

(defn payload
  "The published payload carried to the reader; receipts remain in storage.
  Criteria are filtered against text; all fixture cues resolve."
  [k record]
  (case k
    :criteria (get-in record [:criteria :criteria])
    :locators (into {} (map (fn [[t r]] [t (:locator r)]) (:locators record)))
    :locator-questions (into {} (map (fn [[t r]] [t (:questions r)]) (:locator-questions record)))
    (get record k)))

(defn read-back [root k]
  (case k
    :criteria (reading/published-criteria root target text)
    :coverage (reading/published-coverage root target mission-sha)
    :locators (reading/published-locators root target)
    :locator-questions (reading/published-locator-questions root target)
    :locator-declines (reading/published-locator-declines root target mission-sha)
    :constraints-read (reading/published-constraints root target mission-sha)))

(defn observe [k mutation]
  (let [root (w/tmp-dir "store-wire-") other (w/tmp-dir "store-other-")]
    (try
      (let [written (publish root k "first") alternative (publish other k "second")
            file (io/file root (str target ".edn"))
            disk (edn/read-string (slurp file))]
        (when-not (= written disk) (throw (ex-info "Writer did not persist its return" {:file (str file)})))
        (case mutation
          :none nil
          :remove (io/delete-file file)
          :different (spit file (pr-str (assoc disk k (get alternative k)))))
        {:writer (payload k written) :reader (read-back root k)
         :alternative (payload k alternative) :carrier (str file)})
      (finally
        (doseq [dir [root other] f (reverse (file-seq (io/file dir)))]
          (io/delete-file f))))))
