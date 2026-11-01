(ns futon3c.giraffe.stale-markers-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.giraffe.excursions :as excursions]
            [futon3c.giraffe.run :as run]
            [futon3c.giraffe.stale-markers :as stale]))

(def live-pass
  "/home/joe/code/storage/giraffe/20261101T195517.913Z-c38bc009/input.edn")

(defn- marked-pass []
  (let [input (stale/annotate-input (edn/read-string (slurp live-pass)))]
    {:input input :pass (excursions/combined-pass input)}))

(deftest pinned-files-carry-advisory-markers
  (let [{:keys [input pass]} (marked-pass)
        by-id (into {} (map (juxt :id identity)) (:items input))
        markers (get-in by-id [:M-interim-director :stale-markers])]
    (testing "the live close-out and expired timebox are both visible"
      (is (some #(and (= :body-closeout (:kind %))
                      (str/includes? (:quote %) "READY-TO-CLOSE"))
                markers))
      (is (some #(= {:kind :timebox-expired
                     :date "2026-05-14"
                     :weeks 2
                     :expired-on "2026-05-28"}
                   %)
                markers)))
    (testing "an ordinary pinned file is explicitly unmarked"
      (is (= [] (get-in by-id [:M-aif4iad :stale-markers]))))
    (testing "markers change no disposition or rank"
      ;; Compare against the same pass code on the unannotated input, not the
      ;; retained pass.edn: that file predates GR-1 (met prerequisites), so its
      ;; ranks and statuses differ for reasons that have nothing to do with markers.
      (is (= (mapv #(select-keys % [:rank :id :status])
                   (:order (excursions/combined-pass (edn/read-string (slurp live-pass)))))
             (mapv #(select-keys % [:rank :id :status]) (:order pass)))))))

(deftest report-lists-exactly-the-marked-order-rows-in-rank-order
  (let [{:keys [input pass]} (marked-pass)
        marked (filterv (comp seq :stale-markers) (:order pass))
        root (.toFile (java.nio.file.Files/createTempDirectory
                       "giraffe-stale-markers-"
                       (make-array java.nio.file.attribute.FileAttribute 0)))
        written (run/write-run! {:root (str root)
                                 :adapter-result {:input input :rejected []}
                                 :pass-output pass})
        report (slurp (io/file (:dir written) "report.md"))
        section (-> (second (str/split report #"## Needs operator disposition\n" 2))
                    (str/split #"\n## " 2)
                    first)
        reported (mapv keyword (map second
                                    (re-seq #"(?m)^### ([^ ]+) \(rank " section)))]
    (is (= (mapv :id marked) reported))
    (is (= (sort (map :rank marked)) (map :rank marked)))
    (is (every? #(str/includes? section (str "status " (name (:status %)))) marked))))

(deftest a-phase-closeout-heading-is-not-a-mission-closeout
  ;; Bad case: mission templates carry "## … close-out" sections for a phase
  ;; (M-giraffe line 171, "## MAP close-out (claude-11, 2026-10-26)"); the
  ;; mission is open work. Only READY-TO-CLOSE or a body Status CLOSED/DONE marks.
  (let [f (java.io.File/createTempFile "phase-closeout" ".md")]
    (spit f "# Mission: M-x\n\n**Status:** OPEN\n\n## MAP close-out (claude-11, 2026-10-26)\n\nphase notes\n")
    (let [bytes (java.nio.file.Files/readAllBytes (.toPath f))
          sha (let [md (java.security.MessageDigest/getInstance "SHA-256")]
                (apply str (map #(format "%02x" %) (.digest md bytes))))]
      (is (= [] (stale/markers "2026-11-01" {:source-pin {:path (.getPath f) :sha256 sha}}))))))
