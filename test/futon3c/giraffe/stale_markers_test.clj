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

(def live-pass-output
  "/home/joe/code/storage/giraffe/20261101T195517.913Z-c38bc009/pass.edn")

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
      (is (= (mapv #(select-keys % [:rank :id :status])
                   (:order (edn/read-string (slurp live-pass-output))))
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
