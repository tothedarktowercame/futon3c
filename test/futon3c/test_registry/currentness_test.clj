(ns futon3c.test-registry.currentness-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.evidence.backend :as backend]
            [futon3c.test-registry :as registry]
            [futon3c.test-registry.currentness :as currentness]
            [futon3c.test-registry.sqlite-backend :as sqlite])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "warrant-currentness-"
                                     (make-array FileAttribute 0))))

(defn- cleanup! [dir]
  (doseq [file (reverse (file-seq dir))]
    (io/delete-file file true)))

(defn- append! [store payload]
  (let [row (registry/append-record! store
                                     (merge {:author "currentness-test"
                                             :run/id (str (random-uuid))}
                                            payload)
                                     nil)]
    (backend/-get store (:evidence/id row))))

(deftest classifies-current-stale-not-passing-and-unverifiable
  (let [dir (temp-dir) file (io/file dir "fixture.clj")
        store (sqlite/sqlite-backend (io/file dir "registry.sqlite"))]
    (try
      (spit file "(ns fixture)\n")
      (let [files {"fixture.clj" (registry/file-sha file)}
            current (append! store {:kind :run :warrant? true
                                    :load-closure [] :test-files files})
            not-passing (append! store {:kind :run :warrant? false
                                        :load-closure [] :test-files files})
            unverifiable (append! store {:kind :run :warrant? true
                                         :test-files files})]
        (is (= {:class :current :basis :files}
               (currentness/classify store current (.getPath dir))))
        (spit file "(ns changed)\n")
        (let [stale (currentness/classify store current (.getPath dir))]
          (is (= :stale (:class stale)))
          (is (= :hash-mismatch (get-in stale [:changed :reason])))
          (is (.endsWith (get-in stale [:changed :path]) "fixture.clj")))
        (is (= {:class :not-passing}
               (currentness/classify store not-passing (.getPath dir))))
        (is (= {:class :unverifiable :reason :missing-load-closure}
               (currentness/classify store unverifiable (.getPath dir)))))
      (finally (cleanup! dir)))))

(deftest distinguishes-green-registration-refusal-from-test-failure
  (let [dir (temp-dir) file (io/file dir "fixture.clj")
        store (sqlite/sqlite-backend (io/file dir "registry.sqlite"))
        files {"fixture.clj" nil}
        refusal {:record/type :test-registry/refusal
                 :reason :scope-not-committed}]
    (try
      (spit file "(ns fixture)\n")
      (let [files (assoc files "fixture.clj" (registry/file-sha file))
            entry (fn [payload]
                    (append! store (merge {:kind :run :load-closure []
                                           :test-files files}
                                          payload)))
            green-refusal
            (entry {:warrant? false
                    :results {:exit 0 :failures 0 :errors 0}
                    :postcheck refusal})
            failed-refusal
            (entry {:warrant? false
                    :results {:exit 1 :failures 1 :errors 0}
                    :postcheck refusal})
            failed (entry {:warrant? false
                           :results {:exit 1 :failures 1 :errors 0}})
            warrant (entry {:warrant? true
                            :results {:exit 0 :failures 0 :errors 0}})]
        (is (= {:class :registration-refused :reason :scope-not-committed}
               (currentness/classify store green-refusal (.getPath dir))))
        (is (= {:class :not-passing}
               (currentness/classify store failed-refusal (.getPath dir))))
        (is (= {:class :not-passing}
               (currentness/classify store failed (.getPath dir))))
        (is (= {:class :current :basis :files}
               (currentness/classify store warrant (.getPath dir)))))
      (finally (cleanup! dir)))))

(defn- reach-fixture [dir]
  (let [source (io/file dir "src/sample/core.clj")
        test-file (io/file dir "test/sample/core_test.clj")
        closure (io/file dir "closure.edn")
        reach-dir (io/file dir "reach")
        store (sqlite/sqlite-backend (io/file dir "registry.sqlite"))]
    (.mkdirs (.getParentFile source))
    (.mkdirs (.getParentFile test-file))
    (.mkdirs reach-dir)
    (spit source "(ns sample.core)\n(defn reached [] 1)\n(defn spare [] 2)\n")
    (spit test-file (str "(ns sample.core-test\n"
                         "  (:require [sample.core :as core]))\n"
                         "(defn exercise [] (core/reached))\n"))
    (spit closure
          (pr-str [{:ns "sample.core" :url (str (.toURI source)) :status :loaded-only}
                   {:ns "sample.core-test" :url (str (.toURI test-file))
                    :status :loaded-only}]))
    (let [entry (append! store {:kind :run :namespace "sample.core-test"
                                :warrant? true
                                :load-closure [{:path "src/sample/core.clj"
                                                :sha256 (registry/file-sha source)}]
                                :test-files {"test/sample/core_test.clj"
                                             (registry/file-sha test-file)}})]
      {:source source :test-file test-file :closure closure :reach-dir reach-dir
       :store store :entry entry})))

(defn- write-reach-record! [{:keys [closure reach-dir entry]}]
  (let [path (io/file reach-dir (str (:evidence/id entry) ".json"))
        {:keys [exit err]}
        (shell/sh "python3" "scripts/warrant_reach.py" "record"
                  "--namespace" "sample.core-test"
                  "--closure" (.getPath closure)
                  "--output" (.getPath path)
                  "--cache" (str (io/file reach-dir ".cache")))]
    (is (zero? exit) err)
    (let [record (json/parse-string (slurp path) true)]
      (spit path (json/generate-string
                  (assoc record :entry-id (:evidence/id entry)))))
    path))

(deftest dependency-record-distinguishes-reached-and-unreached-edits
  (let [dir (temp-dir)]
    (try
      (let [{:keys [source reach-dir store entry] :as fixture} (reach-fixture dir)]
        (write-reach-record! fixture)
        (spit source (str/replace (slurp source) "spare [] 2" "spare [] 9"))
        (binding [currentness/*reach-dir* (.getPath reach-dir)]
          (is (= {:class :current :basis :definitions
                  :files-changed-unreached [(.getCanonicalPath source)]}
                 (currentness/classify store entry (.getPath dir)))))
        (spit source (str/replace (slurp source) "reached [] 1" "reached [] 9"))
        (binding [currentness/*reach-dir* (.getPath reach-dir)]
          (let [answer (currentness/classify store entry (.getPath dir))]
            (is (= :stale (:class answer)))
            (is (= :definitions (:basis answer)))
            (is (= :definition-changed (get-in answer [:changed :kind])))
            (is (= (:changed answer) (first (:differences answer)))))))
      (finally (cleanup! dir)))))

(deftest dependency-record-failures-fall-back-to-file-rule
  (doseq [scenario [:absent :wrong-entry :invalid-json :missing-script]]
    (let [dir (temp-dir)]
      (try
        (let [{:keys [source reach-dir store entry] :as fixture} (reach-fixture dir)
              record-file (when-not (= :absent scenario) (write-reach-record! fixture))]
          (case scenario
            :wrong-entry
            (let [record (json/parse-string (slurp record-file) true)]
              (spit record-file (json/generate-string (assoc record :entry-id "wrong"))))
            :invalid-json (spit record-file "{not json")
            nil)
          (spit source (str/replace (slurp source) "spare [] 2" "spare [] 9"))
          (binding [currentness/*reach-dir* (.getPath reach-dir)
                    currentness/*reach-script* (if (= :missing-script scenario)
                                                 (str (io/file dir "absent.py"))
                                                 currentness/*reach-script*)]
            (let [answer (currentness/classify store entry (.getPath dir))]
              (is (= :stale (:class answer)) (name scenario))
              (is (= :files (:basis answer)) (name scenario))
              (is (= :hash-mismatch (get-in answer [:changed :reason])) (name scenario))
              (when-not (= :absent scenario)
                (is (contains? (:reach-record answer) :ignored) (name scenario))))))
        (finally (cleanup! dir))))))

(deftest agrees-with-python-over-live-wire-runs
  (let [source (io/file sqlite/default-path)]
    (if-not (.isFile source)
      (println "CURRENTNESS AGREEMENT SKIPPED: live SQLite store is absent at"
               sqlite/default-path)
      (let [dir (temp-dir) copy (io/file dir "registry-copy.sqlite")]
        (try
          (let [{backup-exit :exit backup-err :err}
                (shell/sh "python3" "-c"
                          (str "import sqlite3,sys; a=sqlite3.connect(sys.argv[1]); "
                               "b=sqlite3.connect(sys.argv[2]); a.backup(b); b.close(); a.close()")
                          (.getPath source) (.getPath copy))]
            (is (zero? backup-exit) backup-err))
          (let [{:keys [exit out err]}
                (shell/sh "python3" "scripts/warrant_index.py"
                          "--db" (.getPath copy) "check" "--wire" "--json")]
            ;; Exit 1 means at least one namespace is not current, which is the
            ;; expected data result rather than a CLI execution failure.
            (is (#{0 1} exit) err)
            (let [rows (:namespaces (json/parse-string out true))
                  store (sqlite/sqlite-backend copy)
                  compared
                  (mapv
                   (fn [{:keys [namespace class entry-id]}]
                     (let [entry (sqlite/latest-run-for-namespace store namespace)
                           payload (some-> entry :evidence/body :payload-edn edn/read-string)
                           actual (:class (currentness/classify
                                           store entry (:repo/root payload)))]
                       {:namespace namespace :python (keyword class) :clojure actual
                        :entry-id entry-id}))
                   (filter :entry-id rows))
                  disagreements (filterv #(not= (:python %) (:clojure %)) compared)]
              (println "CURRENTNESS AGREEMENT"
                       {:compared (count compared) :disagreements disagreements})
              (is (pos? (count compared)))
              (is (empty? disagreements) (pr-str disagreements))))
          (finally (cleanup! dir)))))))
