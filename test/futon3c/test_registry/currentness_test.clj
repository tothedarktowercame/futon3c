(ns futon3c.test-registry.currentness-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
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
        (is (= {:class :current}
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
