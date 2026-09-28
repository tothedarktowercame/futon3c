(ns futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt-test
  (:require [clojure.edn :as edn] [clojure.java.io :as io] [clojure.test :refer [deftest is testing]]
            [futon2.aif.flight :as flight] [futon2.aif.flight-runner :as fr]
            [futon3c.diagramprover.wm-wire :as w])
  (:import [java.security MessageDigest]))
(def seventh {:path (str w/spike-dir "/tick-run-record-2026-09-26-flight-278b6988-click-1.edn") :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"})
(def eighth {:path (str w/spike-dir "/flight-ada87008/flight-ada87008.edn") :sha256 "85f7dcf9cd75f33e5e8ad5834cb7fb18264ab0f31e0d43644870bf541a3ff4de"})
(def old-flight {:path (str w/spike-dir "/flight-278b6988.edn") :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"})
(def seats {:author "claude-6" :reviewer "claude-13"})
(defn observe [tamper]
  (let [rr (w/read-record (:path seventh))
        cf (fr/http-click-fn (merge {:today (constantly "2026-09-26") :post! (constantly {:status 200 :body {:click-id "wm-click-7"}})
                                     :get-status! (constantly {:running? false}) :sleep! (fn [_]) :read-record! (constantly rr)} seats))
        result (cf {:flight {:flight/id "flight-278b6988" :target "M-autoclock-in" :click 1}})
        f (flight/run! (flight/start {:target "M-autoclock-in" :chosen-because {:kind :requested}}
                                     {:kind :a-exits :repo "futon3c" :path "p" :read-text (fn [& _] "")}
                                     {:id "flight-278b6988"})
                       {:click-fn (fn [_] (tamper result)) :observe-fn (fn [_ _] {}) :sources-fn (constantly {}) :max-clicks 1})
        entry (first (:clicks f))]
    {:writer (:cast result) :reader (if (contains? entry :cast) (:cast entry) {:absent :field-not-carried})}))
(defn build-record []
  (let [o (observe identity) a (observe #(dissoc % :cast))
        d (observe #(assoc % :cast (fr/click-cast {:author "claude-13" :reviewer "claude-6"})))
        eighth-record (w/read-record (:path eighth)) old (w/read-record (:path old-flight))]
    {:producer 'futon3c.diagramprover.wm-wire-producer-wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt-test
     :operation ['futon2.aif.flight-runner/http-click-fn 'futon2.aif.flight/run!]
     :inputs {:seats seats :flight-id "flight-278b6988" :target "M-autoclock-in" :max-clicks 1}
     :fields {:writer (:writer o) :reader (:reader o) :writer-present? (some? (:writer o)) :writer-typed-absence? (w/typed-absence? (:writer o))
              :received? (w/received? o) :seventh-pin? (= (:sha256 seventh) (w/sha256-file (:path seventh)))
              :cast-expected? (= {:author "claude-6" :reviewer "claude-13" :repair-reviewer {:absent :no-repair-reviewer-given}} (:reader o))
              :writer-live? (= (get-in eighth-record [:flight :clicks 0 :cast]) (:writer o))
              :absent {:reader (:reader a) :typed? (w/typed-absence? (:reader a)) :received? (w/received? a)
                       :legacy-absent-refused? (and (not (w/received? (assoc o :reader {:absent :no-cast}))) (not (w/received? (assoc o :reader {:status :absent :reason :no-cast}))))}
              :different {:reader (:reader d) :writer-reader-differ? (not= (:writer d) (:reader d)) :received? (w/received? d)}
              :live-records {:eighth-pin? (= (:sha256 eighth) (w/sha256-file (:path eighth))) :old-pin? (= (:sha256 old-flight) (w/sha256-file (:path old-flight)))
                             :old-has-no-cast? (not-any? :cast (:clicks (:flight old)))}}
     :left-out {:full-flight "reader checks cast transfer and pinned live relations only"}}))
(defn- txt [x] (str (pr-str x) "\n"))
(defn- sha [s] (apply str (map #(format "%02x" (bit-and % 255)) (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))))
(defn- write! [x] (let [s (txt x) f (io/file "test/fixtures/wire-producers" (str "wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt@" (subs (sha s) 0 12) ".edn"))] (when (.exists f) (throw (ex-info "exists" {}))) (spit f s) (println f)))
(defn- paths [x] (letfn [(go [p v] (if (map? v) (mapcat (fn [[k x]] (go (conj p k) x)) v) [p]))] (go [] x)))
(deftest flight-cast-producer (let [a (build-record)] (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE")) (write! a) (let [e (edn/read-string (slurp (first (filter #(.startsWith (.getName %) "wm-wire-flight-click-flight-cast-test-cast-test-literal-fixt@") (.listFiles (io/file "test/fixtures/wire-producers"))))))] (doseq [p (paths (:fields e))] (testing (pr-str p) (is (= (get-in e (into [:fields] p)) (get-in a (into [:fields] p))))))))))
