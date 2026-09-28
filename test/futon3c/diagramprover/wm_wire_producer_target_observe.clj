(ns futon3c.diagramprover.wm-wire-producer-target-observe
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-products-16a :as target-products]
            [futon3c.diagramprover.wm-wire-target-readers-11a :as target-readers]
            [futon3c.diagramprover.wm-wire-target-support :as target-support])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-target-observe-test)
(def operation 'futon2.aif.flight/run!)
(def kinds
  [:flight-click
   :flight-judge-opts
   :flight-run
   :r0-enact-step
   :r10-observe-publication
   :r2-flight-read
   :r9-close-cause
   :tick-flight-assembly])

(defn- primary [kind]
  (let [ordinary (target-support/observe kind identity)
        absent (target-support/observe kind (constantly {:absent :target-not-carried}))
        different (target-support/observe kind (constantly "M-other-target"))]
    {:writer (:writer ordinary)
     :reader (:reader ordinary)
     :received? (w/received? ordinary)
     :interventions
     {:absent {:received? (w/received? absent)}
      :different {:reader (:reader different)
                  :received? (w/received? different)}}}))

(defn- click-relations []
  (let [{:keys [flights outputs]} (target-readers/products :click)
        [a b] outputs]
    {:only-target-varies? (= (mapv #(dissoc % :target) flights)
                             (repeat 2 (dissoc (first flights) :target)))
     :targets-carried? (= target-readers/targets (mapv #(get-in % [:sent :target]) [a b]))
     :sent-shape-stable? (= (dissoc (:sent a) :target) (dissoc (:sent b) :target))
     :first-unreached? (= [{:token :done}] (get-in a [:summary :unreached-wants]))
     :second-unreached? (= [] (get-in b [:summary :unreached-wants]))
     :first-chosen? (= :C1 (get-in a [:summary :chosen :candidate]))
     :second-unchosen? (nil? (get-in b [:summary :chosen]))
     :summary-shape-stable? (= (dissoc (:summary a) :chosen :unreached-wants)
                                (dissoc (:summary b) :chosen :unreached-wants))}))

(defn- opts-relations []
  (let [{:keys [flights outputs]} (target-readers/products :opts)
        [a b] outputs]
    {:only-target-varies? (= (mapv #(dissoc % :target) flights)
                             (repeat 2 (dissoc (first flights) :target)))
     :targets-carried? (= target-readers/targets (mapv #(get-in % [:flight :target]) [a b]))
     :output-shape-stable? (= (update a :flight dissoc :target)
                               (update b :flight dissoc :target))}))

(defn- assembly-relations []
  (let [{:keys [flights outputs]} (target-readers/products :assembly)
        [a b] outputs]
    {:only-target-varies? (= (mapv #(dissoc % :target) flights)
                             (repeat 2 (dissoc (first flights) :target)))
     :targets-carried? (= (mapv vector target-readers/targets) (mapv :targets [a b]))
     :wants-carried? (every? true? (map #(= [:done] (get-in %1 [:sources :wants %2]))
                                      [a b] target-readers/targets))
     :locators-carried? (every? true? (map #(= (:locators target-readers/wants)
                                                (get-in %1 [:sources :locators %2]))
                                         [a b] target-readers/targets))
     :universes-carried? (every? true? (map #(= {:done false}
                                                 (get-in %1 [:sources :universes %2]))
                                          [a b] target-readers/targets))
     :contexts-carried? (every? true? (map #(= :WM ((get-in %1 [:sources :context-of]) %2))
                                         [a b] target-readers/targets))
     :assembly-shape-stable? (= (target-readers/normal-assembly a (first target-readers/targets))
                                 (target-readers/normal-assembly b (second target-readers/targets)))}))

(defn- product-relations [kind]
  (let [reader ({:flight-run :run :r0-enact-step :enact
                 :r10-observe-publication :publication :r2-flight-read :read
                 :r9-close-cause :close} kind)
        {:keys [flights products]} (target-products/products reader)
        [a b] products
        only-target? (= (dissoc (first flights) :target) (dissoc (second flights) :target))]
    (case kind
      :flight-run
      {:only-target-varies? only-target?
       :statuses? (= [:closed :no-progress] (mapv :status products))
       :open-after? (= [[] [:done]] (mapv #(get-in % [:clicks 0 :open-after]) products))
       :one-click? (= [1 1] (mapv #(count (:clicks %)) products))}
      :r0-enact-step
      {:only-target-varies? only-target?
       :targets-carried? (= target-products/targets (mapv #(get-in % [:record :attempts 0 :target]) products))
       :paths-related? (= (:path a) (:path b))
       :attempt-shape-stable? (= (dissoc (get-in a [:record :attempts 0]) :target)
                                  (dissoc (get-in b [:record :attempts 0]) :target))
       :both-failed? (= [false false] (mapv #(get-in % [:record :attempts 0 :success]) products))}
      :r10-observe-publication
      {:only-target-varies? only-target?
       :targets-carried? (= target-products/targets (mapv #(get-in % [:publication-observed :target]) products))
       :typed-absence-stable? (= [{:absent :no-repair-obligation-for-target}
                                  {:absent :no-repair-obligation-for-target}]
                                 (mapv #(dissoc (:publication-observed %) :target) products))}
      :r2-flight-read
      {:only-target-varies? only-target?
       :request-ids-differ? (not= (get-in a [:asked 0 :request-id]) (get-in b [:asked 0 :request-id]))
       :request-ids-strings? (every? string? (map #(get-in % [:asked 0 :request-id]) products))
       :served-by-stable? (= (:served-by a) (:served-by b))
       :not-answered? (= [:not-answered :not-answered]
                         (mapv #(get-in % [:asked 0 :outcome]) products))}
      :r9-close-cause
      {:only-target-varies? only-target?
       :targets-carried? (= target-products/targets (mapv #(get-in % [:targets 0 :target]) products))
       :abstention-shape-stable? (= (update a :targets #(mapv (fn [t] (dissoc t :target)) %))
                                     (update b :targets #(mapv (fn [t] (dissoc t :target)) %)))})))

(defn build-record []
  {:producer producer
   :operation operation
   :inputs {:kinds kinds :tamper-modes [:identity :typed-absence :different-target]}
   :fields {:wires (into {} (map (juxt identity primary) kinds))
            :second-layer
            {:flight-click (click-relations)
             :flight-judge-opts (opts-relations)
             :flight-run (product-relations :flight-run)
             :r0-enact-step (product-relations :r0-enact-step)
             :r10-observe-publication (product-relations :r10-observe-publication)
             :r2-flight-read (product-relations :r2-flight-read)
             :r9-close-cause (product-relations :r9-close-cause)
             :tick-flight-assembly (assembly-relations)}}
   :left-out {:request-ids "generated per read; the record stores their string type and inequality relation"
              :temporary-enactment-paths "temporary directories differ per run; the record stores the equality relation"
              :product-values "readers assert the named relations, recorded individually under :second-layer"}})

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record)
        file (io/file "test/fixtures/wire-producers"
                      (str "target-observe@" (subs (sha256 text) 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file)
      (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(defn- checked-field-paths [fields]
  (concat
   (mapcat (fn [kind]
             (concat
              (map #(vector :wires kind %) [:writer :reader :received?])
              [[:wires kind :interventions :absent :received?]
               [:wires kind :interventions :different :reader]
               [:wires kind :interventions :different :received?]]))
           kinds)
   (for [kind kinds
         field (keys (get-in fields [:second-layer kind]))]
     [:second-layer kind field])))

(deftest target-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "target-observe@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (doseq [path (checked-field-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
