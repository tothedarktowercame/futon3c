(ns futon3c.diagramprover.wm-wire-producer-small-observe
  "Producer for the small-observe wire group. Runs
  futon3c.diagramprover.wm-wire-small-support/observe (operation
  futon2.aif.flight/run! and the lane's product calls) once per lane and
  mode, and the second-layer product calls of the six readers, and asserts
  the result equals the committed content-addressed record. Writes the
  record only when WM_WIRE_PRODUCER_WRITE=1; never overwrites."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-ask-library-products :as ask-products]
            [futon3c.diagramprover.wm-wire-precision-horizon-products :as horizon-products]
            [futon3c.diagramprover.wm-wire-small-support :as support]
            [futon3c.diagramprover.wm-wire-summary-conditioning-products :as conditioning]
            [futon3c.diagramprover.wm-wire-summary-products :as summary-products])
  (:import [java.security MessageDigest]))

(def producer 'futon3c.diagramprover.wm-wire-producer-small-observe-test)
(def operation 'futon2.aif.flight/run!)

(def lanes
  [[:dispatch [:dispatch :clock-in :mission-id]]
   [:chosen [:flight-record-summary :flight-run :chosen]]
   [:horizon [:r13-sources-horizon :construction-assemble [:horizon-steps {:record :sources}]]]
   [:prompt [:r3-flight-ask :r3-prompt :library-root]]
   [:ask-test [:r3-flight-ask :r3-test :library-root]]
   [:kernel [:r4-evaluate-state :r4-push-forward [:kernel {:record :evaluation}]]]])

(defn- stable [value]
  (cond
    (map? value) (into (empty value) (map (fn [[k v]] [k (stable v)])) value)
    (vector? value) (mapv stable value)
    (set? value) (set (map stable value))
    (seq? value) (mapv stable value)
    (and (string? value) (.contains value "small-other-library-")) :temporary-path/other
    (and (string? value) (.contains value "small-library-")) :temporary-path/library
    :else value))

(defn- stable-observation [result]
  (update-vals (select-keys result [:writer :reader]) stable))

(defn- primary [kind]
  (let [none (support/observe kind (fn [v _] v))]
    {:writer (stable (:writer none))
     :reader (stable (:reader none))
     :received? (w/received? (stable-observation none))
     :interventions
     (into {}
           (for [[mode tamper] [[:absent (fn [_ _] {:status :absent :reason :not-carried})]
                                [:different (fn [_ other] other)]]
                 :let [result (support/observe kind tamper)]]
             [mode {:writer-present? (some? (:writer result))
                    :reader-present? (some? (:reader result))
                    :received? (w/received? (stable-observation result))}]))}))

(defn- live-relations []
  ;; The relations futon3c.diagramprover.wm-wire-small-support/assert-live-records
  ;; asserts, computed here so the values land in the record.
  (let [f (support/pinned 0) r (support/pinned 1)]
    {:run-chosen-present? (some? (get-in r [:decision :chosen]))
     :flight-clicks-lack-chosen? (not-any? :chosen (:clicks f))}))

(defn- summary-run-chosen-relations []
  (let [[a b] (summary-products/products :run :chosen)
        ra (:record a) rb (:record b)]
    {:carriers-except-chosen-equal? (= (dissoc (:carrier a) :chosen) (dissoc (:carrier b) :chosen))
     :a-chosen-carried? (= (get-in a [:carrier :chosen]) (get-in ra [:clicks 0 :chosen]))
     :b-chosen-carried? (= (get-in b [:carrier :chosen]) (get-in rb [:clicks 0 :chosen]))
     :chosen-values-differ? (not= (get-in ra [:clicks 0 :chosen]) (get-in rb [:clicks 0 :chosen]))
     :records-else-equal? (= (update ra :clicks #(mapv (fn [c] (dissoc c :chosen)) %))
                             (update rb :clicks #(mapv (fn [c] (dissoc c :chosen)) %)))
     :no-progress? (= :no-progress (:status ra) (:status rb))
     :one-click-each? (= 1 (count (:clicks ra)) (count (:clicks rb)))
     :two-observations-each? (= 2 (:observations a) (:observations b))
     :one-click-call-each? (= 1 (:click-calls a) (:click-calls b))}))

(defn- conditioning-chosen-relations []
  (let [[a b] (conditioning/pair :chosen)
        sa (get-in a [:record :enactments 0 :step])
        sb (get-in b [:record :enactments 0 :step])]
    {:run-record-same? (= (:run-record a) (:run-record b))
     :increment-same? (= (:increment a) (:increment b))
     :clicks-except-precedence-equal? (= (update (:click a) :chosen dissoc :precedence)
                                         (update (:click b) :chosen dissoc :precedence))
     :steps-present? (= :present (:status sa) (:status sb))
     :step-fields-same? (= (select-keys sa [:o :measured-a :s-prev :policy-key])
                           (select-keys sb [:o :measured-a :s-prev :policy-key]))
     :a-p-o-eleven-twelfths? (= 11/12 (:p-o sa))
     :b-p-o-one-twelfth? (= 1/12 (:p-o sb))
     :f-increases? (< (:f sa) (:f sb))
     :q-differs? (not= (:q sa) (:q sb))}))

(defn- horizon-relations []
  (let [a (horizon-products/horizon-product identity)
        b (horizon-products/horizon-product inc)]
    {:written-same? (= (:written a) (:written b))
     :carrier-dec-relation? (= (:carrier a) (update-in (:carrier b) [:sources :horizon-steps] dec))
     :horizon-steps-3-4? (= [3 4] (mapv #(get-in % [:problem :horizon-steps]) [a b]))
     :state-same? (= (:state a) (:state b))
     :candidates-same? (= (:candidates a) (:candidates b))
     :opts-except-horizon-same? (= (dissoc (:opts a) :horizon-steps) (dissoc (:opts b) :horizon-steps))
     :scores-numeric? (every? number? (concat (:scores a) (:scores b)))
     :scores-differ? (not= (:scores a) (:scores b))}))

(defn- prompt-relations []
  (ask-products/with-libraries
   (fn [a b]
     (let [[x y] (ask-products/prompts a b)]
       {:written-equals-carrier? (= (:written x) (:written y) (:carrier x))
        :carrier-root-relation? (= (:carrier x) (assoc (:carrier y) :library-root a))
        :text-includes-a? (str/includes? (:text x) a)
        :text-includes-b? (str/includes? (:text y) b)
        :texts-differ? (not= (:text x) (:text y))
        :text-replace-relation? (= (:text x) (str/replace (:text y) b a))
        :no-pattern-content? (not-any? #(str/includes? (str (:text x) (:text y)) %)
                                       ["Pattern Alpha" "Pattern Beta" "a.flexiarg" "b.flexiarg"])}))))

(defn- ask-test-relations []
  (ask-products/with-libraries
   (fn [_ b]
     (let [a (ask-products/assertion-report nil)
           changed (ask-products/assertion-report b)
           counts #(frequencies (map :type (:reports %)))]
       {:written-equals-carrier? (= (:written (first (:calls a))) (:carrier (first (:calls a))))
        :written-same? (= (:written (first (:calls a))) (:written (first (:calls changed))))
        :carrier-root-changed? (= b (get-in changed [:calls 0 :carrier :library-root]))
        :before-three-passes? (= {:pass 3} (counts a))
        :after-two-passes-one-fail? (= {:pass 2 :fail 1} (counts changed))
        :failure-expected-form? (= '(str/includes? with "/fixture/library-root")
                                   (:expected (first (filter #(= :fail (:type %)) (:reports changed)))))}))))

(defn build-record []
  (let [wire-id (fn [kind] (second (first (filter #(= kind (first %)) lanes))))]
    {:producer producer :operation operation
     :inputs {:support ['futon3c.diagramprover.wm-wire-small-support/assert-live-records
                        'futon3c.diagramprover.wm-wire-small-support/observe]
              :lanes lanes :modes [:none :absent :different]}
     :fields {:live (live-relations)
              :wires (into {} (map (fn [[kind id]] [id (primary kind)]) lanes))
              :second-layer
              {(wire-id :chosen) {:summary-run-chosen (summary-run-chosen-relations)
                                  :conditioning-chosen (conditioning-chosen-relations)}
               (wire-id :horizon) (horizon-relations)
               (wire-id :prompt) (prompt-relations)
               (wire-id :ask-test) (ask-test-relations)}}
     :left-out {:temporary-paths
                "observe's :prompt and :ask-test writers/readers are per-run temporary directories; replaced by :temporary-path/library and :temporary-path/other so the identity/different relations are preserved. Readers assert the recorded writer/reader relation, never the path."
                :product-values
                "The second-layer deftests check relations among product values (scores, prompt text, reports); only the boolean relations are recorded, not the values."}}))

(defn- record-text [record] (str (pr-str record) "\n"))
(defn- sha256 [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes text "UTF-8"))]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn- write-record! [record]
  (let [text (record-text record) sha (sha256 text)
        file (io/file "test/fixtures/wire-producers" (str "small-observe@" (subs sha 0 12) ".edn"))]
    (.mkdirs (.getParentFile file))
    (when (.exists file) (throw (ex-info "producer record already exists" {:file (str file)})))
    (spit file text)
    (println (.getPath file))))

(defn- leaf-paths [m]
  (mapcat (fn [[k v]]
            (if (map? v) (map #(into [k] %) (leaf-paths v)) [[k]]))
          m))

(deftest small-observe-producer
  (let [actual (build-record)]
    (if (= "1" (System/getenv "WM_WIRE_PRODUCER_WRITE"))
      (write-record! actual)
      (let [expected (edn/read-string
                      (slurp (first (filter #(.startsWith (.getName %) "small-observe@")
                                            (.listFiles (io/file "test/fixtures/wire-producers"))))))]
        (is (= producer (:producer expected)))
        (is (= operation (:operation expected)))
        (is (= (:inputs actual) (:inputs expected)))
        (doseq [path (leaf-paths (:fields expected))]
          (testing (pr-str path)
            (is (= (get-in expected (into [:fields] path))
                   (get-in actual (into [:fields] path))))))))))
