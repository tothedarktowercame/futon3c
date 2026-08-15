(ns futon3c.apm.cycle-harness-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.apm.cycle-harness :as harness]
            [futon3c.apm.preregistration :as prereg]
            [futon3c.apm.preregistration-test :as fixture])
  (:import [java.nio.file Files]))

(def cycle-id "cycle/t94J02/1")
(def rev-a "1111111111111111111111111111111111111111")
(def rev-b "2222222222222222222222222222222222222222")

(defn base-entities []
  (let [caps prereg/required-capabilities]
    (vec
     (concat
      [{:cycle/id cycle-id :cycle/opened-at "2026-08-14T12:00:00Z"
        :cycle/closed-at "2026-08-14T13:00:00Z" :cycle/mode :harness-mode
        :cycle/deposit-state :n/a :cycle/paired-with nil
        :cycle/stratum-frozen-at 1 :cycle/assigned-at 2}
       {:frame/id (str "frame/" cycle-id) :frame/cycle cycle-id
        :frame/scaffold-hash "a" :frame/closing-hash "b"}
       {:gate/id (str "gate/" cycle-id) :gate/cycle cycle-id
        :gate/refused-without-witness? true}
       {:snap/id (str "snap/" cycle-id) :snap/cycle cycle-id
        :snap/memory-ids ["memory/known"]}
       {:cprobe/id (str "cprobe/frame/" cycle-id)
        :cprobe/frame (str "frame/" cycle-id) :cprobe/claimed? true
        :cprobe/recorded? true :cprobe/passed? true}
       {:attempt/id (str "attempt/" cycle-id "/1")
        :attempt/cycle cycle-id :attempt/seq 1 :cycle/regime "regime/a"
        :cycle/store-revision rev-a :cycle/harness-revision rev-a
        :cycle/environment-checkout "/frames/solver"
        :cycle/environment-revision rev-a
        :cycle/runner-freshness true}
       {:meas/id (str "meas/" cycle-id) :meas/cycle cycle-id
        :meas/values (zipmap prereg/required-measurement-fields
                             (repeat :observed)) :meas/unset {}}
       {:disp/id (str "disp/" cycle-id) :disp/cycle cycle-id}]
      (map-indexed
       (fn [i capability]
         {:probe/id (str "probe/" cycle-id "/" i) :probe/cycle cycle-id
          :probe/capability capability :probe/evidence-id (str "e/" i)
          :probe/recorded? true}) caps)))))

(defn trace-with [entities]
  (harness/derive-trace fixture/registration cycle-id entities))

(defn failures [entities]
  (fixture/checked fixture/registration (trace-with entities)))

(deftest entity-round-trip-is-exact
  (let [store (harness/memory-store)
        entity {:cycle/id cycle-id :value [1 2 3]}]
    (is (= entity (harness/persist-roundtrip! store cycle-id entity)))
    (is (= entity ((:read! store) cycle-id)))))

(deftest scaffold-identical-frame-is-refused-before-persistence
  (let [store (harness/memory-store)
        path (Files/createTempFile "apm-frame" ".lean"
                                   (make-array java.nio.file.attribute.FileAttribute 0))]
    (spit (.toFile path) "theorem x : True := by trivial\n")
    (try
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"scaffold-identical"
                            (harness/emit-frame!
                             store {:frame/id "frame/test"} path path)))
      (is (nil? ((:read! store) "frame/test")))
      (finally (Files/deleteIfExists path)))))

(deftest two-dispositions-are-refused
  (is (some #{:f2-non-unique-disposition}
            (failures (conj (base-entities)
                            {:disp/id "disp/second" :disp/cycle cycle-id})))))

(deftest offer-without-use-is-refused
  (is (some #{:f3-undispositioned-offer}
            (failures (conj (base-entities)
                            {:offer/id "offer/1" :offer/cycle cycle-id
                             :offer/memory-id "memory/known"})))))

(deftest both-channels-varied-is-refused
  (is (some #{:both-channels-varied}
            (failures
             (conj (base-entities)
                   {:attempt/id (str "attempt/" cycle-id "/2")
                    :attempt/cycle cycle-id :attempt/seq 2
                    :cycle/regime "regime/a" :cycle/store-revision rev-b
                    :cycle/harness-revision rev-b
                    :cycle/environment-checkout "/frames/student"
                    :cycle/environment-revision rev-a
                    :cycle/runner-freshness true})))))

(deftest new-memory-in-harness-mode-is-refused
  (is (some #{:new-memory-in-harness-round}
            (failures (conj (base-entities)
                            {:offer/id "offer/1" :offer/cycle cycle-id
                             :offer/memory-id "memory/new"}
                            {:use/id "use/offer/1" :use/offer "offer/1"})))))
