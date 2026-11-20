(ns futon3c.arxana.together-structure-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.together-structure :as s]))

(defn- node [id] {:op :add-node :id id :kind "fixture" :attrs {}})
(defn- nodes [] (reduce s/apply-op s/empty-state (map node ["a" "b" "c" "d" "e" "f"])))
(defn- group [child parent] {:op :group :child child :parent parent})
(defn- refusal [state op error]
  (is (= error (s/check-op state op)))
  (is (= error (try (s/apply-op state op) nil
                   (catch clojure.lang.ExceptionInfo e (:together/error (ex-data e)))))))

(deftest operations-and-refusals
  (let [base (nodes)
        edge {:op :add-edge :id "edge" :type "fixture" :ends ["a" "b"]}
        with-edge (s/apply-op base edge)
        version {:op :supersede :id "v1" :replaces "a" :kind "version" :attrs {:x 1}}
        branched (-> with-edge (s/apply-op version) (s/apply-op (assoc version :id "v2")))
        grouped (s/apply-op base (group "a" "b"))]
    (is (= {:kind "fixture" :attrs {}} (get-in base [:nodes "a"])))
    (is (= (select-keys edge [:type :ends]) (get-in with-edge [:edges "edge"])))
    (is (= ["edge"] (get-in (s/apply-op with-edge (assoc edge :id "edge2" :ends ["edge"])) [:edges "edge2" :ends])))
    (is (= {"v1" "a" "v2" "a"} (:derived branched)))
    (is (= {:kind "version" :attrs {:x 1}} (get-in branched [:nodes "v1"])))
    (is (= #{} (get-in (s/apply-op grouped {:op :ungroup :child "a" :parent "b"}) [:parents "a"])))
    (is (= #{"c"} (get-in (s/apply-op grouped {:op :regroup :child "a" :from "b" :to "c"}) [:parents "a"])))
    (doseq [op [(node "edge") edge (assoc version :id "edge")]]
      (refusal with-edge op :id-in-use))
    (doseq [op [(assoc edge :id "new-edge" :ends ["missing"]) (assoc version :replaces "edge")
                (group "a" "missing") {:op :ungroup :child "missing" :parent "a"}
                {:op :regroup :child "a" :from "missing" :to "a"}]]
      (refusal with-edge op :unknown-id))
    (refusal base {:op :ungroup :child "a" :parent "b"} :not-grouped)
    (refusal grouped {:op :regroup :child "a" :from "c" :to "b"} :not-grouped)
    (refusal grouped (group "a" "b") :already-grouped)
    (refusal (s/apply-op grouped (group "a" "c"))
             {:op :regroup :child "a" :from "b" :to "c"} :already-grouped)
    (doseq [op [nil [] {} {:op :unknown} (assoc (node "a") :attrs nil)
                (assoc (node "a") :kind :keyword) (node "bad id") (node "")
                (node (apply str (repeat 121 "a"))) (assoc (node "a") :extra true)
                (assoc edge :ends []) (assoc edge :ends '("a")) (assoc edge :ends [1])
                (assoc edge :type nil) (dissoc version :replaces)
                (group "a" nil) {:op :ungroup :child "a"}
                {:op :regroup :child "a" :from "b" :to 3}]]
      (refusal base op :invalid-op))
    (is (nil? (s/check-op base (node "A_1.x:-"))))
    (doseq [identity [nil "" "with.dot" (apply str (repeat 61 "a"))]]
      (is (= {:error :invalid-identity}
             (s/submit {:state base :log []} (node "new") identity "time"))))))

(deftest self-parent
  (refusal (nodes) (group "a" "a") :cycle))
(deftest two-step
  (refusal (s/apply-op (nodes) (group "a" "b")) (group "b" "a") :cycle))
(deftest three-step-chain
  (refusal (reduce s/apply-op (nodes) [(group "a" "b") (group "b" "c")])
           (group "c" "a") :cycle))
(deftest diamond-is-allowed
  (let [state (reduce s/apply-op (nodes)
                      [(group "b" "a") (group "c" "a") (group "d" "b") (group "d" "c")])]
    (is (= #{"b" "c"} (get-in state [:parents "d"])))
    (is (= #{"a"} (get-in state [:parents "b"])))))

(deftest regroup-refusals-logged-and-rebuilt
  (let [ops (concat (map node ["a" "b" "c"])
                    [(group "b" "a") (group "c" "b")
                     {:op :regroup :child "b" :from "a" :to "a"}
                     {:op :regroup :child "b" :from "a" :to "c"}
                     {:op :ungroup :child "c" :parent "b"}])
        hubs (reductions (fn [hub op] (:hub (s/submit hub op "fixture_user" "supplied-time")))
                         {:state s/empty-state :log []} ops)
        hub (last hubs)]
    (is (= :already-grouped (:refused (nth (:log hub) 5))))
    (is (= :cycle (:refused (nth (:log hub) 6))))
    (is (= (:state (nth hubs 5)) (:state (nth hubs 6)) (:state (nth hubs 7))))
    (is (= (range 1 9) (map :rev (:log hub))))
    (is (= (:state hub) (s/rebuild (reverse (:log hub)))))
    (is (= {:rev 7 :op (nth (vec ops) 6) :author "fixture_user" :at "supplied-time" :refused :cycle}
           (nth (:log hub) 6)))
    (let [{:keys [hub entry]} (s/submit hub nil "fixture_user" nil)]
      (is (= :invalid-op (:refused entry)))
      (is (= (:state hub) (s/rebuild (:log hub)))))))

(defn- returns-to-self? [state origin]
  (loop [todo (seq (get-in state [:parents origin])) seen #{}]
    (if-let [id (first todo)]
      (cond (= id origin) true
            (seen id) (recur (next todo) seen)
            :else (recur (concat (next todo) (get-in state [:parents id])) (conj seen id)))
      false)))

(deftest seeded-random-sequences-remain-acyclic
  (let [rng (java.util.Random. 73921)
        ids ["a" "b" "c" "d" "e" "f"]
        pick #(nth ids (.nextInt rng 6))]
    (dotimes [trial 300]
      (loop [hub {:state s/empty-state :log []} step 0]
        (when (< step 60)
          (let [op (case (.nextInt rng 6)
                     0 (node (pick))
                     1 {:op :add-edge :id (pick) :type "fixture" :ends [(pick)]}
                     2 {:op :supersede :id (pick) :replaces (pick) :kind "fixture" :attrs {}}
                     3 (group (pick) (pick))
                     4 {:op :ungroup :child (pick) :parent (pick)}
                     5 {:op :regroup :child (pick) :from (pick) :to (pick)})
                next-hub (:hub (s/submit hub op "fixture" step))]
            (testing (str "trial " trial " step " step)
              (is (not-any? #(returns-to-self? (:state next-hub) %) ids))
              (is (= (:state next-hub) (s/rebuild (:log next-hub)))))
            (recur next-hub (inc step))))))))
