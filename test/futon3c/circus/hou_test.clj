(ns futon3c.circus.hou-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.turn-mentions :as mentions]
            [futon3c.circus.hou :as hou]))

(defn field-ref [field text]
  (records/make-external-reference
   {:ref/kind :file-record :store/id "toy-turns" :store/endpoint "/invented"
    :record/id (str "toy/" field) :schema/authority :futon3c/turn-record
    :valid-time "2026-01-01" :revision/digest (digest/sha256-hex (.getBytes text "UTF-8"))}))

(defn fixture
  ([] (fixture "M-a M-b" "" [["M-b" "M-c"] ["M-c" "M-d"]]))
  ([source reply edges]
   (let [items ["M-a" "M-b" "M-c" "M-d"]
         a (field-ref "source_text" source) b (field-ref "reply_text" reply)
         ms (vec (concat (mentions/turn-mentions source a items) (mentions/turn-mentions reply b items)))
         rs (mapv (fn [i pair] {:record {:id (str "reading-" i) :needs [pair]} :needs [pair]}) (range) edges)
         all (concat [a b] ms (map :record rs))]
     {:turn-id "toy" :at "2026-01-01" :settled? true :reply-landed? true :items items
      :source-ref (:id a) :references [a b] :text-by-ref {(:id a) source (:id b) reply}
      :mention-records ms :readings rs :stored (into {} (map (juxt :id identity)) all) :sources []})))

(deftest guards-ordered-and-no-focus-on-failure
  (is (= {:failed :turnNotSettled} (hou/hou-run {})))
  (is (= {:failed :replyNotLanded} (hou/hou-run {:settled? true})))
  (is (= {:failed :invalidEvidence} (hou/hou-run {:settled? true :reply-landed? true})))
  (is (= {:failed :invalidEvidence} (hou/hou-run (assoc (fixture) :stored {}))))
  (is (= {:failed :focusNotLocated} (hou/hou-run (fixture "M-a" "M-b" [])))))

(deftest named-items-radius-distance-and-identity
  (let [in (fixture) out (hou/hou-run in) h (:written out) c (:asserted-content h)]
    (is h)
    (is (= [["M-a" :focus] ["M-b" :focus] ["M-c" :associated]] (:hou/relations c)))
    (is (= 4 (:hou/hop-bound c)))
    (is (= 1 (:hou/associated-radius c)))
    (is (= out (hou/hou-run in)))
    (is (hou/header-links-agree? h (:links out)))
    (is (false? (hou/header-links-agree? h (pop (:links out)))))
    (is (false? (hou/header-links-agree? h (conj (:links out) (first (:links out))))))
    (is (= [["M-a" :focus] ["M-b" :focus]]
           (get-in (hou/hou-run (assoc in :associated-radius 0)) [:written :asserted-content :hou/relations])))
    (is (= [["M-a" :focus] ["M-b" :focus] ["M-c" :associated] ["M-d" :associated]]
           (get-in (hou/hou-run (assoc in :associated-radius 2)) [:written :asserted-content :hou/relations])))))

(deftest alternate-rule-and-anchor
  (let [in (assoc (fixture "M-a" "M-b" [["M-a" "M-b"] ["M-b" "M-c"]])
                  :mission-id "M-c" :rule :mission-and-named)]
    (is (= [["M-a" :focus] ["M-b" :associated] ["M-c" :focus]]
           (get-in (hou/hou-run in) [:written :asserted-content :hou/relations])))
    (is (= {:failed :focusNotLocated}
           (hou/hou-run (assoc (fixture "nothing named" "" [])
                               :mission-id "M-a" :rule :mission-and-named)))))
  (testing "valid empty evidence is a successful empty focus, never a failure alias"
    (let [out (hou/hou-run (fixture "nothing named" "" []))]
      (is (:written out))
      (is (= [] (get-in out [:written :asserted-content :hou/relations]))))))

(deftest every-read-record-is-pinned
  (let [in (fixture) out (hou/hou-run in)]
    (is (= (set (keys (:stored in))) (set (map first (get-in out [:written :asserted-content :hou/evidence-pin])))))
    (is (= {:failed :invalidEvidence}
           (hou/hou-run (update in :mention-records conj (first (:mention-records in))))))))

(deftest reading-node-includes-accepted-non-needs-incidence
  (let [in (fixture "M-a" "M-c" [["M-a" "M-b"]])
        in (assoc-in in [:readings 0 :incident-items] ["M-a" "M-b" "M-c"])]
    (is (= 2 (get-in (hou/hou-run in) [:written :asserted-content :hou/hop-bound])))))

(deftest missing-turntext-cannot-manufacture-a-path
  (let [in (fixture "M-a" "M-b" [["M-a" "M-b"]])]
    (is (= {:failed :invalidEvidence}
           (hou/hou-run (update in :references #(vec (take 1 %))))))))
