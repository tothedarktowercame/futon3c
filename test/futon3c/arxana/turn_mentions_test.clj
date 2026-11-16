(ns futon3c.arxana.turn-mentions-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.records :as records]
            [futon3c.arxana.turn-mentions :as mentions]
            [futon3c.arxana.type-registry :as types])
  (:import [java.nio.charset StandardCharsets]))

(defn- surface [text r]
  (let [[a b] (get-in r [:asserted-content :mention/range])]
    (.substring ^String text (.offsetByCodePoints ^String text 0 (int a))
                (.offsetByCodePoints ^String text 0 (int b)))))

(defn- reference [text]
  (records/make-external-reference
   {:ref/kind :http-record :store/id "toy-turns" :store/endpoint "https://example.invalid/text"
    :record/id "toy-turn/source_text" :schema/authority :toy/text :valid-time 1
    :revision/digest (digest/sha256-hex (.getBytes ^String text StandardCharsets/UTF_8))}))

(deftest unicode-ranges-and-repeated-identities
  (let [text "🈸 猴 M-toy; M-toy!"
        rs (mentions/turn-mentions text (reference text) ["M-toy"])]
    (is (= [[4 9] [11 16]] (mapv #(get-in % [:asserted-content :mention/range]) rs)))
    (is (= ["M-toy" "M-toy"] (mapv #(surface text %) rs)))
    (testing "default: the quoted text is the item id, so no quote is stored"
      (is (every? #(not (contains? (:asserted-content %) :mention/quote)) rs)))
    (is (= 2 (count (set (map :id rs)))))
    (is (every? #(mentions/mention-valid? text %) rs))
    (is (= rs (mentions/turn-mentions text (reference text) ["M-toy" "M-toy"])))
    (is (= (:id (records/make-clusion {:type-revision [:workplan/item 1] :endpoints []
                                      :asserted-content {:item/id "M-toy"}}))
           (get-in rs [0 :endpoints 0 :target :clusion/id])))))

(deftest exact-whole-tokens-and-deterministic-order
  (let [text "M-toy-2 preM-toy M-toy_suffix M-toý 猴M-toy (M-toy), E-toy."
        rs (mentions/turn-mentions text (reference text) ["M-toy" "E-toy" "M-toy-2"])]
    (is (= ["M-toy-2" "M-toy" "E-toy"] (mapv #(surface text %) rs)))
    (is (= rs (mentions/turn-mentions text (reference text) ["E-toy" "M-toy-2" "M-toy"])))
    (is (= [] (mentions/turn-mentions text (reference text) [])))))

(deftest rejects-invalid-standoff-and-pin
  (let [text "🈸 M-toy"
        r (first (mentions/turn-mentions text (reference text) ["M-toy"]))
        change-content (fn [field value]
                         (records/make-clusion (assoc-in r [:asserted-content field] value)))]
    (testing "recompute IDs so rejection tests the locator, not just stale IDs"
      (is (false? (mentions/mention-valid? text (change-content :mention/range [3 7]))))
      (is (false? (mentions/mention-valid? text (change-content :mention/quote "wrong")))))
    (is (false? (mentions/mention-valid? (str text "!") r)))
    (testing "without a stored quote the range must spell the item endpoint's id"
      (let [other (records/make-clusion {:type-revision [:workplan/item 1] :endpoints []
                                         :asserted-content {:item/id "E-toy"}})]
        (is (false? (mentions/mention-valid?
                     text (records/make-clusion
                           (assoc-in r [:endpoints 0 :target :clusion/id] (:id other))))))))
    (testing "a stored quote (non-default surface) must equal the text at the range"
      (is (true? (mentions/mention-valid? text (change-content :mention/quote "M-toy")))))
    (is (false? (mentions/mention-valid? text (change-content :mention/range [-1 7]))))
    (is (false? (mentions/mention-valid? text (change-content :mention/range [2 999]))))
    (is (false? (mentions/mention-valid? text (change-content :mention/range nil))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"evidence pin"
                         (mentions/turn-mentions text (reference "different") ["M-toy"])))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"reference id"
                         (mentions/turn-mentions text (assoc (reference text) :id "wrong") ["M-toy"])))))

(deftest version-one-identity-unchanged
  (is (= [:mention/role] (:identity/content-fields (types/declaration [:workplan/mention 1]))))
  (is (= [:mention/role :mention/range]
         (:identity/content-fields (types/declaration [:workplan/mention 2])))))
