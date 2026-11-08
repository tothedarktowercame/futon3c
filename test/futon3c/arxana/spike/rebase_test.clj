(ns futon3c.arxana.spike.rebase-test
  "Tests for the F2 three-way rebase spike. See SPIKE-f2-rebase.md."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.spike.rebase :as r]))

(defn- s
  "canonical bytes -> String, for assertions"
  ^String [bs]
  (r/canonical-str bs))

(def base-text "the quick brown fox jumps over the lazy dog")
(def base-digest (r/revision-digest base-text))

(defn- patch [author op-id hunks]
  (r/->patch {:base base-digest :author author :op-id op-id :hunks hunks}))

;;; ---------------------------------------------------------------------
;;; 1. The five overlap classes, hand-written

(deftest overlap-classes
  (testing "disjoint/commuting — both edits land, order irrelevant"
    (let [a (patch "alice" "a-1" [{:start 4 :end 9 :replacement "slow"}])
          b (patch "bob" "b-1" [{:start 35 :end 39 :replacement "tired"}])
          ab (r/rebase base-text a b)
          ba (r/rebase base-text b a)]
      (is (some? (:result ab)))
      (is (= "the slow brown fox jumps over the tired dog" (s (:result ab))))
      (is (r/bytes= (:result ab) (:result ba)))
      (is (:locatable? (r/locate (:result ab) base-text a (:merged-hunks ab))))
      (is (:locatable? (r/locate (:result ab) base-text b (:merged-hunks ab))))))
  (testing "identical — same edit from both sides applied once"
    (let [a (patch "alice" "a-2" [{:start 4 :end 9 :replacement "slow"}])
          b (patch "bob" "b-2" [{:start 4 :end 9 :replacement "slow"}])
          res (r/rebase base-text a b)]
      (is (some? (:result res)))
      (is (= 1 (:dropped-identical res)))
      (is (= "the slow brown fox jumps over the lazy dog" (s (:result res))))))
  (testing "same-position — never a silent winner"
    (let [a (patch "alice" "a-3" [{:start 4 :end 9 :replacement "slow"}])
          b (patch "bob" "b-3" [{:start 4 :end 9 :replacement "swift"}])
          res (r/rebase base-text a b)]
      (is (nil? (:result res)))
      (is (= 2 (count (:alternatives res))))
      (is (= [:same-position] (map :class (:conflicts res))))
      (is (= [["alice" "bob"]] (map :authors (:conflicts res))))
      (is (= #{"the slow brown fox jumps over the lazy dog"
               "the swift brown fox jumps over the lazy dog"}
             (set (map (comp s :bytes) (:alternatives res)))))))
  (testing "nested — outer vs inner edit, explicit alternatives"
    (let [a (patch "alice" "a-4" [{:start 4 :end 15 :replacement "red cat"}])
          b (patch "bob" "b-4" [{:start 10 :end 15 :replacement "green"}])
          res (r/rebase base-text a b)]
      (is (nil? (:result res)))
      (is (= [:nested] (map :class (:conflicts res))))
      (is (= 2 (count (:alternatives res))))))
  (testing "partial overlap — interleaved spans, explicit alternatives"
    (let [a (patch "alice" "a-5" [{:start 4 :end 15 :replacement "A"}])
          b (patch "bob" "b-5" [{:start 10 :end 19 :replacement "B"}])
          res (r/rebase base-text a b)]
      (is (nil? (:result res)))
      (is (= [:partial-overlap] (map :class (:conflicts res))))
      (is (= 2 (count (:alternatives res)))))))

(deftest stale-base-is-a-typed-conflict
  (let [a (patch "alice" "a-stale" [{:start 4 :end 9 :replacement "slow"}])]
    (is (thrown? clojure.lang.ExceptionInfo
                 (r/apply-patch "completely different text" a)))))

;;; ---------------------------------------------------------------------
;;; 2. Offline replay: B edits offline against h0 while A lands two patches

(def replay-base "Hello world. The sky is blue. Goodbye world.")
(def replay-digest (r/revision-digest replay-base))

(defn- replay-patch [author op-id hunks]
  (r/->patch {:base replay-digest :author author :op-id op-id :hunks hunks}))

(deftest offline-replay
  (let [;; A lands two sequential patches: h0 -> h1 -> h2
        a1 (replay-patch "alice" "a-1" [{:start 24 :end 28 :replacement "green"}])
        a2 (r/->patch {:base (r/revision-digest (r/apply-patch replay-base a1))
                       :author "alice" :op-id "a-2"
                       :hunks [{:start 45 :end 45 :replacement " —A"}]})
        h1 (r/apply-patch replay-base a1)
        h2 (r/apply-patch h1 a2)
        ;; B, offline against h0, makes three sequential patches
        b1 (replay-patch "bob" "b-1" [{:start 0 :end 5 :replacement "Hi"}])
        b2 (r/->patch {:base (r/revision-digest (r/apply-patch replay-base b1))
                       :author "bob" :op-id "b-2"
                       :hunks [{:start 9 :end 9 :replacement " How are you?"}]})
        b2-base (r/apply-patch replay-base b1)
        b3 (r/->patch {:base (r/revision-digest (r/apply-patch b2-base b2))
                       :author "bob" :op-id "b-3"
                       :hunks [{:start 48 :end 53 :replacement "friend"}]})
        ;; each side composes its sequential chain into one patch vs h0
        ac (r/compose a1 a2)
        bc (r/compose (r/compose b1 b2) b3)
        ;; sanity: composed patches reproduce the sequential application
        _ (is (r/bytes= (r/apply-patch replay-base ac) h2))
        _ (is (r/bytes= (r/apply-patch replay-base bc)
                        (r/apply-patch (r/apply-patch b2-base b2) b3)))
        ;; B comes back online and rebases its three patches onto h2 IN ORDER:
        ;; each B hunk is transformed across A's landed hunks (shifted into
        ;; the coordinate system of B's local doc at that step).
        b1' (r/transform b1 ac)
        h3 (r/apply-patch h2 b1')
        b2' (r/transform b2 (r/transform ac b1))
        h4 (r/apply-patch h3 b2')
        b3' (r/transform b3 (r/transform ac (r/compose b1 b2)))
        h5 (r/apply-patch h4 b3')
        ;; ...and the one-shot combined rebase must agree
        combined (r/rebase replay-base ac bc)]
    (is (some? (:result combined)) (str "expected mergeable rebase, got " combined))
    (is (r/bytes= h5 (:result combined))
        "in-order replay and combined rebase disagree")
    ;; expected final text, written out by hand:
    ;; h0:  "Hello world. The sky is blue. Goodbye world."
    ;; +B1: "Hi world. The sky is blue. Goodbye world."
    ;; +B2: "Hi world. How are you? The sky is blue. Goodbye world."
    ;; +B3: "Hi world. How are you? The sky is blue. Goodbye friend."
    ;; +A1: "Hi world. How are you? The sky is green. Goodbye friend."
    ;; +A2: "Hi world. How are you? The sky is green. Goodbye friend. —A"
    (is (= "Hi world. How are you? The sky is green. Goodbye friend. —A"
           (s h5)))
    ;; B's intent is addressable in the result
    (is (:locatable? (r/locate h5 replay-base bc (:merged-hunks combined))))))

;;; ---------------------------------------------------------------------
;;; 3. Property-style: >= 500 random patch pairs on random text

(def ^:private alphabet
  (vec (concat (map str "abcxyz 0123456789")
               ["α" "β" "中" "文" "字" "😀" "🎉"])))

(defn- rand-text [^java.util.Random rnd n]
  (apply str (repeatedly n #(nth alphabet (.nextInt rnd (count alphabet))))))

(defn- rand-hunks [^java.util.Random rnd n-cps k]
  (loop [acc [], attempts 0]
    (if (or (= k (count acc)) (> attempts 40))
      (vec acc)
      (let [start (.nextInt rnd (inc n-cps))
            end (min n-cps (+ start (.nextInt rnd 13)))
            replacement (rand-text rnd (.nextInt rnd 9))
            h {:start start :end end :replacement replacement}]
        (if (some #(and (< (:start %) end) (< start (:end %))) acc)
          (recur acc (inc attempts)) ; overlapping within same patch: retry
          (recur (conj acc h) (inc attempts)))))))

(defn- rand-patch [^java.util.Random rnd digest author op-id n-cps]
  (r/->patch {:base digest :author author :op-id op-id
              :hunks (rand-hunks rnd n-cps (inc (.nextInt rnd 3)))}))

(deftest property-random-pairs
  (let [rnd (java.util.Random. 20261108)
        trials 500
        tally (atom {:mergeable 0 :conflict 0 :identical-seen 0})]
    (doseq [i (range trials)]
      (let [text (rand-text rnd (+ 20 (.nextInt rnd 100)))
            digest (r/revision-digest text)
            n-cps (.codePointCount ^String text 0 (count text))
            a (rand-patch rnd digest "alice" (str "a-" i) n-cps)
            b (rand-patch rnd digest "bob" (str "b-" i) n-cps)
            cls (r/classify a b)
            ab (r/rebase text a b)
            ba (r/rebase text b a)]
        (if (= :mergeable (:class cls))
          (do
            (swap! tally update :mergeable inc)
            (when (some #(= :identical (:class %)) (:pairs cls))
              (swap! tally update :identical-seen inc))
            (is (some? (:result ab)) (str "mergeable pair produced no result, trial " i))
            (is (r/bytes= (:result ab) (:result ba))
                (str "disjoint rebase not commutative, trial " i))
            (is (:locatable? (r/locate (:result ab) text a (:merged-hunks ab)))
                (str "patch a not locatable after rebase, trial " i))
            (is (:locatable? (r/locate (:result ab) text b (:merged-hunks ab)))
                (str "patch b not locatable after rebase, trial " i)))
          (do
            (swap! tally update :conflict inc)
            (is (nil? (:result ab)) (str "conflict silently resolved, trial " i))
            (is (nil? (:result ba)))
            (is (seq (:conflicts ab)))
            (is (= 2 (count (:alternatives ab))))
            ;; attributable: every conflicting hunk names both authors/op-ids
            (doseq [c (:conflicts ab)]
              (is (= ["alice" "bob"] (:authors c)))
              (is (= [(str "a-" i) (str "b-" i)] (:op-ids c))))
            ;; no silent winner: neither alternative is byte-equal to just
            ;; applying one side alone when the other side conflicts
            (is (every? #(= 2 (count (:retained %))) (:alternatives ab)))))))
    (println "property tally:" @tally)
    (is (pos? (:mergeable @tally)) "no mergeable cases generated")
    (is (pos? (:conflict @tally)) "no conflict cases generated")))

;;; ---------------------------------------------------------------------
;;; 4. Multi-byte characters: codepoint vs byte offsets

(def cjk-base "αβγ 中文字 😀🎉 end")

(deftest multibyte-codepoint-offsets
  (testing "hunks address codepoints, not bytes"
    (let [digest (r/revision-digest cjk-base)
          ;; 中文字 occupies codepoints 4..7
          a (r/->patch {:base digest :author "alice" :op-id "a-cjk"
                        :hunks [{:start 4 :end 7 :replacement "漢字"}]})
          ;; insert 🚀 before "end" (codepoint 11: "αβγ 中文字 😀🎉 " = 11 cps)
          b (r/->patch {:base digest :author "bob" :op-id "b-emoji"
                        :hunks [{:start 11 :end 11 :replacement "🚀 "}]})
          res (r/rebase cjk-base a b)]
      (is (some? (:result res)))
      (is (= "αβγ 漢字 😀🎉 🚀 end" (s (:result res))))
      (is (:locatable? (r/locate (:result res) cjk-base a (:merged-hunks res))))
      (is (:locatable? (r/locate (:result res) cjk-base b (:merged-hunks res))))))
  (testing "same-position conflict on an emoji span"
    (let [digest (r/revision-digest cjk-base)
          ;; 😀🎉 occupies codepoints 8..10
          a (r/->patch {:base digest :author "alice" :op-id "a-e2"
                        :hunks [{:start 8 :end 10 :replacement "🐢"}]})
          b (r/->patch {:base digest :author "bob" :op-id "b-e2"
                        :hunks [{:start 8 :end 10 :replacement "🦊"}]})
          res (r/rebase cjk-base a b)]
      (is (nil? (:result res)))
      (is (= [:same-position] (map :class (:conflicts res))))
      (is (= #{"αβγ 中文字 🐢 end" "αβγ 中文字 🦊 end"}
             (set (map (comp s :bytes) (:alternatives res)))))))
  (testing "canonicalisation normalises CRLF before digest and patch"
    (let [crlf "line one\r\nline two\r\n"
          lf "line one\nline two\n"
          digest (r/revision-digest lf)
          a (r/->patch {:base digest :author "alice" :op-id "a-nl"
                        :hunks [{:start 0 :end 4 :replacement "LINE"}]})]
      (is (= digest (r/revision-digest crlf)))
      (is (= "LINE one\nline two\n" (s (:result (r/rebase crlf a (assoc a :op-id "b-nl" :hunks [])))))))))
