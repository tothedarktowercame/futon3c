(ns futon3c.diagramprover.wm-positional-passes-test
  "A :passes entry on a box is evidence of occurrence for the declaring box and the
  callee box, and only when (a) the call exists in the caller's :var scope, (b) the
  nth argument comes from what :from says (through let, loop, {:keys} destructuring,
  return positions), and (c) the callee's nth parameter is a plain symbol used in
  its body. Each has a failing counterpart here, and each failure leaves the
  declared entries :declaration-without-occurrence with :passes-failed naming why.
  The shapes are reduced from the seven positional hops of WM-PROVER-POSITIONAL-D."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wiring :as wiring]))

(defn- with-root [files f]
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "wm-passes" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [[n t] files] (spit (io/file root n) t))
    (f (str root))))

(defn- pass [from call arg & [value]]
  {:value (or value :v) :from from :to {:call call :arg arg :callee-box :callee}})

(defn- boxes
  "The caller (judge in caller.clj, writing :v, carrying PASS) and the callee (h in
  callee.clj, reading :v)."
  [pass & [caller-extra callee-var]]
  [(merge {:box/id :caller :box/kind :component :reads [] :writes [:v]
           :site {:file "caller.clj" :var "judge"} :passes [pass]}
          caller-extra)
   {:box/id :callee :box/kind :component :reads [:v] :writes []
    :site {:file "callee.clj" :var (or callee-var "h")}}])

(defn- findings [files bs]
  (with-root files
    (fn [root] (wiring/conformance root {:spec/id :t :boxes bs} {:heuristic? true}))))

(defn- accepted? [caller callee p & [caller-extra callee-var]]
  (= [] (findings {"caller.clj" caller "callee.clj" callee} (boxes p caller-extra callee-var))))

(defn- refused
  "The :passes-failed reason recorded on the callee's finding, or nil when accepted."
  [caller callee p & [caller-extra callee-var]]
  (let [f (findings {"caller.clj" caller "callee.clj" callee} (boxes p caller-extra callee-var))
        c (first (filter #(and (= :callee (:box/id %)) (= :declaration-without-occurrence (:finding %))) f))]
    (when c (:passes-failed c))))

(def callee-2 "(defn h [a carried] (if (seq carried) (get carried :k) a))")

(deftest keyed-read-as-the-argument-expression
  (testing "E2-c: the argument IS (:mu-post prev), prev an alias of :trace"
    (is (accepted? "(defn judge [prev fresh] (h fresh (:mu-post prev)))" callee-2
                   (pass {:keyed-read [:mu-post :trace]} "h" 2)
                   {:record-aliases {:trace ["prev"]}})))
  (testing "(b) fails: another key"
    (is (= :not-the-keyed-read
           (refused "(defn judge [prev fresh] (h fresh (:other prev)))" callee-2
                    (pass {:keyed-read [:mu-post :trace]} "h" 2) {:record-aliases {:trace ["prev"]}}))))
  (testing "(b) fails: the right key on a receiver that is not the record"
    (is (= :not-the-keyed-read
           (refused "(defn judge [prev fresh] (h fresh (:mu-post fresh)))" callee-2
                    (pass {:keyed-read [:mu-post :trace]} "h" 2) {:record-aliases {:trace ["prev"]}}))))
  (testing "(get rec :k) and (get-in rec [:k]) are keyed reads too"
    (is (accepted? "(defn judge [trace fresh] (h fresh (get trace :mu-post)))" callee-2
                   (pass {:keyed-read [:mu-post :trace]} "h" 2)))
    (is (accepted? "(defn judge [trace fresh] (h fresh (get-in trace [:mu-post])))" callee-2
                   (pass {:keyed-read [:mu-post :trace]} "h" 2)))))

(deftest a-local-bound-to-a-keyed-read
  (testing "let-bound"
    (is (accepted? "(defn judge [fold ev] (let [b (:belief fold)] (h b ev)))" "(defn h [x e] (count x))"
                   (pass {:keyed-read [:belief :fold]} "h" 1))))
  (testing "bound by {:keys [belief]} destructuring of the record"
    (is (accepted? "(defn judge [fold ev] (let [{:keys [belief]} fold] (h belief ev)))" "(defn h [x e] (count x))"
                   (pass {:keyed-read [:belief :fold]} "h" 1))))
  (testing "a destructured key of a different receiver is refused"
    (is (= :destructured-key-not-declared
           (refused "(defn judge [fold ev] (let [{:keys [belief]} ev] (h belief ev)))" "(defn h [x e] (count x))"
                    (pass {:keyed-read [:belief :fold]} "h" 1)))))
  (testing "a later shadowing binding hides the earlier one"
    (is (some? (refused "(defn judge [fold ev] (let [b (:belief fold) b (count ev)] (h b ev)))" "(defn h [x e] (count x))"
                        (pass {:keyed-read [:belief :fold]} "h" 1))))))

(deftest a-local-bound-to-a-returns-of-call
  (is (accepted? "(defn judge [scan] (let [o (obs/observe scan)] (h o)))" "(defn h [o] (get o :c))"
                 (pass {:returns-of "obs/observe"} "h" 1)))
  (testing "(b) fails: bound from a different call"
    (is (= :not-a-call-to-the-source
           (refused "(defn judge [scan] (let [o (other/observe scan)] (h o)))" "(defn h [o] (get o :c))"
                    (pass {:returns-of "obs/observe"} "h" 1)))))
  (testing "(b) fails: the argument is a fresh literal"
    (is (= :not-a-call-to-the-source
           (refused "(defn judge [scan] (h {:a 1}))" "(defn h [o] (get o :c))"
                    (pass {:returns-of "obs/observe"} "h" 1)))))
  (testing "through the return positions of an if"
    (is (accepted? "(defn judge [scan c] (let [o (if c (obs/observe scan) (obs/observe c))] (h o)))" "(defn h [o] (get o :c))"
                   (pass {:returns-of "obs/observe"} "h" 1)))))

(def e1-caller
  (str "(defn judge [fold events]\n"
       "  (loop [step 0 belief (:belief fold)]\n"
       "    (let [belief' (if (seq events) (apply-events belief events) belief)]\n"
       "      (if (> step 2) belief' (recur (inc step) belief')))))"))
(def e1-callee "(defn apply-events [belief-state events] (update-batch belief-state events))")

(deftest a-loop-local-with-a-self-recurrent-source
  (let [bs (fn [p] (assoc-in (boxes p) [1 :site :var] "apply-events"))
        p (pass {:keyed-read [:belief :fold]} "apply-events" 1)
        run (fn [caller] (findings {"caller.clj" caller "callee.clj" e1-callee} (bs p)))]
    (testing "E1-c: the init is the keyed read, the recur is the passing call's own return"
      (is (= [] (run e1-caller)))
      (let [r (with-root {"caller.clj" e1-caller "callee.clj" e1-callee}
                (fn [root]
                  (#'wiring/check-pass root {:callee (second (bs p))} {:fold #{}} (first (bs p)) p)))]
        (is (:ok? r))
        (is (true? (:self-recurrent? r)) "typed :self-recurrent, not passed silently")))
    (testing "an unrelated call as the recur source is refused"
      (is (some? (first (filter #(= :declaration-without-occurrence (:finding %))
                                (run (str "(defn judge [fold events]\n"
                                          "  (loop [step 0 belief (:belief fold)]\n"
                                          "    (let [belief' (if (seq events) (apply-events belief events) (something-else belief))]\n"
                                          "      (if (> step 2) belief' (recur (inc step) belief')))))")))))))
    (testing "if every source is the call's own return there is no real provenance"
      (is (some? (first (filter #(= :declaration-without-occurrence (:finding %))
                                (run (str "(defn judge [fold events]\n"
                                          "  (loop [step 0 belief (apply-events fold events)]\n"
                                          "    (if (> step 2) belief (recur (inc step) (apply-events belief events)))))")))))))))

(deftest an-element-of-a-bound-local
  (let [callee "(defn cpe [o ch pred] (get pred ch))"
        p (pass {:element-of {:returns-of "predict"}} "cpe" 3)
        bs (fn [] (assoc-in (boxes p) [1 :site :var] "cpe"))
        run (fn [caller] (findings {"caller.clj" caller "callee.clj" callee} (bs)))]
    (is (= [] (run "(defn judge [b o ch] (let [ps (predict b)] (cpe o ch (get ps ch))))")))
    (testing "(get other ch) where other is not the returns-bind is refused"
      (is (seq (run "(defn judge [b o ch other] (let [ps (predict b)] (cpe o ch (get other ch))))"))))
    (testing "an argument that is not an element read is refused"
      (is (seq (run "(defn judge [b o ch] (let [ps (predict b)] (cpe o ch ps)))"))))))

(deftest a-map-literal-argument-key
  (let [callee "(defn sparse [{:keys [rates q0] :as m}] (count rates))"
        p (pass {:literal-arg-key :rates} "sparse" 1)
        run (fn [caller] (findings {"caller.clj" caller "callee.clj" callee}
                                   (assoc-in (boxes p) [1 :site :var] "sparse")))]
    (testing "under cond->, and let-bound"
      (is (seq (run "(defn judge [rates q x] (sparse (cond-> {:rates rates :q0 q} x (assoc :z 1))))"))
          "the callee's param is destructured, so it is not a plain-symbol :passes target (b passes, c refuses)")
      (let [callee2 "(defn sparse [m] (count (:rates m)))"]
        (is (= [] (findings {"caller.clj" "(defn judge [rates q x] (sparse (cond-> {:rates rates :q0 q} x (assoc :z 1))))"
                             "callee.clj" callee2}
                            (assoc-in (boxes p) [1 :site :var] "sparse"))))
        (is (= [] (findings {"caller.clj" "(defn judge [rates q] (let [m {:rates rates :q0 q}] (sparse m)))"
                             "callee.clj" callee2}
                            (assoc-in (boxes p) [1 :site :var] "sparse"))))))
    (testing "a literal without the key is refused"
      (let [f (findings {"caller.clj" "(defn judge [rates q] (sparse {:q0 q}))" "callee.clj" "(defn sparse [m] (count m))"}
                        (assoc-in (boxes p) [1 :site :var] "sparse"))]
        (is (some #(= :literal-lacks-the-key (:passes-failed %)) f))))))

(deftest partial-application
  (let [callee "(defn tl [rates state obs] (count rates))"
        bs (fn [p] (assoc-in (boxes p) [1 :site :var] "tl"))
        run (fn [p] (findings {"caller.clj" "(defn judge [cfg state xs] (let [r (:rates cfg)] (map (partial tl r state) xs)))"
                               "callee.clj" callee} (bs p)))]
    (is (= [] (run (pass {:keyed-read [:rates :cfg]} "tl" 1))))
    (testing "an argument the partial does not carry yet is refused"
      (is (some #(= :call-has-too-few-arguments (:passes-failed %)) (run (pass {:keyed-read [:rates :cfg]} "tl" 3)))))))

(deftest the-callee-end
  (let [p (pass {:returns-of "obs/observe"} "h" 1)
        caller "(defn judge [scan] (let [o (obs/observe scan)] (h o)))"]
    (testing "(c) fails: the parameter is unused"
      (is (= :param-unused (refused caller "(defn h [o] 1)" p))))
    (testing "(c) fails: the call has more arguments than any arity"
      (is (= :passes-arity-mismatch
             (refused "(defn judge [scan] (let [o (obs/observe scan)] (h o 1 2)))" "(defn h [o x] (count o))" p))))
    (testing "(c) fails: the parameter is destructured (declare the destructured keys instead)"
      (is (= :param-not-a-plain-symbol (refused caller "(defn h [{:keys [a]}] a)" p))))
    (testing "the arity matching the call's argument count is the one checked"
      (is (accepted? "(defn judge [scan] (let [o (obs/observe scan)] (h o 1)))"
                     "(defn h ([o] 1) ([o x] (count o)))" p)))
    (testing "(c) fails: the callee box's site is not the called fn"
      (is (= :callee-var-mismatch
             (refused caller "(defn other [o] (count o))\n(defn h [o] (count o))" p nil "other"))))))

(deftest the-call-must-exist
  (let [p (pass {:returns-of "obs/observe"} "h" 1)]
    (testing "(a) fails: no call to h in the :var scope"
      (is (= :call-not-found (refused "(defn judge [scan] (let [o (obs/observe scan)] (count o)))" "(defn h [o] (count o))" p))))
    (testing "(a) fails: a computed head is not a call to h"
      (is (= :call-not-found (refused "(defn judge [scan a] ((or a (h2)) (obs/observe scan)))" "(defn h [o] (count o))" p))))
    (testing "(a) fails: a call to h outside the :var scope does not count"
      (is (= :call-not-found
             (refused "(defn other [scan] (let [o (obs/observe scan)] (h o)))\n(defn judge [scan] 1)" "(defn h [o] (count o))" p))))))

(deftest occurrence-evidence
  (let [callee "(defn h [o] (get o :c))"
        good "(defn judge [scan] (let [o (obs/observe scan)] (h o)))"
        p (pass {:returns-of "obs/observe"} "h" 1)
        no-pass (mapv #(dissoc % :passes) (boxes p))]
    (is (= [] (findings {"caller.clj" good "callee.clj" callee} (boxes p))))
    (testing "the same declarations without :passes are not found"
      (let [f (findings {"caller.clj" good "callee.clj" callee} no-pass)]
        (is (= #{:caller :callee} (set (map :box/id (filter #(= :declaration-without-occurrence (:finding %)) f)))))))
    (testing "a stale :passes (the call edited away) turns them back"
      (let [f (findings {"caller.clj" "(defn judge [scan] (obs/observe scan))" "callee.clj" callee} (boxes p))]
        (is (= #{:caller :callee} (set (map :box/id (filter #(= :declaration-without-occurrence (:finding %)) f)))))
        (is (every? #(= :call-not-found (:passes-failed %)) (filter #(= :declaration-without-occurrence (:finding %)) f)))))
    (testing "a malformed :passes gives no evidence"
      (let [f (findings {"caller.clj" good "callee.clj" callee} [(assoc-in (first (boxes p)) [:passes 0 :to :arg] 0) (second (boxes p))])]
        (is (some #(= :malformed-passes (:passes-failed %)) f))))
    (testing "usage counts the evidence in the declared role"
      (with-root {"caller.clj" good "callee.clj" callee}
        (fn [root]
          (let [u (wiring/usage root {:spec/id :t :boxes (boxes p)})
                by (fn [id role] (:usage (first (filter #(and (= id (:box/id %)) (= role (:role %))) u))))]
            (is (= 1 (:writes (by :caller :writes))))
            (is (= 1 (:reads (by :callee :reads))))))))))

(deftest return-position-enters-loop
  (let [w (fn [src]
            (with-root {"a.clj" src}
              (fn [root]
                (:usage (first (filter #(= :writes (:role %))
                                       (wiring/usage root {:spec/id :t
                                                           :boxes [{:box/id :w :box/kind :component :reads []
                                                                    :writes [[:target {:record :flight}]]
                                                                    :returns-record :flight
                                                                    :site {:file "a.clj"}}]})))))))]
    (is (= 1 (:writes (w "(defn f [x] (loop [i 0] (if (> i 3) {:target i} (recur (inc i)))))"))))
    (is (= 1 (:writes (w "(defn f [x] (loop [i 0] (let [j (inc i)] (if (> j 3) {:target j} (recur j)))))"))))
    (testing "a literal passed to recur is not a return"
      (is (= 0 (:writes (w "(defn f [x] (loop [m {}] (if x m (recur {:target 1}))))")))))))

(deftest a-box-without-passes-is-read-as-before
  (with-root {"a.clj" "(defn f [m] (assoc m :k 1))"}
    (fn [root]
      (is (= [] (wiring/conformance root {:spec/id :t :boxes [{:box/id :b :box/kind :component :reads []
                                                              :writes [:k] :site {:file "a.clj"}}]}
                                    {:heuristic? true}))))))
