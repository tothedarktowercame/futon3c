(ns futon3c.arxana.holes-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.holes :as h])
  (:import (java.io File)
           (java.util Random)))

(def ^:private relations [:r :s])
(def ^:private values [:a :b :c :you])
(def ^:private names [:x :y :z])
(def ^:private property-cases 3000)
(def ^:private differential-cases 2000)

(defn- pick [^Random rng xs]
  (nth xs (.nextInt rng (count xs))))

(defn- random-arg [^Random rng]
  (if (< (.nextDouble rng) 0.6)
    (h/hole (pick rng names))
    (pick rng values)))

(defn- random-form [^Random rng depth]
  (case (if (zero? depth) 0 (.nextInt rng 3))
    0 [:atom (pick rng relations) (random-arg rng) (random-arg rng)]
    1 [:conj (random-form rng (dec depth)) (random-form rng (dec depth))]
    2 [:neg (random-form rng (dec depth))]))

(defn- random-slot [^Random rng]
  (case (.nextInt rng 4)
    0 [:own (pick rng values)]
    1 [:unknown]
    2 [:offered (pick rng [:speaker :listener :we]) (pick rng [nil 3])]
    3 [:free]))

(defn- random-act [^Random rng]
  {:body (random-form rng 2)
   :slots (into {} (keep (fn [x]
                           (let [slot (random-slot rng)]
                             (when-not (= [:free] slot) [x slot]))))
                names)})

(defn- random-env
  ([rng] (random-env rng 0.6))
  ([^Random rng probability]
   (into {} (keep #(when (< (.nextDouble rng) probability)
                     [% (pick rng values)]))
         names)))

(defn- random-model [^Random rng]
  {:domain values
   :facts (set (for [r relations, a values, b values
                     :when (< (.nextDouble rng) 0.4)]
                 [r a b]))})

(defn- property! [seed property]
  (let [rng (Random. seed)]
    (dotimes [i property-cases]
      (is (property rng) (str "seed=" seed " case=" i)))))

(deftest hygienic-no-capture-property
  (property! 1729
             (fn [rng]
               (let [act (random-act rng)]
                 (or (not (h/hygienic? act))
                     (empty? (h/capture act (random-env rng 1.0))))))))

(deftest unhygienic-can-capture-property
  (property! 1730
             (fn [rng]
               (let [act (random-act rng)]
                 (or (h/hygienic? act)
                     (seq (h/capture act (zipmap names (repeat :a)))))))))

(deftest expand-hygienic-property
  (property! 1731 #(h/hygienic? (h/expand (random-act %) (random-env %)))))

(deftest expand-sound-property
  (property! 1732
             (fn [rng]
               (let [act (random-act rng)
                     model (random-model rng)]
                 (or (not (h/holds? model (h/expand act (random-env rng))))
                     (h/holds? model act))))))

(deftest expand-preserves-truth-property
  (property! 1733
             (fn [rng]
               (let [act (random-act rng)
                     model (random-model rng)
                     rho (h/witness model act)]
                 (or (nil? rho)
                     (let [sigma (into {} (filter (fn [_] (< (.nextDouble rng) 0.6))) rho)]
                       (h/holds? model (h/expand act sigma))))))))

(deftest honest-equally-true-and-hygienic-property
  (property! 1734
             (fn [rng]
               (let [act (random-act rng)
                     model (random-model rng)
                     honest (h/honest act)]
                 (and (= (h/holds? model honest) (h/holds? model act))
                      (empty? (h/capture honest (random-env rng 1.0))))))))

(deftest sealed-hygienic-property
  (property! 1735
             (fn [rng]
               (let [act (random-act rng)]
                 (or (not (h/sealed? act)) (h/hygienic? act))))))

(deftest sealed-not-invitation-property
  (property! 1736
             (fn [rng]
               (let [act (random-act rng)]
                 (or (not (h/sealed? act))
                     (not (h/invitation? :you act)))))))

(deftest shrapnel-not-invitation-property
  (property! 1737
             (fn [rng]
               (let [act (random-act rng)]
                 (or (not (h/shrapnel? :you act))
                     (not (h/invitation? :you act)))))))

(deftest invite-not-demand-property
  (property! 1738
             (fn [rng]
               (let [act {:body [:atom :r (h/hole :x) :a]
                          :slots {:x (random-slot rng)}}]
                 (or (not (h/invitation? :you act))
                     (not (h/demand? act)))))))

(def ^:private example-model
  ;; The worked example of the Mozilla copy of Holes.lean: one reviewer
  ;; concern from the call's criteria, true in one world.
  {:domain [:applicant :futon :we :pipeline :intent-tracking]
   :facts #{[:calls-frontier :applicant :pipeline]
            [:calls-frontier :futon :pipeline]
            [:small-would-do :intent-tracking :pipeline]}})

(def ^:private sealed-act
  {:body [:conj [:atom :calls-frontier (h/hole :who) :pipeline]
          [:atom :small-would-do (h/hole :task) :pipeline]]
   :slots {:who [:own :futon] :task [:own :intent-tracking]}})

(def ^:private shrapnel-act
  {:body [:atom :calls-frontier (h/hole :who) :pipeline]
   :slots {:who [:own :applicant]}})

(def ^:private invitation-act
  {:body [:conj [:atom :calls-frontier (h/hole :who) :pipeline]
          [:atom :small-would-do (h/hole :task) :pipeline]]
   :slots {:who [:own :futon] :task [:offered :we nil]}})

(deftest worked-example
  (is (every? #(h/holds? example-model %)
              [sealed-act shrapnel-act invitation-act]))
  (is (h/sealed? sealed-act))
  (is (h/shrapnel? :applicant shrapnel-act))
  (is (h/invitation? :applicant invitation-act)))

(deftest construction-returns-hygiene-warning-as-data
  (let [{:keys [act warning]}
        (h/act [:atom :r (h/hole :x) :a])]
    (is (= {:kind :unhygienic :free-holes [:x]} warning))
    (is (= [:x] (h/what-did-you-mean act))))
  (is (nil? (:warning (h/act [:atom :r (h/hole :x) :a]
                              {:x [:unknown]})))))

(defn- lisp-name [x]
  (str/upper-case (name x)))

(defn- arg->lisp [arg]
  (if (h/hole? arg)
    (str "?" (lisp-name (:hole arg)))
    (str ":" (lisp-name arg))))

(defn- form->lisp [form]
  (case (first form)
    :atom (format "(%s %s %s)" (lisp-name (second form))
                  (arg->lisp (nth form 2)) (arg->lisp (nth form 3)))
    :conj (format "(AND %s %s)" (form->lisp (nth form 1))
                  (form->lisp (nth form 2)))
    :neg (format "(NOT %s)" (form->lisp (nth form 1)))))

(defn- slot->lisp [slot]
  (case (first slot)
    :own (format "(:OWN :%s)" (lisp-name (second slot)))
    :unknown "(:UNKNOWN)"
    :offered (format "(:OFFERED :%s %s)" (lisp-name (second slot))
                     (if-let [d (nth slot 2)] d "NIL"))
    :free "(:FREE)"))

(defn- act->lisp [act]
  (format "(%%make-act '%s '(%s))"
          (form->lisp (:body act))
          (str/join " " (for [[x slot] (:slots act)]
                          (format "(?%s . %s)" (lisp-name x)
                                  (slot->lisp slot))))))

(defn- model->lisp [model]
  (format "(make-model '(%s) '(%s))"
          (str/join " " (map #(str ":" (lisp-name %)) (:domain model)))
          (str/join " " (map (fn [[r a b]]
                                (format "(%s :%s :%s)" (lisp-name r)
                                        (lisp-name a) (lisp-name b)))
                              (:facts model)))))

(defn- slot-token [slot]
  (case (first slot)
    :own (str "own:" (lisp-name (second slot)))
    :unknown "unknown"
    :offered (str "offered:" (lisp-name (second slot)) ":"
                  (or (nth slot 2) "NIL"))
    :free "free"))

(defn- expected-line [act model]
  (let [body-names (sort (distinct (h/holes (:body act))))]
    (str/join "|"
              [(str/join "," (map lisp-name (h/free-holes act)))
             (if (h/hygienic? act) "1" "0")
             (if (h/sealed? act) "1" "0")
             (if (h/shrapnel? :you act) "1" "0")
             (if (h/invitation? :you act) "1" "0")
             (if (h/demand? act) "1" "0")
             (str/join "," (map lisp-name (h/classify act)))
               (str/join "," (map #(slot-token (h/slot-of (h/honest act) %))
                                   body-names))
               (if (h/holds? model act) "1" "0")])))

(def ^:private lisp-emitter
  "
(defun bool-token (x) (if x 1 0))
(defun names-token (xs)
  (format nil \"~{~A~^,~}\" (mapcar #'symbol-name xs)))
(defun holes-token (xs)
  (format nil \"~{~A~^,~}\"
          (mapcar (lambda (x) (subseq (symbol-name x) 1)) xs)))
(defun slot-token (slot)
  (case (slot-kind slot)
    (:own (format nil \"own:~A\" (symbol-name (second slot))))
    (:unknown \"unknown\")
    (:offered (format nil \"offered:~A:~A\" (symbol-name (second slot))
                      (or (third slot) \"NIL\")))
    (:free \"free\")))
(defun emit-case (act model)
  (format t \"~A|~D|~D|~D|~D|~D|~A|~A|~D~%\"
          (holes-token (free-holes act))
          (bool-token (hygienic-p act)) (bool-token (sealed-p act))
          (bool-token (shrapnel-p act :you))
          (bool-token (invitation-p act :you)) (bool-token (demand-p act))
          (names-token (classify act))
          (format nil \"~{~A~^,~}\"
                  (mapcar (lambda (x) (slot-token (slot-of (honest act) x)))
                          (sort (remove-duplicates (holes (act-body act)))
                                #'string< :key #'symbol-name)))
          (bool-token (act-holds act model))))
")

(defn- reference-without-demo [reference]
  (str/replace (slurp reference)
               #"(?s)\n\(demo\)\s*\n\(run-tests\)\s*$" "\n"))

(def ^:private golden-file
  "Joe's Common Lisp reference's answers for the seeded differential cases,
  committed so the comparison reruns anywhere.  The reference itself (Joe's
  theory text) is not in this public repo; only its outputs are."
  "test/futon3c/arxana/holes_lisp_golden.txt")

(defn- differential-cases-data []
  (let [rng (Random. 424242)]
    (vec (repeatedly differential-cases
                     #(let [act (random-act rng)] [act (random-model rng)])))))

(defn- cases-lisp [cases]
  (apply str (map (fn [[act model]]
                    (format "(emit-case %s %s)\n" (act->lisp act) (model->lisp model)))
                  cases)))

(defn- sha256 [^String s]
  (let [d (.digest (java.security.MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8"))]
    (apply str (map #(format "%02x" %) d))))

(defn- run-lisp-reference
  "Run SBCL on REFERENCE (a holes.lisp) over CASES; return [exit output-lines]."
  [^File sbcl reference cases]
  (let [script (File/createTempFile "holes-differential-" ".lisp")]
    (try
      (spit script (str (reference-without-demo reference) lisp-emitter (cases-lisp cases)))
      (let [process (-> (ProcessBuilder. [(.getPath sbcl) "--script" (.getPath script)])
                        (.redirectErrorStream true) .start)
            output (slurp (.getInputStream process))]
        [(.waitFor process) (str/split-lines output)])
      (finally (.delete script)))))

(defn regenerate-golden!
  "Maintainer step: rerun Joe's Lisp reference at REFERENCE-PATH over the
  seeded cases and rewrite the golden file.  Line 1 records the sha256 of the
  cases' Lisp text, so a drift in the generator is caught, not compared."
  [reference-path]
  (let [sbcl (some-> (System/getenv "PATH") (str/split #":")
                     (->> (map #(io/file % "sbcl")) (filter #(.canExecute ^File %)) first))
        cases (differential-cases-data)
        [exit lines] (run-lisp-reference sbcl (io/file reference-path) cases)]
    (when-not (and (zero? exit) (= differential-cases (count lines)))
      (throw (ex-info "reference run failed" {:exit exit :lines (take 5 lines)})))
    (spit golden-file (str "cases-sha256 " (sha256 (cases-lisp cases)) "\n"
                           (str/join "\n" lines) "\n"))))

(defn- first-disagreement [expected actual]
  (when-let [i (first (keep-indexed #(when (not= %2 (nth actual %1 nil)) %1) expected))]
    (format "first disagreement case %d\nexpected %s\nactual   %s" i (nth expected i) (nth actual i nil))))

(deftest differential-against-common-lisp
  ;; Against the committed answers of Joe's Lisp reference: runs anywhere.
  (let [cases (differential-cases-data)
        [header & actual] (str/split-lines (slurp golden-file))
        expected (mapv (fn [[act model]] (expected-line act model)) cases)]
    (is (= (str "cases-sha256 " (sha256 (cases-lisp cases))) header)
        "the seeded generator changed; regenerate the golden file from the reference")
    (is (= differential-cases (count actual)))
    (is (= expected (vec actual)) (first-disagreement expected (vec actual)))
    (println "Differential (golden):" (count (remove true? (map = expected actual)))
             "disagreements over" differential-cases "acts")))

(deftest differential-against-live-lisp
  ;; Optional: set HOLES_LISP_REFERENCE to a holes.lisp to compare against a
  ;; live SBCL run as well.  Without it this test says it skipped.
  (let [ref (System/getenv "HOLES_LISP_REFERENCE")
        sbcl (some-> (System/getenv "PATH") (str/split #":")
                     (->> (map #(io/file % "sbcl")) (filter #(.canExecute ^File %)) first))]
    (if-not (and ref sbcl (.isFile (io/file ref)))
      (println "SKIP live differential: set HOLES_LISP_REFERENCE and install sbcl")
      (let [cases (differential-cases-data)
            [exit actual] (run-lisp-reference sbcl (io/file ref) cases)
            expected (mapv (fn [[act model]] (expected-line act model)) cases)]
        (is (zero? exit))
        (is (= expected actual) (first-disagreement expected actual))
        (println "Differential (live):" (count (remove true? (map = expected actual)))
                 "disagreements over" differential-cases "acts")))))
