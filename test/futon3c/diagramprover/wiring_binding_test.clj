(ns futon3c.diagramprover.wiring-binding-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.diagramprover.wiring :as wiring]))

(defn- findings [source field opts]
  (let [file (java.io.File/createTempFile "wiring-binding-" ".clj")]
    (try
      (spit file source)
      (wiring/conformance
       (.getParent file)
       {:boxes [(merge {:box/id :reader
                        :site {:file (.getName file) :var "f"}
                        :reads [field]} opts)]}
       {:heuristic? true})
      (finally (.delete file)))))

(def helper-field [:beta {:record :record}])
(def helper-opts {:record-aliases {:record ["r"]}})
(defn- helper-missing? [source]
  (some #(= :declaration-without-occurrence (:finding %))
        (findings source helper-field helper-opts)))

(deftest delegated-helper-reads-the-positional-record
  (doseq [source ["(defn- h [ignored p] (:beta p)) (defn f [r] (h 0 r))"
                  "(defn h [p] (get-in p [:beta])) (defn f [record] (h record))"
                  "(defn h [p tau] (get-in p [:beta tau])) (defn f [r] (h r 2))"
                  "(defn h ([p] (:beta p)) ([x p] (:other p))) (defn f [r] (h r))"]]
    (is (empty? (findings source helper-field helper-opts)))))

(deftest delegated-helper-must-read-the-declared-field
  (is (helper-missing? "(defn h [p] (:other p)) (defn f [r] (h r))")))

(deftest delegated-helper-must-receive-the-record
  (is (helper-missing? "(defn h [p] (:beta p)) (defn f [r unrelated] (h unrelated))")))

(deftest delegated-helper-does-not-follow-a-second-hop
  (is (helper-missing? "(defn h2 [p] (:beta p)) (defn h [p] (h2 p)) (defn f [r] (h r))")))

(deftest delegated-helper-is-same-file-only
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                     "helper-scope" (make-array java.nio.file.attribute.FileAttribute 0)))
        file (java.io.File. dir "reader.clj")
        other (java.io.File. dir "other.clj")]
    (try
      (spit file "(ns reader) (defn h [p] (:beta p)) (defn f [r] (other/h r))")
      (spit other "(ns other) (defn h [p] (:beta p))")
      (is (some #(= :declaration-without-occurrence (:finding %))
                (wiring/conformance (str dir)
                                    {:boxes [(merge helper-opts
                                                    {:box/id :reader :site {:file "reader.clj" :var "f"}
                                                     :reads [helper-field]})]}
                                    {:heuristic? true})))
      (finally (.delete file) (.delete other) (.delete dir)))))

(deftest delegated-helper-is-not-a-shadowing-local
  (doseq [source ["(defn h [p] (:beta p)) (defn f [r] (let [h (fn [x] x)] (h r)))"
                  "(defn h [p] (:beta p)) (defn f [r] (letfn [(h [x] x)] (h r)))"
                  "(defn h [p] (:beta p)) (defn f [h r] (h r))"
                  "(defn h [p] (:beta p)) (defn f [r] ((fn h [x] (h r)) r))"]]
    (is (helper-missing? source)))
  (is (empty? (findings "(defn h [p] (:beta p)) (defn f [r] (let [g (fn [x] x)] (g r) (h r)))"
                        helper-field helper-opts))))

(deftest delegated-helper-rejects-nonpositional-parameters-and-writes
  (doseq [source ["(defn h [{:keys [beta]}] beta) (defn f [r] (h r))"
                  "(defn h [& p] (:beta p)) (defn f [r] (h r))"
                  "(defn h [p] (:beta other)) (defn f [r] (h r))"
                  "(defn h [p] {:beta 1}) (defn f [r] (h r))"]]
    (is (helper-missing? source)))
  (is (seq (findings "(defn h [p] (:beta p)) (defn f [r] (h r))"
                     helper-field (assoc helper-opts :reads [] :writes [helper-field])))))

(deftest bindings-reach-the-public-occurrence-and-role-checks
  (doseq [source ["(defn f [{:keys [beta]}] beta)"
                  "(defn f [{b :beta}] b)"
                  "(defn f [m] (let [{:keys [beta]} m] beta))"
                  "(defn f [m] (if-let [{b :beta} m] b nil))"
                  "(defn f [m] (loop [{:keys [beta]} m] beta))"
                  "(defn f ([] nil) ([{:keys [beta]}] beta))"
                  "(defn f [xs] (for [{:keys [beta]} xs :when beta] beta))"
                  "(defn f [xs] (for [m xs :let [{b :beta} m]] b))"
                  "(defn f [m] ((fn [{:keys [beta]}] beta) m))"
                  "(defn f [m] ((fn named ([{:keys [beta]}] beta)) m))"
                  "(defn f [[{:keys [beta]}]] beta)"
                  "(defn f [{{:keys [beta]} :inner}] beta)"]]
    (testing source
      (is (= {:reads 1 :writes 0 :unclassified 0}
             (wiring/field-usage source :beta)))
      (is (empty? (findings source :beta {}))))))

(deftest namespaced-bindings-keep-their-namespace
  (doseq [source ["(defn f [{:keys [selection/beta]}] beta)"
                  "(defn f [{:selection/keys [beta]}] beta)"
                  "(defn f [{b :selection/beta}] b)"]]
    (is (= 1 (:reads (wiring/field-usage source :selection/beta))))
    (is (zero? (:reads (wiring/field-usage source :beta))))
    (is (empty? (findings source :selection/beta {})))))

(deftest data-and-quotation-never-supply-binding-evidence
  (doseq [source ["(def beta 1) (defn f [] {:keys [beta]})"
                  "(defn f [] {:keys '[beta]})"
                  "(defn f [] '{:keys [beta]})"
                  "(defn f [] {:selection/keys '[beta]})"
                  "(defn f [] (quote (fn [{:keys [beta]}] beta)))"
                  "(defn f [] '(fn [{b :beta}] b))"
                  "(defn f [] `(fn [{b :beta}] b))"
                  "(defn f [] #_(fn [{:keys [beta]}] beta) nil)"
                  "(defn f [] \"(fn [{:keys [beta]}] beta)\")"
                  "(defn f [] nil) ; {:keys [beta]}"
                  "(defn f [] {:data (quote (let [{:keys [beta]} m] beta))})"]]
    (testing source
      (is (zero? (:reads (wiring/field-usage source :beta))))
      (is (seq (findings source :beta {}))))))

(deftest quoted-definitions-do-not-become-sites
  (is (nil? (wiring/var-form "'(defn f [{:keys [beta]}] beta)" "f")))
  (is (nil? (wiring/var-form "`(defn f [{:keys [beta]}] beta)" "f")))
  (is (nil? (wiring/var-form "(quote (defn f [{:keys [beta]}] beta))" "f"))))

(deftest scoped-binding-evidence-needs-a-record-name
  (let [field [:beta {:record :selection-opts}]
        opts {:record-aliases {:selection-opts ["opts"]}}]
    (doseq [source ["(defn f [{:keys [beta] :as opts}] beta)"
                    "(defn f [opts] (let [{b :beta} opts] b))"
                    "(defn f [opts] (let [{:keys [beta]} opts] beta))"]]
      (is (empty? (findings source field opts))))
    (doseq [source ["(defn f [{:keys [beta] :as unrelated}] beta)"
                    "(defn f [{:keys [beta]}] beta)"
                    "(defn f [opts other] (let [{:keys [beta]} other] beta))"
                    "(defn f [opts] (let [{:keys [beta]} 'opts] beta))"
                    "(defn f [{{:keys [beta]} :inner :as opts}] beta)"
                    "(defn f [] (quote (fn [{:keys [beta] :as opts}] beta))"]]
      (testing source
        (is (some #(= :declaration-without-occurrence (:finding %))
                  (findings source field opts)))))
    (is (seq (findings "(defn f [{:keys [beta] :as opts}] beta)"
                       [:beta {:record :another-record}] opts)))))

(deftest defaults-and-returned-values-are-not-selectors
  (is (= 1 (:reads (wiring/field-usage
                    "(defn f [{:keys [beta] :or {beta :beta}}] beta)" :beta))))
  (is (zero? (:reads (wiring/field-usage "(defn f [] {:beta 1})" :beta))))
  (is (seq (findings "(defn f [] {:beta 1})" :beta {})))
  (is (zero? (:reads (wiring/field-usage
                      "(defn f [m] (unknown-binding [{:keys [beta]} m] beta))" :beta)))))
