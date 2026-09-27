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
