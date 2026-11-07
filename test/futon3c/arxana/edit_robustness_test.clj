(ns futon3c.arxana.edit-robustness-test
  "Edit Robustness contract proof over a sequence of immutable throw-away
  artifact revisions. One logical scope and one strategy revision are reused
  throughout; only the artifact revision changes."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.resolve.evaluator :as evaluator]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store :as store])
  (:import [java.nio.charset StandardCharsets]))

(def scope-id "scope:throwaway/compactness-argument")
(def artifact-id "repo:throwaway-paper.tex")
(def anchor-text "the compactness argument")
(def base-text
  (str "Alpha intro section.\n"
       "Target: " anchor-text ".\n"
       "Omega outro."))

(defn- utf8 ^bytes [^String s]
  (.getBytes s StandardCharsets/UTF_8))

(defn- codepoint-offset [^String text ^String target]
  (let [utf16-offset (.indexOf text target)]
    (when-not (neg? utf16-offset)
      (.codePointCount text 0 utf16-offset))))

(defn- strategy-value []
  {:arxana/semantics :arxana.resolution/v1
   :arxana/strategy-id "edit-robustness/anchor-v1"
   :arxana/scope {:scope/id scope-id
                  :artifact/id artifact-id}
   :arxana/cascade-mode :first-unique
   :arxana/finders
   [{:arxana/type :arxana.finder/anchor
     :arxana/on [:ambiguous :missing]
     :arxana/config
     {:anchor/content-hash (digest/sha256-hex (utf8 anchor-text))
      :anchor/content-length (.codePointCount anchor-text 0 (.length anchor-text))
      :anchor/context-before "Target: "
      :anchor/context-after ".\nOmega outro."
      :anchor/hint {:offset (codepoint-offset base-text anchor-text) :line 2}
      :anchor/fuzzy {:length-tolerance 0 :min-context-score 1.0}}}]})

(defn- make-world []
  (let [graph-store (store/mock-store)
        provider (artifact/in-memory-provider)
        strategy (strategy-value)
        strategy-revision-id (service/register-strategy! graph-store strategy)]
    {:store graph-store
     :provider provider
     :strategy strategy
     :strategy-revision-id strategy-revision-id}))

(defn- ingest! [{:keys [provider]} text]
  (get-in (artifact/ingest! provider artifact-id (utf8 text) "text/plain")
          [:ok :artifact/revision-id]))

(defn- resolve! [{:keys [store provider strategy-revision-id]} revision-id request-id]
  (evaluator/evaluate
   {:artifact-provider provider
    :strategy-source (fn [srid]
                       (get-in (:ok (store/get-entity store srid))
                               [:props :strategy]))}
   {:request/id request-id
    :artifact/revision-id revision-id
    :strategy/revision-id strategy-revision-id
    :options {:allow-unverified-fuzzy? true}}))

(defn- first-range [result]
  (get-in result [:result/candidates 0 :location :range]))

(deftest one-scope-and-strategy-survive-an-edit-sequence
  (let [world (make-world)
        variants
        {:base base-text
         :insert-before (str "New preamble.\n" base-text)
         :insert-after (str base-text "\nNew afterword.")
         :move (str "New location.\nTarget: " anchor-text ".\nOmega outro.\n"
                    "Alpha intro section.")
         :inside-edit (str "Alpha intro section.\n"
                           "Target: the compactness ARGUMENT.\n"
                           "Omega outro.")
         :copy (str "Target: " anchor-text ".\nOmega outro.\nBridge.\n"
                    "Target: " anchor-text ".\nOmega outro.")
         :delete "Alpha intro section.\nOmega outro."}
        revisions (into {} (map (fn [[k text]] [k (ingest! world text)]) variants))
        results (into {} (map (fn [[k rid]]
                                [k (resolve! world rid (str "edit-robustness/" (name k)))])
                              revisions))]
    (testing "the logical scope and strategy do not change when artifact bytes do"
      (is (= 7 (count (set (vals revisions))))
          "each distinct snapshot has its own content-addressed revision")
      (is (= (evaluator/strategy-revision-id (:strategy world))
             (:strategy-revision-id world)))
      (is (every? #(= (:strategy/revision-id %) (:strategy-revision-id world))
                  (vals results))))

    (testing "insertion before relocates; insertion after preserves the old bounds"
      (is (= :resolved (get-in results [:insert-before :result/status])))
      (is (= :verified-hash
             (get-in results [:insert-before :result/candidates 0 :verification])))
      (is (> (:start (first-range (:insert-before results)))
             (:start (first-range (:base results)))))
      (is (= (first-range (:base results)) (first-range (:insert-after results))))
      (is (= :verified-exact
             (get-in results [:insert-after :result/candidates 0 :verification]))))

    (testing "cut/paste movement is recovered by portable context evidence"
      (is (= :resolved (get-in results [:move :result/status])))
      (is (= :verified-hash
             (get-in results [:move :result/candidates 0 :verification])))
      (is (= (codepoint-offset (:move variants) anchor-text)
             (get-in results [:move :result/candidates 0 :location :range :start]))))

    (testing "an internal edit remains usable but is never falsely called verified"
      (is (= :resolved-unverified (get-in results [:inside-edit :result/status])))
      (is (= :unverified-fuzzy
             (get-in results [:inside-edit :result/candidates 0 :verification]))))

    (testing "copy preserves ambiguity and delete produces typed absence"
      (is (= :ambiguous (get-in results [:copy :result/status])))
      (is (= 2 (count (get-in results [:copy :result/candidates]))))
      (is (= :missing (get-in results [:delete :result/status])))
      (is (empty? (get-in results [:delete :result/candidates]))))

    (testing "new revisions do not rewrite or destabilize earlier evidence"
      (let [base-again (resolve! world (:base revisions) "edit-robustness/base-again")]
        (is (= (:result/candidates (:base results)) (:result/candidates base-again)))
        (is (= (:result/evidence-digests (:base results))
               (:result/evidence-digests base-again)))))))
