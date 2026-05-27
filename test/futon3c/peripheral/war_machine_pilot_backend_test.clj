(ns futon3c.peripheral.war-machine-pilot-backend-test
  (:require [babashka.http-client :as http]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.peripheral.war-machine-pilot-backend :as backend]
            [futon3c.peripheral.common :as common]
            [futon3c.peripheral.war-machine-pilot-shapes :as shapes]
            [futon3c.util.edn-comment-preserving :as edn-comments]
            [rewrite-clj.zip :as z]))

(def ^:private anchors-fixture
  "/home/joe/code/futon5a/data/wm-ui-anchors.edn")

(defn- temp-anchors-path
  []
  (str (System/getProperty "java.io.tmpdir")
       "/wm-ui-anchors-pilot-test-"
       (System/nanoTime)
       ".edn"))

(defn- temp-edn-path
  []
  (str (System/getProperty "java.io.tmpdir")
       "/edn-comment-preserving-"
       (System/nanoTime)
       ".edn"))

(defn- allowed-pilot-action-path
  []
  (str "/home/joe/code/futon2/web/war-machine/.pilot-action-test-"
       (System/nanoTime)
       ".txt"))

(defn- comment-strings
  [text]
  (loop [loc (z/of-string text)
         comments []]
    (cond
      (nil? loc) comments
      (z/end? loc) comments
      :else
      (recur (z/next loc)
             (cond-> comments
               (= :comment (z/tag loc)) (conj (z/string loc)))))))

(defn- without-targeted-anchor-fields
  [data]
  (update data :anchors
          (fn [anchors]
            (mapv (fn [anchor]
                    (if (= "wm-ui-anchor:0010" (:id anchor))
                      (dissoc anchor :status :pilot-flip-trail)
                      anchor))
                  anchors))))

(deftest comment-preserving-writer-keeps-inline-comments-during-status-flip
  (testing "status replacement + pilot trail append preserve inline comments byte-equal"
    (let [temp-path (temp-edn-path)
          fixture (str "{:anchors\n"
                       " [;; anchor collection comment\n"
                       "  {:id \"a-1\" ; id comment\n"
                       "   :status :open ; status comment\n"
                       "   :note \"keep me\"} ; anchor tail comment\n"
                       "  {:id \"a-2\"\n"
                       "   :status :addressed}]\n"
                       " ; coherence comment\n"
                       " :coherence-evidence\n"
                       " [;; row comment\n"
                       "  {:id \"row-1\"\n"
                       "   :pairs []\n"
                       "   :coherence-check :ok\n"
                       "   :evidence-kind :demo\n"
                       "   :landed \"2026-05-25\"\n"
                       "   :template-status :landed}]}\n")
          _ (spit temp-path fixture)
          before-text (slurp temp-path)
          before-data (edn/read-string before-text)
          after-data (update before-data :anchors
                             (fn [anchors]
                               (mapv (fn [anchor]
                                       (if (= "a-1" (:id anchor))
                                         (-> anchor
                                             (assoc :status :addressed)
                                             (assoc :pilot-flip-trail
                                                    [{:flipped-by-pilot true
                                                      :cited-consent-gate-event-id "cg-inline"
                                                      :prior-status :open}]))
                                         anchor))
                                     anchors)))]
      (edn-comments/write-edn-preserving-comments temp-path after-data)
      (let [after-text (slurp temp-path)
            reparsed (edn/read-string after-text)
            target (first (filter #(= "a-1" (:id %)) (:anchors reparsed)))]
        (is (= (comment-strings before-text)
               (comment-strings after-text)))
        (is (= :addressed (:status target)))
        (is (= "cg-inline"
               (:cited-consent-gate-event-id (last (:pilot-flip-trail target))))))
      (io/delete-file temp-path true))))

(deftest anchor-flip-preserves-comments-on-live-substrate-copy
  (testing "anchor-flip preserves comments while changing only status + pilot trail"
    (let [temp-path (temp-anchors-path)
          _ (io/copy (io/file anchors-fixture) (io/file temp-path))
          before-text (slurp temp-path)
          before-data (edn/read-string before-text)
          result (backend/anchor-flip {:anchor-id "wm-ui-anchor:0010"
                                       :new-status :addressed
                                       :consent-gate-event-id "cg-test-uuid"
                                       :rationale "test round-trip"
                                       :anchors-path temp-path})
          after-text (slurp temp-path)
          after-data (edn/read-string after-text)
          target (first (filter #(= "wm-ui-anchor:0010" (:id %)) (:anchors after-data)))
          before-target (first (filter #(= "wm-ui-anchor:0010" (:id %)) (:anchors before-data)))]
      (is (:ok result))
      (is (= (comment-strings before-text)
             (comment-strings after-text)))
      (is (= (:status target) :addressed))
      (is (= (:prior-status (last (:pilot-flip-trail target)))
             (:status before-target)))
      (is (= (:cited-consent-gate-event-id (last (:pilot-flip-trail target)))
             "cg-test-uuid"))
      (is (= (without-targeted-anchor-fields before-data)
             (without-targeted-anchor-fields after-data)))
      (is (str/includes? after-text ":pilot-flip-trail"))
      (io/delete-file temp-path true))))

(deftest coherence-row-author-appends-row-without-disturbing-comments
  (testing "coherence-row-author appends a validated row while preserving comments"
    (let [temp-path (temp-anchors-path)
          _ (io/copy (io/file anchors-fixture) (io/file temp-path))
          before-text (slurp temp-path)
          before-data (edn/read-string before-text)
          row {:id "coherence:test-row"
               :pairs [{:anchor {:id "wm-ui-anchor:0010"}}]
               :coherence-check :matches
               :evidence-kind :pilot-demo
               :landed "2026-05-25"
               :template-status :landed}
          result (backend/coherence-row-author {:row-content row
                                                :consent-gate-event-id "cg-row-test"
                                                :anchors-path temp-path})
          after-text (slurp temp-path)
          after-data (edn/read-string after-text)]
      (is (:ok result))
      (is (= (comment-strings before-text)
             (comment-strings after-text)))
      (is (= (inc (count (:coherence-evidence before-data)))
             (count (:coherence-evidence after-data))))
      (is (= row (last (:coherence-evidence after-data))))
      (is (= (:anchors before-data) (:anchors after-data)))
      (io/delete-file temp-path true))))

(deftest append-top-level-vector-preserves-comments
  (testing "generic append-only vector helper preserves comments while appending"
    (let [temp-path (temp-edn-path)
          fixture (str "{:header \"keep\"\n"
                       " ;; bilateral block comment\n"
                       " :bilateral-evidence [;; first entry comment\n"
                       "  {:vsatarcs-id \"hx:test:1\"\n"
                       "   :evidence-kind :one-sided-extension}]\n"
                       " :tail {:k 1}}")
          _ (spit temp-path fixture)
          before-text (slurp temp-path)
          before-data (edn/read-string before-text)
          new-entry {:vsatarcs-id "hx:test:2"
                     :evidence-kind :one-sided-extension
                     :auto-generated true}
          result (edn-comments/append-items-to-top-level-vector-preserving-comments
                  temp-path :bilateral-evidence [new-entry])
          after-text (slurp temp-path)
          after-data (edn/read-string after-text)]
      (is (:ok result))
      (is (= (comment-strings before-text)
             (comment-strings after-text)))
      (is (= (inc (count (:bilateral-evidence before-data)))
             (count (:bilateral-evidence after-data))))
      (is (= new-entry (last (:bilateral-evidence after-data))))
      (is (= (:header before-data) (:header after-data)))
      (is (= (:tail before-data) (:tail after-data)))
      (io/delete-file temp-path true))))

(deftest pilot-action-applies-structured-change-set-within-allowed-roots
  (testing "pilot-action performs replace-first edits inside the allowed roots"
    (let [path (allowed-pilot-action-path)]
      (spit path "alpha beta alpha")
      (let [result (backend/pilot-action {:target-file path
                                          :change-set [{:op :replace-first
                                                        :old-string "alpha"
                                                        :new-string "gamma"}]
                                          :consent-gate-event-id "cg-action-test"
                                          :rationale "bounded pilot action"})]
        (is (:ok result))
        (is (= "gamma beta alpha" (slurp path)))
        (io/delete-file path true)))))

(deftest pilot-action-rejects-targets-outside-allowed-roots
  (testing "pilot-action refuses writes outside the v0 scope roots"
    (let [path (temp-edn-path)]
      (spit path "alpha")
      (let [result (backend/pilot-action {:target-file path
                                          :change-set [{:op :replace-first
                                                        :old-string "alpha"
                                                        :new-string "beta"}]
                                          :consent-gate-event-id "cg-action-test"
                                          :rationale "should fail"})]
        (is (false? (:ok result)))
        (is (str/includes? (:error result) "outside pilot-action v0 allowed roots")))
      (io/delete-file path true))))

(deftest heartbeat-emit-posts-typed-progress-bell
  (testing "heartbeat-emit emits a typed progress heartbeat distinct from completion"
    (let [captured (atom nil)]
      (with-redefs [http/post
                    (fn [url opts]
                      (reset! captured {:url url :opts opts})
                      {:status 202
                       :body "{\"job-id\":\"hb-123\",\"accepted\":true}"})]
        (let [result (backend/heartbeat-emit {:cycle-id "cycle-1"
                                              :step-count 3
                                              :elapsed-ms 1250
                                              :work-tag :anchors-read
                                              :detail {:anchor-id "wm-ui-anchor:0010"}})]
          (is (:ok result))
          (is (= "hb-123" (get-in result [:result :bell-job-id])))
          (is (= "http://127.0.0.1:7070/api/alpha/bell" (:url @captured)))
          (is (= :pilot/heartbeat
                 (get-in result [:result :payload-schema :pilot-event])))
          (is (= 3 (get-in result [:result :payload-schema :step-count])))
          (is (= :anchors-read
                 (get-in result [:result :payload-schema :work-tag]))))))))

(deftest heartbeat-tool-is-declared-on-the-envelope
  (testing "heartbeat-emit is available during :observe as a non-substantive action"
    (let [spec (common/load-spec :war-machine-pilot)]
      (is (contains? (:peripheral/tools spec) :heartbeat-emit))
      (is (= :action (get shapes/pilot-tool-operation-kinds :heartbeat-emit)))
      (is (contains? (get shapes/phase-allowed-tools :observe) :heartbeat-emit))
      (is (not (contains? shapes/substantive-tools :heartbeat-emit))))))
