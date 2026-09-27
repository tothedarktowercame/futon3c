(ns futon3c.diagramprover.wm-wire-ask-out-support
  "Real published interpretations and click-wants, tampered before consumers."
  (:require [clojure.set :as set]
            [clojure.edn :as edn] [clojure.java.io :as io]
            [cheshire.core :as json]
            [futon2.aif.want-interpretation :as wi]
            [futon2.aif.mission-criteria :as mc]
            [futon2.aif.observation-checks :as checks]
            [futon2.aif.locator-fixtures :as loc]
            [futon2.aif.cascade-problems :as cp]
            [futon2.aif.cascade-policy :as policy]
            [futon2.aif.interpretation-construction :as ic]
            [futon2.aif.flight :as flight] [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.report.war-machine :as wm]
            [futon2.report.cascade-decision-test :as decision-fixture]
            [futon2.report.observation-labels-consume-test :as population]
            [futon3c.transport.http :as http]
            [futon3c.wm.runner-service :as service]
            [futon3c.diagramprover.wm-wire :as w]
            [futon3c.diagramprover.wm-wire-target-support :as target]
            [futon3c.diagramprover.wm-wire-measured-support :as measured]))

(def live-records-read
  (mapv #(assoc % :why "Census of wants, universe, flight options and interpretations: flight wants are click bookkeeping, not judge options or assembly output beside click-wants; no :universe carrier or published-store source beside a conditioning/constructor product.") target/live-records-read))
(defn live-census []
  (mapv (fn [pin]
          (let [r (target/pinned pin)]
            {:path (:path pin)
             :fields (frequencies (mapcat #(when (map? %) (filter #{:wants :universe :interpretations :flight} (keys %)))
                                         (tree-seq coll? seq r)))})) live-records-read))

(def target-id "M-futon-seams")
(def pattern-id :writing-coherence/meet-the-reader-where-they-are)
(def document :exit/h54d16050a9dc)
(def argue :exit/hac75428b9c97)
(def fixture-root "/home/joe/code/futon2/test/fixtures")
(defn mission-text [] (slurp (io/file fixture-root "mission-criteria/M-futon-seams@futon3c-d05cb755.md")))
(defn published [root]
  (let [text (mission-text)
        wants (mc/wants (mc/criteria target-id text)
                        {:repo "futon3c" :path "holes/missions/M-futon-seams.md"
                         :observe #(checks/decl-present? text (:decl %))})
        sources {:universes {target-id (:universe wants)} :wants {target-id (:wants wants)}
                 :locators {target-id (:locators wants)}
                 :interpretations {target-id {:patterns {} :receipts {}}}
                 :horizon-steps 4 :beta-by-context {:WM {:beta 1}} :context-of (constantly :WM)
                 :construction {:construct ic/construct :budget {:max-moves 4 :max-expansions 20000}
                                :move-cost 0 :evaluate-g wm/constructed-candidate-g}}
        proposals (edn/read-string (slurp (io/file fixture-root "want-interp-library/M-futon-seams-interpretations@futon2-78439f58.edn")))
        response (assoc (get-in proposals [:patterns pattern-id]) :pattern pattern-id
                        :receipt (get-in proposals [:interpretation-receipts pattern-id]))
        request {:target target-id :want {:token document}}
        validated (wi/validate-response request response
                    {:sources sources :constraints [] :admit #'wm/admit-cascade-problem
                     :code-root (str (io/file fixture-root "want-interp-library"))})]
    (assert (= :valid (:status validated)) (pr-str validated))
    (wi/publish! root (wi/issue! root request) response validated)
    (loc/locate-all (wi/merge-published sources root [target-id]))))

(defn changed-sources [sources mutation]
  (case mutation
    :none sources
    :absent (assoc-in sources [:interpretations target-id] {:absent :not-carried})
    :different (update-in sources [:interpretations target-id :patterns pattern-id :produces] conj argue)))

(defn interpretations [kind mutation]
  (let [root (w/tmp-dir "ask-out-store-")]
    (try
      (let [sources (published root) changed (changed-sources sources mutation)
            patterns (get-in sources [:interpretations target-id :patterns])
            seen (atom nil) supplied (atom nil) real ic/construct
            assembled (with-redefs [ic/construct (fn [input] (reset! supplied input) (let [r (real input)] (reset! seen r) r))]
                        (#'cp/assemble-one (assoc-in (if (= kind :construct) sources changed) [:construction :construct] ic/construct) 4 target-id))
            problem (:cascade-problem assembled)
            _ (when (= kind :construct)
                (reset! seen (real (assoc @supplied :interpretations
                                         (get-in changed [:interpretations target-id :patterns])))))]
        {:writer (if (= kind :assemble) patterns
                   (get-in patterns [pattern-id :produces]))
         :reader (if (= kind :assemble) (:interpretations problem)
                   (when-let [candidate (first (:candidates @seen))]
                     (set/difference
                       (set (remove #(true? (get-in sources [:universes target-id %]))
                                    (get-in sources [:wants target-id])))
                       (set (map :token (get-in candidate [:construction-receipt :unreached-wants]))))))
         :assembled assembled :constructed @seen
         :tokens (when problem (cp/problem-tokens (:facts problem) (:want problem) (:interpretations problem)))})
      (finally (measured/cleanup root)))))

(defn click [kind field mutation]
  (let [root (w/tmp-dir "ask-out-click-")]
    (try
      (let [text (mission-text)
            f (flight/start {:target target-id :chosen-because {:kind :requested}}
                            {:kind :a-exits :repo "futon3c" :path "mission.md" :store root
                             :read-text (fn [& _] text)} {:id "offline-ask-out"})
            value (flight/click-wants f {})
            changed (case mutation :none value :absent (dissoc value field)
                          :different (update value field #(if (= field :wants) (conj % :different-want)
                                                             (update % (first (keys %)) not))))
            opts (flight/judge-opts f changed)
            result (case kind
                     :opts opts
                     :assembly (wm/flight-assembly-input (:flight opts) {:sources {}})
                     :http
                     (let [seen (atom nil)]
                       (with-redefs [service/cast-preflight-refusal (constantly nil)
                                     service/click! (fn [o] (reset! seen o) {:started true :click-id "offline"})]
                         ((fr/http-click-fn
                            {:today (constantly "fixture")
                             :post! (fn [body]
                                      (let [r (#'http/handle-wm-click-start
                                                {:headers {} :body (java.io.ByteArrayInputStream.
                                                                    (.getBytes (json/generate-string body) "UTF-8"))} {})]
                                        {:status (:status r) :body (json/parse-string (:body r) true)}))
                             :get-status! (constantly {:running? false}) :read-record! (constantly {})}) opts))
                       @seen))]
        {:writer (get value field)
         :reader (if (= kind :assembly)
                   (get-in result [:sources (if (= field :wants) :wants :universes) target-id])
                   (get-in result [:flight field]))
         :result result})
      (finally (measured/cleanup root)))))

(defn step [mutation]
  (let [root (w/tmp-dir "ask-out-step-")]
    (try
      (binding [population/*dir* (io/file root)]
        (#'population/fill! 5)
        (let [sources (published (str (io/file root "interpretations")))
              changed (changed-sources sources mutation)
              assembled (cp/assemble {:targets [target-id] :sources changed})
              result (wm/cascade-decision assembled
                       (-> decision-fixture/live-c-opts
                           (assoc :observation-labels-path (#'population/path))
                           (update-in [:focus-inputs :relations] conj
                                      (assoc (first (get-in decision-fixture/live-c-opts [:focus-inputs :relations]))
                                             :target target-id))))
              saved (#'runner/persist-run-record!
                      {:run-record-dir root} "offline-ask-step" "2026-09-26T00:00:00Z"
                      {:outcome :offline-no-selection
                       :checkpoints {:selection {:judgment {:controller-decision (:decision result)}}}})
              record (edn/read-string (slurp (:run-record saved)))
              input (measured/inputs record)
              converted (atom {}) real policy/declared->interpreted
              r (with-redefs [policy/declared->interpreted
                              (fn [id p] (let [r (real id p)] (swap! converted assoc id r) r))]
                  (flight/conditioning-step input))]
          {:writer (get-in sources [:interpretations target-id :patterns pattern-id :produces])
           ;; Capture the real converter result consumed by the rollout.
           :reader (when (= :present (:status r))
                     (get-in @converted [pattern-id :transition :produces]))
           :step r :assembled assembled :decision (:decision result)
           :record record :sources sources}))
      (finally (measured/cleanup root)))))
