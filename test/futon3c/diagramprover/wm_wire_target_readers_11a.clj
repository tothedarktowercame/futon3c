(ns futon3c.diagramprover.wm-wire-target-readers-11a
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-driver :as driver]
            [futon2.aif.flight-runner :as runner]
            [futon2.aif.wm.construction-inputs :as construction-inputs]
            [futon3c.diagramprover.wm-wire :as w]))

(def targets ["M-first" "M-second"])
(def wants {:wants [:done] :locators {:done {:class :C8 :repo "fixture" :namespace "fixture-test"}}
            :universe {:done false} :source {}})

(defn products [reader]
  (let [root (w/tmp-dir "target-readers-11a-")]
    (try
      (let [f (flight/start (driver/resolve-target {:chosen-target (first targets) :draw-seed 42})
                            {:kind :a-exits :repo "fixture" :path "mission.md" :store root
                             :read-text (fn [& _] "# Mission\n")}
                            {:id "wire-11a" :at "fixture"})
            fs [f (assoc f :target (second targets))]
            sources {:universes {(first targets) {:done true} (second targets) {:done false}}
                     :horizon-steps 1 :beta-by-context {:WM 1}}
            outputs
            (mapv (fn [f]
                    (case reader
                      :opts (flight/judge-opts f wants)
                      :assembly (construction-inputs/flight-assembly-input (merge f (dissoc wants :source))
                                                          {:sources {}})
                      ;; ask-fn:397-410 selects the target's universe and computes
                      ;; unproduced wants. No criterion: ask-one:346-354 records it,
                      ;; and never invokes an answer port or issues a request.
                      :ask ((runner/ask-fn {:store root
                                            :answer-fn (fn [_] (throw (ex-info "must not answer" {})))})
                            f (dissoc wants :universe) sources)
                      ;; http-click-fn:527-555 feeds target into record-summary:
                      ;; 442-456 retains only the chosen plan for that target.
                      :click (let [sent (atom nil)
                                   r ((runner/http-click-fn
                                        {:today (constantly "fixture")
                                         :post! (fn [body] (reset! sent (edn/read-string (:flight-edn body)))
                                                  {:status 200 :body {:click-id "fixture"}})
                                         :get-status! (constantly {:running? false})
                                         :read-record! (fn [_] {:decision {:chosen {:target (first targets)
                                                                                   :id :p :candidate :C1
                                                                                   :precedence [:p]
                                                                                   :unreached-wants [{:token :done}]}}})})
                                      (flight/judge-opts f wants))]
                               {:summary r :sent @sent})))
                  fs)]
        {:outputs outputs :flights fs})
      (finally (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f true))))))

(defn normal-assembly [r target]
  (-> r
      (assoc :targets [::target])
      (update :sources (fn [s]
                         (-> (reduce (fn [s k] (update s k #(hash-map ::target (get % target))))
                                     s [:wants :locators :universes])
                             (assoc :context-of (mapv (:context-of s) [target "unmentioned"])))))))
