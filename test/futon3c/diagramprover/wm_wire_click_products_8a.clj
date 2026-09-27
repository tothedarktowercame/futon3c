(ns futon3c.diagramprover.wm-wire-click-products-8a
  "The declared readers carry values, without computing G or selection:
  flight/judge-opts (flight.clj:195-208), and flight-assembly-input
  (war_machine.clj:6119-6143). Context resolution is the latter's only
  added function; its results must remain unchanged by these interventions."
  (:require [clojure.java.io :as io]
            [futon2.aif.flight :as flight]
            [futon2.aif.observation-checks :as checks]
            [futon2.report.war-machine :as wm]
            [futon3c.diagramprover.wm-wire :as w]))

(def target "M-futon-seams")

(defn products [reader field]
  (let [root (w/tmp-dir "click-products-8a-")]
    (try
      (let [text (slurp (io/resource "fixtures/mission-criteria/M-futon-seams@futon3c-d05cb755.md"))
            f (flight/start {:target target :chosen-because {:kind :requested}}
                            {:kind :a-exits :repo "fixture" :path "mission.md" :store root
                             :read-text (fn [& _] text)
                             :observe #(checks/decl-present? text (:decl %))}
                            {:id "fixture-8a" :at "fixture"})
            value (flight/click-wants f {})
            token (first (:wants value))
            changed (if (= field :wants)
                      (update value :wants #(vec (remove #{token} %)))
                      (update-in value [:universe token] not))
            consume (fn [v]
                      (let [opts (flight/judge-opts f v)]
                        (if (= reader :opts) opts
                          (wm/flight-assembly-input (:flight opts)
                                                    {:targets ["other"]
                                                     :sources {:wants {"other" [:other]}
                                                               :universes {"other" {:other true}}
                                                               :context-of (constantly :original)}}))))
            before (consume value)
            after (consume changed)
            path (if (= reader :opts) [:flight field]
                   [:sources (if (= field :wants) :wants :universes) target])
            ;; Compare the function by its produced values, not object identity.
            normalise (fn [r]
                        (if (= reader :opts) r
                          (update-in r [:sources :context-of]
                                     #(mapv % [target "other" "unmentioned"]))))
            strip #(assoc-in (normalise %) path ::intervened)]
        {:written [(get value field) (get changed field)]
         :products [(get-in before path) (get-in after path)]
         :unchanged [(strip before) (strip after)]
         :context (when (= reader :assembly)
                    [(get-in (normalise before) [:sources :context-of])
                     (get-in (normalise after) [:sources :context-of])])
         :token token})
      (finally
        (doseq [file (reverse (file-seq (io/file root)))] (io/delete-file file true))))))
