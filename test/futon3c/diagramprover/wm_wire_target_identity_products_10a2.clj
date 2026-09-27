(ns futon3c.diagramprover.wm-wire-target-identity-products-10a2
  (:require [clojure.java.io :as io]
            [futon2.aif.flight :as flight]
            [futon2.aif.flight-driver :as driver]
            [futon2.aif.outer-cascade :as outer]
            [futon2.aif.mission-reading :as reading]
            [futon2.aif.want-interpretation :as wi]
            [futon3c.diagramprover.wm-wire :as w]))

(def targets ["M-futon-seams" "M-other-target"])
(def locator {:class :C8 :repo "fixture" :namespace "fixture-test"})

(defn- pair [root text]
  (let [target (first targets)
        selection (outer/select {:field {:considered [{:target target}]
                                         :feasible [{:target target :eligible true :next-step :read-criteria}]
                                         :exclusions []}
                                 :seed 42 :trigger :wallclock-cron})
        opts {:repo "fixture" :path "mission.md" :store root :id "target-identity"
              :read-text (fn [& _] text) :observe (constantly false)}
        flights (mapv #(#'driver/flight-for (merge opts %))
                      [selection (assoc selection :chosen-target (second targets))])]
    {:flights flights :clicks (mapv #(flight/click-wants % {}) flights)}))

(defn products []
  (let [root (w/tmp-dir "target-identity-10a2-")]
    (try
      (let [text (slurp (io/resource "fixtures/mission-criteria/M-futon-seams@futon3c-d05cb755.md"))
            tokens (pair root text)
            unlocated-text "## MAP\n\n**Exit criterion:** the tests pass.\n"
            initial (pair root unlocated-text)
            token (get-in initial [:clicks 0 :source :unlocated 0 :token])
            criterion (assoc (get-in initial [:clicks 0 :source :criteria-by-token token]) :token token)
            request (reading/locator-request (first targets) {} criterion)
            issued (wi/issue! root request)
            response {:locator locator :cue {:quote (:stated criterion)}
                      :reading "The registered run decides whether tests pass."}
            validated (reading/validate-locator
                        issued response
                        {:observe (fn [_] {:observed #{} :refused {}})})
            published (reading/publish-locator! root issued response validated {:seat "fixture"})
            located (pair root unlocated-text)]
        {:tokens tokens :initial initial :located located
         :published published :criterion criterion :validated validated})
      (finally
        (doseq [f (reverse (file-seq (io/file root)))] (io/delete-file f true))))))
