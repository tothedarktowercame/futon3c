(ns futon3c.diagramprover.wm-wire-enact-driver
  "Hermetic driver for the lane-8 wire tests (PROOF-2a-PLAN <2>3): the wires
  out of :r0-enact-step and :r5-flight-call, both sited at
  futon2.aif.flight-runner/enact-fn, to :r0-test, :r5-test, :wc-checker and
  :r5-grain-gate.

  No live record carries either writer's end: every flight record under
  holes/labs/M-wm-wiring/spike/ records its one enactment as a typed
  absence ({:absent :no-dispatch-configured} or {:absent :no-decision}),
  and the M-futon-seams exemplar enactment is hand-authored (schema
  :m-futon-seams/proof2a-enactment-v1, author claude-10 2026-09-24), not
  enact-fn's :wm/enactment-v1 output. So each lane-8 wire is witnessed
  hermetically: this driver runs enact-fn with a fixture seat
  (dispatch-step!), records the grain-gate calls it makes, reads the
  enactment record it writes from disk, and (for the W_c wires) runs the
  real checker, exemplar/proof2a_check.clj, on that record.

  The grains are real values from live records, pinned by sha256: the role
  grain is click-001-enactment.edn's :grain, the provider grain
  click-001-outcome.edn's (the grain the failed first attempt built)."
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as sh]
            [clojure.pprint :as pp]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.grain-gate :as gate]
            [futon3c.diagramprover.wm-wire :as w]))

(def exemplar-dir "holes/labs/M-futon-seams/exemplar")

(def click-001
  {:path (str exemplar-dir "/click-001.edn")
   :sha256 "98aa1cba12cdd1c759474d54447e89de38bcea58aa00fe6cd7cb392776a41409"})

(def click-001-enactment
  {:path (str exemplar-dir "/click-001-enactment.edn")
   :sha256 "e51063896e2a42096718d902e0b4dfe0e4652323de0b42848c2c0cf318bf6c89"})

(def click-001-outcome
  {:path (str exemplar-dir "/click-001-outcome.edn")
   :sha256 "b8dcbe1e4eedb0b8645ba507a4beadc5e93ce0037cce8340694752023f86740d"})

(def checker-path (str exemplar-dir "/proof2a_check.clj"))

(defn role-grain [] (:grain (w/read-record (:path click-001-enactment))))
(defn provider-grain [] (:grain (w/read-record (:path click-001-outcome))))

;; The three enactment shapes the lane-8 reader tests exercise.

(def r0-precedence [:p/a :p/b])
(def r0-interps {:p/a {:produces #{:t/a}} :p/b {:produces #{:t/b}}})

(def r5-precedence [:p/grain :p/after])
(defn r5-interps [] {:p/grain {:produces #{:t/roles} :grain (role-grain)}
                     :p/after {:produces #{:t/after}}})

;; click-001's :cand/a-registry-first: its seven patterns (grain pattern
;; first) and the token each declares it produces, so a fully successful
;; hermetic enactment passes every decidable W_c condition and the verdict
;; is the typed {:status :join-unverifiable} (the click predates the
;; selection law's :candidate id).
(def wc-candidate :cand/a-registry-first)
(def wc-grain-pattern :cascade-construction/choose-the-grain-where-state-lives)
(def wc-precedence [wc-grain-pattern
                    :or3/count-every-card-back
                    :coordination/assignment-binding
                    :cycle-machine/single-producer
                    :gauntlet/placenta-transfer
                    :translation/test-by-reproducing-behaviour
                    :realtime/mode-gate])
(defn wc-interps []
  {wc-grain-pattern {:produces #{:roles-named} :grain (role-grain)}
   :or3/count-every-card-back {:produces #{:sites-enumerated}}
   :coordination/assignment-binding {:produces #{:binding-recorded}}
   :cycle-machine/single-producer {:produces #{:one-producer}}
   :gauntlet/placenta-transfer {:produces #{:caller-converted}}
   :translation/test-by-reproducing-behaviour {:produces #{:redirect-test}}
   :realtime/mode-gate {:produces #{:prefix-routing-retired}}})

(defn enact
  "Run flight-runner/enact-fn (the :r0-enact-step and :r5-flight-call
  writer) hermetically: a fixture seat answers :plan with PLANNED-GRAIN and
  :commit with a commit whose produced token is the pattern's declared one;
  every check observes true. Returns {:enactment the returned record
  :record-path the file written :record the record read back from that
  file :gate-calls the arguments of every grain-gate call, in order}."
  [{:keys [candidate precedence interps planned-grain]}]
  (let [dir (w/tmp-dir "wire-enactments")
        gate-calls (atom [])
        real-gate @#'gate/grain-gate
        {:keys [enactment record-path]}
        (with-redefs [gate/grain-gate (fn [cand att root]
                                        (swap! gate-calls conj {:candidate cand :attempt att :repo-root root})
                                        (real-gate cand att root))]
          ((fr/enact-fn {:dispatch-step! (fn [step]
                                           (if (= :plan (:phase step))
                                             {:grain planned-grain}
                                             {:commit (str "c-" (name (:pattern step)))
                                              :produced (first (get-in step [:interpretation :produces]))
                                              :check {:class :fixture}}))
                         :check-fn (constantly {:observed true})
                         :interpretations (constantly interps)
                         :record-dir dir})
           {:flight/id "flight-wire" :target "M-t"}
           {:click-id "click-1" :chosen {:candidate candidate :precedence precedence}}))]
    {:enactment enactment
     :record-path record-path
     :record (w/read-record record-path)
     :gate-calls @gate-calls}))

(defn enact-r0 [] (enact {:candidate :cand/x :precedence r0-precedence :interps r0-interps}))
(defn enact-r5 [] (enact {:candidate :cand/g :precedence r5-precedence :interps (r5-interps)
                          :planned-grain (role-grain)}))
(defn enact-wc [] (enact {:candidate wc-candidate :precedence wc-precedence :interps (wc-interps)
                          :planned-grain (role-grain)}))

(defn rewrite
  "Replace the record at PATH by F of it (the bad cases: the reader reads a
  record the writer did not write)."
  [path f]
  (spit path (with-out-str (pp/pprint (f (w/read-record path))))))

(defn run-wc
  "The :wc-checker reader: run exemplar/proof2a_check.clj on click-001 and
  the enactment record at ENACTMENT-PATH with --wc --edn; {:exit :err
  :verdict} (the verdict parsed from stdout when exit 0)."
  [enactment-path]
  (let [{:keys [exit out err]} (sh/sh "bb" checker-path (:path click-001) enactment-path
                                      "--wc" "--edn" :dir "/home/joe/code/futon3c")]
    {:exit exit :err err :verdict (when (zero? exit) (edn/read-string out))}))
