(ns futon3c.diagramprover.wm-wire-outer-inputs-support
  "Real outer-input writers through select's recorded input, with tampering
  exclusively at the reader's door. Clock HTTP is isolated; no live writes."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [futon2.aif.target-field :as tf]
            [futon2.aif.outer-cascade :as oc]
            [futon2.aif.enactment-habit :as eh]
            [futon2.aif.flight-runner :as fr]
            [futon3c.agency.clock-lineage :as clock]
            [futon3c.diagramprover.wm-wire :as w]))

(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "Tick predates 628a8837a; no target-selection inputs, so no reader-produced end for these five fields."}
   {:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn"
    :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "Flight plan/placement predates 628a8837a; no target-selection inputs, so neither publication nor lineage is recorded as received by outer select."}])

(defn live-reader-absent? []
  (every? (fn [{:keys [path sha256]}]
            (and (= sha256 (w/sha256-file path))
                 (not-any? #(and (map? %) (contains? (:target-selection %) :inputs))
                           (tree-seq coll? seq (w/read-record path)))))
          live-records-read))

(def target "M-autoclock-in")
(defn field-entries []
  (let [text (slurp "../futon2/test/fixtures/target-field/M-autoclock-in@futon3c-7466251c.md")
        step-var (ns-resolve 'futon2.aif.target-field 'step)
        real-step @step-var
        written (atom [])
        entries (with-redefs-fn
                  {step-var (fn [& args]
                              (let [e (apply real-step args)] (swap! written conj e) e))}
                  #(mapv (fn [id]
                           (tf/assess {:read-text (fn [& _] text) :observe (constantly false)
                                       :sources {} :store "/nonexistent/wire-outer-inputs"}
                                      {:target id :kind :mission :repo "futon3c"
                                       :path "holes/missions/M-autoclock-in.md"
                                       :status-line (re-find #"(?m)^\*\*Status:\*\*.*$" text)}))
                         [target "M-other"]))]
    {:entries entries :written @written}))

(defn folded-records []
  (:enactment-records
   (eh/fold nil
            (mapv (fn [click]
                    (eh/increment {:click click :candidate :c
                                   :attempts [{:pattern :p :success true}]}
                                  [:pattern-cascade target [:p] {}] []))
                  ["click-1" "click-2"]))))

(defn publication []
  (:publication-observed
   ((fr/observe-publication-fn
     {:repair-id-fn (fn [_ _] "repair-1")
      :fetch-run-record (fn [_] {:repair/publication [{:repair/id "repair-1"
                                                      :status :receipt-committed
                                                      :receipt "published-receipt"}]})})
    {:target target} {:click-id "click-2"})))

(defn clock-props []
  (let [posted (atom [])
        result (with-redefs [http/get (fn [& _] {:status 200 :body "{:hx/id \"existing\" :hyperedges []}"})
                            http/post (fn [_ opts]
                                        (swap! posted conj (json/parse-string (:body opts)))
                                        {:status 200 :body "{}"})]
                 ;; Keep the HTTP ports isolated until the real async writer finishes.
                 (deref (clock/persist-clock! {:agent-id "wire-codex-1" :session-id "wire-session"
                                              :new-clock {:mission-id target}
                                              :witness "wire-test" :now-ms 1000})
                        5000 {:absent :clock-write-timeout}))]
    (when-not (:ok? result) (throw (ex-info "Clock fixture did not publish" {:result result})))
    (when-not (= 1 (count @posted)) (throw (ex-info "Unexpected clock writes" {:writes @posted})))
    (get (first @posted) "hx/props")))

(defn observe [field tamper]
  (let [{:keys [entries written]} (field-entries)
        entries (if (= field :pair-overlap)
                  (tf/with-pair-overlap
                   (mapv #(assoc % :universe #{:shared}
                                  :constructed-candidate {:produces #{:shared}}) entries))
                  entries)
        per-entry? (contains? #{:next-step :pair-overlap} field)
        writer (case field
                 :next-step (:next-step (first (filter #(= target (:target %)) written)))
                 :pair-overlap (:pair-overlap (first entries))
                 :enactment-records (folded-records)
                 :publication-observed (publication)
                 :clock-lineage (clock-props))
        value (case tamper
                :none writer
                :absent {:absent :writer-unavailable}
                :missing nil
                :different (case field
                             :next-step :different-step
                             :pair-overlap {"M-other" {:comparable true}}
                             :enactment-records {[:different-id :c] (first (vals writer))}
                             :publication-observed {:observed false :checked {:entries 0}}
                             :clock-lineage (assoc writer "mission-id" "M-different")))
        opts {:field {:considered entries :feasible entries :exclusions []} :seed 42}
        baseline (oc/select (if per-entry? opts (assoc opts field writer)))
        delivered (if per-entry?
                    (update-in opts [:field :feasible]
                               #(mapv (fn [e] (if (= target (:target e))
                                                (if (= tamper :missing) (dissoc e field) (assoc e field value)) e)) %))
                    (if (= tamper :missing) opts (assoc opts field value)))
        result (oc/select delivered)]
    {:writer writer
     :reader (get-in result (cond-> [:target-selection :inputs field] per-entry? (conj target)))
     :record (:target-selection result)
     :unchanged-law? (= (dissoc (:target-selection baseline) :inputs)
                        (dissoc (:target-selection result) :inputs))}))
