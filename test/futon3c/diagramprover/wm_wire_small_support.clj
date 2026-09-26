(ns futon3c.diagramprover.wm-wire-small-support
  "Hermetic calls for small lanes. Only IO boundaries are replaced; wrappers
  call the real writer and reader. TAMPER changes the named carrier field."
  (:require [clojure.edn :as edn] [clojure.java.io :as io]
            [clojure.test :refer [is]]
            [futon2.aif.flight :as flight] [futon2.aif.flight-runner :as fr]
            [futon2.aif.full-loop-runner :as runner]
            [futon2.aif.want-interpretation :as wi]
            [futon2.aif.cascade-model-manifest]
            [futon2.aif.cascade-problems :as problems]
            [futon2.report.war-machine :as wm]
            [futon3c.agency.clock-lineage :as clock]
            [futon3c.diagramprover.wm-wire :as w]))

(def live-records-read
  [{:path "holes/labs/M-wm-wiring/spike/flight-278b6988.edn"
    :sha256 "2e27390797bb6332ba4a40e67ef92c452bca6a70eee8acb57ce2ff68f88fe212"
    :why "Flight click entry predates the chosen carrier; no clock persistence, prompt library or scoped construction pair."}
   {:path "holes/labs/M-wm-wiring/spike/tick-run-record-2026-09-26-flight-278b6988-click-1.edn"
    :sha256 "f634b05c8020472aed90eb3c0333226788264142f572b62b301bf84aee8c6dfa"
    :why "Chosen producer input only; no flight reader value, persisted clock edge, prompt library or scoped construction pair."}])

(defn pinned [i]
  (let [{:keys [path sha256]} (nth live-records-read i)]
    (when-not (= sha256 (w/sha256-file path))
      (throw (ex-info "Moved pin" {:path path})))
    (w/read-record path)))

(defn assert-live-records []
  (let [f (pinned 0) r (pinned 1)]
    (is (some? (get-in r [:decision :chosen])))
    (is (not-any? :chosen (:clicks f)))))

(defn ask [kind tamper]
  (let [root (w/tmp-dir "small-library-") other (w/tmp-dir "small-other-library-")
        _ (spit (io/file root "pattern.flexiarg") "@title Fixture pattern\n")
        prompt wi/prompt written (atom nil) read-value (atom nil)
        changed (fn [v] (tamper v other))
        af (fr/agency-answer-fn {:seat "fixture-seat" :opts {} :library-root root
                                :dispatch! (fn [& _] {})})
        answer (with-redefs [wi/prompt
                            (fn [issued opts]
                              (reset! written (:library-root opts))
                              (let [s (prompt issued (if (= kind :prompt)
                                                       (update opts :library-root changed) opts))
                                    v (second (re-find #"captured library under (.*?) and append conformant" s))]
                                (reset! read-value (if (.startsWith v "{") (edn/read-string v) v))
                                s))]
                 (af {:target "M-fixture" :request-id "fixture" :want {:token :done}}))]
    (when-not (.isDirectory (io/file root)) (throw (ex-info "Missing library" {})))
    (if (= kind :prompt)
      {:writer @written :reader @read-value}
      {:writer (:library-root answer) :reader (:library-root (update answer :library-root changed))})))

(def sources
  {:universes {"M-wire" {:start true :done false}}
   :wants {"M-wire" [:done]}
   :locators {"M-wire" {:start {:class :C4} :done {:class :C4}}}
   :interpretations {"M-wire" {:patterns {:p {:guard {:needs #{:start} :forbids #{}}
                                             :produces #{:done}}}}}
   :candidates {"M-wire" [{:precedence [:p] :construction-receipt {:kind :fixture}}]}
   :beta-by-context {:WM {:beta 1}} :context-of (constantly :WM)})

(defn horizon [tamper]
  (let [assemble problems/assemble written (atom nil) read-value (atom nil)]
    (with-redefs [problems/assemble
                  (fn [input]
                    (reset! written (get-in input [:sources :horizon-steps]))
                    (let [r (assemble (update-in input [:sources :horizon-steps] #(tamper % 3)))]
                      (reset! read-value (get-in (first (:problems r)) [:cascade-problem :horizon-steps]))
                      r))]
      (wm/assemble-cascade-problems-with-published (w/tmp-dir "small-store-")
                                                  {:targets ["M-wire"] :sources sources}))
    {:writer @written :reader @read-value}))

(defn kernel [tamper]
  (let [evaluate-var (ns-resolve 'futon2.aif.cascade-model-manifest 'evaluate-state)
        push (ns-resolve 'futon2.aif.cascade-model-manifest 'push-forward)
        evaluate @evaluate-var written (atom nil)
        result (with-redefs-fn {evaluate-var
                               (fn [& args]
                                 (let [r (apply evaluate args)]
                                   (reset! written (:kernel r))
                                   (update r :kernel #(tamper % {#{:other} 1}))))}
                 #(push [] {#{} 1} true))]
    {:writer @written :reader (get-in result [:evaluation :states 0 :kernel])}))

(defn chosen [tamper]
  (let [record (pinned 1) target (get-in record [:decision :chosen :target])
        summary (fr/record-summary target "fixture-click" record)
        f (flight/start {:target target :chosen-because {:kind :requested}}
                        {:kind :operator-declared :wants [:done] :declared-by "wire-test"}
                        {:id "wire-small"})
        r (flight/run! f {:sources-fn (constantly {}) :max-clicks 1
                          :observe-fn (fn [& _] {:done false})
                          :click-fn (fn [_] (update summary :chosen #(tamper % {:candidate :other})))})]
    {:writer (:chosen summary) :reader (get-in r [:clicks 0 :chosen])}))

(defn dispatch [tamper]
  (let [written (atom nil) read-value (atom nil)
        post (ns-resolve 'futon2.aif.full-loop-runner 'post-json!)
        current (ns-resolve 'futon3c.agency.clock-lineage 'agent-current-targets)
        post-hx (ns-resolve 'futon3c.agency.clock-lineage 'post-hx!)]
    (with-redefs-fn
      {post (fn [_ payload]
              (reset! written (:mission-id payload))
              (let [carrier (update payload :mission-id #(tamper % "M-other"))
                    result (clock/persist-clock! {:agent-id (:agent-id carrier) :session-id "fixture"
                                                  :new-clock (select-keys carrier [:mission-id])
                                                  :now-ms 1})]
                (when result @result))
              {})
       ;; Isolate canonical substrate lookup and persistence; retain the real
       ;; persist-clock! field read and the props it writes to the IO port.
       (ns-resolve 'futon3c.agency.clock-lineage 'doc-repo) (fn [& _] "fixture")
       (ns-resolve 'futon3c.agency.clock-lineage 'endpoint-exists?) (constantly true)
       current (constantly [])
       post-hx (fn [edge] (reset! read-value (get-in edge [:props "mission-id"])) {:ok? true})}
      #(runner/dispatch! {:agency-base "http://fixture.invalid"} "fixture-seat" "wire-test" "M-wire" "fixture"))
    {:writer @written :reader @read-value}))

(defn observe [kind tamper]
  (case kind
    :prompt (ask kind tamper) :ask-test (ask kind tamper)
    :horizon (horizon tamper) :kernel (kernel tamper)
    :chosen (chosen tamper) :dispatch (dispatch tamper)))
