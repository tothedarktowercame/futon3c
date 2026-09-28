(ns futon3c.agency.overreach-cli-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.disclosure-record :as disclosure]
            [futon3c.agency.overreach-cli :as cli]))

(def pin "2026-09-28T18:30:00Z")
(def harness {:kind :none :basis :producer-context :source-ref "test"})
(def stamp {:executor "agent-a" :signer "agent-a"
            :authority {:grant "act:grant"} :executor-basis :session-bound})
(def selection
  {:hx/id "act:card" :hx/type :pattern-card/selection
   :hx/props {:author "agent-a" :agent "agent-a" :session "s"
              :pattern-id "p" :at "2026-09-28T18:00:00Z"
              :act/harness harness :act/stamp stamp}})
(def legacy
  {:hx/id "act:legacy" :hx/type :pattern-card/selection
   :hx/props {:author "agent-a" :agent "agent-a" :session "s"
              :pattern-id "old" :at "2026-09-28T17:00:00Z"
              :act/harness harness}})
(def unreadable
  {:hx/id "act:bad" :hx/type :pattern-card/selection
   :hx/props {:author "agent-a"}})
(def missing-target-withdrawal
  {:hx/id "act:withdraw" :hx/type :act/withdrawal
   :hx/props {:author "agent-a" :target "act:not-listed" :status :effective
              :basis {:kind :self} :at "2026-09-28T18:10:00Z"
              :act/harness harness :act/stamp stamp}})
(def grant
  {:hx/id "act:grant" :hx/type :grant/record
   :hx/props {:grant/grantor "joe" :grant/grantee "agent-a"
              :grant/scope {:description "select" :act-kinds [:pattern-card/selection]}
              :grant/interval {:from "2026-09-28T17:00:00Z"}
              :grant/source {:id "e:joe" :author "joe"
                             :at "2026-09-28T17:00:00Z" :quote "select"}
              :grant/basis :explicit}})

(deftest report-is-get-only-pinned-and-nonfatal
  (let [calls (atom [])
        request-fn
        (fn [_ method path body]
          (swap! calls conj [method path body])
          (cond
            (str/includes? path "type=pattern-card%2Fselection")
            {:hyperedges [selection legacy unreadable]}
            (str/includes? path "type=act%2Fwithdrawal")
            {:hyperedges [missing-target-withdrawal]}
            (str/includes? path "type=disclosure%2Fchoice")
            {:hyperedges []}
            (str/includes? path "type=grant%2Frecord")
            ;; A full page must be reported even when the response omits a cursor.
            {:hyperedges (into [grant]
                               (map #(assoc grant :hx/id (str "act:filler-" %)))
                               (range (dec cli/limit)))}
            :else (throw (ex-info "unexpected path" {:path path}))))
        report (cli/generate-report "http://store" pin request-fn)]
    (is (every? #(and (= "GET" (first %)) (nil? (nth % 2))) @calls))
    (is (every? #(and (str/includes? (second %) "system-as-of=")
                      (str/includes? (second %) "valid-as-of=")) @calls))
    (is (= {:authorised 1 :overreach 1 :unverified-executor 0
            :outside-coverage 1}
           (:counts report)))
    (is (= ["act:legacy"] (:outside-coverage-ids report)))
    (is (= [{:id "act:bad" :reason :missing-at}] (:unreadable report)))
    (is (= [{:withdrawal-id "act:withdraw" :target-id "act:not-listed"}]
           (:missing-withdrawal-targets report)))
    (is (true? (get-in report [:truncated :grant/record])))
    (is (false? (get-in report [:truncated :pattern-card/selection])))))

(deftest disclosure-withdrawal-finds-its-target-and-is-not-overreach
  ;; Before the report read disclosures, a dispatch-edge withdrawal of one had
  ;; no target in the population: a missing target and a false overreach.
  (let [edge-stamp (fn [who] {:executor who :signer who
                              :authority {:dispatch-edge "e-edge"}
                              :executor-basis :declared})
        choice (disclosure/->hyperedge
                {:id "act:choice" :kind :disclosure/choice :schema 1
                 :author "codex-5" :at "2026-09-28T18:00:00Z"
                 :source-job "invoke-1" :unspecified "u" :chosen "c"
                 :affects {:kind :file :id "f"}
                 :inside-request {:basis :source-span :quote "q"
                                  :text-sha256 (apply str (repeat 64 "a"))}
                 :act/stamp (edge-stamp "codex-5") :act/harness harness})
        withdrawal {:hx/id "act:w" :hx/type :act/withdrawal
                    :hx/props {:author "claude-17" :target "act:choice"
                               :status :effective :basis {:kind :dispatch-edge}
                               :reason "no" :at "2026-09-28T18:10:00Z"
                               :act/harness harness
                               :act/stamp (edge-stamp "claude-17")}}
        request-fn (fn [_ _ path _]
                     (cond (str/includes? path "type=disclosure%2Fchoice") {:hyperedges [choice]}
                           (str/includes? path "type=act%2Fwithdrawal") {:hyperedges [withdrawal]}
                           :else {:hyperedges []}))
        report (cli/generate-report "http://store" pin request-fn)]
    (is (empty? (:missing-withdrawal-targets report)))
    (is (empty? (:unreadable report)) (pr-str (:unreadable report)))
    (is (= 2 (get-in report [:counts :authorised])) (pr-str (:findings report)))
    (is (zero? (get-in report [:counts :overreach])))))
