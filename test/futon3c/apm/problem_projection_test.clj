(ns futon3c.apm.problem-projection-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.apm.campaign-ledger :as ledger]
            [futon3c.apm.campaign-machine :as machine]
            [futon3c.apm.campaign-snapshot :as snapshot]
            [futon3c.apm.problem-projection :as problem])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(defn temp-dir []
  (Files/createTempDirectory "problem-projection-"
                             (make-array FileAttribute 0)))

(defn event [seq type body]
  (let [base {:event/seq seq :event/type type :event/campaign-id "campaign"
              :event/actor "test" :event/at "2026-08-20T12:00:00Z"
              :event/expected-version seq :event/body body}]
    (assoc base :event/id (machine/ledger-digest [base]))))

(defn fixture []
  (let [dir (temp-dir)
        ledger-path (.resolve dir "ledger.edn")
        registered
        (event 0 :campaign/registered
               {:series :apm :manifest-hash "manifest"
                :phase-order [:solve :close-frame]
                :block-plan [{:block-id "b" :ordinal 1
                              :units [{:frame-id "f18" :problem-id "p18"}]}]
                :obligation-plan {:solve {:kind :solve :role :solver}
                                  :close-frame {:kind :close-frame :role :guide}}})
        opened-block (event 1 :block/opened
                            {:block-id "b" :ordinal 1
                             :units [{:frame-id "f18" :problem-id "p18"}]})
        opened-frame (event 2 :frame/opened
                            {:block-id "b" :frame-id "f18" :problem-id "p18"
                             :registration-hash "reg" :harness-hash "harness"})
        solve-body {:receipt/type :frame-solve :receipt/frame-id "f18"
                    :receipt/problem-id "p18"}
        solve (assoc solve-body :receipt/id (machine/ledger-digest [solve-body]))
        advanced (event 3 :frame/advanced
                        {:frame-id "f18" :from :solve :to :close-frame
                         :certificate solve})
        events [registered opened-block opened-frame advanced]]
    (doseq [e events]
      (let [loaded (ledger/read-ledger ledger-path)]
        (ledger/compare-and-append! ledger-path
                                    (get-in loaded [:projection :campaign/version])
                                    (get-in loaded [:projection :ledger/digest]) e)))
    (let [snap (:certificate
                (snapshot/snapshot
                 {:ledger-path ledger-path
                  :observation {:binding-response {:ok true :bound? false}
                                :jobs-response {:ok true :jobs []}}
                  :now (Instant/parse "2026-08-20T12:00:01Z")}))
          cert-path (:path (snapshot/persist! (.resolve dir "certificates") snap))]
      {:dir dir :ledger-path ledger-path :certificate-path cert-path
       :certificate snap :solve solve})))

(deftest projection-binds-ledger-frame-phase-and-receipts
  (let [{:keys [dir ledger-path certificate-path solve]} (fixture)
        calls (atom [])
        output (.resolve dir "live-problem.md")
        result (problem/project!
                {:ledger-path ledger-path :certificate-path certificate-path
                 :output-path output :expected-frame-id "f18"
                 :expected-problem-id "p18"
                 :buffer-name "*problem: f18-p18*"
                 :solver-progress {:rounds/completed 10 :rounds/max 50
                                   :round/active 11 :checkpoint/next 20}
                 :operation {:status :waiting-for-terminal-result
                             :role :scribe :agent-id "f18-scribe"
                             :job-id "job-scribe"}
                 :buffer-sink (fn [payload]
                                (swap! calls conj payload)
                                {:ok true :atomic? true})})]
    (is (:ok result) (pr-str result))
    (is (= "f18" (get-in result [:projection :frame :frame-id])))
    (is (= :close-frame (get-in result [:projection :frame :phase])))
    (is (= 10 (get-in result [:projection :solver/progress
                              :rounds/completed])))
    (is (str/includes? (Files/readString output)
                       "Solver rounds completed: **10 / 50**"))
    (is (str/includes? (Files/readString output)
                       "Operational state: **waiting-for-terminal-result**"))
    (is (str/includes? (Files/readString output)
                       "Waiting for role: **scribe**"))
    (is (str/includes? (Files/readString output) "`job-scribe`"))
    (is (= (:receipt/id solve)
           (get-in result [:projection :receipts 0 :receipt/id])))
    (is (= (get-in result [:projection :projection/id])
           (get-in @calls [0 :problem-projection :projection/id])))
    (is (= "*problem: f18-p18*" (get-in @calls [0 :buffer-name])))
    (is (= (get-in @calls [0 :content]) (Files/readString output)))))

(deftest stale-certificate-and-wrong-identity-fail-before-publication
  (let [{:keys [dir ledger-path certificate-path]} (fixture)
        output (.resolve dir "must-not-exist.md")
        calls (atom [])
        wrong (problem/project!
               {:ledger-path ledger-path :certificate-path certificate-path
                :output-path output :expected-frame-id "f19"
                :buffer-sink #(do (swap! calls conj %) {:ok true})})]
    (is (= :problem-projection-frame-mismatch (:error/code wrong)))
    (is (false? (Files/exists output (make-array java.nio.file.LinkOption 0))))
    (is (empty? @calls))))

(deftest emacs-sink-uses-one-base64-evaluation
  (let [call (atom nil)
        result (problem/emacs-buffer-sink
                (fn [& args]
                  (reset! call args)
                  {:exit 0 :out "t\n" :err ""})
                {:buffer-name "*problem*"
                 :content "quotes: \" and Lisp: (erase-buffer) λ"})]
    (is (:ok result))
    (is (= ["emacsclient" "--eval"] (take 2 @call)))
    (is (str/includes? (nth @call 2) "base64-decode-string"))
    (is (not (str/includes? (nth @call 2) "erase-buffer) λ")))))
