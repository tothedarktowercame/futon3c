(ns futon3c.apm.problem-projection-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
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

(defn lifecycle-fixture []
  (let [dir (temp-dir)
        ledger-path (.resolve dir "ledger.edn")
        certificate-dir (.resolve dir "certificates")
        append! (fn [e]
                  (let [loaded (ledger/read-ledger ledger-path)]
                    (ledger/compare-and-append!
                     ledger-path
                     (get-in loaded [:projection :campaign/version])
                     (get-in loaded [:projection :ledger/digest]) e)))
        persist-snapshot!
        (fn []
          (let [certificate
                (:certificate
                 (snapshot/snapshot
                  {:ledger-path ledger-path
                   :observation {:binding-response {:ok true :bound? false}
                                 :jobs-response {:ok true :jobs []}}
                   :now (Instant/parse "2026-08-20T12:00:01Z")}))]
            (:path (snapshot/persist! certificate-dir certificate))))
        registered
        (event 0 :campaign/registered
               {:series :apm :manifest-hash "manifest"
                :phase-order [:preflight :solve :verify :close-frame]
                :block-plan [{:block-id "b" :ordinal 1
                              :units [{:frame-id "f27" :problem-id "p27"}]}]
                :obligation-plan
                {:preflight {:kind :preflight :role :proctor}
                 :solve {:kind :solve :role :solver}
                 :verify {:kind :verify :role :proctor}
                 :close-frame {:kind :close-frame :role :guide}}})]
    (append! registered)
    (append! (event 1 :block/opened
                    {:block-id "b" :ordinal 1
                     :units [{:frame-id "f27" :problem-id "p27"}]}))
    (append! (event 2 :frame/opened
                    {:block-id "b" :frame-id "f27" :problem-id "p27"
                     :registration-hash "reg" :harness-hash "harness"}))
    (let [preflight-path (persist-snapshot!)
          preflight-ledger (.resolve dir "preflight-ledger.edn")
          _ (Files/copy ledger-path preflight-ledger
                        (make-array java.nio.file.CopyOption 0))
          preflight-body {:receipt/type :frame-preflight
                          :receipt/frame-id "f27" :receipt/problem-id "p27"}
          preflight (assoc preflight-body :receipt/id
                           (machine/ledger-digest [preflight-body]))]
      (append! (event 3 :frame/advanced
                      {:frame-id "f27" :from :preflight :to :solve
                       :certificate preflight}))
      (let [solve-path (persist-snapshot!)
            solve-ledger (.resolve dir "solve-ledger.edn")
            _ (Files/copy ledger-path solve-ledger
                          (make-array java.nio.file.CopyOption 0))
            solve-body {:receipt/type :frame-solve
                        :receipt/frame-id "f27" :receipt/problem-id "p27"}
            solve (assoc solve-body :receipt/id
                         (machine/ledger-digest [solve-body]))]
        (append! (event 4 :frame/advanced
                        {:frame-id "f27" :from :solve :to :verify
                         :certificate solve}))
        {:dir dir :ledger-path ledger-path
         :preflight-ledger-path preflight-ledger
         :solve-ledger-path solve-ledger
         :preflight-certificate-path preflight-path
         :solve-certificate-path solve-path
         :verify-certificate-path (persist-snapshot!)}))))

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
                                {:ok true :atomic? true
                                 :content/digest (:content/digest payload)})})]
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
    (is (= (get-in @calls [0 :content]) (Files/readString output)))
    (is (= :problem-projection-publication
           (get-in result [:publication-receipt :receipt/type])))
    (is (= (get-in result [:publication-receipt :receipt/id])
           (get-in result [:publication-pointer :receipt/id])))
    (is (= :problem-projection-transition
           (get-in result [:transition :event/type])))
    (is (string? (get-in result [:transition :transition/key])))
    (is (= 0 (get-in result [:transition :event/sequence])))
    (is (Files/exists
         (.resolve (.resolve dir "publications")
                   (str (get-in result [:publication-receipt :receipt/id]) ".edn"))
         (make-array java.nio.file.LinkOption 0)))))

(deftest stale-certificate-and-wrong-identity-fail-before-publication
  (let [{:keys [dir ledger-path certificate-path]} (fixture)
        output (.resolve dir "must-not-exist.md")
        calls (atom [])
        wrong (problem/project!
               {:ledger-path ledger-path :certificate-path certificate-path
                :output-path output :expected-frame-id "f19"
                :buffer-sink #(do (swap! calls conj %)
                                  {:ok true :content/digest (:content/digest %)})})]
    (is (= :problem-projection-frame-mismatch (:error/code wrong)))
    (is (false? (Files/exists output (make-array java.nio.file.LinkOption 0))))
    (is (empty? @calls))))

(deftest emacs-sink-uses-one-base64-evaluation
  (let [call (atom nil)
        result (problem/emacs-buffer-sink
                (fn [& args]
                  (reset! call args)
                  {:exit 0 :out (str (pr-str (apply str (repeat 64 "a"))) "\n")
                   :err ""})
                {:buffer-name "*problem*"
                 :content "quotes: \" and Lisp: (erase-buffer) λ"})]
    (is (:ok result))
    (is (= ["emacsclient" "--eval"] (take 2 @call)))
    (is (= (apply str (repeat 64 "a")) (:content/digest result)))
    (is (str/includes? (nth @call 2) "base64-decode-string"))
    (is (not (str/includes? (nth @call 2) "erase-buffer) λ")))))

(deftest publication-fails-closed-when-buffer-readback-differs
  (let [{:keys [dir ledger-path certificate-path]} (fixture)
        output (.resolve dir "mismatch.md")
        result (problem/project!
                {:ledger-path ledger-path :certificate-path certificate-path
                 :output-path output
                 :buffer-sink (fn [_]
                                {:ok true :content/digest "wrong"})})]
    (is (= :problem-projection-buffer-readback-mismatch
           (:error/code result)))
    (is (false? (Files/exists (.resolve (.resolve dir "publications") "latest.edn")
                              (make-array java.nio.file.LinkOption 0))))))

(deftest transition-log-deduplicates-certificate-refreshes
  (let [dir (temp-dir)
        output (.resolve dir "problem.md")
        base {:ledger/digest "ledger" :ledger/event-count 7
              :frame-id "f27" :problem-id "p27" :phase :solve
              :operation {:status :waiting-for-terminal-result
                          :job-id "job"}
              :solver/progress {:rounds/completed 1 :round/active 2}
              :buffer/name "*problem: f27-p27*"}
        first (#'problem/append-transition!
               {:output-path output}
               (assoc base :receipt/id "receipt-a" :certificate/id "cert-a"
                      :content/digest "content-a"))
        second (#'problem/append-transition!
                {:output-path output}
                (assoc base :receipt/id "receipt-b" :certificate/id "cert-b"
                       :content/digest "content-b"))
        lines (Files/readAllLines (.resolve dir "problem-transitions.edn"))]
    (is (= :logged (:status first)))
    (is (= :already-logged (:status second)))
    (is (= 1 (count lines)))))

(deftest fast-lifecycle-publication-qualification
  (let [{:keys [dir ledger-path preflight-ledger-path solve-ledger-path
                preflight-certificate-path
                solve-certificate-path verify-certificate-path]}
        (lifecycle-fixture)
        buffer (atom nil)
        sink (fn [{:keys [content content/digest]}]
               (reset! buffer content)
               {:ok true :content/digest digest})
        stages [{:ledger-path preflight-ledger-path
                 :certificate-path preflight-certificate-path
                 :operation {:status :waiting-for-terminal-result
                             :role :proctor :agent-id "f27-proctor"
                             :job-id "preflight-job"}}
                {:ledger-path solve-ledger-path
                 :certificate-path solve-certificate-path
                 :operation {:status :waiting-for-terminal-result
                             :role :solver :agent-id "f27-solver"
                             :job-id "solver-round-2"}
                 :solver-progress {:rounds/completed 1 :rounds/max 50
                                   :round/active 2 :checkpoint/next 10}}
                {:ledger-path ledger-path
                 :certificate-path verify-certificate-path
                 :operation {:status :waiting-for-terminal-result
                             :role :proctor :agent-id "f27-proctor"
                             :job-id "verify-job"}
                 :solver-progress {:rounds/completed 2 :rounds/max 50
                                   :round/active nil :checkpoint/next 10}}]
        results
        (mapv (fn [stage]
                (let [result
                      (problem/project!
                       (merge stage
                              {:output-path (.resolve dir "problem.md")
                               :publication-directory (.resolve dir "publication-log")
                               :buffer-name "*problem: f27-p27*"
                               :expected-frame-id "f27"
                               :expected-problem-id "p27"
                               :buffer-sink sink}))]
                  (is (:ok result) (pr-str result))
                  (is (= @buffer (Files/readString (.resolve dir "problem.md"))))
                  result))
              stages)
        latest (edn/read-string
                (Files/readString (.resolve (.resolve dir "publication-log")
                                                "latest.edn")))
        transitions (->> (Files/readAllLines
                          (.resolve dir "problem-transitions.edn"))
                         (remove str/blank?)
                         (mapv edn/read-string))]
    (is (= [:preflight :solve :verify]
           (mapv #(get-in % [:projection :frame :phase]) results)))
    (is (= [nil 2 nil]
           (mapv #(get-in % [:projection :solver/progress :round/active]) results)))
    (is (= 3 (count (set (map #(get-in % [:publication-receipt :receipt/id])
                               results)))))
    (is (= [0 1 2] (mapv :event/sequence transitions)))
    (is (= [:preflight :solve :verify] (mapv :phase transitions)))
    (is (= ["preflight-job" "solver-round-2" "verify-job"]
           (mapv #(get-in % [:operation :job-id]) transitions)))
    (is (= (:receipt/id latest)
           (get-in (last results) [:publication-receipt :receipt/id])))))
