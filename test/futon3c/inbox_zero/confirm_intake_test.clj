(ns futon3c.inbox-zero.confirm-intake-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [futon3.inbox-zero.state :as state]
            [futon3c.inbox-zero.confirm-intake :as intake])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def seat-id "seat:claude-3:7cdc25b0-2189-4f90-801e-c517e7f37d4d")
(def path-key {:repo/id "futon3c-d" :worktree/id "worktree:tracer"
               :path "scripts/session-cost.py"})
(def input {:agent "claude-3" :session "7cdc25b0-2189-4f90-801e-c517e7f37d4d"
            :path-key path-key :response-id "response:one"})

(defn temp-dir []
  (.toFile (Files/createTempDirectory "confirm-intake-"
                                      (make-array FileAttribute 0))))

(defn fixture-state []
  (state/replay
   [{:record/type :inbox-zero/session-seat :seat/id seat-id
     :agent/id "claude-3" :session/id "7cdc25b0-2189-4f90-801e-c517e7f37d4d"
     :surface :agent-tool-stream :host/id "zone"
     :workspace/root "/repo" :observed-at #inst "2026-08-24T11:00:00Z"
     :registry-witness {:endpoint :invoke-tool-result
                        :session/id "7cdc25b0-2189-4f90-801e-c517e7f37d4d"}}
    {:record/type :inbox-zero/file-observation :observation/id "observation:path"
     :repo/id "futon3c-d" :repo/root "/repo" :worktree/id "worktree:tracer"
     :path "scripts/session-cost.py" :git/status :modified
     :content/hash "sha256:path" :head/sha "head"
     :observed-at #inst "2026-08-24T11:01:00Z" :source :multi-watcher}]))

(def candidate
  {:seat/id seat-id :rank 1 :confidence :corroborated
   :evidence [{:evidence/type :substrate-modification-mention
               :at #inst "2026-08-24T08:21:54Z" :source/id "emacs-6427"}
              {:evidence/type :filesystem-mtime
               :at #inst "2026-08-24T08:21:27Z" :source/id "stat:path"}]
   :against []})

(defn options [dir result now]
  {:state-path "/unused/state.edn" :witness-dir (.getPath dir)
   :load-state-fn (fn [_] (fixture-state))
   :build-bundle-fn (fn [& _] {:fixture true})
   :infer-fn (fn [& _] result)
   :now-fn (constantly now)})

(def proposed {:path/key path-key :verdict :propose :candidates [candidate]})

(deftest happy-path-publishes-seat-and-claim-and-repeat-keeps-first-provenance
  (let [dir (temp-dir)
        first-result (intake/confirm-attribution!
                      input (options dir proposed #inst "2026-08-24T12:00:00Z"))
        first-records (mapv #(edn/read-string (slurp %)) (.listFiles dir))
        second-result (intake/confirm-attribution!
                       (assoc input :response-id "response:later")
                       (options dir proposed #inst "2026-08-24T13:00:00Z"))
        records (mapv #(edn/read-string (slurp %)) (.listFiles dir))
        claim (first (filter #(= :inbox-zero/session-file-claim (:record/type %))
                             records))]
    (is (:ok first-result))
    (is (false? (:already? first-result)))
    (is (= 2 (count first-records)))
    (is (= #{:inbox-zero/session-seat :inbox-zero/session-file-claim}
           (set (map :record/type records))))
    (is (= (:claim/id first-result) (:claim/id second-result)))
    (is (true? (:already? second-result)))
    (is (= 2 (count records)))
    (is (= "response:one" (:witness/id claim)))
    (is (= #inst "2026-08-24T12:00:00Z" (:first-observed-at claim)))))

(deftest wrong-caller-is-refused-without-candidate-leak-or-write
  (let [dir (temp-dir)
        other (assoc candidate :seat/id "seat:claude-4:other")
        result (intake/confirm-attribution!
                input (options dir (assoc proposed :candidates [other])
                               #inst "2026-08-24T12:00:00Z"))]
    (is (= {:ok false :refused :not-your-attribution} result))
    (is (not (some #(= "seat:claude-4:other" %) (vals result))))
    (is (empty? (.listFiles dir)))))

(deftest non-proposal-is-refused-without-write
  (let [dir (temp-dir)
        result (intake/confirm-attribution!
                input (options dir {:path/key path-key :verdict :insufficient
                                    :candidates []}
                               #inst "2026-08-24T12:00:00Z"))]
    (is (= {:ok false :refused true :verdict :insufficient} result))
    (is (empty? (.listFiles dir)))))

(deftest malformed-input-is-typed-and-writes-nothing
  (let [dir (temp-dir)
        error (try
                (intake/confirm-attribution!
                 (dissoc input :session) (options dir proposed #inst "2026-08-24T12:00:00Z"))
                nil
                (catch clojure.lang.ExceptionInfo caught caught))]
    (is (= :inbox-zero/invalid-confirmation-input (:error/type (ex-data error))))
    (is (empty? (.listFiles dir)))))
