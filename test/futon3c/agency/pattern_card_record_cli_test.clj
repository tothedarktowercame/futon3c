(ns futon3c.agency.pattern-card-record-cli-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.rule-record :as store]
            [futon3c.agency.pattern-card-record-cli :as cli]))

(def harness {:kind :none :basis :producer-context :source-ref "test:p10-2a-3"})
(def grant-id "act:own-acts")
(def stamp {:executor "claude-17" :signer "claude-17"
            :authority {:grant grant-id} :executor-basis :declared})
(def grant
  {:hx/id grant-id :hx/type :grant/record
   :hx/props
   {:grant/grantor "joe" :grant/grantee "*" :grant/basis :explicit
    :grant/scope {:description "own acts"
                  :act-kinds [:pattern-card/selection :act/withdrawal]
                  :own-acts-only true}
    :grant/interval {:from "2026-09-28T10:00:00Z"}
    :grant/source {:id "e:joe" :author "joe" :at "2026-09-28T10:00:00Z"
                   :quote "own acts"}}})
(def selection-record
  {:kind :pattern-card/selection :author "claude-17" :agent "claude-17"
   :session "session-1" :at "2026-09-28T11:00:00Z" :pattern-id "pattern/a"})
(def selection-request {:record selection-record :idempotency-key "p10-selection-1"})
(def target
  {:hx/id "act:card-a" :hx/type :pattern-card/selection
   :hx/valid-time "2026-09-28T11:00:00Z"
   :hx/endpoints ["agent:claude-17" "session:session-1" "pattern:pattern/a"]
   :hx/props (-> selection-record
                 (dissoc :kind)
                 (assoc :act/harness harness :act/stamp stamp
                        :pattern-card/schema 1))})
(def withdrawal-record
  {:kind :act/withdrawal :author "claude-17" :at "2026-09-28T11:30:00Z"
   :target "act:card-a" :status :effective :basis {:kind :self}})
(def withdrawal-request {:record withdrawal-record :idempotency-key "p10-withdraw-1"})

(defn reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest selection-payload-is-minted-and-stamped
  (let [payload (cli/selection-payload selection-request harness stamp)]
    (is (nil? (:hx/id payload)))
    (is (true? (:hx/mint-id payload)))
    (is (= "p10-selection-1" (:hx/idempotency-key payload)))
    (is (= 1 (get-in payload [:hx/props :pattern-card/schema])))
    (is (= (:at selection-record) (get-in payload [:hx/props :at])))
    (is (= harness (get-in payload [:hx/props :act/harness])))
    (is (= stamp (get-in payload [:hx/props :act/stamp])))))

(deftest withdrawal-payload-validates-the-stored-target
  (let [payload (cli/withdrawal-payload withdrawal-request target harness stamp)]
    (is (true? (:hx/mint-id payload)))
    (is (= :act/withdrawal (:hx/type payload)))
    (is (= "p10-withdraw-1" (:hx/idempotency-key payload)))
    (is (= 1 (get-in payload [:hx/props :pattern-card/schema])))
    (is (= harness (get-in payload [:hx/props :act/harness])))
    (is (= stamp (get-in payload [:hx/props :act/stamp])))))

(deftest target-refusals-are-typed
  (is (= :target-absent
         (reason #(cli/withdrawal-payload withdrawal-request nil harness stamp))))
  (is (= :target-wrong-type
         (reason #(cli/withdrawal-payload
                   withdrawal-request (assoc target :hx/type :rule/record) harness stamp))))
  (is (= :not-author
         (reason #(cli/withdrawal-payload
                   (assoc-in withdrawal-request [:record :author] "agent-b")
                   target harness stamp)))))

(deftest live-target-uses-http-and-translates-absence
  (testing "stored target"
    (with-redefs [store/request! (fn [base method path body]
                                   (is (= "http://store" base))
                                   (is (= "GET" method))
                                   (is (= "/api/alpha/hyperedge/act%3Acard-a" path))
                                   (is (nil? body))
                                   target)]
      (is (= target (cli/live-target! "http://store" "act:card-a")))))
  (testing "404"
    (with-redefs [store/request! (fn [& _]
                                   (throw (ex-info "missing" {:status 404})))]
      (is (= :target-absent
             (reason #(cli/live-target! "http://store" "act:missing")))))))

(deftest harness-is-required-and-caller-storage-fields-are-refused
  (is (= :invalid-harness-map
         (reason #(cli/selection-payload selection-request nil stamp))))
  (is (= :invalid-stamp-map
         (reason #(cli/selection-payload selection-request harness nil))))
  (is (= :caller-assigned-storage-field
         (reason #(cli/selection-payload
                   (assoc-in selection-request [:record :id] "act:caller") harness stamp))))
  (is (= :caller-assigned-storage-field
         (reason #(cli/selection-payload
                   (assoc-in selection-request [:record :act/harness] harness) harness stamp))))
  (is (= :caller-assigned-storage-field
         (reason #(cli/selection-payload
                   (assoc-in selection-request [:record :act/stamp] stamp) harness stamp)))))

(deftest cli-requires-and-builds-the-act-stamp
  (is (= :missing-stamp-field (reason #(cli/parse-stamp-flags []))))
  (let [{:keys [args stamp]}
        (cli/parse-stamp-flags
         ["file.edn" "--act-executor" "claude-17"
          "--act-signer" "claude-17" "--act-grant-id" grant-id
          "--executor-basis" "declared" "--write"])]
    (is (= ["file.edn" "--write"] args))
    (is (= {:executor "claude-17" :signer "claude-17"
            :authority {:grant grant-id} :executor-basis :declared}
           stamp))))

(deftest write-selection-verifies-through-system-as-of-list
  (let [payload (cli/selection-payload selection-request harness stamp)
        id "act:minted-selection"
        listed (-> payload
                   (dissoc :hx/mint-id :hx/idempotency-key :hx/valid-time)
                   (assoc :hx/id id))
        calls (atom [])]
    (with-redefs [store/request!
                  (fn [_ method path body]
                    (swap! calls conj [method path body])
                    (cond
                      (and (= method "GET") (str/includes? path "act%3Aown-acts")) grant
                      (= method "POST") {:ok true :hx/id id :minted? true}
                      (re-find #"type=pattern-card%2Fselection" path)
                      {:hyperedges [listed]}
                      (re-find #"type=act%2Fwithdrawal" path)
                      {:hyperedges []}
                      :else (throw (ex-info "unexpected fake request" {:path path}))))]
      (let [result (cli/write-selection! "http://store" selection-request harness stamp)]
        (is (= id (get-in result [:receipt :hx/id])))
        (is (true? (get-in result [:receipt :verified?])))
        (is (= id (get-in result [:card-as-of :active :id])))
        (is (every? #(re-find #"system-as-of=" (second %))
                    (filter #(and (= "GET" (first %))
                                  (str/includes? (second %) "/hyperedges?"))
                            @calls)))))))

(deftest a-stranded-stored-record-does-not-block-readback
  (let [payload (cli/selection-payload selection-request harness stamp)
        id "act:minted-selection"
        listed (-> payload
                   (dissoc :hx/mint-id :hx/idempotency-key :hx/valid-time)
                   (assoc :hx/id id))
        ;; as act:cb9bff2a… was stored: no :at in props, no valid time returned
        stranded (-> listed
                     (assoc :hx/id "act:stranded")
                     (update :hx/props dissoc :at))]
    (with-redefs [store/request!
                  (fn [_ method path _]
                    (cond
                      (and (= method "GET") (str/includes? path "act%3Aown-acts")) grant
                      (= method "POST") {:ok true :hx/id id :minted? true}
                      (re-find #"type=pattern-card%2Fselection" path)
                      {:hyperedges [stranded listed]}
                      :else {:hyperedges []}))]
      (let [result (cli/write-selection! "http://store" selection-request harness stamp)]
        (is (= id (get-in result [:card-as-of :active :id])))
        (is (= [{:hx/id "act:stranded" :reason :missing-at}]
               (:unreadable result)))))))
