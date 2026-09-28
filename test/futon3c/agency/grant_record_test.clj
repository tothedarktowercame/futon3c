(ns futon3c.agency.grant-record-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [futon3c.agency.grant-record :as grant]
            [futon3c.agency.rule-record :as store]))

(def lab "holes/labs/M-象-2000/")
(def request (edn/read-string (slurp (str lab "P3-1-grant-1620.edn"))))
(def real-record (:record request))
(def context (edn/read-string (slurp (str lab "P3-1-source-fixture.edn"))))
(def target :kimi/create-target-enforcement)
(def from (get-in real-record [:grant/interval :from]))
(def until "2026-09-25T00:00:00Z")
(defn node [id r] {:hx/id id :hx/type :grant/record :hx/props (assoc r :grant/schema 1)})
(def root (node "act:root" (assoc-in real-record [:grant/interval :until] until)))
(defn reason [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest real-explicit-record-and-payload
  (is (= real-record (grant/validate! real-record context)))
  (is (= (dissoc real-record :grant/parent) (grant/validate! (dissoc real-record :grant/parent) context)))
  (let [p (grant/payload request context)]
    (is (= :grant/record (:hx/type p)))
    (is (true? (:hx/mint-id p)))
    (is (= from (:hx/valid-time p)))
    (is (nil? (:hx/id p)))))

(deftest refusals-are-mutations-of-real-record
  (doseq [[r c expected]
          [[nil context :missing-grant]
           [(dissoc real-record :grant/source) context :missing-grant]
           [real-record (assoc context :evidence []) :unsourced-grant]
           [real-record (assoc-in context [:evidence 0 :evidence/author] "someone-else") :source-author-mismatch]
           [(assoc real-record :grant/basis :inferred) context :interpretation-not-grant]
           [(assoc real-record :grant/grantor "claude-11"
                   :grant/source (assoc (:grant/source real-record) :author "claude-11"))
            context :non-operator-root]
           [(assoc-in real-record [:grant/source :quote] "yes do an unrelated task") context :unsourced-grant]
           [(assoc-in real-record [:grant/interval :until] from) context :invalid-interval]]]
    (is (= expected (reason #(grant/validate! r c))) (str expected))))

(def child (-> real-record
               (assoc :grant/parent "act:root" :grant/grantor "claude-11" :grant/grantee "codex-5")
               (assoc-in [:grant/source :author] "claude-11")
               (assoc-in [:grant/source :id] "e-child")
               (assoc-in [:grant/interval :until] until)))
(def child-source (-> (first (:evidence context))
                      (assoc :evidence/id "e-child" :evidence/author "claude-11")))
(def child-context (-> context (assoc :records [root]) (update :evidence conj child-source)))

(deftest delegation-refusals
  (is (= child (grant/validate! child child-context)))
  (doseq [[r c expected]
          [[(assoc-in child [:grant/scope :act-kinds] [:unrelated]) child-context :scope-exceeds-parent]
           [(assoc child :grant/scope {:description "text cannot prove containment"}) child-context :scope-unchecked]
           [(assoc-in child [:grant/interval :until] nil) child-context :interval-exceeds-parent]
           [child (assoc child-context :records [(assoc-in root [:hx/props :grant/interval :from] "2026-09-24T16:21:00Z")]) :interval-exceeds-parent]
           [child (assoc child-context :records []) :broken-parent-chain]
           [(assoc child :grant/grantor "other" :grant/source (assoc (:grant/source child) :author "other")) child-context :delegation-identity-mismatch]
           [child (assoc child-context :records [(assoc-in root [:hx/props :grant/parent] "act:root")]) :parent-cycle]]]
    (is (= expected (reason #(grant/validate! r c))) (str expected))))

(deftest covers-checks-whole-chain-and-half-open-time
  (let [records [root (node "act:child" child)]]
    (is (= ["act:root" "act:child"] (mapv :hx/id (:chain (grant/grant-covers? records "codex-5" target from)))))
    (doseq [[kind time expected] [[target "2026-09-24T16:20:00Z" :out-of-time]
                                 [target until :out-of-time]
                                 [target "2026-09-26T00:00:00Z" :out-of-time]
                                 [:unrelated from :out-of-scope]]]
      (is (= {:status :no-grant :reason expected} (grant/grant-covers? records "codex-5" kind time))))
    (is (= :broken-parent-chain (:reason (grant/grant-covers? [(second records)] "codex-5" target from))))
    (is (= :scope-unchecked (:reason (grant/grant-covers? [(node "act:text" (assoc real-record :grant/scope {:description "anything"}))] "claude-11" target from))))))

(deftest adoption-reference-is-query-only
  (let [event {:at from :source {:ref (str "evidence:" (get-in real-record [:grant/source :id]))}}
        original root]
    (is (= {:status :recorded :act-id "act:root"}
           (grant/grant-status-for [root] "claude-11" target event)))
    (is (= :unrecorded (:status (grant/grant-status-for [root] "claude-11" :unrelated event))))
    (is (= :unrecorded (:status (grant/grant-status-for [root] "claude-11" target (assoc-in event [:source :ref] "evidence:other")))))
    (is (= {:status :recorded :act-id "act:root"}
           (grant/grant-status-for [root] "claude-11"
                                  "act:4b526112-dd5d-4765-a8a6-ed8701d0089c" event)))
    (is (= original root))))

(deftest own-acts-root-grants
  (let [wild-record (-> real-record
                        (assoc :grant/grantee "*")
                        (assoc-in [:grant/scope :own-acts-only] true))
        wild (node "act:any-own" wild-record)
        options {:leaf-id "act:any-own" :target-signer "claude-17"}]
    (is (= :granted
           (:status (grant/grant-covers? [wild] "claude-17" target from options))))
    (is (= {:status :no-grant :reason :not-own-act}
           (grant/grant-covers? [wild] "claude-17" target from
                                (assoc options :target-signer "codex-5"))))
    (is (= {:status :no-grant :reason :not-own-act}
           (grant/grant-covers? [wild] "claude-17" target from
                                {:leaf-id "act:any-own"})))
    (is (= {:status :no-grant :reason :wildcard-not-a-grantee}
           (grant/grant-covers? [wild] "*" target from
                                {:leaf-id "act:any-own" :target-signer "*"})))
    (is (= :wildcard-needs-own-acts
           (reason #(grant/validate! (assoc real-record :grant/grantee "*") context))))
    (is (= :wildcard-not-root
           (reason #(grant/validate! (assoc wild-record :grant/parent "act:root")
                                     (assoc context :records [root])))))
    (is (= :wildcard-not-root
           (reason #(grant/validate! (-> wild-record
                                         (assoc :grant/grantor "claude-17")
                                         (assoc-in [:grant/source :author] "claude-17"))
                                     context))))
    (is (= :invalid-scope
           (reason #(grant/validate! (assoc-in real-record
                                               [:grant/scope :own-acts-only] false)
                                     context))))
    (let [child-under-wildcard (-> child
                                   (assoc :grant/grantor "*" :grant/parent "act:any-own")
                                   (assoc-in [:grant/source :author] "*"))]
      (is (= :wildcard-not-delegable
             (reason #(grant/validate! child-under-wildcard
                                       (assoc child-context :records [wild]))))))
    (testing "a named own-acts grant applies the same signer check"
      (let [named (node "act:named-own"
                        (assoc-in real-record [:grant/scope :own-acts-only] true))]
        (is (= :not-own-act
               (:reason (grant/grant-covers? [named] "claude-11" target from
                                             {:target-signer "codex-5"}))))))))

(deftest write-rechecks-source-and-verifies-minted-readback
  (let [calls (atom []) p (grant/payload request context)]
    (with-redefs [store/request! (fn [_ method path value]
                                  (swap! calls conj [method path value])
                                  (cond
                                    (= method "POST") {:ok true :hx/id "act:new"}
                                    (= path "/api/alpha/hyperedge/act%3Anew") (assoc p :hx/id "act:new")
                                    :else (first (:evidence context))))]
      (is (true? (:verified? (grant/write! "http://unused" request))))
      (is (= ["GET" "POST" "GET"] (mapv first @calls)))))
  (with-redefs [store/request! (fn [_ method _ _]
                                (if (= method "POST") {:ok true :hx/id "act:new"}
                                    (first (:evidence context))))]
    (is (= :readback-mismatch (reason #(grant/write! "http://unused" request))))))

(deftest stored-shape-without-nils-validates-and-matches-payload
  (let [p (grant/payload request context)
        stored (:hx/props p)
        walk (fn walk [m] (some (fn [[_ v]] (or (nil? v) (and (map? v) (walk v)))) m))]
    (is (not (walk stored)))
    (is (not (contains? (:grant/interval stored) :until)))
    (is (= (dissoc stored :grant/schema) (grant/validate! (dissoc stored :grant/schema) context)))
    (is (= :invalid-interval
           (reason #(grant/validate! (assoc-in real-record [:grant/interval :to] until) context))))))
