(ns futon3c.agency.obligations-reader-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agency.obligations :as obligations]
            [futon3c.agency.obligations-reader :as reader]
            [futon3c.agency.promise-log :as promise-log]))

(def t "2026-11-05T20:00:00Z")
(defn empty-response [path]
  (if (str/includes? path "/evidence?") {:entries [] :count 0}
      {:hyperedges [] :count 0}))

(defn- history-row [pid n type debtor creditor predecessor details]
  {:evidence/id (str "promise-history:" pid ":" n)
   :evidence/type type :evidence/author debtor
   :evidence/at (format "2026-11-05T19:%02d:00Z" n)
   :evidence/body
   (merge {:history/format 3 :history/promise-id pid
           :history/promise-sequence n
           :history/payload-edn
           (pr-str {:record {:id pid :agent debtor :beneficiary creditor
                             :fulfilment-criterion {:kind :manual}}})}
          details
          (when predecessor {:history/predecessor predecessor}))})

(defn- state-edge [pid status debtor creditor checkable?]
  {:hx/id (str "promise-state:" pid) :hx/type :promise/state
   :hx/endpoints (cond-> [pid (str "agent:" debtor) (str "agent:" creditor)
                          (str "promise-status:" (name status))]
                   checkable? (conj "promise-obligation:checkable"))
   :hx/props {:promise-id pid :status status :debtor debtor :creditor creditor
              :checkable? checkable?}})

(defn- hyperedge-response [edges path]
  (let [decoded (java.net.URLDecoder/decode path "UTF-8")
        type (some-> (second (re-find #"type=([^&]+)" decoded)) keyword)
        end (second (re-find #"end=([^&]+)" decoded))]
    {:hyperedges (filterv #(and (= type (:hx/type %))
                                (some #{end} (:hx/endpoints %))) edges)}))

(deftest open-union-checkable-selects-agent-parties-and-preserves-release-semantics
  (let [states [(state-edge "debt" :open "agent-a" "agent-b" true)
                (state-edge "credit" :open "agent-b" "agent-a" true)
                (state-edge "other" :open "agent-b" "agent-c" true)
                (state-edge "closed" :closed "agent-a" "agent-b" true)]
        creation #(history-row % 1 :promise/park-made
                               (if (= % "credit") "agent-b" "agent-a")
                               (if (= % "credit") "agent-a" "agent-b") nil {})
        closed-one (creation "closed")
        closed-two (history-row "closed" 2 :promise/released "agent-a" "agent-b"
                                {:sequence 1 :id (:evidence/id closed-one)
                                 :type :promise/park-made}
                                {:release/basis :explicit :release/role :creditor
                                 :release/by "agent-b" :act/stamp {:signer "agent-b"}})
        histories [(creation "debt") (creation "credit") closed-one closed-two]
        edges (into states (map promise-log/entry->hyperedge histories))
        paths (atom [])]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (swap! paths conj path)
                (if (str/includes? path "/hyperedges?")
                  (hyperedge-response edges path)
                  (empty-response path)))]
      (let [inputs (reader/read-inputs "http://store" "agent-a" t :current)
            result (obligations/obligations-as-of inputs "agent-a" t)]
        (is (= ["debt"] (mapv :obligation/id (:owes result))))
        (is (= ["credit"] (mapv :obligation/id (:owed result))))
        (is (= [[:released "closed"]]
               (mapv (juxt :status :obligation/id) (:ignored result))))
        (is (not-any? #(str/includes? % "tags=promise-history") @paths))))))

(deftest current-mode-omits-store-as-of-parameters
  (let [paths (atom [])]
    (binding [reader/*request!* (fn [_ _ path _]
                                 (swap! paths conj path) (empty-response path))]
      (let [result (reader/read-inputs "http://store" "agent-a" t :current)]
        (is (= :current (get-in result [:basis :mode])))
        (is (= t (get-in result [:basis :t])))
        (is (= :unpinned (get-in result [:basis :population :read :system-as-of])))
        (is (= t (get-in result [:basis :population :read :cutoff])))))
    (is (every? #(not (re-find #"(?:system|valid)-as-of=" %)) @paths))
    (is (some #(str/includes? % "end=agent%3Aagent-a") @paths))))

(deftest page-cap-refuses-and-names-source
  (let [n (atom 0)]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (if (str/includes? path "promise-status%3Aopen")
                  (let [i (swap! n inc)]
                    {:hyperedges [] :next-cursor (str "next-" i)})
                  (empty-response path)))]
      (try
        (reader/read-inputs "http://store" "a" t :current)
        (is false "expected page cap refusal")
        (catch clojure.lang.ExceptionInfo e
          (is (= :truncated-input (:reason (ex-data e))))
          (is (= :promise-states-open (:source (ex-data e))))
          (is (= reader/max-pages (:pages (ex-data e)))))))))

(deftest timeout-is-typed-with-source
  (binding [reader/*request!*
            (fn [& _] (throw (ex-info "query deadline" {:status 504})))]
    (try
      (reader/read-inputs "http://store" "a" t :as-of)
      (is false "expected timeout")
      (catch clojure.lang.ExceptionInfo e
        (is (= :store-timeout (:reason (ex-data e))))
        (is (= :promise-states-open (:source (ex-data e))))))))

(deftest as-of-before-state-log-is-typed-and-does-not-read
  (let [calls (atom 0)]
    (binding [reader/*request!* (fn [& _] (swap! calls inc))]
      (is (= :as-of-before-state-log
             (try (reader/read-inputs "http://store" "a"
                                      "2026-11-05T19:04:21Z" :as-of)
                  nil
                  (catch clojure.lang.ExceptionInfo error
                    (:reason (ex-data error))))))
      (is (zero? @calls)))))

(deftest fulfilment-check-survives-the-outcome-reader
  (let [check {:evidence/id "promise-check:x"
               :evidence/type :promise/fulfilment-check
               :evidence/body {:promise-id "x" :verdict :unfulfilled}}
        other {:evidence/id "promise-check:y"
               :evidence/type :promise/fulfilment-check
               :evidence/body {:promise-id "y" :verdict :unfulfilled}}
        states [(state-edge "x" :open "a" "b" true)]]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (cond
                  (str/includes? path "promise-outcome") {:entries [check other] :count 2}
                  (str/includes? path "/hyperedges?") (hyperedge-response states path)
                  :else (empty-response path)))]
      (is (= [check]
             (:promise-outcomes (reader/read-inputs "http://store" "a" t :as-of)))))))

(deftest check-for-an-unselected-promise-is-not-an-orphan
  ;; Live 2026-11-05: every agent showed the same 5-6 :orphan-check rows,
  ;; checks of closed promises whose history the bounded read never fetches.
  (let [check {:evidence/id "promise-check:closed"
               :evidence/type :promise/fulfilment-check
               :evidence/at "2026-11-05T19:30:00Z"
               :evidence/body {:promise-id "closed-elsewhere" :verdict :unfulfilled}}]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (if (str/includes? path "promise-outcome")
                  {:entries [check] :count 1}
                  (empty-response path)))]
      (let [inputs (reader/read-inputs "http://store" "agent-a" t :current)
            result (obligations/obligations-as-of inputs "agent-a" t)]
        (is (empty? (:incomplete result)))))))

(deftest full-endpoint-window-without-cursor-refuses
  ;; futon1b's type+end path returns no cursor, so 1000 rows may be a cut.
  (binding [reader/*request!*
            (fn [_ _ path _]
              (if (str/includes? path "promise-status%3Aopen")
                {:hyperedges (vec (repeat reader/page-limit {:hx/id "s"}))}
                (empty-response path)))]
    (is (= [:truncated-input :promise-states-open]
           (try (reader/read-inputs "http://store" "a" t :current) nil
                (catch clojure.lang.ExceptionInfo error
                  ((juxt :reason :source) (ex-data error))))))))

(deftest missing-middle-history-remains-an-incomplete-obligation
  (let [one (history-row "gap" 1 :promise/park-made "agent-a" "agent-b" nil {})
        three (history-row "gap" 3 :promise/woken "agent-a" "agent-b"
                           {:sequence 2 :id "promise-history:gap:2"
                            :type :promise/woken} {})
        edges (into [(state-edge "gap" :open "agent-a" "agent-b" true)]
                    (map promise-log/entry->hyperedge [one three]))]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (if (str/includes? path "/hyperedges?")
                  (hyperedge-response edges path)
                  (empty-response path)))]
      (let [inputs (reader/read-inputs "http://store" "agent-a" t :current)
            result (obligations/obligations-as-of inputs "agent-a" t)]
        (is (empty? (:owes result)))
        (is (= :missing-transition (get-in result [:incomplete 0 :reason])))))))

(deftest population-declares-actual-source-filters-and-counts
  (let [used {:evidence/id "o" :evidence/type :promise/lapsed
              :evidence/body {:promise-id "p"}}
        unused {:evidence/id "x" :evidence/type :something/else
                :evidence/body {:promise-id "p"}}
        states [(state-edge "p" :open "agent-a" "agent-b" false)]]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (cond
                  (str/includes? path "promise-outcome") {:entries [used unused]}
                  (str/includes? path "/hyperedges?") (hyperedge-response states path)
                  :else (empty-response path)))]
      (let [population (get-in (reader/read-inputs "http://store" "agent-a" t :as-of)
                               [:basis :population])]
        (is (= {:mode :as-of :system-as-of t :valid-as-of t}
               (:read population)))
        (is (= {:tags ["promise-outcome"]
                :types [:promise/fulfilled :promise/lapsed
                        :promise/fulfilment-check]}
               (get-in population [:sources 3 :filter])))
        (is (= 2 (get-in population [:sources 3 :rows-fetched])))
        (is (= 1 (get-in population [:sources 3 :rows-used])))))))

(deftest unreadable-agreement-is-kept-as-incomplete
  (let [bad {:hx/id "act:bad" :hx/type :agreement/record :hx/props {}}]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (if (str/includes? path "agreement%2Frecord")
                  {:hyperedges [bad] :count 1}
                  (empty-response path)))]
      (let [result (reader/read-inputs "http://store" "a" t :as-of)]
        (is (empty? (:agreements result)))
        (is (= :unreadable-agreement
               (get-in result [:reader-incomplete 0 :reason])))))))

(deftest agreement-offer-is-found-through-the-offerors-endpoint
  ;; Live 2026-09-29: an offer's ends are its author, seat and addressee, never
  ;; its own id, so looking it up by end=<offer id> found nothing and Joe's
  ;; accepted agreement never reached P9. The fake store answers `end` as
  ;; futon1b does: an edge matches only if the end is one of its endpoints.
  ;; Shapes are the live act:104d037c… offer and act:ba40d9aa… agreement.
  (let [scope {:description "doc"}
        offer {:hx/id "act:offer" :hx/type :offer/record
               :hx/endpoints ["agent:claude-17" "session:s" "agent:joe"]
               :hx/props {:act/harness {:kind :none :basis :producer-context
                                        :source-ref "route:futon3c.offer"}
                          :act/stamp {:executor "claude-17" :signer "claude-17"
                                      :authority {:grant "act:grant"}
                                      :executor-basis :declared}
                          :author "claude-17" :at "2026-09-28T10:00:00Z"
                          :addressee "joe"
                          :options [{:option/id "document" :option/label "Write it"
                                     :option/scope scope}]
                          :offer/schema 1
                          :seat {:agent "claude-17" :session "s"}}}
        agreement {:hx/id "act:agreement" :hx/type :agreement/record
                   :hx/endpoints ["act:offer" "agent:claude-17" "agent:joe" "emacs-e"]
                   :hx/props {:agreement/acceptance-evidence "emacs-e"
                              :act/harness {:kind :none :basis :producer-context
                                            :source-ref "route:futon3c.agreement"}
                              :agreement/offer "act:offer" :agreement/schema 1
                              :act/stamp {:executor "joe" :signer "joe"
                                          :authority {:operator true}
                                          :executor-basis :session-bound}
                              :agreement/offeror "claude-17" :agreement/acceptor "joe"
                              :agreement/at "2026-09-28T11:00:00Z"
                              :agreement/option-id "document"
                              :agreement/scope scope}}
        edges [offer agreement]]
    (binding [reader/*request!*
              (fn [_ _ path _]
                (if (str/includes? path "/hyperedges?")
                  (let [q (java.net.URLDecoder/decode path "UTF-8")
                        type (keyword (second (re-find #"type=([^&]+)" q)))
                        end (second (re-find #"end=([^&]+)" q))]
                    {:hyperedges (filterv #(and (= type (:hx/type %))
                                                (some #{end} (:hx/endpoints %)))
                                          edges)})
                  (empty-response path)))]
      (let [result (reader/read-inputs "http://store" "claude-17" t :current)]
        (is (= ["act:agreement"] (mapv :id (:agreements result))))
        (is (= 1 (get-in result [:basis :population :sources 5 :rows-used])))
        (is (empty? (filter #(= :unknown-offer (:reason %))
                            (get-in result [:basis :reader-incomplete]))))))))
