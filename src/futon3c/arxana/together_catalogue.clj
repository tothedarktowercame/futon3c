(ns futon3c.arxana.together-catalogue
  "Offline Demo A records and finite probes for Together. No catalogue writes."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [futon3c.arxana.design-catalogue :as catalogue]
            [futon3c.arxana.together :as together]
            [futon3c.arxana.together-http :as together-http]
            [futon3c.arxana.together-structure :as structure])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.nio.file Files]
           [java.time Duration]))

(def features
  [{:feature :convergence :kind :decision
    :statement "The hub and every client place an inserted character by one rule (RGA: after its left neighbour, skipping characters with greater ids), so concurrent entries applied in any order give the same text and ids."
    :repo "futon3c" :revision "bff8826dfa5ffe8111e3339593ae4fa5216f43d5" :path "resources/arxana/together/conformance.json" :section "all cases" :probe :convergence}
   {:feature :ids-not-positions :kind :decision
    :statement "An edit names character ids ([site counter], a Lamport counter), never buffer positions; an id is chosen by the editor and is unique without asking the hub."
    :repo "futon3c" :revision "a13d9a71bb525d3474574959dd1f64c2b70a2070" :path "src/futon3c/arxana/together.clj" :section "check-op, next-counter" :probe :ids-not-positions}
   {:feature :log-is-the-record :kind :decision
    :statement "The hub's append-only log is the record: the document is rebuilt from the log alone."
    :repo "futon3c" :revision "a13d9a71bb525d3474574959dd1f64c2b70a2070" :path "src/futon3c/arxana/together.clj" :section "submit, rebuild" :probe :log-is-the-record}
   {:feature :authorship-checked :kind :constraint
    :statement "The hub refuses an entry whose id's site does not belong to the connection's identity."
    :repo "futon3c" :revision "a13d9a71bb525d3474574959dd1f64c2b70a2070" :path "src/futon3c/arxana/together.clj" :section "site-of?, check-op" :probe :authorship-checked}
   {:feature :provenance-fixed-at-insert :kind :decision
    :statement "Author and source attributes are fixed when a character is inserted and travel with it; deleting keeps the character and its attributes in the list."
    :repo "futon3c" :revision "a13d9a71bb525d3474574959dd1f64c2b70a2070" :path "src/futon3c/arxana/together.clj" :section "apply-op, runs, from-runs" :probe :provenance-fixed-at-insert}
   {:feature :repeated-insert-acknowledged :kind :decision
    :statement "An insert identical to one already logged from the same person is answered with that entry's revision, so a retry after a timeout loses nothing."
    :repo "futon3c" :revision "33043dadad6077f026ce3a93192f6c399ef80018" :path "src/futon3c/arxana/together_http.clj" :section "submit!" :probe :repeated-insert-acknowledged}
   {:feature :seeding-alone :kind :decision
    :statement "A client seeding a document from text it already holds marks its operations alone; the hub accepts them only while nobody else has written, so of two simultaneous seeders one is refused."
    :repo "futon3c" :revision "33043dadad6077f026ce3a93192f6c399ef80018" :path "src/futon3c/arxana/together_http.clj" :section "submit!" :probe :seeding-alone}
   {:feature :loopback-identity :kind :constraint
    :statement "The hub takes identity from the X-Together-Editor header and therefore binds loopback only; a proxy must set that header from its authenticated user."
    :repo "futon3c" :revision "33043dadad6077f026ce3a93192f6c399ef80018" :path "src/futon3c/arxana/together_http.clj" :section "start!, handler" :probe :loopback-identity}
   {:feature :structure-is-serialised :kind :decision
    :statement "Nodes, hyperedges, versions and grouping change by serialised acts at the hub, separate from live text; a refused act is kept in the log."
    :repo "futon3c" :revision "4f1eada00a574f756f1d2c1571280ea5f6a36c29" :path "src/futon3c/arxana/together_structure.clj" :section "submit, rebuild" :probe :structure-is-serialised}
   {:feature :regroup-cycle-refused :kind :constraint
    :statement "A group or regroup that would make a thing its own ancestor is refused."
    :repo "futon3c" :revision "4f1eada00a574f756f1d2c1571280ea5f6a36c29" :path "src/futon3c/arxana/together_structure.clj" :section "cycle?, check-op" :probe :regroup-cycle-refused}
   {:feature :emacs-client-conforms :kind :decision
    :statement "The Emacs client applies the same rule and passes the shared conformance fixture."
    :repo "futon4a" :revision "7ab10379135e25264a444b62fb83d53f98fc61df" :path "lisp/futon4a-together.el" :section "whole file" :probe :emacs-client-conforms}
   {:feature :browser-client-conforms :kind :decision
    :statement "The browser client applies the same rule and passes the shared conformance fixture."
    :repo "futon3c" :revision "33043dadad6077f026ce3a93192f6c399ef80018" :path "resources/arxana/together/web/together.js" :section "whole file" :probe :browser-client-conforms}
   {:feature :peer-version-stays-possible :kind :decision
    :statement "Ids unique without the hub, operations by id, entries recording what the editor had seen, and rebuild from the log are kept so a peer-to-peer version stays possible."
    :repo "futon4" :revision "3179c049cdb7fcd5e86147709c1374c176ca4f6e" :path "holes/missions/M-arxana-together.md" :section "Requirements kept so a peer version stays possible"}])

(defn- outcome [violations]
  {:result (if (seq violations) :violation :pass) :violations (vec violations)})

(defn- insert [site counter after text attrs]
  {:op :insert :id [site counter] :after after :text text :attrs attrs})

(defn- submit-all [ops]
  (reduce (fn [hub [identity op]]
            (:hub (together/submit hub op identity (count (:log hub)) "2026-11-20T00:00:00Z")))
          {:doc together/empty-doc :log []} ops))

(defn- convergence-probe []
  (let [cases (json/parse-string (slurp (io/resource "arxana/together/conformance.json")) true)]
    (outcome
     (for [{:keys [name ops text ids orders]} cases
           order (or orders [(vec (range (count ops)))])
           :let [ops (mapv #(update % :op keyword) ops)
                 doc (reduce together/apply-op together/empty-doc (map ops order))]
           :when (or (not= text (together/text doc)) (not= ids (together/ids doc)))]
       {:case name :order order :expected {:text text :ids ids}
        :actual {:text (together/text doc) :ids (together/ids doc)}}))))

(defn- ids-probe []
  (let [base (together/apply-op [] (insert "root" 1 nil "r" {}))
        counter (together/next-counter base)
        a (insert "a" counter ["root" 1] "a" {})
        b (insert "b" counter ["root" 1] "b" {})
        docs [(-> base (together/apply-op a) (together/apply-op b))
              (-> base (together/apply-op b) (together/apply-op a))]]
    (outcome (concat
              (when-not (and (= (first docs) (second docs)) (= 3 (count (first docs))))
                [{:reason :concurrent-id-refused-or-diverged}])
              (when-not (= :id-in-use (together/check-op (first docs) a "a"))
                [{:reason :reused-id-accepted}])))))

(defn- log-probe []
  (let [ops [["joe" (insert "joe.web" 1 nil "ab" {:author "joe"})]
             ["rob" (insert "rob" 3 ["joe.web" 1] "X" {:author "rob"})]
             ["joe" {:op :delete :ids [["joe.web" 2]]}]]
        hub (submit-all ops)]
    (outcome (when-not (= (:doc hub) (together/rebuild (:log hub))
                          (together/rebuild (reverse (:log hub))))
               [{:reason :rebuild-differs}]))))

(defn- authorship-probe []
  (let [doc [] wrong (insert "joe.web" 1 nil "x" {}) good (insert "joe" 2 nil "x" {})]
    (outcome (concat
              (when-not (= :wrong-author (together/check-op doc wrong "rob")) [{:case :wrong-author}])
              (when (together/check-op doc wrong "joe") [{:case :surface-author}])
              (when (together/check-op doc good "joe") [{:case :plain-author}])))))

(defn- provenance-probe []
  (let [original {:author "joe" :source "emacs"}
        doc (-> []
                (together/apply-op (insert "joe" 1 nil "ab" original))
                (together/apply-op {:op :delete :ids [["joe" 1]]})
                (together/apply-op (insert "rob" 3 ["joe" 1] "X" {:author "rob" :source "web"})))
        rebuilt (together/from-runs (together/runs doc))
        originals (filter #(= "joe" (first (:id %))) rebuilt)]
    (outcome (concat
              (when-not (= doc rebuilt) [{:reason :snapshot-roundtrip}])
              (when-not (every? #(= original (:attrs %)) originals) [{:reason :attrs-changed}])
              (when-not (:deleted? (first originals)) [{:reason :tombstone-lost}])))))

(defn- temp-dir []
  (str (Files/createTempDirectory "together-catalogue-" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- remove-dir! [dir]
  (doseq [file (reverse (file-seq (io/file dir)))] (io/delete-file file)))

(defn- request [server method path editor body]
  (let [builder (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" (:port server) path)))
                    (.timeout (Duration/ofSeconds 10)))
        _ (when editor (.header builder "X-Together-Editor" editor))
        _ (if (= method :post)
            (.POST builder (HttpRequest$BodyPublishers/ofString (json/generate-string body)))
            (.GET builder))
        response (.send (HttpClient/newHttpClient) (.build builder) (HttpResponse$BodyHandlers/ofString))]
    {:status (.statusCode response) :body (json/parse-string (.body response) true)}))

(defn- with-server [f]
  (let [dir (temp-dir) server (atom nil)]
    (try
      (reset! server (together-http/start! {:port 0 :dir dir}))
      (f @server dir)
      (finally (when @server (together-http/stop! @server)) (remove-dir! dir)))))

(defn- post-body [op rev & [alone]]
  (cond-> {:parents {:rev rev} :op (update op :op name)} alone (assoc :alone true)))

(defn- retry-probe []
  (with-server
    (fn [server dir]
      (let [op (insert "joe" 1 nil "x" {:author "joe"})
            a (request server :post "/together/v1/doc/retry/ops" "joe" (post-body op 0))
            b (request server :post "/together/v1/doc/retry/ops" "joe" (post-body op 0))
            changed (request server :post "/together/v1/doc/retry/ops" "joe"
                             (post-body (assoc op :text "y") 1))
            lines (count (line-seq (io/reader (io/file dir "retry.log.edn"))))]
        (outcome (concat
                  (when-not (= [200 200 409] (mapv :status [a b changed])) [{:reason :statuses}])
                  (when-not (= (get-in a [:body :rev]) (get-in b [:body :rev])) [{:reason :revision}])
                  (when-not (= 1 lines) [{:reason :duplicate-log :lines lines}])))))))

(defn- seeding-probe []
  (with-server
    (fn [server _]
      (let [a (request server :post "/together/v1/doc/seed/ops" "joe"
                       (post-body (insert "joe" 1 nil "seed" {}) 0 true))
            b (request server :post "/together/v1/doc/seed/ops" "rob"
                       (post-body (insert "rob" 1 nil "seed" {}) 0 true))
            state (request server :get "/together/v1/doc/seed/state" "observer" nil)]
        (outcome (concat
                  (when-not (= [200 409] (mapv :status [a b])) [{:reason :statuses}])
                  (when-not (= "not-alone" (get-in b [:body :error])) [{:reason :wrong-refusal}])
                  (when-not (= "seed" (together/text (together/from-runs (get-in state [:body :runs]))))
                    [{:reason :text-duplicated}])))))))

(defn- loopback-probe []
  (let [dir (temp-dir)]
    (try
      (let [non-loopback (try (together-http/start! {:host "0.0.0.0" :port 0 :dir dir}) nil
                              (catch clojure.lang.ExceptionInfo e (:together/error (ex-data e))))
            missing-header (with-server (fn [server _]
                                          (request server :get "/together/v1/health" nil nil)))]
        (outcome (concat
                  (when-not (= :non-loopback non-loopback) [{:reason :non-loopback-accepted}])
                  (when-not (= 401 (:status missing-header)) [{:reason :missing-header-accepted}]))))
      (finally (remove-dir! dir)))))

(defn- node [id] {:op :add-node :id id :kind "fixture" :attrs {}})
(defn- group-op [child parent] {:op :group :child child :parent parent})

(defn- structure-probe []
  (let [ops [(node "a") (node "b") {:op :add-edge :id "e" :type "pair" :ends ["a" "b"]}
             {:op :supersede :id "v" :replaces "a" :kind "version" :attrs {}}
             (group-op "b" "a") (group-op "missing" "a")]
        hub (reduce (fn [hub op] (:hub (structure/submit hub op "joe" "2026-11-20T00:00:00Z")))
                    {:state structure/empty-state :log []} ops)]
    (outcome (concat
              (when-not (= (count ops) (count (:log hub))) [{:reason :act-not-logged}])
              (when-not (= :unknown-id (:refused (peek (:log hub)))) [{:reason :refusal-not-marked}])
              (when-not (= (:state hub) (structure/rebuild (:log hub))) [{:reason :rebuild-differs}])))))

(defn- cycle-probe []
  (let [base (reduce structure/apply-op structure/empty-state (map node ["a" "b" "c" "d"]))
        chain (-> base (structure/apply-op (group-op "b" "a")) (structure/apply-op (group-op "c" "b")))
        regroup {:op :regroup :child "b" :from "a" :to "c"}
        cycle (group-op "a" "c")
        diamond (-> base (structure/apply-op (group-op "b" "a"))
                    (structure/apply-op (group-op "c" "a"))
                    (structure/apply-op (group-op "d" "b")))]
    (outcome (concat
              (when-not (= :cycle (structure/check-op chain regroup)) [{:case :regroup}])
              (when-not (= :cycle (structure/check-op chain cycle)) [{:case :group}])
              (when (structure/check-op diamond (group-op "d" "c")) [{:case :diamond}])))))

(defn run-process
  "Run ARGV in DIR for at most a minute. Returns {:exit n :out text}."
  [dir argv]
  (let [process (-> (ProcessBuilder. ^java.util.List (vec argv))
                    (.directory (io/file dir)) (.redirectErrorStream true) (.start))
        out (future (slurp (.getInputStream process)))]
    (if (.waitFor process 60 java.util.concurrent.TimeUnit/SECONDS)
      {:exit (.exitValue process) :out @out}
      (do (.destroyForcibly process)
          (throw (ex-info "Client test run timed out" {:argv argv}))))))

(defn- tail [text] (apply str (take-last 400 text)))

;; The Emacs and browser models are not Clojure, so these two probes run each
;; client's own conformance test as a separate process, in sibling checkouts of
;; this repository, and read the counts it prints. A run that reports no tests
;; is a violation: an empty test file must not pass.
(defn- emacs-client-probe []
  (let [dir (io/file (System/getProperty "user.dir") ".." "futon4a")
        {:keys [exit out]} (run-process dir ["emacs" "-Q" "--batch" "-L" "lisp"
                                             "-l" "test/futon4a-together-test.el"
                                             "-f" "ert-run-tests-batch-and-exit"])
        [_ ran expected] (re-find #"Ran (\d+) tests, (\d+) results as expected" out)]
    (outcome (when-not (and (zero? exit) ran (pos? (parse-long ran)) (= ran expected))
               [{:reason :emacs-conformance-failed :exit exit :output (tail out)}]))))

(defn- browser-client-probe []
  (let [{:keys [exit out]} (run-process (System/getProperty "user.dir")
                                        ["node" "--test" "test/js/together.test.mjs"])
        passed (some-> (re-find #"(?m)^\S+ pass (\d+)" out) second parse-long)
        failed (some-> (re-find #"(?m)^\S+ fail (\d+)" out) second parse-long)]
    (outcome (when-not (and (zero? exit) passed (pos? passed) (= 0 failed))
               [{:reason :javascript-conformance-failed :exit exit :output (tail out)}]))))

(defn probe [feature]
  (try
    (case feature
      :convergence (convergence-probe)
      :ids-not-positions (ids-probe)
      :log-is-the-record (log-probe)
      :authorship-checked (authorship-probe)
      :provenance-fixed-at-insert (provenance-probe)
      :repeated-insert-acknowledged (retry-probe)
      :seeding-alone (seeding-probe)
      :loopback-identity (loopback-probe)
      :structure-is-serialised (structure-probe)
      :regroup-cycle-refused (cycle-probe)
      :emacs-client-conforms (emacs-client-probe)
      :browser-client-conforms (browser-client-probe)
      (throw (ex-info "Feature has no executable probe" {:feature feature})))
    (catch Exception e
      {:result :error :violations [{:reason :probe-exception :message (ex-message e)}]})))

(defn build [{:keys [pins at implementation-digest]}]
  (let [groups
        (mapv
         (fn [{:keys [feature statement kind] :as specification}]
           (let [choice (catalogue/choice
                         {:choice/key (keyword "together" (name feature)) :kind kind :statement statement
                          :sources [(select-keys specification [:repo :revision :path :section])]
                          :lifecycle :accepted :applies-to [feature]
                          :rationale "Make the real-time editing design and its bounded evidence visible in Demo A."
                          :alternatives []})
                 executable? (:probe specification)
                 check-spec (when executable?
                              {:check/key (keyword "together-probe" (name feature))
                               :runtime :clojure
                               :entry-point "futon3c.arxana.together-catalogue/probe"})
                 check (when check-spec
                         (catalogue/check-record
                          (merge check-spec
                                 {:input-contract {:feature feature :scope :finite-local-fixture}
                                  :output-contract {:result [:pass :violation :error] :coverage :partial}
                                  :implementation-digest implementation-digest
                                  :fixtures [{:feature feature}]})))
                 link (when check (catalogue/link :design/checked-by choice check))
                 run (when executable?
                       (catalogue/check-run choice check pins at (probe feature)))]
             {:choice choice :check check :link link :run run})) features)
        values #(vec (keep % groups))
        choices (values :choice) checks (values :check) links (values :link) runs (values :run)]
    {:choices choices :checks checks :links links :runs runs
     :records (vec (mapcat #(keep % [:choice :check :link :run]) groups))}))
