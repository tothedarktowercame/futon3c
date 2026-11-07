(ns futon3c.arxana.demo
  "Standalone loopback demo stack for the futon4a Emacs client
  (INSTANTIATE increment 6, per futon4a README isolation contract).

  Builds the full service config from the committed D4 positive fixture
  (futon5 gate 17dc53a): mock graph store, in-memory ArtifactProvider,
  fixture correspondence repository, store-backed journal and subscription
  registry. The JVM receives its bind explicitly (default 127.0.0.1:7084),
  preflights 7084/6778 before binding, never infers upstream ports, and never
  binds publicly. No deployment, no lucy, no restart of anything."
  (:gen-class)
  (:require [clojure.java.io :as io]
            [futon3c.arxana.apm-fixture :as apm]
            [futon3c.arxana.artifact :as artifact]
            [futon3c.arxana.correspond-fixture :as fixture]
            [futon3c.arxana.digest :as digest]
            [futon3c.arxana.envelope :as env]
            [futon3c.arxana.http :as http]
            [futon3c.arxana.journal :as journal]
            [futon3c.arxana.json :as ajson]
            [futon3c.arxana.service :as service]
            [futon3c.arxana.store :as store]
            [futon3c.arxana.subscribe :as subscribe])
  (:import [java.nio.charset StandardCharsets]))

(def demo-ports
  "futon4a loopback allocation: arxana 7084, drawbridge 6778 (preflight only;
  drawbridge is not started by this demo)."
  [["arxana" 7084] ["drawbridge" 6778]])

(defn- utf8 [^String s] (.getBytes s StandardCharsets/UTF_8))

(defn demo-strategy
  "Anchor strategy over the fixture's intro paragraph quote, captured against
  the exact fixture bytes (D2 golden-anchor shape)."
  [anchor-text context-before context-after]
  {:arxana/semantics :arxana.resolution/v1
   :arxana/strategy-id "futon4a-demo/anchor"
   :arxana/scope {:repository "futon4a-demo"}
   :arxana/cascade-mode :first-unique
   :arxana/finders [{:arxana/type :arxana.finder/anchor
                     :arxana/on [:ambiguous :missing]
                     :arxana/config
                     {:anchor/content-hash
                      (digest/sha256-hex (utf8 anchor-text))
                      :anchor/content-length (count anchor-text)
                      :anchor/context-before context-before
                      :anchor/context-after context-after
                      :anchor/hint {:offset 0 :line 10}}}]})

(defn build-stack
  "Verify the fixture and build the demo service config. Returns
  {:service-cfg .. :fixture .. :revisions {..} :strategy/revision-id ..
   :artifact/revision-id ..}. The correspondence repository also carries the
   APM a93J02 fixture (resources/arxana/apm-demo) for the two-pane
   code-server demo; its artifact revisions are ingested alongside."
  []
  (let [{:keys [repo] :as fx} (fixture/build-fixture-repository)
        apm (apm/assert-into-repository! repo)
        scopes (fixture/load-scopes)
        manifest (fixture/load-manifest)
        revisions (into {}
                        (map (fn [a] [(:logical-id a) (:revision-id a)]))
                        (:artifacts manifest))
        graph-store (store/mock-store)
        provider (artifact/in-memory-provider)
        j (journal/journal graph-store)
        subr (subscribe/subscriptions j)
        intro-text (slurp (str (:src-dir scopes) "/intro-generated.tex"))
        anchor-text "The object of study is a finite family"
        idx (.indexOf ^String intro-text anchor-text)
        line-start (.lastIndexOf ^String intro-text "\n" idx)
        line-end (.indexOf ^String intro-text "\n" idx)
        strategy (demo-strategy anchor-text
                                (subs intro-text (inc line-start) idx)
                                (subs intro-text (+ idx (count anchor-text)) line-end))
        srid (service/register-strategy! graph-store strategy)]
    {:service-cfg {:graph-store graph-store
                   :artifact-provider provider
                   :correspondence-repo repo
                   :journal j
                   :subscriptions subr}
     :fixture fx
     :apm apm
     :revisions revisions
     :intro-revision-id (get revisions "intro-generated.tex")
     :strategy/revision-id srid}))

(defn ingest-apm-revisions!
  "Ingest the APM fixture's exact committed bytes (tex + html) into the
  provider and set heads, so :scope/at-position revision pins resolve."
  [{:keys [service-cfg apm]}]
  (let [{:keys [artifact-provider graph-store]} service-cfg]
    (doseq [[logical rid] (:revisions apm)]
      (let [f (apm/fixture-resource logical)
            {:keys [ok]} (artifact/ingest!
                          artifact-provider logical (apm/file-bytes f)
                          (if (= logical "index.html") "text/html" "text/x-tex"))]
        (when-not (= rid (:artifact/revision-id ok))
          (throw (ex-info "APM ingest revision mismatch"
                          {:expected rid :actual (:artifact/revision-id ok)})))
        (store/put-entity! graph-store
                           {:entity/id (str "arxana:head:" logical)
                            :type :arxana/artifact-head
                            :external-id logical
                            :props {:head/revision-id (:artifact/revision-id ok)}}))))
  true)

(defn ingest-fixture-revisions!
  "Ingest every fixture artifact's exact bytes into the provider so revision
  IDs resolve (idempotent; content-addressed), and set each artifact's head
  entity so guarded ingests see a current head."
  [{:keys [service-cfg revisions]}]
  (let [{:keys [artifact-provider graph-store]} service-cfg
        scopes (fixture/load-scopes)]
    (doseq [[logical _rid] revisions]
      (let [path (if (= logical "draft8.html")
                   (fixture/fixture-path "draft8.html")
                   (str (:src-dir scopes) "/" logical))
            bs (with-open [in (io/input-stream path)
                           out (java.io.ByteArrayOutputStream.)]
                 (io/copy in out)
                 (.toByteArray out))
            {:keys [ok]} (artifact/ingest! artifact-provider logical bs
                                           (if (= logical "draft8.html") "text/html" "text/x-tex"))]
        (store/put-entity! graph-store
                           {:entity/id (str "arxana:head:" logical)
                            :type :arxana/artifact-head
                            :external-id logical
                            :props {:head/revision-id (:artifact/revision-id ok)}})))
    true))

(defn start-demo!
  "Preflight ports, build the stack, ingest fixture bytes, bind loopback.
  Returns {:server stop-fn :stack ..}."
  ([] (start-demo! {}))
  ([{:keys [port] :or {port http/default-port}}]
   (http/preflight-ports! "127.0.0.1" demo-ports)
   (let [stack (build-stack)]
     (ingest-fixture-revisions! stack)
     (ingest-apm-revisions! stack)
     {:server (http/start! {:port port :service-cfg (:service-cfg stack)})
      :stack stack})))

;;; Demo transcript (semantic-digest equality, D5) ------------------------------

(defn- req [op body mid principal]
  {:protocol/version env/protocol-version
   :message/id mid
   :operation op
   :principal/id principal
   :sent-at "2026-11-07T00:00:00Z"
   :body body})

(defn run-transcript
  "Run the futon4a first-demonstration flow in-process against a built stack
  and return the transcript: a vector of {:step :request :response} maps.
  Two simulated clients (differing message/principal IDs, same semantic
  requests) must receive identical :semantic/digest values — the D5 equality
  contract this transcript exists to witness."
  [stack]
  (let [cfg (:service-cfg stack)
        d (fn [m] (service/dispatch cfg m))
        intro-rid (:intro-revision-id stack)
        srid (:strategy/revision-id stack)
        resolve-body {:artifact/revision-id intro-rid
                      :strategy/revision-id srid
                      :subject/id "draft8/introduction"}
        corr-body {:subject/id "draft8/introduction"}]
    [{:step :capabilities
      :request (req :capabilities/handshake {} "t1" "futon4a-demo/emacs")
      :response (d (req :capabilities/handshake {} "t1" "futon4a-demo/emacs"))}
     {:step :resolve/emacs
      :request (req :scope/resolve resolve-body "t2" "futon4a-demo/emacs")
      :response (d (req :scope/resolve resolve-body "t2" "futon4a-demo/emacs"))}
     {:step :resolve/other-client
      :request (req :scope/resolve resolve-body "t3" "futon4a-demo/other")
      :response (d (req :scope/resolve resolve-body "t3" "futon4a-demo/other"))}
     {:step :correspondence
      :request (req :correspondence/query corr-body "t4" "futon4a-demo/emacs")
      :response (d (req :correspondence/query corr-body "t4" "futon4a-demo/emacs"))}
     {:step :typed-refusal/stale-ingest
      :request (req :revision/ingest
                    {:artifact/id "intro-generated.tex"
                     :bytes "stale overwrite attempt"
                     :media-type "text/x-tex"
                     :expected/revision-id :absent}
                    "t5" "futon4a-demo/emacs")
      :response (d (req :revision/ingest
                        {:artifact/id "intro-generated.tex"
                         :bytes "stale overwrite attempt"
                         :media-type "text/x-tex"
                         :expected/revision-id :absent}
                        "t5" "futon4a-demo/emacs"))}
     {:step :subscription/replay
      :request (req :subscription/open {:topics [:revision/ingested
                                                 :assertion/promoted]}
                    "t6" "futon4a-demo/emacs")
      :response (d (req :subscription/open {:topics [:revision/ingested
                                                     :assertion/promoted]}
                        "t6" "futon4a-demo/emacs"))}]))

(defn transcript-digests
  "Step -> semantic digest map extracted from a transcript."
  [transcript]
  (into {}
        (map (fn [{:keys [step response]}]
               [step (or (get-in response [:ok :semantic/digest])
                         (get-in response [:error :semantic/digest]))]))
        transcript))

(defn write-transcript!
  "Serialize a transcript as arxana-JSON for the futon4a examples directory."
  [transcript ^String path]
  (spit path (ajson/encode
              {:transcript/version 1
               :generated-by "futon3c.arxana.demo/run-transcript"
               :steps transcript})))

(defn -main
  "Standalone loopback entry: clojure -M -m futon3c.arxana.demo [port]"
  [& [port]]
  (let [{:keys [stack]} (start-demo! {:port (if port (Long/parseLong port)
                                                http/default-port)})]
    (println (str "futon4a-demo arxana.v1 listening on 127.0.0.1:"
                  (or port http/default-port)))
    (println (str "strategy/revision-id " (:strategy/revision-id stack)))
    (println (str "intro revision " (:intro-revision-id stack)))
    @(promise)))
