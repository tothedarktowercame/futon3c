(ns futon3c.diagramprover.wm-wire-ask-library-products
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]
            [futon2.aif.flight-runner :as fr]
            [futon2.aif.want-interpretation :as wi]
            [futon3c.diagramprover.wm-wire :as w]))

(defn with-libraries [f]
  (let [a (w/tmp-dir "library-a-") b (w/tmp-dir "library-b-")]
    (try
      (spit (io/file a "a.flexiarg") "@title Pattern Alpha\n")
      (spit (io/file b "b.flexiarg") "@title Pattern Beta\n")
      (f a b)
      (finally (doseq [root [a b] file (reverse (file-seq (io/file root)))] (io/delete-file file))))))

(defn prompts [a b]
  (let [prompt wi/prompt calls (atom [])
        answer (fr/agency-answer-fn {:seat "fixture" :opts {} :library-root a
                                     :dispatch! (fn [& _] {})})
        run (fn [change]
              (with-redefs [wi/prompt
                            (fn [issued opts]
                              (let [changed (update opts :library-root change)
                                    text (prompt issued changed)]
                                (swap! calls conj {:written opts :carrier changed :text text}) text))]
                (answer {:target "fixture" :request-id "library" :want {:token :done}})))]
    (run identity) (run (constantly b)) @calls))

(defn assertion-report [changed-root]
  (let [read-file slurp]
    ;; Only translate the futon2 test's relative fixture paths, preserving
    ;; their exact bytes on this JVM's classpath. No absolute checkout path.
    (with-redefs [clojure.core/slurp
                  (fn [path & opts]
                    (apply read-file (if (and (string? path) (str/starts-with? path "test/fixtures/"))
                                       (or (io/resource (subs path 5))
                                           (throw (ex-info "Missing classpath fixture" {:path path}))) path) opts))]
      (require 'futon2.aif.flight-ask-library-test)))
  (let [prompt wi/prompt reports (atom []) calls (atom [])]
    (with-redefs [wi/prompt
                  (fn
                    ([issued] (prompt issued))
                    ([issued opts]
                     (let [v (cond-> opts (and changed-root (:library-root opts)) (assoc :library-root changed-root))]
                       (swap! calls conj {:written opts :carrier v})
                       (prompt issued v))))
                  t/report #(when (#{:pass :fail :error} (:type %))
                              (swap! reports conj (select-keys % [:type :expected :actual :message])))]
      ((ns-resolve 'futon2.aif.flight-ask-library-test
                   'the-library-root-is-on-the-prompt-from-the-flight-opts)))
    {:calls @calls :reports @reports}))
