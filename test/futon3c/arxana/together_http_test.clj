(ns futon3c.arxana.together-http-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.together :as t]
            [futon3c.arxana.together-http :as h])
  (:import [java.net URI]
           [java.net.http HttpClient HttpClient$Version HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.nio.file Files]
           [java.time Duration]
           [java.io BufferedReader InputStreamReader]))

(defn temp-dir []
  (str (Files/createTempDirectory "together-test-" (make-array java.nio.file.attribute.FileAttribute 0))))
(defn remove-dir! [dir]
  (doseq [file (reverse (file-seq (io/file dir)))] (io/delete-file file)))
(defn client [] (-> (HttpClient/newBuilder) (.version HttpClient$Version/HTTP_1_1) (.build)))
(defn request [server client method path editor body headers stream?]
  (let [builder (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" (:port server) path)))
                    (.timeout (Duration/ofSeconds 10)))]
    (when editor (.header builder "X-Together-Editor" editor))
    (doseq [[k v] headers] (.header builder k v))
    (if (= method :post)
      (.POST builder (HttpRequest$BodyPublishers/ofString (if (string? body) body (json/generate-string body))))
      (.GET builder))
    (let [resp (.send ^HttpClient client (.build builder)
                      (if stream? (HttpResponse$BodyHandlers/ofInputStream) (HttpResponse$BodyHandlers/ofString)))]
      {:status (.statusCode resp) :body (if stream? (.body resp) (json/parse-string (.body resp) true))})))
(defn state! [s c key] (request s c :get (str "/together/v1/doc/" key "/state") "observer" nil {} false))
(defn op [editor n after text]
  {:op "insert" :id [editor n] :after after :text text :attrs {:author editor}})
(defn post! [s c key editor operation parent]
  (request s c :post (str "/together/v1/doc/" key "/ops") editor
           {:parents {:rev parent} :op operation} {} false))
(defn stream! [s c key suffix headers]
  (let [resp (request s c :get (str "/together/v1/doc/" key "/events" suffix) "observer" nil headers true)]
    (is (= 200 (:status resp)))
    (BufferedReader. (InputStreamReader. (:body resp) "UTF-8"))))
(defn read-event [^BufferedReader reader]
  (loop [id nil]
    (let [line (.readLine reader)]
      (cond
        (nil? line) (throw (ex-info "Stream ended" {}))
        (str/starts-with? line "id: ") (recur (parse-long (subs line 4)))
        (str/starts-with? line "data: ") {:id id :entry (json/parse-string (subs line 6) true)}
        :else (recur id)))))
(defn events! [reader n]
  (let [job (future (vec (repeatedly n #(read-event reader))))
        result (deref job 10000 ::timeout)]
    (when (= result ::timeout) (.close ^BufferedReader reader) (future-cancel job)
          (throw (ex-info "Timed out reading events" {:expected n})))
    result))
(defn with-server [f]
  (let [dir (temp-dir) server (h/start! {:port 0 :dir dir}) c (client)]
    (try (f server c dir) (finally (h/stop! server) (remove-dir! dir)))))

(deftest snapshots-and-refusals
  (with-server (fn [s c dir]
    (is (= {:status 200 :body {:key "new" :rev 0 :runs []}} (state! s c "new")))
    (is (= 200 (:status (post! s c "new" "joe" (op "joe" 1 nil "😀") 0))))
    (is (= 200 (:status (post! s c "new" "rob" (op "rob" 2 ["joe" 1] "𝕏") 1))))
    (is (= "😀𝕏" (t/text (t/from-runs (get-in (state! s c "new") [:body :runs])))))
    (doseq [[expected result]
            [[409 (post! s c "new" "rob" (op "joe" 3 nil "x") 2)]
             [401 (post! s c "new" nil (op "joe" 3 nil "x") 2)]
             [401 (request s c :get "/together/v1/health" "bad name" nil {} false)]
             [400 (state! s c "bad%20key")]
             [400 (request s c :post "/together/v1/doc/new/ops" "joe" "{broken" {} false)]
             [400 (request s c :post "/together/v1/doc/new/ops" "joe" {:op {}} {} false)]
             [400 (request s c :get "/together/v1/doc/new/events?after=no" "joe" nil {} false)]
             [409 (post! s c "new" "joe" (op "joe" 3 ["missing" 1] "x") 2)]
             [404 (request s c :get "/missing" "joe" nil {} false)]]]
      (is (= expected (:status result))))
    (doseq [[error operation parent] [["invalid-id" (op "" 3 nil "x") 2]
                                       ["id-in-use" (op "joe" 1 nil "x") 2]
                                       ["empty-text" (op "joe" 3 nil "") 2]
                                       ["invalid-counter" (op "joe" 0 nil "x") 2]
                                       ["counter-not-after" (op "joe" 2 ["rob" 2] "x") 2]
                                       ["unknown-id" {:op "delete" :ids [["none" 1]]} 2]
                                       ["invalid-parents" (op "joe" 3 nil "x") 99]]]
      (is (= {:status 409 :body {:error error}} (post! s c "new" "joe" operation parent))))
    (is (= "wrong-author" (get-in (post! s c "new" "rob" (op "joe" 3 nil "x") 2) [:body :error])))
    (is (= "unknown-after" (get-in (post! s c "new" "joe" (op "joe" 3 ["none" 1] "x") 2) [:body :error])))
    (is (= 2 (get-in (state! s c "new") [:body :rev])))
    (is (= 2 (count (str/split-lines (slurp (io/file dir "new.log.edn"))))))
    (is (= {:ok true :documents 1}
           (:body (request s c :get "/together/v1/health" "joe" nil {} false))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"port already" (h/start! {:port (:port s) :dir dir})))) ))

(deftest stream-replay-and-own-acknowledgement
  (with-server (fn [s c _dir]
    (with-open [first-stream (stream! s c "stream" "" {})]
      (doseq [[i editor] [[1 "observer"] [2 "rob"] [3 "observer"]]]
        (is (= 200 (:status (post! s c "stream" editor (op editor i nil (str i)) 0)))))
      (let [es (events! first-stream 3)]
        (is (= [1 2 3] (mapv :id es)))
        (is (= ["observer" "rob" "observer"] (mapv #(get-in % [:entry :author]) es)))
        (is (= [1 2 3] (mapv #(get-in % [:entry :rev]) es))))
      (with-open [second-stream (stream! s c "stream" "?after=1" {})
                  resumed (stream! s c "stream" "?after=100" {"Last-Event-ID" "2"})]
        (is (= [2 3] (mapv :id (events! second-stream 2))))
        (is (= [3] (mapv :id (events! resumed 1))))
        (post! s c "stream" "rob" (op "rob" 4 nil "4") 3)
        ;; If replay duplicated rev 3, these next reads would see it, not 4.
        (doseq [reader [first-stream second-stream resumed]]
          (is (= [4] (mapv :id (events! reader 1))))))))))

(deftest concurrent-posts-and-durable-log
  (with-server (fn [s c dir]
    (let [jobs (mapv (fn [editor]
                       (future
                         (mapv (fn [n] (post! s c "parallel" editor (op editor n nil "x") 0))
                               (range 1 51)))) ["a" "b" "c" "d"])]
      (doseq [job jobs] (is (every? #(= 200 (:status %)) (deref job 10000 [])))))
    (let [snapshot (:body (state! s c "parallel"))
          log (mapv edn/read-string (str/split-lines (slurp (io/file dir "parallel.log.edn"))))]
      (is (= 200 (:rev snapshot)))
      (is (= (vec (range 1 201)) (mapv :rev log)))
      (is (= (t/rebuild log) (t/from-runs (:runs snapshot))))))))

(deftest snapshot-stream-race
  (with-server (fn [s c _dir]
    (let [reached (promise) resume (promise)
          job (future
                (mapv (fn [n]
                        (when (= n 31) (deliver reached true) @resume)
                        (post! s c "race" "joe" (op "joe" n nil "x") 0))
                      (range 1 201)))]
      (is (= true (deref reached 5000 false)))
      (let [rev (get-in (state! s c "race") [:body :rev])]
        (deliver resume true)
        (with-open [reader (stream! s c "race" (str "?after=" rev) {})]
          (let [seen (mapv :id (events! reader (- 200 rev)))]
            (is (= (vec (range 1 201)) (into (vec (range 1 (inc rev))) seen))))))
      (is (= 200 (count (deref job 10000 []))))))))

(deftest restart-and-damage-isolation
  (let [dir (temp-dir) c (client) s (h/start! {:port 0 :dir dir})]
    (try
      (post! s c "saved" "joe" (op "joe" 1 nil "a😀b") 0)
      (post! s c "saved" "joe" {:op "delete" :ids [["joe" 2]]} 1)
      (let [before (state! s c "saved")]
        (h/stop! s)
        (let [next-server (h/start! {:port 0 :dir dir})]
          (try
            (is (= before (state! next-server c "saved")))
            (is (= {:status 200 :body {:rev 3}}
                   (post! next-server c "saved" "joe" (op "joe" 4 ["joe" 2] "X") 2)))
            (finally (h/stop! next-server)))))
      (let [file (io/file dir "saved.log.edn") bytes (slurp file)
            damaged (subs bytes 0 (- (count bytes) 7))]
        (spit file damaged)
        (let [broken (h/start! {:port 0 :dir dir})]
          (try
            (doseq [action ["state" "events"]]
              (let [response (request broken c :get (str "/together/v1/doc/saved/" action) "joe" nil {} false)]
                (is (= 500 (:status response)))
                (is (str/includes? (get-in response [:body :error]) "Damaged document log"))))
            (is (= 500 (:status (post! broken c "saved" "joe" (op "joe" 5 nil "x") 3))))
            (is (= damaged (slurp file)))
            (is (= 0 (get-in (state! broken c "other") [:body :rev])))
            (finally (h/stop! broken)))))
      (finally (h/stop! s) (remove-dir! dir)))))

(deftest explicit-directory-and-loopback
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"explicit :dir" (h/start! {:port 0})))
  (let [dir (temp-dir)]
    (try (is (thrown-with-msg? clojure.lang.ExceptionInfo #"loopback only"
                              (h/start! {:port 0 :dir dir :host "0.0.0.0"})))
         (finally (remove-dir! dir)))))

(deftest failed-append-is-not-published
  (with-server
    (fn [s c dir]
      (state! s c "blocked")
      ;; Real filesystem refusal after the document was loaded as empty.
      (.mkdir (io/file dir "blocked.log.edn"))
      (is (= 500 (:status (post! s c "blocked" "joe" (op "joe" 1 nil "x") 0))))
      (is (= [] (get-in @(get @(:documents s) "blocked") [:hub :log])))
      (is (= 500 (:status (state! s c "blocked"))))
      (is (= 0 (get-in (state! s c "healthy") [:body :rev]))))))

(deftest sites-belong-to-their-identity
  (with-server (fn [s c _dir]
    ;; One person's two surfaces are two sites and both are accepted.
    (is (= 200 (:status (post! s c "sites" "joe" (op "joe.emacs" 1 nil "a") 0))))
    (is (= 200 (:status (post! s c "sites" "joe" (op "joe.web" 2 ["joe.emacs" 1] "b") 1))))
    ;; A site of another person, and a name that merely starts with "joe", are not joe's.
    (is (= {:status 409 :body {:error "wrong-author"}}
           (post! s c "sites" "joe" (op "murch.web" 3 nil "x") 2)))
    (is (= {:status 409 :body {:error "wrong-author"}}
           (post! s c "sites" "joe" (op "joex.emacs" 3 nil "x") 2)))
    (is (= {:status 409 :body {:error "wrong-author"}}
           (post! s c "sites" "joe" (op "joe-2" 3 nil "x") 2)))
    ;; An identity may not itself contain a dot, or "joe.emacs" could claim "joe.emacs.x".
    (is (= 401 (:status (post! s c "sites" "joe.emacs" (op "joe.emacs" 3 nil "x") 2))))
    (is (= "ab" (t/text (t/from-runs (get-in (state! s c "sites") [:body :runs])))))
    (is (= ["joe" "joe"]
           (mapv :author (map read-string (line-seq (io/reader (io/file _dir "sites.log.edn")))))))))
  (is (t/site-of? "joe" "joe"))
  (is (t/site-of? "joe.emacs" "joe"))
  (is (not (t/site-of? "joe" "joe.emacs")))
  (is (not (t/site-of? "joex" "joe")))
  (is (not (t/site-of? "joe.emacs" nil))))

(deftest web-files-are-served-without-identity-and-cannot-escape
  (with-server (fn [s c _dir]
    (let [raw (fn [path]
                (let [resp (.send ^HttpClient c
                                  (.build (.GET (HttpRequest/newBuilder
                                                 (URI/create (str "http://127.0.0.1:" (:port s) path)))))
                                  (HttpResponse$BodyHandlers/ofString))]
                  {:status (.statusCode resp)
                   :type (.orElse (.firstValue (.headers resp) "content-type") nil)
                   :body (.body resp)}))
          js (raw "/together/v1/web/together.js")]
      (is (= 200 (:status js)))
      (is (= "text/javascript; charset=utf-8" (:type js)))
      (is (= (slurp (io/resource "arxana/together/web/together.js")) (:body js)))
      ;; Not a web file: the conformance fixture one directory up, a missing file,
      ;; an encoded traversal, and a type that is not served.
      (doseq [path ["/together/v1/web/../conformance.json"
                    "/together/v1/web/%2e%2e%2fconformance.json"
                    "/together/v1/web/nothing.js"
                    "/together/v1/web/together.json"
                    "/together/v1/web/sub/together.js"]]
        (is (= 404 (:status (raw path))) path))))))

(deftest whoami-reports-the-header-identity
  (with-server (fn [s c _dir]
    (is (= {:status 200 :body {:identity "murch"}}
           (request s c :get "/together/v1/whoami" "murch" nil {} false)))
    (is (= 401 (:status (request s c :get "/together/v1/whoami" nil nil {} false))))
    (is (= 401 (:status (request s c :get "/together/v1/whoami" "murch.web" nil {} false)))))))
