(ns futon3c.arxana.together-structure-http-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.together-http :as h]
            [futon3c.arxana.together-http-test :as fixture]))

(defn- req [server client method key action body]
  (fixture/request server client method (str "/together/v1/structure/" key "/" action)
                   "fixture-author" body {} false))
(defn- state [s c key] (:body (req s c :get key "state" nil)))
(defn- act [s c key op] (req s c :post key "acts" {:op op}))
(defn- node [id] {:op "add-node" :id id :kind "fixture" :attrs {:title "Invented"}})
(defn- entries [dir key]
  (mapv edn/read-string (str/split-lines (slurp (io/file dir "structure" (str key ".log.edn"))))))

(deftest cycle-refusals-survive-restart
  (let [dir (fixture/temp-dir) c (fixture/client)
        first-server (h/start! {:port 0 :dir dir})
        expected
        (try
          (doseq [op (concat (map node ["a" "b" "c"])
                            [{:op "group" :child "b" :parent "a"}
                             {:op "group" :child "c" :parent "b"}])]
            (is (= 200 (:status (act first-server c "fixture" op)))))
          (let [before (state first-server c "fixture")]
            (is (= {:b ["a"] :c ["b"]} (:parents before)))
            (doseq [[revision op] [[6 {:op "regroup" :child "b" :from "a" :to "c"}]
                                  [7 {:op "group" :child "a" :parent "c"}]]]
              (is (= {:status 409 :body {:rev revision :error "cycle"}}
                     (act first-server c "fixture" op)))
              (is (= (dissoc before :rev) (dissoc (state first-server c "fixture") :rev)))))
          (let [log (:body (req first-server c :get "fixture" "log" nil))
                disk (entries dir "fixture")]
            (is (= 7 (:rev log) (count disk)))
            (is (= ["cycle" "cycle"] (mapv :refused (drop 5 (:entries log)))))
            (is (= [:cycle :cycle] (mapv :refused (drop 5 disk))))
            (is (every? #(= "fixture-author" (:author %)) disk))
            (is (= [6 7] (mapv :rev (get-in (req first-server c :get "fixture" "log?after=5" nil)
                                           [:body :entries]))))
            {:state (state first-server c "fixture") :log log})
          (finally (h/stop! first-server)))]
    (try
      (let [server (h/start! {:port 0 :dir dir})]
        (try
          (is (= (:state expected) (state server c "fixture")))
          (is (= (:log expected) (:body (req server c :get "fixture" "log" nil))))
          (is (= {:status 200 :body {:docs []}}
                 (fixture/request server c :get "/together/v1/docs" "fixture-author" nil {} false)))
          (finally (h/stop! server))))
      (finally (fixture/remove-dir! dir)))))

(deftest malformed-bodies-and-authentication-never-log
  (fixture/with-server
   (fn [s c dir]
     (is (= 200 (:status (act s c "fixture" (node "a")))))
     (let [disk (slurp (io/file dir "structure" "fixture.log.edn"))]
       (doseq [body [(str (json/generate-string {:op (node "b")}) " garbage")
                     (str (json/generate-string {:op (node "b")}) " {}")
                     "{" "null" "[]" "{}" {:op (node "b") :extra 1}
                     {:op nil} {:op {:op "unknown"}}
                     {:op (assoc (node "b") :attrs [])}
                     {:op (assoc (node "b") :id "bad id")}
                     {:op (assoc (node "b") :kind nil)}
                     {:op (assoc (node "b") :extra true)}
                     {:op {:op "add-edge" :id "e" :type "fixture" :ends []}}
                     {:op {:op "supersede" :id "v" :replaces "a" :kind "fixture"}}
                     {:op {:op "group" :child "a"}}
                     {:op {:op "ungroup" :child "a" :parent 1}}
                     {:op {:op "regroup" :child "a" :from "b" :to nil}}]]
         (is (= {:status 400 :body {:error "invalid-body"}} (req s c :post "fixture" "acts" body)))
         (is (= disk (slurp (io/file dir "structure" "fixture.log.edn")))))
       (doseq [action ["log?after=no" "log?after=-1" "log?after=999999999999999999999999"]]
         (is (= 400 (:status (req s c :get "fixture" action nil)))))
       (is (= 400 (:status (req s c :get "bad:key" "state" nil))))
       (doseq [[method action body] [[:get "state" nil] [:get "log" nil] [:post "acts" {:op (node "b")}]]]
         (is (= 401 (:status (fixture/request s c method (str "/together/v1/structure/fixture/" action)
                                             nil body {} false)))))
       (is (= disk (slurp (io/file dir "structure" "fixture.log.edn"))))
       (is (= 1 (:rev (state s c "fixture")))))
     ;; Well-shaped semantic errors are logged, unlike malformed requests.
     (is (= {:status 409 :body {:rev 2 :error "unknown-id"}}
            (act s c "fixture" {:op "group" :child "a" :parent "missing"}))))))

(deftest damaged-workspace-is-isolated
  (fixture/with-server
   (fn [s c dir]
     (.mkdirs (io/file dir "structure"))
     (doseq [[key text] [["truncated" "{:rev 1"]
                         ["syntax" "invalid EDN [\n"]
                         ["revision" "{:rev 2 :op {} :author \"fixture\" :at \"bad\"}\n"]
                         ["false-refusal" "{:rev 1 :op {:op :add-node :id \"a\" :kind \"fixture\" :attrs {}} :author \"fixture\" :at \"2026-01-01T00:00:00Z\" :refused :cycle}\n"]]]
       (spit (io/file dir "structure" (str key ".log.edn")) text)
       (let [r (req s c :get key "state" nil)]
         (is (= 500 (:status r)))
         (is (str/includes? (get-in r [:body :error]) "Damaged structure log"))))
     (is (= 200 (:status (act s c "good" (node "a")))))
     (is (= 200 (:status (fixture/post! s c "text" "fixture" (fixture/op "fixture" 1 nil "hello") 0))))
     (is (= ["text"] (mapv :key (get-in (fixture/request s c :get "/together/v1/docs" "fixture" nil {} false)
                                        [:body :docs])))))))

(deftest workspace-and-server-isolation-and-sorted-parents
  (fixture/with-server
   (fn [s c _]
     (fixture/with-server
      (fn [other other-c _]
        (doseq [op (concat (map node ["a" "b" "c"])
                          [{:op "group" :child "c" :parent "b"}
                           {:op "group" :child "c" :parent "a"}])]
          (is (= 200 (:status (act s c "same" op)))))
        (is (= ["a" "b"] (get-in (state s c "same") [:parents :c])))
        (is (= 0 (:rev (state other other-c "same")) (:rev (state s c "other"))))
        (doseq [parent ["a" "b"]]
          (is (= 200 (:status (act s c "same" {:op "ungroup" :child "c" :parent parent})))))
        (is (= {} (:parents (state s c "same")))))))))

(deftest concurrent-acts-are-serialised
  (fixture/with-server
   (fn [s c dir]
     (let [jobs (mapv (fn [i] (future (act s c "parallel" (node (str "n" i))))) (range 12))
           replies (mapv deref jobs)]
       (is (every? #(= 200 (:status %)) replies))
       (is (= (range 1 13) (sort (map #(get-in % [:body :rev]) replies))))
       (is (= (range 1 13) (map :rev (entries dir "parallel"))))
       (is (= 12 (count (:nodes (state s c "parallel")))))))))
