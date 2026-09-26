(ns futon3c.admin
  "Transient Drawbridge administrator.

   This process never boots Futon3c and owns no copy of its registries.  It
   sends authenticated operations to the one serving JVM and marks them as
   dev-admin operations for the server's forensic log."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]))

(defn- getenv [k]
  (some-> (System/getenv k) str/trim not-empty))

(defn- admin-token []
  (or (getenv "FUTON3C_ADMIN_TOKEN")
      (getenv "ADMIN_TOKEN")
      (let [f (io/file ".admintoken")]
        (when (.isFile f) (some-> (slurp f) str/trim not-empty)))
      (throw (ex-info "No admin token: set FUTON3C_ADMIN_TOKEN or create .admintoken" {}))))

(defn- endpoint []
  (str (str/replace (or (getenv "FUTON3C_DRAWBRIDGE_URL")
                        "http://127.0.0.1:6768")
                    #"/$" "")
       "/admin/eval"))

(defn- canonical-futon2-source? [path]
  (let [file (.getCanonicalFile (io/file path))
        root (.getCanonicalFile (io/file "../futon2"))]
    (.startsWith (.toPath file) (.toPath root))))

(defn- read-form [[command & args]]
  (case command
    "eval" (let [code (str/join " " args)]
             (when (str/blank? code)
               (throw (ex-info "eval requires a Clojure form" {})))
             code)
    "file" (let [path (first args)]
             (when (str/blank? path)
               (throw (ex-info "file requires a path containing Clojure forms" {})))
             (slurp path))
    "load-file" (let [path (first args)]
                  (when (str/blank? path)
                    (throw (ex-info "load-file requires a source path" {})))
                  (if (canonical-futon2-source? path)
                    ;; The serving JVM, not this transient client, records the
                    ;; exact source/commit/dirty state it loaded. The wrapper
                    ;; refuses worktree paths and a source that changes during
                    ;; the load. Futon3c's own reload path remains unchanged.
                    (pr-str
                     (list (list 'requiring-resolve
                                 (list 'quote 'futon3c.wm.code-identity/load-file-recorded!))
                           path))
                    (pr-str (list 'load-file path))))
    "status" "{:profile :dev-admin :pid (-> (java.lang.management.ManagementFactory/getRuntimeMXBean) .getPid) :registry (futon3c.agency.registry/registry-status)}"
    (throw (ex-info
            (str "Usage:\n"
                 "  clojure -M:dev-admin status\n"
                 "  clojure -M:dev-admin eval '<form>'\n"
                 "  clojure -M:dev-admin file PATH\n"
                 "  clojure -M:dev-admin load-file SOURCE\n\n"
                 "Namespace refresh/removal is refused by the serving JVM.")
            {}))))

(defn- post! [code]
  (let [request (-> (HttpRequest/newBuilder (URI/create (endpoint)))
                    (.header "content-type" "text/plain; charset=utf-8")
                    (.header "x-admin-token" (admin-token))
                    (.header "x-drawbridge-profile" "dev-admin")
                    (.POST (HttpRequest$BodyPublishers/ofString code))
                    .build)
        response (.send (HttpClient/newHttpClient)
                        request
                        (HttpResponse$BodyHandlers/ofString))]
    (println (.body response))
    (when-not (<= 200 (.statusCode response) 299)
      (System/exit 1))))

(defn -main [& args]
  (try
    (post! (read-form args))
    (catch Throwable t
      (binding [*out* *err*]
        (println (.getMessage t)))
      (System/exit 2))))
