(ns futon3c.diagramprover.wm-wire-initialization-hash-products
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon2.aif.flight-runner :as runner]
            [futon2.aif.hermetic-repair-fixture :as hermetic]
            [futon2.aif.served-by-reading :as served]
            [futon2.aif.token-belief-predecessor :as predecessor]
            [futon2.aif.token-initialization-policy :as policy]
            [futon3c.diagramprover.wm-wire :as w]))

(defn initialization-products [reader]
  (let [read-file slurp result (atom nil)]
    ;; Preserve the owning test's fixture bytes while resolving from either repo.
    (with-redefs [clojure.core/slurp
                  (fn [path & opts]
                    (apply read-file
                           (if (and (string? path) (str/starts-with? path "test/fixtures/"))
                             (or (io/resource (subs path 5))
                                 (throw (ex-info "Missing fixture" {:path path}))) path) opts))]
      (require 'futon2.aif.token-observation-initialization-test)
      (hermetic/with-hermetic-stores
       (fn []
         ((ns-resolve 'futon2.aif.token-observation-initialization-test 'with-two-ticks)
          (fn [{:keys [second signed execution]}]
            (let [stage (get-in second [:selection-certificate :token-belief-stage])
                  inspection (get-in second [:selection-certificate :token-belief-input :inspection])
                  token @(ns-resolve 'futon2.aif.token-observation-initialization-test 'unknown)
                  q0 (get-in stage [:initialization :value])
                  q1 (reduce-kv (fn [q s mass] (update q (conj s token) (fnil + 0) mass)) {} q0)
                  changed (assoc-in stage [:initialization :value] q1)
                  read! (fn [s observations]
                          (if (= reader :receipt)
                            (#'predecessor/initialization-input-receipt s inspection execution observations)
                            (policy/apply-observations s inspection observations)))]
              (reset! result {:stages [stage changed] :token token :initial [q0 q1]
                              :products [(read! stage signed) (read! changed signed)]
                              :no-updates [(read! stage (assoc signed :observations {}))
                                           (read! changed (assoc signed :observations {}))]})))))))
    @result))

(defn hash-products []
  (let [root (w/tmp-dir "hash-products-") text "# Target\nBuild the artifact.\n"
        reading served/reading
        flight {:target "hash-fixture" :want-source {:kind :a-exits :repo "fixture"
                                                     :path "mission.md" :store root}}
        read! (runner/read-fn {:store root :read-text (fn [& _] text)
                              :answer-fn (fn [_] {:state "pending" :seat "fixture"})})]
    (try
      (mapv (fn [change]
              (let [carrier (atom nil)
                    product (with-redefs [served/reading
                                          (fn [& args]
                                            (let [v (update (apply reading args) :text-sha256 change)]
                                              (reset! carrier v) v))]
                              (read! flight {}))
                    path (io/file root "readback.edn")]
                (spit path (pr-str product))
                {:carrier @carrier :product (edn/read-string (slurp path))}))
            [identity (constantly (served/sha256 "different text"))])
      (finally (doseq [file (reverse (file-seq (io/file root)))] (io/delete-file file))))))
