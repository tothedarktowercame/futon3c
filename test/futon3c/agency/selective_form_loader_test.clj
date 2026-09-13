(ns futon3c.agency.selective-form-loader-test
  (:require [clojure.test :refer [deftest is]]
            [futon3c.agency.selective-form-loader :as loader]))

(def proof {:schema :agency/invoke-ingress-quiescence-v1 :authority :independently-measured
            :admission-rejected? true :active-creations 0 :waiting-creations 0
            :all-creation-surfaces-covered? true})
(defn- refusal [f] (:refusal (try (f) (catch clojure.lang.ExceptionInfo e (ex-data e)))))

(deftest offline-selective-loader-contract
  (let [forms (loader/read-pinned-forms "src/futon3c/transport/http.clj" loader/http-sha256)
        selected (:forms (loader/select-exact-forms forms loader/required-http-forms))]
    (is (= 18 (count selected)))
    (is (= :loader/source-sha-mismatch
           (refusal #(loader/read-pinned-forms "src/futon3c/transport/http.clj" (apply str (repeat 64 "0"))))))
    (is (= :loader/allowlist-missing
           (refusal #(loader/select-exact-forms forms (conj loader/required-http-forms 'not-present)))))
    (is (= :loader/allowlist-missing
           (refusal #(loader/select-exact-forms (conj forms (first selected)) loader/required-http-forms))))
    (is (= :loader/ingress-quiescence-unproved
           (refusal #(loader/preflight! 'futon3c.agency.selective-form-loader-test
                                        (loader/current-serving-preflight) #{} #{}))))
    (is (= :loader/ingress-quiescence-unproved
           (refusal #(loader/preflight! 'futon3c.agency.selective-form-loader-test
                                        (assoc proof :waiting-creations 1) #{} #{}))))
    (is (= :loader/ingress-fence-unavailable
           (refusal #(loader/activate-http-retention! {:source-path "src/futon3c/transport/http.clj"}))))
    (let [n (create-ns (gensym "loader-fixture"))]
      (binding [*ns* n] (clojure.core/refer 'clojure.core) (eval '(def existing :old)))
      (is (= :loader/partial-load-rolled-back
             (refusal #(loader/load-transactionally!
                        {:target-ns (ns-name n) :forms '[(def existing :new) (def newly-interned :new)]
                         :required '[existing newly-interned] :ingress-proof proof :fail-after 1
                         :aliases-required #{} :classes-required #{}}))))
      (is (= :old (var-get (ns-resolve n 'existing))))
      (is (nil? (ns-resolve n 'newly-interned)))
      (let [loaded (loader/load-transactionally!
                    {:target-ns (ns-name n) :forms '[(def existing :loaded) (def loaded-new :loaded)]
                     :required '[existing loaded-new] :ingress-proof proof
                     :aliases-required #{} :classes-required #{}})]
        (is (= :loaded (:status loaded)))
        (is (= '[existing loaded-new] (mapv :name (:forms loaded))))
        (is (= :loaded (var-get (ns-resolve n 'loaded-new)))))
      (remove-ns (ns-name n)))))
