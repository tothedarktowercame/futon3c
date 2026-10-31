(ns futon3c.giraffe.provenance-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.giraffe.provenance :as sut]
            [futon3c.giraffe.readings :as readings]))

(defn- ticket-item [text]
  (let [f (java.io.File/createTempFile "giraffe-ticket-" ".md")]
    (spit f text)
    {:id :T-open :state-group :unstarted
     :source-pin {:path (.getAbsolutePath f) :sha256 (readings/sha256 text)}}))

(deftest closed-origin-open-ticket-requires-current-relevance-evidence
  (let [ticket (ticket-item "# T-open\n\nParent: `E-closed.md` slice 4.\n")
        input {:items [{:id :E-closed :state-group :completed} ticket]
               :reading-evidence {:links []}}
        row (first (:tickets (sut/project input)))]
    (is (= {"E-closed" "completed"} (:origin-states row)))
    (is (= {:status :review-required
            :reason :closed-origin-open-ticket}
           (:relevance row)))
    (is (= 1 (get-in (sut/project input) [:summary :with-origin])))
    (testing "a current witness changes relevance, not lineage or readiness"
      (let [witness {:status :current :evidence-id "e-current"
                     :claim "the replay mechanism remains"}
            witnessed (sut/project (assoc input :relevance-witnesses
                                          {:T-open witness}))]
        (is (= {:status :current :witness witness}
               (get-in witnessed [:tickets 0 :relevance])))
        (is (= (:lineage row) (get-in witnessed [:tickets 0 :lineage])))))))

(deftest checked-and-source-lineage-merge-with-both-attestations
  (let [ticket (ticket-item "# T-open\n\nParent: M-parent\n")
        path (get-in ticket [:source-pin :path])
        sha (get-in ticket [:source-pin :sha256])
        out (sut/project
             {:items [{:id :M-parent :state-group :started} ticket]
              :reading-evidence
              {:links [{:from "T-open" :to "M-parent" :kind "part-of"
                        :basis "declared" :evidence {:path path :line 3
                                                     :quote "Parent: M-parent"
                                                     :sha256 sha}}]}})
        edge (get-in out [:tickets 0 :lineage 0])]
    (is (= ["checked-reading" "source-declaration"]
           (mapv :source (:attestations edge))))
    (is (= {:status :review-required :reason :no-current-relevance-witness}
           (get-in out [:tickets 0 :relevance])))))

(deftest source-drift-is-refused-not-treated-as-missing-lineage
  (let [ticket (ticket-item "# T-open\n\nParent: M-parent\n")]
    (spit (io/file (get-in ticket [:source-pin :path]))
          "# T-open\n\nParent: M-other\n")
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"changed after population pin"
         (sut/project {:items [{:id :M-parent :state-group :started} ticket]
                       :reading-evidence {:links []}})))))
