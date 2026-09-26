(require '[clojure.edn :as edn]
         '[futon2.aif.c-fold-config :as digest]
         '[futon3c.wm.run4-historical-qualification :as q])
(defn one [text]
  (with-open [r (java.io.PushbackReader. (java.io.StringReader. text))]
    (let [x (edn/read r)] (assert (= ::end (edn/read {:eof ::end} r))) x)))
(let [path "holes/labs/wm-contract/runs/RUN4-U88-successor-deployment-2026-09-11/repair-057-revalidation-20260911-v1.qualification.edn"
      text (slurp path) receipt (one text)
      plan-text (slurp (get-in receipt [:manifest :path]))
      plan (q/validate-plan! (one plan-text))]
  (assert (= "32701a6ef7204828fa65608097ba865691ec47198ea7dbf33e5b9407fed6c582" (digest/sha256 text)))
  (assert (= (get-in receipt [:manifest :sha256]) (digest/sha256 plan-text)))
  (assert (= (:sources plan) (:sources receipt)))
  (doseq [{:keys [path sha256]} (:sources plan)] (assert (= sha256 (digest/sha256 (slurp path)))))
  (assert (= (:checks plan) (mapv #(select-keys % [:id :argv :timeout-ms]) (:checks receipt))))
  (assert (= [:full-runner-regression :repair-store-regression] (mapv :id (:checks receipt))))
  (doseq [row (:checks receipt)]
    (assert (zero? (:exit row))) (assert (false? (:timed-out? row)))
    (doseq [k [:stdout :stderr]] (assert (= (get-in row [k :sha256]) (digest/sha256 (get-in row [k :utf8])))))
    (assert (re-find #"0 failures, 0 errors" (get-in row [:stdout :utf8])))
    (assert (= "" (get-in row [:stderr :utf8]))))
  (assert (true? (:qualification-passed? receipt)))
  (assert (= :not-performed (:independent-review receipt)))
  (assert (false? (:repair-admitted? receipt)))
  (prn {:receipt-sha256 (digest/sha256 text) :source-count (count (:sources receipt))
        :rows (mapv #(select-keys % [:id :exit :timed-out?]) (:checks receipt))
        :independent-content-review :passed :repair-admitted? false}))
