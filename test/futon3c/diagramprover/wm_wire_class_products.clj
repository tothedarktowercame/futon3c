(ns futon3c.diagramprover.wm-wire-class-products
  (:require [clojure.java.io :as io]
            [futon2.aif.cascade-problems :as cp]
            [futon2.aif.locator-fixtures :as loc]
            [futon2.aif.focus-receipt :as focus]
            [futon2.aif.efe :as efe]
            [futon2.report.cascade-decision-test :as fixture]
             [futon2.aif.wm.cascade-decision :as wm-cd]
            [futon3c.diagramprover.wm-wire :as w]))

(defn class-product [hop changed?]
  (let [classify focus/classify-target model @#'wm-cd/class-observation-model
        rank efe/rank-actions calls (atom []) carrier (atom nil)
        assembled (cp/assemble {:targets [fixture/tick-1-target]
                                :sources (loc/locate-all fixture/tick-1-sources)})
        result
        (with-redefs-fn
          {#'focus/classify-target
           (fn [& args]
             (let [r (apply classify args)]
               (if (= hop :class)
                 (let [r (if changed? (assoc r :class :associated) r)]
                   (reset! carrier (:class r)) r) r)))
           #'wm-cd/class-observation-model
           (fn [inputs]
             (let [inputs (if (and (= hop :target-class) changed?)
                            (update inputs :target-class #(update-vals % (constantly :related))) inputs)]
               (when (= hop :target-class) (reset! carrier (:target-class inputs)))
               (model inputs)))
           #'efe/rank-actions
           (fn [state candidates opts]
             (let [r (rank state candidates opts)]
               (when (:prediction-context opts)
                 (swap! calls conj
                        {:scores (mapv :controller-score r)
                         :controls {:state state :candidates candidates
                                    :opts (-> opts
                                              (update :prediction-context dissoc :occurrence-id)
                                              (update :observation-model dissoc :target-class))}})) r))}
          #(wm-cd/cascade-decision assembled fixture/live-c-opts))]
    {:carrier @carrier :scores (:scores (first @calls))
     :calls (count @calls) :controls (:controls (first @calls))
     :beta (get-in result [:decision :selection-law :beta])}))

(defn provenance-products []
  ;; Tiny generated embedding: no host-specific checkout or fixture path.
  (let [root (.toFile (java.nio.file.Files/createTempDirectory
                       "wire-class-" (make-array java.nio.file.attribute.FileAttribute 0)))
        dir (doto (io/file root "mission-structure-embed") .mkdirs)
        json (io/file dir "mission-embed.json") npy (io/file dir "structure-embeddings.npy")
        header "{'descr': '<f8', 'fortran_order': False, 'shape': (2, 2), }\n"
        buf (doto (java.nio.ByteBuffer/allocate (+ 10 (count header) 32))
              (.order java.nio.ByteOrder/LITTLE_ENDIAN))]
    (try
      (spit json "{\"stems\":[\"query\",\"neighbour\"]}")
      (.put buf (byte-array [(unchecked-byte 147) 78 85 77 80 89 1 0]))
      (.putShort buf (short (count header))) (.put buf (.getBytes header "ISO-8859-1"))
      (doseq [x [1.0 0.0 1.0 0.0]] (.putDouble buf x))
      (with-open [out (io/output-stream npy)] (.write out (.array buf)))
      (let [row {:target "M-neighbour" :facet "WM" :relation "associated"
                 :source {:repo "fixture"} :effective-from "2026-01-01T00:00:00Z"}
            inputs {:relations [row] :embedding {:source-pins
                                                (mapv (fn [f] {:path (str f) :sha256 (w/sha256-file (str f))}) [json npy])}}
            discovery {:status :retained :facet-graph {:active ["WM"] :background []}}
            writer focus/embedding-neighbour]
        (mapv (fn [changed?]
                (let [supplied (atom nil)
                      r (with-redefs [focus/embedding-neighbour
                                      (fn [& args]
                                        (let [r (apply writer args)
                                              r (if changed? (assoc-in r [:derived-via :cosine] 0.25) r)]
                                          (reset! supplied (:derived-via r)) r))]
                          (focus/classify-target inputs discovery "2026-09-27T00:00:00Z" "M-query"
                                                 {:mission-text-fn (constantly nil)}))]
                  {:supplied @supplied :recorded (:derived-via r)
                   :calculation (dissoc r :derived-via)})) [false true]))
      (finally (doseq [f (reverse (file-seq root))] (io/delete-file f))))))
