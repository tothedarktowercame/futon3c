#!/usr/bin/env bb
;; catalog_census.bb — the reproducible honest census of substrate-2's DECLARED catalog.
;;
;; E-clean-up-substrate-2 exit-2: a durable (non-/tmp) tool that re-derives the
;; honest denominator on demand, so "% populated" means something and the
;; explainer never drifts to a hand-counted, stale figure again. (The registry
;; is an append-only run-write! byproduct — it grew 208→216 docs in a few hours
;; on 2026-06-26, which is exactly why this must be a tool, not a number.)
;;
;; Method (per futon1a/README-census.md): per-type COUNT-PUSHDOWN over the
;; declared catalog (entity → :entity/type, relation → :hx/type) via Drawbridge
;; (:6768). Never a high-limit ?type= scan (footgun #1: a 0 from that is a
;; timeout lie). One round-trip: all counting happens in-process server-side.
;;
;;   bb catalog_census.bb            # human summary + write the EDN snapshot
;;   bb catalog_census.bb --quiet    # write snapshot only
;;   bb catalog_census.bb --out PATH # snapshot path (default beside the excursion)
(require '[babashka.http-client :as http]
         '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[clojure.pprint :as pp])

(def TOKEN (str/trim (slurp "/home/joe/code/futon3c/.admintoken")))
(def DRAWBRIDGE "http://127.0.0.1:6768/eval")
(def DEFAULT-OUT
  "/home/joe/code/futon3c/holes/excursions/substrate-2-catalog-census.edn")

;; The server-side census form. All count-pushdowns run in-process (one HTTP
;; round-trip). Counts a declared type by its kind's index; globs (`*`) excluded
;; from the denominator; dup type-ids (same :type/id, >1 doc) surfaced.
(def FORM "
(require '[clojure.string :as str] '[clojure.set :as set])
(let [node (:node @futon3c.dev/!f1-sys)
      db   (xtdb.api/db node)
      tdocs (map first (xtdb.api/q db '{:find [(pull d [:type/id :type/kind :xt/id])]
                                        :where [[d :type/id]]}))
      by-id (group-by :type/id tdocs)
      glob? (fn [id] (str/includes? (str id) \"*\"))
      glob-ids (->> by-id keys (filter glob?) (sort) vec)
      dup-ids  (->> by-id (filter #(> (count (val %)) 1)) (map key) (sort) vec)
      decl-ids (->> by-id keys (remove glob?) vec)
      cnt  (fn [attr tid] (or (ffirst (xtdb.api/q db {:find ['(count e)] :where [['e attr tid]]})) 0))
      count-for (fn [id kind]
                  (cond (= kind :relation) (cnt :hx/type id)
                        (= kind :entity)   (cnt :entity/type id)
                        :else (max (cnt :entity/type id) (cnt :hx/type id))))
      counted (mapv (fn [id]
                      (let [kind (-> (by-id id) first :type/kind)]
                        {:id id :kind kind :n (count-for id kind)}))
                    decl-ids)
      populated (->> counted (filter #(pos? (:n %))) (sort-by :n >) vec)
      empties   (->> counted (filter #(zero? (:n %))) (sort-by (comp str :id)) (mapv #(dissoc % :n)))
      ;; off-catalog: distinct :entity/type values in the store NOT declared
      ;; (the :entity/type free-distinct works; the :hx/type one errors in
      ;; Drawbridge — README footgun — so hyperedge off-catalog is reported via
      ;; the known heavies only).
      ent-in-store (->> (xtdb.api/q db '{:find [t] :where [[e :entity/type t]]}) (map first) set)
      offcat-entity (->> (set/difference ent-in-store (set (keys by-id))) (sort) vec)
      heavies {:code/v05/var (cnt :hx/type :code/v05/var)
               :code/v05/edits (cnt :hx/type :code/v05/edits)
               :mission-scope/eightfold-phase (cnt :hx/type :mission-scope/eightfold-phase)}]
  ;; stringify all type-ids — the registry holds malformed keys (e.g. the
  ;; double-colon glob ::pattern/* §3.1) that are not valid EDN to read back.
  {:total-docs (count tdocs)
   :distinct-ids (count by-id)
   :n-globs (count glob-ids) :globs (mapv str glob-ids)
   :n-dup-ids (count dup-ids) :dup-ids (mapv str dup-ids)
   :honest-denom (count decl-ids)
   :n-populated (count populated)
   :n-empty (count empties)
   :pct (when (pos? (count decl-ids)) (long (Math/round (* 100.0 (/ (count populated) (double (count decl-ids)))))))
   :populated (mapv (fn [m] {:id (str (:id m)) :kind (str (:kind m)) :n (:n m)}) populated)
   :empty (mapv (fn [m] {:id (str (:id m)) :kind (str (:kind m))}) empties)
   :offcat-entity-types (mapv str offcat-entity)
   :heavies (into {} (map (fn [[k v]] [(str k) v]) heavies))})
")

(defn eval-drawbridge [form]
  (let [resp (http/post DRAWBRIDGE
                        {:headers {"x-admin-token" TOKEN "Content-Type" "text/plain"}
                         :body form :throw false})]
    (when (not= 200 (:status resp))
      (binding [*out* *err*]
        (println "Drawbridge eval failed:" (:status resp) (:body resp)))
      (System/exit 1))
    (let [out (edn/read-string (:body resp))]
      (if (:ok out)
        (:value out)
        (do (binding [*out* *err*] (println "eval error:" (pr-str out)))
            (System/exit 1))))))

(defn -main [& args]
  (let [quiet? (some #{"--quiet"} args)
        out-path (or (second (drop-while #(not= "--out" %) args)) DEFAULT-OUT)
        r (eval-drawbridge FORM)]
    (spit out-path (with-out-str (pp/pprint (assoc r :method "per-type count-pushdown via Drawbridge :6768"
                                                     :source "futon3c/scripts/catalog_census.bb"))))
    (when-not quiet?
      (println "=== substrate-2 catalog census (count-pushdown, live :7071) ===")
      (println (format "declared type-docs ............ %d" (:total-docs r)))
      (println (format "  distinct type-ids .......... %d  (%d dup-id docs)" (:distinct-ids r) (:n-dup-ids r)))
      (println (format "  abstract globs (*) ......... %d  → excluded from denominator" (:n-globs r)))
      (println (format "HONEST DENOMINATOR ........... %d real declared types" (:honest-denom r)))
      (println (format "  populated .................. %d  (%d%%)" (:n-populated r) (:pct r)))
      (println (format "  empty ...................... %d" (:n-empty r)))
      (println "")
      (println "top declared populations:")
      (doseq [{:keys [id kind n]} (take 8 (:populated r))]
        (println (format "  %-34s %-9s %d" (str id) (str (or kind "?")) n)))
      (println "")
      (println "heaviest OFF-CATALOG populations (not declared types):")
      (doseq [[id n] (sort-by val > (:heavies r))]
        (println (format "  %-34s %d" (str id) n)))
      (println (format "off-catalog ENTITY types (in store, not declared): %d" (count (:offcat-entity-types r))))
      (println "")
      (println (format "globs (catalog hygiene — never populate): %s" (str/join " " (map str (:globs r)))))
      (when (seq (:dup-ids r))
        (println (format "dup type-ids (same id, >1 doc): %s" (str/join " " (map str (:dup-ids r))))))
      (println "")
      (println (str "wrote snapshot → " out-path)))))

(apply -main *command-line-args*)
