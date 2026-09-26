#!/usr/bin/env bb
;; wm_org_paths_test.bb -- ORG-PATHS-I: the organisation layer keeps every distinct call
;; path to a box, and the coverage partition takes the disjunction over them.
;;
;;   bb holes/labs/M-wm-wiring/spike/wm_org_paths_test.bb
;;
;; It regenerates the org layer at the OLD inputs the committed coverage was derived from
;; (map 47513216, futon2 3fcfa0df; ledger 06d451d6) into /tmp, so nothing under the repo
;; is written, and checks three things:
;;   1. the paths the layer has always kept are unchanged: the regenerated calls, with
;;      :other-paths removed, equal the committed layer at 7901a5a0;
;;   2. the bad case: coverage over a layer with ONLY the first path (the committed layer,
;;      which has no :other-paths, and the regenerated one under
;;      WM_COVERAGE_FIRST_PATH_ONLY=1) reproduces the committed counts 98/14/4/20;
;;   3. over the full-path layer exactly the six wires COMMISSION-D names move, each from
;;      :conditional to :witness, and the four :failure-path wires stay :failure-path.
;; Takes a few minutes (the generator reads every site file from git).
(require '[clojure.edn :as edn] '[clojure.java.shell :as sh] '[clojure.string :as str])
(def home (System/getProperty "user.home"))
(def f3c (str home "/code/futon3c"))
(def lab "holes/labs/M-wm-wiring/")
(def spike (str f3c "/" lab "spike/"))
(def failures (atom []))
(defn check [what ok?] (println (if ok? "ok  " "FAIL") what) (when-not ok? (swap! failures conj what)))
(defn run [env & args]
  (let [{:keys [exit out err]} (apply sh/sh (concat args [:env (merge (into {} (System/getenv)) env)]))]
    (when-not (zero? exit) (println err) (System/exit 2))
    out))
(defn read-edn [s] (edn/read-string {:default (fn [_ v] v)} s))
(def tmp (str (System/getProperty "java.io.tmpdir") "/wm-org-paths-test"))
(.mkdirs (java.io.File. tmp))
(def committed-src (:out (sh/sh "git" "-C" f3c "show" (str "7901a5a0:" lab "wm-org-layer.edn"))))
(def committed-file (str tmp "/committed.edn"))
(spit committed-file committed-src)
(def full-file (str tmp "/old-full.edn"))
(run {"WM_ORG_OUT" full-file} "bb" (str spike "wm_org_layer.bb") "47513216" "3fcfa0df")
(defn coverage [org-file out first-only?]
  (run (cond-> {"WM_COVERAGE_OUT" out} first-only? (assoc "WM_COVERAGE_FIRST_PATH_ONLY" "1"))
       "bb" (str spike "wm_coverage.bb") "06d451d6" org-file)
  (read-edn (slurp out)))
(def committed-cov (read-edn (:out (sh/sh "git" "-C" f3c "show" (str "440624b3:" lab "wm-wire-coverage.edn")))))
(def want {:witness 98 :conditional 14 :failure-path 4 :unreachable 20 :wires 136})

(let [strip #(mapv (fn [c] (dissoc c :other-paths)) (:calls %))
      full (read-edn (slurp full-file)) old (read-edn committed-src)]
  (check "the paths kept before are unchanged (calls minus :other-paths = the committed layer)" (= (strip full) (strip old)))
  (check "the committed layer has no :other-paths" (not-any? :other-paths (:calls old)))
  (check "the full layer has :other-paths" (some :other-paths (:calls full))))

(check "the committed coverage is 98/14/4/20" (= want (into {} (:counts committed-cov))))
(let [c (coverage committed-file (str tmp "/cov-committed-layer.edn") false)]
  (check "an org layer with only the first path reproduces 98/14/4/20" (= want (into {} (:counts c)))))
(let [c (coverage full-file (str tmp "/cov-first.edn") true)]
  (check "the full layer read first-path-only reproduces 98/14/4/20" (= want (into {} (:counts c)))))
(def six
  #{[:flight-entry :r9-close-cause [:target {:record :flight}]]
    [:r9-close-cause :failure-cause-record-test :failure-cause]
    [:r9-close-cause :r9-finding-store :failure-cause]
    [:r9-decision :r9-judge-refusal-read :kind]
    [:r9-phase-kind :phase-kind-test :failure-kind]
    [:r9-phase-kind :r9-failure-classifier :failure-kind]})
(let [c (coverage full-file (str tmp "/cov-full.edn") false)
      ch (get-in c [:method :changed])
      by (into {} (map (juxt :wire :coverage) (:wires c)))
      failure-wires (set (map :wire (filter #(= :failure-path (:coverage %)) (:wires committed-cov))))]
  (check "exactly the six wires COMMISSION-D names moved" (= six (set (map :wire ch))))
  (check "each moved from :conditional to :witness"
         (every? #(and (= :conditional (get-in % [:first-path :coverage])) (= :witness (get-in % [:all-paths :coverage]))) ch))
  (check "each moved wire's first-path condition was (if commissioned?) then"
         (every? #(= ["(if commissioned?) then"] (get-in % [:first-path :conditions])) ch))
  (check "the four :failure-path wires are unchanged" (and (= 4 (count failure-wires)) (every? #(= :failure-path (by %)) failure-wires)))
  (check "counts move 98/14/4/20 -> 104/8/4/20" (= {:witness 104 :conditional 8 :failure-path 4 :unreachable 20 :wires 136} (into {} (:counts c))))
  (check "no undecided condition text" (empty? (:undecided c))))
(if (seq @failures) (do (println (count @failures) "FAILED") (System/exit 1)) (println "all checks passed"))
