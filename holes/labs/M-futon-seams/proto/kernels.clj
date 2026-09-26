;; Kernel prototype for PROOF-2a (claude-10, 2026-09-24). Prototype, not spec.
;; Run: bb holes/labs/M-futon-seams/proto/kernels.clj [theta] [horizon] FILE...
;;
;; Candidate transition kernels for a cascade whose patterns carry an
;; `above` structure (context -> pattern edges), run on the same file:
;;   :list    the current WM kernel: first enabled pattern in a precedence
;;            list. The list is a linear extension of `above`; incomparable
;;            patterns get an arbitrary order, so EVERY linear extension is
;;            run (capped) and the spread is reported.
;;   :coapp   (A) every pattern in the enabled frontier fires independently,
;;            each succeeding with theta.
;;   :inter   (B) exactly one frontier pattern fires, chosen uniformly.
;; enabled(p,s): needs ⊆ s, forbids ∩ s = ∅, produces ⊄ s  (CascadeTransition.guard)
;; frontier(s):  enabled patterns with no enabled ancestor under `above`.
;; On a chain, frontier = the first enabled pattern, so all three agree there.
;; The kernel functions live in kernels_lib.clj (one source, shared with
;; ../build_page.clj, which draws the page from the same code).
(load-file (str (.getParent (java.io.File. (System/getProperty "babashka.file"))) "/kernels_lib.clj"))

(let [[theta T & files] *command-line-args*
      theta (Double/parseDouble (or theta "0.8"))
      T (Integer/parseInt (or T "6"))]
  (doseq [f files]
    (let [c (edn/read-string (slurp f))
          pats (:patterns c) ids (keys pats)
          anc (ancestors-map ids (:above c))
          s0 (:initial c) want (:want c)
          exts (linear-extensions ids anc 200)]
      (println "=====" f "| instance" (:instance c) "| theta" theta "horizon" T)
      (println "patterns" (count ids) "above-edges" (count (:above c)) "linear extensions" (count exts) (if (= 200 (count exts)) "(capped)" ""))
      (let [lists (map #(summary (rollout :list pats anc % theta s0 T) want) exts)]
        (println " :list   p(all wants) range" (r3 (apply min (map :p-all-wants lists))) "-" (r3 (apply max (map :p-all-wants lists)))
                 "| E[#wants] range" (r3 (apply min (map :e-wants lists))) "-" (r3 (apply max (map :e-wants lists)))))
      (doseq [k [:coapp :inter]]
        (let [sm (summary (rollout k pats anc nil theta s0 T) want)]
          (println " " k " p(all wants)" (r3 (:p-all-wants sm)) "| E[#wants]" (r3 (:e-wants sm)))))
      ;; where the definitions part ways: reachable states with a frontier of 2+
      (let [reach (keys (rollout :coapp pats anc nil 0.5 s0 T))
            wide (filter #(> (count (frontier pats anc %)) 1) reach)]
        (println " states with frontier >= 2:" (count wide) "of" (count reach) "reachable")
        (doseq [s (take 3 wide)]
          (println "   at" (sort s))
          (println "     frontier" (sort (frontier pats anc s)) "conflicts" (vec (conflicts pats (frontier pats anc s)))))))))
