(ns futon3c.arxana.together-pdf
  "A Together document as a PDF for printing: US letter, LuaLaTeX, wide line
  spacing and a wide right margin for notes by hand. `tex` is pure. `pdf!` runs
  lualatex in a temporary directory, one run at a time, and keeps the last
  result per document key and revision."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.concurrent TimeUnit]))

;; Same colours as FACE_STYLES and MARK_COLOURS in web/binding.js.
(def ^:private face-colours
  {"muted" "6b7280" "typed" "1d7a1d" "call" "1f5fbf" "agent" "7a2fb0"
   "teammate" "a35200" "peeragogy" "8b2a7a" "struck" "b42318"})

(def ^:private mark-colours {"ok" "1d7a1d" "debt" "b26a00" "bad" "b42318"})

(def ^:private emacs-faces
  {"futon4a-variants-diff-added" "typed" "futon4a-incubator-clip" "call"
   "futon4a-incubator-agent" "agent" "futon4a-incubator-teammate" "teammate"
   "futon4a-incubator-peeragogy" "peeragogy" "futon4a-variants-diff-dropped" "struck"
   "futon4a-incubator-struck" "struck" "bold" "heading" "shadow" "muted"})

(defn- get* [m k] (when (map? m) (or (get m k) (get m (keyword k)))))

(defn- face-of
  "The face name for ATTRS, as faceOf in web/binding.js: a direct name, else the
  first known Emacs face under prov.face."
  [attrs]
  (let [direct (get* attrs "face")
        emacs (get* (get* attrs "prov") "face")
        names (case (get* emacs "tag")
                "symbol" [(get* emacs "value")]
                "list" (keep #(when (= "symbol" (get* % "tag")) (get* % "value"))
                             (let [v (get* emacs "value")] (when (sequential? v) v)))
                [])]
    (cond
      (and (string? direct) (or (= direct "plain") (= direct "heading")
                                (contains? face-colours direct))) direct
      :else (some emacs-faces names))))

(def ^:private specials
  {\\ "\\textbackslash{}" \{ "\\{" \} "\\}" \$ "\\$" \& "\\&" \# "\\#" \_ "\\_"
   \% "\\%" \^ "\\textasciicircum{}" \~ "\\textasciitilde{}" \` "\\textasciigrave{}"
   \- "-{}" \< "\\textless{}" \> "\\textgreater{}" \" "\\textquotedbl{}"
   \tab "\\hspace*{2em}"
   ;; U+2442 (the incubator's fork sign) is in none of the fonts on this host.
   \u2442 "\u22d4"})

(defn escape
  "TEXT with every character TeX treats specially made literal. Control
  characters other than tab are dropped."
  [text]
  (let [out (StringBuilder.)]
    (doseq [c text]
      (cond
        (contains? specials c) (.append out ^String (specials c))
        (< (int c) 32) nil
        (= (int c) 127) nil
        :else (.append out c)))
    (str out)))

(defn- lines
  "Split live RUNS into lines; each line is a vector of [style text] pieces,
  where style is {:face name :mark name}."
  [runs]
  (loop [pieces (for [run runs
                      :when (not (get* run "deleted"))
                      :let [attrs (get* run "attrs")
                            mark (get* attrs "mark")
                            style {:face (face-of attrs)
                                   :mark (when (contains? mark-colours mark) mark)}]
                      part (interpose :newline (str/split (str (get* run "text")) #"\n" -1))]
                  (if (= part :newline) :newline [style part]))
         line [] out []]
    (if-let [piece (first pieces)]
      (if (= piece :newline)
        (recur (rest pieces) [] (conj out line))
        (recur (rest pieces) (if (seq (second piece)) (conj line piece) line) out))
      (conj out line))))

(defn- piece-tex [[{:keys [face mark]} text]]
  (let [body (escape text)
        ;; \color ends in \ignorespaces and TeX skips spaces after \small, so
        ;; a piece that starts with a space would lose it ("theeconomic").
        body (if (str/starts-with? body " ") (str "{}" body) body)
        colour (or (mark-colours mark) (face-colours face))
        body (if (= face "struck") (str "\\strikeThrough{" body "}") body)
        body (if (= face "muted") (str "{\\small{}" body "}") body)]
    (if colour (str "\\textcolor[HTML]{" colour "}{" body "}") body)))

(defn- line-tex [line]
  (let [text (apply str (map second line))
        indent (count (re-find #"^ +" text))
        heading? (and (seq line) (every? #(= "heading" (:face (first %))) line))
        ;; drop the leading spaces from the first pieces; \hspace* restores them
        trimmed (loop [n indent ps line acc []]
                  (if (and (pos? n) (seq ps))
                    (let [[style t] (first ps) k (min n (count t))]
                      (recur (- n k) (rest ps)
                             (if (< k (count t)) (conj acc [style (subs t k)]) acc)))
                    (into acc ps)))
        body (apply str (map piece-tex trimmed))]
    (cond
      (str/blank? text) "\\blankline"
      heading? (str "\\docheading{" (escape (str/trim text)) "}")
      :else (str "\\docline{" (format "%.2f" (* 0.5 indent)) "em}{" body "}"))))

(defn tex
  "The LuaLaTeX source for a document. KEY and REV go in the running head; AT
  is the printing time as a string."
  [{:keys [key rev at runs]}]
  (str/join
   "\n"
   (concat
    ["\\documentclass[11pt,letterpaper]{article}"
     "\\usepackage[letterpaper,left=1.2in,right=1.7in,top=1.1in,bottom=1.2in,headsep=0.3in,footskip=0.5in]{geometry}"
     "\\usepackage{fontspec}"
     "\\directlua{luaotfload.add_fallback('glyphs', {'DejaVu Sans:mode=harf;'})}"
     "\\setmainfont{Libertinus Serif}[RawFeature={fallback=glyphs}]"
     "\\setsansfont{Libertinus Sans}[RawFeature={fallback=glyphs}]"
     "\\usepackage{microtype}"
     "\\usepackage[table]{xcolor}"
     "\\usepackage{lua-ul}"
     "\\usepackage{setspace}"
     "\\usepackage{fancyhdr}"
     "\\setstretch{1.45}"
     "\\setlength{\\parindent}{0pt}"
     "\\setlength{\\parskip}{0pt}"
     "\\raggedright"
     "\\hyphenpenalty=600 \\emergencystretch=2em"
     "\\pagestyle{fancy}\\fancyhf{}"
     "\\renewcommand{\\headrulewidth}{0pt}"
     (str "\\fancyhead[L]{\\footnotesize\\sffamily\\color[HTML]{6b7280}" (escape (str key))
          " \\textperiodcentered{} revision " (escape (str rev)) "}")
     (str "\\fancyhead[R]{\\footnotesize\\sffamily\\color[HTML]{6b7280}printed " (escape (str at)) "}")
     "\\fancyfoot[C]{\\footnotesize\\sffamily\\color[HTML]{6b7280}\\thepage}"
     "\\newcommand{\\blankline}{\\par\\vspace{0.55\\baselineskip}}"
     "\\newcommand{\\docheading}[1]{\\par\\vspace{0.35\\baselineskip}{\\sffamily\\bfseries\\large #1\\par}\\nopagebreak\\vspace{0.1\\baselineskip}}"
     "\\newcommand{\\docline}[2]{\\par\\hangindent=#1\\hangafter=0\\relax #2\\par}"
     "\\begin{document}"]
    (map line-tex (lines runs))
    ["\\end{document}" ""])))

(defonce ^:private render-lock (Object.))
(defonce ^:private last-result (atom {}))

(defn- delete-tree! [^File dir]
  (doseq [^File f (reverse (file-seq dir))] (.delete f)))

(defn- lualatex
  "The lualatex to run. The JVM's PATH on zone finds /usr/bin/lualatex, a
  distribution TeX Live without fontspec (2026-11-20), so prefer the newest
  /usr/local/texlive/<year>; TOGETHER_LUALATEX overrides both."
  []
  (or (System/getenv "TOGETHER_LUALATEX")
      (some->> (.listFiles (io/file "/usr/local/texlive"))
               (filter #(re-matches #"\d{4}" (.getName ^File %)))
               (sort-by #(.getName ^File %))
               reverse
               (mapcat #(file-seq (io/file % "bin")))
               (filter #(and (= "lualatex" (.getName ^File %)) (.canExecute ^File %)))
               first
               str)
      "lualatex"))

(defn pdf!
  "Run lualatex on SOURCE and return the PDF's bytes, or throw ex-info with
  :log (the end of the TeX log). Shell escape is off; the run is killed after
  TIMEOUT-S seconds."
  ([source] (pdf! source 90))
  ([source timeout-s]
   (locking render-lock
     (let [dir (.toFile (Files/createTempDirectory "together-pdf" (make-array FileAttribute 0)))]
       (try
         (spit (io/file dir "doc.tex") source :encoding "UTF-8")
         (let [builder (doto (ProcessBuilder.
                              ^java.util.List [(lualatex) "-no-shell-escape" "-interaction=batchmode"
                                               "-halt-on-error" "doc.tex"])
                         (.directory dir)
                         (.redirectErrorStream true)
                         (.redirectOutput (io/file dir "run.out")))
               _ (.put (.environment builder) "openin_any" "p")
               _ (.put (.environment builder) "openout_any" "p")
               process (.start builder)
               done? (.waitFor process timeout-s TimeUnit/SECONDS)
               pdf (io/file dir "doc.pdf")]
           (when-not done? (.destroyForcibly process))
           (if (and done? (zero? (.exitValue process)) (.isFile pdf))
             (Files/readAllBytes (.toPath pdf))
             (let [log (io/file dir "doc.log")
                   text (if (.isFile log) (slurp log :encoding "UTF-8") "")]
               (throw (ex-info (if done? "lualatex failed" "lualatex timed out")
                               {:log (subs text (max 0 (- (count text) 1500)))})))))
         (finally (delete-tree! dir)))))))

(defn document-pdf!
  "PDF bytes for SNAPSHOT {:key :rev :at :runs}; reuses the last render of the
  same key and revision."
  [{:keys [key rev] :as snapshot}]
  (let [cached (get @last-result key)]
    (if (= rev (:rev cached))
      (:bytes cached)
      (let [bytes (pdf! (tex snapshot))]
        (swap! last-result assoc key {:rev rev :bytes bytes})
        bytes))))
