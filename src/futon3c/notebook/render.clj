(ns futon3c.notebook.render
  "A minimal literate-notebook renderer: a .clj file read top to bottom,
   where runs of `;; ` comment lines are prose and every other form is code.
   Each form is evaluated in order in the notebook's own namespace; the page
   shows the prose, the code, whatever it printed, and its value.

   Prose markup is deliberately small: a line starting `# ` or `## ` is a
   heading, a blank `;;` line separates paragraphs, `code` is inline code.

   Usage: clojure -M -m futon3c.notebook.render NOTEBOOK.clj OUT.html"
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str])
  (:import [java.io PushbackReader StringReader]))

(defn- esc [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))

(defn- inline [s]
  (str/replace (esc s) #"`([^`]+)`" "<code>$1</code>"))

(defn- prose->html [lines]
  (->> (partition-by str/blank? lines)
       (remove #(str/blank? (first %)))
       (map (fn [para]
              (let [l (first para)]
                (cond
                  (str/starts-with? l "## ") (str "<h2>" (inline (subs l 3)) "</h2>")
                  (str/starts-with? l "# ") (str "<h1>" (inline (subs l 2)) "</h1>")
                  :else (str "<p>" (inline (str/join " " para)) "</p>")))))
       (str/join "\n")))

(defn blocks
  "The notebook TEXT as [{:prose [lines]} | {:code source}] in order."
  [text]
  (let [lines (str/split-lines text)]
    (loop [ls lines acc [] prose nil code nil]
      (let [flush-prose #(if prose (conj % {:prose prose}) %)
            flush-code #(if (and code (not (str/blank? (str/join "\n" code))))
                          (conj % {:code (str/trimr (str/join "\n" code))}) %)]
        (if-let [[l & more] (seq ls)]
          (cond
            (re-matches #"\s*;;( .*)?" l)
            (recur more (flush-code acc) (conj (or prose []) (str/replace l #"^\s*;; ?" "")) nil)
            (and (str/blank? l) (nil? code))
            (recur more acc (when prose (conj prose "")) nil)
            :else
            (recur more (flush-prose acc) nil (conj (or code []) l)))
          (-> acc flush-prose flush-code))))))

(defn- read-forms [source]
  (let [r (PushbackReader. (StringReader. source))]
    (loop [acc []]
      (let [f (read {:eof ::eof} r)]
        (if (= ::eof f) acc (recur (conj acc f)))))))

(defn- show [v]
  (let [s (with-out-str (pp/pprint v))]
    (if (> (count s) 6000) (str (subs s 0 6000) "\n…") s)))

(defn run
  "Evaluate NOTEBOOK-PATH's blocks in order; return the rendered HTML."
  [notebook-path]
  (let [text (slurp notebook-path :encoding "UTF-8")
        title (or (some #(when (str/starts-with? % ";; # ") (subs % 5)) (str/split-lines text))
                  "Notebook")
        sections
        (binding [*ns* (create-ns (gensym "notebook-"))]
          (refer-clojure)
          (vec (for [{:keys [prose code]} (blocks text)]
                 (if prose
                   (prose->html prose)
                   (let [results (for [form (read-forms code)]
                                   (let [out (java.io.StringWriter.)
                                         t0 (System/nanoTime)
                                         v (binding [*out* out] (eval form))
                                         ms (/ (- (System/nanoTime) t0) 1e6)]
                                     {:out (str out) :v v :ms ms :form form}))
                         results (doall results)
                         last-r (last results)]
                     (str "<pre class=\"code\">" (esc code) "</pre>\n"
                          (str/join (for [{:keys [out]} results :when (seq out)]
                                      (str "<pre class=\"out\">" (esc out) "</pre>\n")))
                          (when (and last-r (not (and (seq? (:form last-r))
                                                      (#{'ns 'require 'def 'defn 'defn-} (first (:form last-r))))))
                            (str "<pre class=\"val\">" (esc (show (:v last-r))) "</pre>\n"))))))))]
    (str "<!doctype html><html><head><meta charset=\"utf-8\"><title>" (esc title) "</title>"
         "<style>body{max-width:52rem;margin:2rem auto;font:16px/1.55 Georgia,serif;color:#222;padding:0 1rem}"
         "h1{font-size:1.6rem}h2{font-size:1.2rem;margin-top:2rem}"
         "pre{white-space:pre-wrap;font:.8rem/1.45 ui-monospace,Menlo,monospace;padding:.6rem .8rem;margin:.4rem 0}"
         "pre.code{background:#f4f2ea;border-left:3px solid #c9c4b0}"
         "pre.out{background:#fff;border-left:3px solid #9bc}"
         "pre.val{background:#f7f7f7;border-left:3px solid #bbb;color:#333}"
         "code{font:.85em ui-monospace,Menlo,monospace;background:#f4f2ea;padding:0 .2em}</style></head><body>\n"
         (str/join "\n" sections)
         "\n</body></html>\n")))

(defn -main [notebook out]
  (let [html (run notebook)]
    (io/make-parents out)
    (spit out html :encoding "UTF-8")
    (println "wrote" out)
    (shutdown-agents)))
