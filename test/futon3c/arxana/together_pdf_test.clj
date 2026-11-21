(ns futon3c.arxana.together-pdf-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.arxana.together-http-test :as h]
            [futon3c.arxana.together-pdf :as pdf])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers]))

(def hostile "\\input{/etc/passwd} $x^2_3$ 50% & #1 ~ {a} `b` -- \"q\" <t> \\write18{id}")

(deftest every-tex-special-is-made-literal
  (let [out (pdf/escape hostile)
        ;; what is left once the escapes this function writes are taken out
        rest (reduce #(str/replace %1 %2 "") out
                     ["\\textbackslash{}" "\\textasciicircum{}" "\\textasciitilde{}"
                      "\\textasciigrave{}" "\\textless{}" "\\textgreater{}" "\\textquotedbl{}"
                      "\\{" "\\}" "\\$" "\\&" "\\#" "\\_" "\\%" "-{}"])]
    (is (not (re-find #"[\\{}$&#_%^~`\"<>]" rest)) rest)
    (is (not (str/includes? out "--")))))

(deftest lines-headings-colours-and-deleted-text
  (let [src (pdf/tex {:key "k" :rev 3 :at "now"
                      :runs [{"text" "Title\n" "attrs" {"face" "heading"} "deleted" false}
                             {"text" "gone" "attrs" {} "deleted" true}
                             {"text" "  body " "attrs" {"face" "plain"} "deleted" false}
                             {"text" "mine" "attrs" {"face" "typed"} "deleted" false}
                             {"text" "\nold" "attrs" {"face" "struck"} "deleted" false}
                             {"text" "\nfine" "attrs" {"mark" "ok"} "deleted" false}]})]
    (is (str/includes? src "\\docheading{Title}"))
    (is (not (str/includes? src "gone")))
    (is (str/includes? src "\\docline{1.00em}{body \\textcolor[HTML]{1d7a1d}{mine}}"))
    (is (str/includes? src "\\textcolor[HTML]{b42318}{\\strikeThrough{old}}"))
    (is (str/includes? src "\\textcolor[HTML]{1d7a1d}{fine}"))
    (is (str/includes? src "letterpaper"))))

(defn- lualatex? []
  (try (zero? (.waitFor (.start (doto (ProcessBuilder. ^java.util.List ["lualatex" "--version"])
                                  (.redirectErrorStream true)
                                  (.redirectOutput (java.io.File. "/dev/null"))))))
       (catch Exception _ false)))

(defn- pdf-text
  "The text of PDF bytes via pdftotext, or nil when it is not installed."
  [^bytes bytes]
  (try
    (let [f (java.io.File/createTempFile "together-pdf-test" ".pdf")]
      (try
        (java.nio.file.Files/write (.toPath f) bytes (make-array java.nio.file.OpenOption 0))
        (let [p (.start (ProcessBuilder. ^java.util.List ["pdftotext" "-layout" (str f) "-"]))
              out (slurp (.getInputStream p))]
          (when (zero? (.waitFor p)) out))
        (finally (.delete f))))
    (catch java.io.IOException _ nil)))

(deftest a-space-at-the-start-of-a-coloured-piece-is-printed
  ;; 2026-11-21: "the" (typed) then " economic" (teammate) printed as
  ;; "theeconomic", since \\color ends in \\ignorespaces.
  (let [runs (fn [face] [{"text" "believe that the" "attrs" {"face" "typed"} "deleted" false}
                         {"text" " economic surplus" "attrs" {"face" face} "deleted" false}
                         {"text" " " "attrs" {"face" "typed"} "deleted" false}
                         {"text" "to them" "attrs" {"face" "plain"} "deleted" false}])
        src (pdf/tex {:key "k" :rev 1 :at "now" :runs (runs "teammate")})]
    (is (str/includes? src "{{} economic surplus}"))
    (when (lualatex?)
      (doseq [face ["teammate" "muted" "struck" "plain"]]
        (when-let [text (pdf-text (pdf/pdf! (pdf/tex {:key "k" :rev 1 :at "now" :runs (runs face)})))]
          (is (re-find #"believe that the economic surplus to them" text) face))))))

(deftest the-route-returns-a-pdf-of-hostile-text
  (when (lualatex?)
    (h/with-server
      (fn [s c _dir]
        (is (= 200 (:status (h/post! s c "print-me" "joe" (h/op "joe" 1 nil (str "Heading\n" hostile)) 0))))
        (let [get-pdf (fn [editor]
                        (let [b (HttpRequest/newBuilder
                                 (URI/create (str "http://127.0.0.1:" (:port s) "/together/v1/doc/print-me/pdf")))]
                          (when editor (.header b "X-Together-Editor" editor))
                          (.send ^HttpClient c (.build (.GET b)) (HttpResponse$BodyHandlers/ofByteArray))))
              resp (get-pdf "joe")
              ^bytes body (.body resp)]
          (is (= 200 (.statusCode resp)))
          (is (= "application/pdf" (.orElse (.firstValue (.headers resp) "content-type") nil)))
          (is (= "%PDF-" (String. body 0 5 "ISO-8859-1")))
          (is (= 401 (.statusCode (get-pdf nil))))
          ;; printing is a read: the document is unchanged
          (is (= 1 (:rev (:body (h/state! s c "print-me"))))))))))
