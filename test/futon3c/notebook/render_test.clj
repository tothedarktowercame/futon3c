(ns futon3c.notebook.render-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.notebook.render :as render]))

(def ^:private inline #'render/inline)

(deftest a-link-in-prose
  (is (= "see <a href=\"https://arxiv.org/abs/1301.6201\">arXiv:1301.6201</a> now"
         (inline "see [arXiv:1301.6201](https://arxiv.org/abs/1301.6201) now")))
  (is (= "<a href=\"http://www-formal.stanford.edu/jmc/elephant/elephant.html\">elephant</a>"
         (inline "[elephant](http://www-formal.stanford.edu/jmc/elephant/elephant.html)"))))

(deftest a-link-inside-backticks-stays-literal
  (is (= "<code>[a](https://x.org)</code>" (inline "`[a](https://x.org)`"))))

(deftest only-http-and-https-become-links
  (testing "javascript: and other schemes stay literal text"
    (is (= "[click](javascript:alert(1)" (subs (inline "[click](javascript:alert(1))") 0 27)))
    (is (not (re-find #"<a " (inline "[click](javascript:alert(1))"))))
    (is (not (re-find #"<a " (inline "[f](file:///etc/passwd)"))))
    (is (not (re-find #"<a " (inline "[f](JaVaScRiPt:x)"))))))

(deftest a-url-cannot-leave-the-href
  (is (= "<a href=\"https://x.org/a&quot;onmouseover=&quot;b\">t</a>"
         (inline "[t](https://x.org/a\"onmouseover=\"b)")))
  (is (= "<a href=\"https://x.org/&lt;script&gt;\">t</a>"
         (inline "[t](https://x.org/<script>)")))
  (is (= "<a href=\"https://x.org/&#39;q\">t</a>"
         (inline "[t](https://x.org/'q)"))))

(deftest text-is-escaped-once
  (is (= "a &amp; b &lt; c" (inline "a & b < c")))
  (is (= "<a href=\"https://x.org/?a=1&amp;b=2\">R&amp;D &lt;x&gt;</a>"
         (inline "[R&D <x>](https://x.org/?a=1&b=2)")))
  (is (= "<code>a &amp;&amp; b</code>" (inline "`a && b`"))))
