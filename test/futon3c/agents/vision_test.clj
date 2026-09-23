(ns futon3c.agents.vision-test
  "Image input for the shared harness: what view_image builds, what it refuses,
   and how images leave the context once they have been looked at."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [futon3c.agents.kimi-api :as kimi]
            [futon3c.agents.zai-api :as zai])
  (:import [java.awt Color]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]))

(def ^:private read-image-part #'zai/read-image-part)
(def ^:private elide-stale-images #'zai/elide-stale-images)
(def ^:private openai-tools #'zai/openai-tools)

(defn- write-png!
  [w h]
  (let [f (java.io.File/createTempFile "vision-test" ".png")
        img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
        g (.createGraphics img)]
    (try
      (.setColor g Color/WHITE)
      (.fillRect g 0 0 w h)
      (finally (.dispose g)))
    (ImageIO/write img "png" f)
    (.deleteOnExit f)
    f))

(defn- tool-names [& args]
  (set (map #(get-in % [:function :name]) (apply openai-tools args))))

;;; Provider gating

(deftest view-image-is-offered-only-to-a-vision-provider
  (is (contains? (tool-names :full true) "view_image"))
  (is (not (contains? (tool-names :full false) "view_image")))
  ;; Gating must not disturb anything else in the list.
  (is (= (disj (tool-names :full true) "view_image") (tool-names :full false))))

(deftest a-kimi-seat-declares-vision-and-a-zai-seat-does-not
  (let [captured (atom nil)]
    (with-redefs [zai/make-invoke-fn (fn [opts] (reset! captured opts) identity)]
      (kimi/make-invoke-fn {:agent-id "kimi-test"})
      (is (true? (:vision? @captured)))))
  ;; zai-api's own default: absent, so (boolean nil) => false downstream.
  (is (nil? (:vision? {}))))

;;; Reading

(deftest a-png-becomes-an-image-part-plus-a-caption
  (let [f (write-png! 40 20)
        {:keys [ok content-parts]} (read-image-part "/tmp" (.getPath f) (* 8 1024 1024))
        [image caption] content-parts]
    (is (true? ok))
    (is (= 2 (count content-parts)))
    (is (= "image_url" (:type image)))
    (is (str/starts-with? (get-in image [:image_url :url]) "data:image/png;base64,"))
    (is (= "text" (:type caption)))
    (is (str/includes? (:text caption) "40x20"))
    (is (str/includes? (:text caption) (.getPath f)))))

(deftest an-oversized-image-is-downscaled-not-refused
  ;; Moonshot's guidance: past 4096x2160 a bigger image costs time and buys
  ;; nothing, so the harness shrinks it rather than making the model fail.
  (let [f (write-png! 5000 400)
        {:keys [ok content-parts]} (read-image-part "/tmp" (.getPath f) (* 8 1024 1024))
        caption (:text (second content-parts))]
    (is (true? ok))
    (is (str/includes? caption "downscaled"))
    (is (str/includes? caption "4096x"))))

(deftest an-image-over-the-byte-cap-is-refused-with-a-next-step
  (let [f (write-png! 400 400)
        {:keys [ok error]} (read-image-part "/tmp" (.getPath f) 16)]
    (is (false? ok))
    (is (str/includes? error "per-image limit"))
    (is (str/includes? error "view_image again"))))

(deftest svg-is-refused-by-name-and-redirected-to-read-file
  ;; The vendor rejects SVG as image input; the useful answer is not "error"
  ;; but "read the XML instead", which the model can act on.
  (let [f (java.io.File/createTempFile "vision-test" ".svg")]
    (spit f "<svg/>")
    (.deleteOnExit f)
    (let [{:keys [ok error]} (read-image-part "/tmp" (.getPath f) (* 8 1024 1024))]
      (is (false? ok))
      (is (str/includes? error "read_file")))))

(deftest a-missing-or-unsupported-file-says-which
  (is (str/includes? (:error (read-image-part "/tmp" "/tmp/no-such-image.png" 1000))
                     "no such file"))
  (let [f (java.io.File/createTempFile "vision-test" ".txt")]
    (.deleteOnExit f)
    (is (str/includes? (:error (read-image-part "/tmp" (.getPath f) 1000))
                       "not a supported image type"))))

(deftest a-relative-path-resolves-against-the-agent-cwd
  (let [f (write-png! 8 8)
        dir (.getParent f)
        {:keys [ok]} (read-image-part dir (.getName f) (* 8 1024 1024))]
    (is (true? ok))))

;;; Elision

(defn- img-msg [tag]
  {:role "tool" :name "view_image"
   :content [{:type "image_url" :image_url {:url (str "data:image/png;base64," tag)}}
             {:type "text" :text (str "viewed " tag)}]})

(deftest only-the-most-recent-images-stay-inline
  (let [messages [(img-msg "a")
                  {:role "assistant" :content "looking"}
                  (img-msg "b")
                  (img-msg "c")]
        elided (elide-stale-images messages 2)
        inline (fn [m] (some #(= "image_url" (:type %)) (:content m)))]
    (is (= 4 (count elided)))
    (is (not (inline (nth elided 0))) "the oldest image is gone")
    (is (inline (nth elided 2)))
    (is (inline (nth elided 3)))
    ;; Its caption survives, so the record that the look happened survives.
    (is (str/includes? (get-in (nth elided 0) [:content 0 :text]) "viewed a"))
    (is (str/includes? (get-in (nth elided 0) [:content 0 :text]) "elided"))
    ;; Untouched messages pass through identically.
    (is (= {:role "assistant" :content "looking"} (nth elided 1)))))

(deftest eliding-is-idempotent-and-leaves-a-short-history-alone
  (let [messages [(img-msg "a") (img-msg "b")]]
    (is (= messages (elide-stale-images messages 2)))
    (is (= (elide-stale-images messages 1)
           (elide-stale-images (elide-stale-images messages 1) 1)))))
