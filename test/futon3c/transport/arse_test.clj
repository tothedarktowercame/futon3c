(ns futon3c.transport.arse-test
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [futon3c.evidence.boundary :as boundary]
            [futon3c.transport.http :as http]))

(defn- with-isolated-arse
  [f]
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "arse-test" (make-array java.nio.file.attribute.FileAttribute 0)))
        appended (atom [])]
    (try
      (with-redefs-fn {#'http/arse-store-dir (.getAbsolutePath dir)
                       #'boundary/append! (fn [_ entry]
                                            (swap! appended conj entry)
                                            {:ok true :entry entry})}
        #(f appended))
      (finally
        (doseq [file (reverse (file-seq dir))]
          (io/delete-file file true))))))

(defn- response-body
  [response]
  (json/parse-string (:body response) true))

(deftest reply-appends-a-four-turn-chain
  (with-isolated-arse
    (fn [appended]
      (let [ask (#'http/arse-ask! {} {:title "Why?"
                                      :question "Initial question"
                                      :tags ["proof"]
                                      :author "asker"})
            thread-id (:thread-id ask)
            answer (#'http/arse-answer! {} {:thread-id thread-id
                                            :answer "Initial answer"
                                            :author "answerer"})
            follow-up (#'http/arse-reply! {} {:thread-id thread-id
                                              :text "Follow-up question"
                                              :author "asker"
                                              :role "question"})
            after-question (first (#'http/arse-load-entities))
            second-answer (#'http/arse-reply! {} {:thread-id thread-id
                                                  :text "Second answer"
                                                  :author "answerer"
                                                  :role "answer"})
            entity (first (#'http/arse-load-entities))
            turns (:turns entity)
            entries @appended]
        (is (:ok answer))
        (is (= 2 (:turn-index follow-up)))
        (is (= 3 (:turn-index second-answer)))
        (is (true? (:unanswered after-question)))
        (is (not (contains? entity :unanswered)))
        (is (= "Second answer" (:answer-body entity)))
        (is (= ["question" "answer" "question" "answer"]
               (mapv :role turns)))
        (is (= [(str "arse-q-" thread-id)
                (str "arse-a-" thread-id)
                (str "arse-t-" thread-id "-2")
                (str "arse-t-" thread-id "-3")]
               (mapv :evidence-id turns)))
        (is (= [nil
                (str "arse-q-" thread-id)
                (str "arse-a-" thread-id)
                (str "arse-t-" thread-id "-2")]
               (mapv :in-reply-to entries)))))))

(deftest legacy-thread-read-is-a-non-mutating-projection
  (with-isolated-arse
    (fn [_]
      (let [legacy {:entity/id "ask-legacy"
                    :entity/type "QAPair"
                    :title "Legacy"
                    :question-body "Old question"
                    :answer-body "Old answer"
                    :tags ["old"]
                    :author "asker"}]
        (#'http/arse-save-entities [legacy])
        (let [before (#'http/arse-load-entities)
              response (#'http/handle-arse-thread {} {} "ask-legacy")
              body (response-body response)
              after (#'http/arse-load-entities)]
          (is (= 200 (:status response)))
          (is (false? (:unanswered body)))
          (is (= ["arse-q-ask-legacy" "arse-a-ask-legacy"]
                 (mapv :evidence-id (:turns body))))
          (is (= before after))
          (is (not (contains? (first after) :turns))))))))

(deftest reply-validates-role-and-thread
  (with-isolated-arse
    (fn [_]
      (testing "bad role"
        (is (= 400 (:status (#'http/arse-reply! {}
                             {:thread-id "ask-x"
                              :text "text"
                              :role "comment"})))))
      (testing "unknown thread"
        (is (= 404 (:status (#'http/arse-reply! {}
                             {:thread-id "ask-missing"
                              :text "text"
                              :role "question"}))))))))
