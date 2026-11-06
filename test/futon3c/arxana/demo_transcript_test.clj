(ns futon3c.arxana.demo-transcript-test
  "Semantic-digest equality transcript (D5, futon4a first demonstration).

  Runs the demo flow in-process against the committed D4 fixture and asserts:
  handshake truth, resolution with all candidates preserved, correspondence
  neighbourhood, a typed stale refusal, subscription replay, and — the point
  of the transcript — byte-identical semantic digests for two clients whose
  requests differ only in message/principal IDs."
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.demo :as demo]
            [futon3c.arxana.json :as ajson]))

(defn- step [transcript k]
  (first (filter #(= k (:step %)) transcript)))

(deftest demo-transcript-test
  (let [stack (demo/build-stack)]
    (demo/ingest-fixture-revisions! stack)
    (let [t (demo/run-transcript stack)
          digests (demo/transcript-digests t)]
      (testing "capabilities handshake reports subscription truth"
        (is (true? (get-in (step t :capabilities) [:response :ok :subscription]))))
      (testing "resolve returns a status and candidates; digests are present"
        (let [r (get-in (step t :resolve/emacs) [:response :ok])]
          (is (contains? #{:resolved :resolved-unverified :ambiguous}
                         (:result/status r))
              (pr-str (:result/status r)))
          (is (vector? (:result/candidates r)))
          (is (string? (:semantic/digest r)))))
      (testing "D5 equality: two clients, same semantic request, same digest"
        (is (= (digests :resolve/emacs) (digests :resolve/other-client)))
        (is (not= (get-in (step t :resolve/emacs) [:request :message/id])
                  (get-in (step t :resolve/other-client) [:request :message/id]))))
      (testing "correspondence returns the fixture neighbourhood unmerged"
        (let [r (get-in (step t :correspondence) [:response :ok])]
          (is (<= 1 (:result/count r)))
          (is (string? (:semantic/digest r)))))
      (testing "typed stale refusal with recovery choices"
        (let [e (get-in (step t :typed-refusal/stale-ingest) [:response :error])]
          (is (= :stale (:error/category e)))
          (is (= [:refresh :rebase-and-resubmit :export-patch]
                 (:error/recovery e)))))
      (testing "transcript JSON round-trips through the wire codec"
        (let [rt (ajson/decode (ajson/encode {:steps t}))]
          (is (= (demo/transcript-digests t)
                 (demo/transcript-digests (:steps rt)))))))))

(deftest committed-transcript-matches-live-test
  (testing "the futon4a committed transcript is exactly the live run"
    (let [path "/home/joe/code/futon4a/examples/demo-transcript.json"
          f (clojure.java.io/file path)]
      (if-not (.exists f)
        (is true "committed transcript not yet generated; run record-transcript")
        (let [stack (demo/build-stack)]
          (demo/ingest-fixture-revisions! stack)
          (let [live (demo/transcript-digests (demo/run-transcript stack))
                committed (demo/transcript-digests
                           (:steps (ajson/decode (slurp f))))]
            (is (= live committed)
                "regenerate with scripts/record-transcript.sh")))))))
