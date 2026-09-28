(ns futon3c.agency.act-stamp-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon3c.agency.act-stamp :as act-stamp]))

(def valid
  {:executor "claude-17"
   :signer "claude-17"
   :authority {:grant "act:grant-1"}
   :executor-basis :session-bound})

(defn reason [value]
  (try
    (act-stamp/validate! value)
    nil
    (catch clojure.lang.ExceptionInfo e
      (:reason (ex-data e)))))

(deftest valid-act-stamps
  (testing "self with an explicit grant"
    (is (= valid (act-stamp/validate! valid))))
  (testing "Joe acting as operator"
    (is (= {:executor "joe" :signer "joe"
            :authority {:operator true} :executor-basis :declared}
           (act-stamp/stamp "joe" "joe" {:operator true} :declared))))
  (testing "delegation with a grant"
    (is (= "joe"
           (:signer (act-stamp/stamp "codex-5" "joe"
                                     {:grant "act:delegation"}
                                     :session-bound))))))

(deftest typed-refusals
  (is (= :invalid-stamp-map (reason nil)))
  (is (= :unexpected-stamp-key (reason (assoc valid :extra true))))
  (is (= :missing-stamp-field (reason (dissoc valid :executor))))
  (is (= :missing-stamp-field (reason (assoc valid :authority nil))))
  (is (= :blank-stamp-field (reason (assoc valid :signer "  "))))
  (is (= :unknown-executor-basis
         (reason (assoc valid :executor-basis :inferred))))
  (is (= :interpretation-not-authority
         (reason (assoc valid :authority {:interpretation "analysis:1"}))))
  (is (= :invalid-authority
         (reason (assoc valid :authority {:grant "act:g" :operator true}))))
  (is (= :operator-authority-requires-joe
         (reason (assoc valid :authority {:operator true}))))
  (is (= :overreach-without-grant
         (reason (assoc valid :executor "codex-5"
                              :signer "joe"
                              :authority {:operator true}))))
  (is (= :invalid-grant-id
         (reason (assoc valid :authority {:grant "grant-1"}))))
  (is (= :invalid-grant-id
         (reason (assoc valid :authority {:grant "act:"})))))
