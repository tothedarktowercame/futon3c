(ns futon3c.arxana.holes-mozilla-test
  "Joe's theory of holes, tested on the problem it is for: the Mozilla
  Human-Scale AI Incubator application.

  Joe 2026-11-19: anything committed could be paired with test data relevant
  to the problem at hand, and building a test suite from the Mozilla material
  is itself a test of whether the approach is relevant.  Each case quotes its
  source: a Draft 2 sentence (Joe's draft, by segment number in
  ~/mozilla-incubator-draft-2.txt) or the call's question text.  Frames are
  claude-5's reading of the sentence; the expected kinds are what the theory
  says of that reading.  Notes from the 2026-11-19 call with Rob and Murch are
  not used here: they are third parties' words in a public repo.

  The listener is the reviewer (`:reviewer`).  `coverage-of-the-material`
  reports which kinds of act the material actually contains; a kind with no
  instance is a finding, not something to invent."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [futon3c.arxana.holes :as h]))

(defn- ?- [n] (h/hole n))

(def thesis-quote
  "Draft 2 segment 17, placed as the public-benefit thesis."
  "Broader collective benefit can be foreseen from various smaller collectives working together.")

(def thesis-body
  "The thesis read as a claim frame: foreseen by whom, a benefit for whom,
  broader than what, from which collectives."
  [:conj [:atom :foresees (?- :who) :collective-benefit]
   [:conj [:atom :benefits (?- :for-whom) :collective-benefit]
    [:conj [:atom :broader-than :collective-benefit (?- :standard)]
     [:atom :arises-from :collective-benefit (?- :which-collectives)]]]])

(def world
  "One model in which the draft's claims are true, built from other Draft 2
  sentences (segments 15, 16, 32 and the question's 'beyond your organization')."
  {:domain #{:we :plumbers-and-hydrologists :ad-hoc-expert-groups :one-organisation
             :collective-benefit :reviewer :each-person-total :yearly-salary}
   :facts #{[:foresees :we :collective-benefit]
            [:benefits :plumbers-and-hydrologists :collective-benefit]
            [:broader-than :collective-benefit :one-organisation]
            [:arises-from :collective-benefit :ad-hoc-expert-groups]
            [:budget-reading :salaries :each-person-total]}})

(defn- act [body slots] (:act (h/act body slots)))

(def cases
  [{:name "thesis as drafted"
    :quote thesis-quote
    :act (act thesis-body {})
    :kinds [:unhygienic]
    :what-did-you-mean [:who :for-whom :standard :which-collectives]}
   {:name "thesis, honest expansion: what cannot be named is said to be unknown"
    :quote thesis-quote
    :act (h/honest (act thesis-body {}))
    :kinds []}
   {:name "thesis, expanded from Joe's own draft: every hole owned"
    :quote (str thesis-quote " / seg 16: 'A group of plumbers ... hydrologists, urban planners'; "
                "seg 32: 'an accountant, a lawyer, a database specialist, and a programmer'; "
                "question: 'beyond your organization'")
    :act (act thesis-body {:who [:own :we]
                           :for-whom [:own :plumbers-and-hydrologists]
                           :standard [:own :one-organisation]
                           :which-collectives [:own :ad-hoc-expert-groups]})
    :kinds [:sealed]}
   {:name "thesis, jointly voiced: how far the benefit reaches is left open to the cohort"
    :quote (str thesis-quote " / the next draft is jointly voiced (Joe 2026-11-19)")
    :act (act thesis-body {:who [:own :we]
                           :for-whom [:own :plumbers-and-hydrologists]
                           :standard [:offered :we nil]
                           :which-collectives [:own :ad-hoc-expert-groups]})
    :kinds [:invitation]}
   {:name "the call's question is a demand on the applicant"
    :quote (str "Briefly describe the public benefit of this project and who will benefit "
                "from this work beyond your organization.")
    :act (act [:conj [:atom :describes :applicant (?- :public-benefit)]
               [:atom :describes :applicant (?- :beneficiaries)]]
              {:public-benefit [:offered :speaker 150]
               :beneficiaries [:offered :speaker 150]})
    :kinds [:demand]}
   {:name "budget sentence: 'each' left to the reader"
    :quote (str "Draft 2 segment 51: 'Concretely, $300K could pay salaries for 3 people for 1 year "
                "($100K each), 6 people for 2 years ($50K each), or 12 people for 3 years ($25K each)'")
    :act (act [:atom :budget-reading :salaries (?- :each)] {})
    :kinds [:unhygienic]
    :what-did-you-mean [:each]}
   {:name "budget sentence with the reading owned"
    :quote "Draft 2 segment 51, with 'each' bound to each person's total"
    :act (act [:atom :budget-reading :salaries (?- :each)] {:each [:own :each-person-total]})
    :kinds [:sealed]}])

(deftest mozilla-cases-classify-as-expected
  (doseq [{:keys [name act kinds what-did-you-mean]} cases]
    (testing name
      (is (= kinds (h/classify act :reviewer)))
      (when what-did-you-mean
        (is (= what-did-you-mean (vec (h/what-did-you-mean act))))))))

(deftest the-thesis-is-true-in-every-treatment
  ;; Truth underdetermines the act (Holes.lean): the drafted thesis, its honest
  ;; expansion, the owned and the invitational versions are all true in the
  ;; draft's own world.  What differs is who fills the holes.
  (doseq [{:keys [name act quote]} (filter #(str/starts-with? (:quote %) thesis-quote) cases)]
    (testing name
      (is (some? quote))
      (is (h/holds? world act)))))

(deftest expansion-from-the-draft-is-hygienic-and-no-truer
  ;; expand_hygienic and expand_sound, on the real sentence: binding the
  ;; thesis's holes from Joe's own draft removes every free hole, and the
  ;; expanded thesis holds only where the drafted one does.
  (let [drafted (act thesis-body {})
        sigma {:who :we :for-whom :plumbers-and-hydrologists :standard :one-organisation}
        expanded (h/expand drafted sigma)]
    (is (h/hygienic? expanded))
    (is (= [:unknown] (h/slot-of expanded :which-collectives)))
    (is (or (not (h/holds? world expanded)) (h/holds? world drafted)))))

(def devils-advocate
  "Devil's-advocate reviewer verdicts (Joe 2026-11-19: repurpose the call's own
  criteria, ~/mozilla-panels.txt, to make shrapnel the draft does not contain).
  The speaker is a reviewer and the listener is the applicant, so these are
  classified with `you` = :applicant.  Each verdict is paired with the same
  concern put as an invitation and as a demand: the content is the same and
  only who fills the holes differs."
  (let [frontier [:conj [:atom :calls-frontier-model (?- :who) :pipeline]
                  [:atom :small-model-would-do (?- :task) :pipeline]]]
    [{:name "verdict: frontier models where a 7B would do (lands on the applicant)"
      :quote (str "panels line 31: 'Projects that use a frontier-class model when a 7B model will do.' / "
                  "Draft 2 segment 26: 'I use Claude Sonnet and Haiku to process the text that I submit to Opus and Fable'")
      :act (act frontier {:who [:own :applicant] :task [:own :intent-tracking]})
      :kinds [:sealed :shrapnel]}
     {:name "the same concern as an invitation: which parts could run small, worked out together"
      :quote "panels line 31, put as a question for the cohort"
      :act (act frontier {:who [:own :futon] :task [:offered :we nil]})
      :kinds [:invitation]}
     {:name "the same concern as a demand: the reviewer names the hole and sets a clock"
      :quote "panels line 31, put as a required revision with a deadline"
      :act (act frontier {:who [:own :futon] :task [:offered :listener 14]})
      :kinds [:demand]}
     {:name "verdict: technology without users (lands on the applicant)"
      :quote (str "panels line 29: 'Technologies without users, contributors, or another meaningful signal of early product-community fit.' / "
                  "Draft 2 segment 37: '... there is one other active user'")
      :act (act [:atom :lacks-users (?- :who) :product-community-fit] {:who [:own :applicant]})
      :kinds [:sealed :shrapnel]}
     {:name "verdict: research without a working technology (lands on the applicant)"
      :quote "panels line 28: 'research projects without a working technology component'"
      :act (act [:atom :research-without-technology (?- :who) :working-component] {:who [:own :applicant]})
      :kinds [:sealed :shrapnel]}
     {:name "the applicant's honest reply: which parts could be small is not yet known"
      :quote "reply to panels line 31"
      :act (h/honest (act [:atom :small-model-would-do (?- :task) :pipeline] {}))
      :kinds []}]))

(def review-world
  "A world in which the frontier-model concern is true, so the verdict, the
  invitation and the demand are all true together."
  {:domain #{:applicant :futon :intent-tracking :pipeline}
   :facts #{[:calls-frontier-model :applicant :pipeline]
            [:calls-frontier-model :futon :pipeline]
            [:small-model-would-do :intent-tracking :pipeline]}})

(deftest devils-advocate-verdicts-classify-as-expected
  (doseq [{:keys [name act kinds]} devils-advocate]
    (testing name
      (is (= kinds (h/classify act :applicant))))))

(deftest one-true-concern-three-acts
  ;; truth_underdetermines_act on the call's own criterion: the shrapnel
  ;; verdict, the invitation and the demand are all true in one world.
  (doseq [{:keys [name act]} (take 3 devils-advocate)]
    (testing name (is (h/holds? review-world act)))))

(deftest coverage-of-the-material
  ;; Which kinds of act does the Mozilla material contain?  Draft 2 alone has
  ;; no shrapnel (nothing binds a cause onto the reviewer); the
  ;; devil's-advocate verdicts built from the call's criteria supply it.
  (let [draft (set (mapcat #(h/classify (:act %) :reviewer) cases))
        review (set (mapcat #(h/classify (:act %) :applicant) devils-advocate))]
    (is (= #{:unhygienic :sealed :invitation :demand} draft))
    (is (contains? review :shrapnel))
    (is (= #{:unhygienic :sealed :shrapnel :invitation :demand} (into draft review)))
    (println "Draft 2 covers:" (sort draft) "| with devil's-advocate verdicts:" (sort (into draft review)))))
