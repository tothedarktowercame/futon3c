#!/usr/bin/env bb
;; ALFWorld Deterministic Runner
;;
;; Plays ALFWorld games using strategies compiled from the 10 flexiarg
;; design patterns in library/alfworld/. No LLM — just pattern-derived
;; heuristics and deterministic action selection.
;;
;; Usage:
;;   bb scripts/alfworld_runner.clj              # play 1 game
;;   bb scripts/alfworld_runner.clj 10           # play 10 games
;;   bb scripts/alfworld_runner.clj 10 verbose   # play 10 with trace
;;
;; Requires: ALFWorld server running on localhost:3456
;;   .venv-alfworld/bin/python3 scripts/alfworld-server.py --port 3456

(require '[cheshire.core :as json]
         '[clojure.string :as str]
         '[babashka.process :as p])

(def server-url "http://127.0.0.1:3456")
(def verbose? (some #{"verbose" "v"} *command-line-args*))

(defn trace [& args]
  (when verbose?
    (apply println "  [trace]" args)))

;; =============================================================================
;; HTTP client (curl-based, matches peripheral implementation)
;; =============================================================================

(defn post! [path body]
  (let [result (p/shell {:out :string :err :string}
                        "curl" "-s" "-X" "POST"
                        "-H" "Content-Type: application/json"
                        "-d" (json/generate-string body)
                        (str server-url path))]
    (json/parse-string (:out result) true)))

(defn get! [path]
  (let [result (p/shell {:out :string :err :string}
                        "curl" "-s" (str server-url path))]
    (json/parse-string (:out result) true)))

(defn reset! [] (post! "/reset" {}))
(defn step! [action] (post! "/step" {:action action}))
(defn state! [] (get! "/state"))

;; =============================================================================
;; Pattern 8: Room type identification from receptacles
;; =============================================================================

(defn receptacles
  "Extract receptacle names from admissible commands."
  [state]
  (->> (:admissible_commands state)
       (filter #(str/starts-with? % "go to "))
       (map #(subs % 6))))

(defn identify-room
  "Identify room type from receptacle set."
  [recs]
  (let [has? (fn [kw] (some #(str/includes? % kw) recs))]
    (cond
      (or (has? "stoveburner") (has? "microwave") (has? "fridge")) :kitchen
      (or (has? "bed ") (has? "desklamp"))                        :bedroom
      (has? "toilet")                                              :bathroom
      (or (has? "sofa") (has? "armchair"))                         :living-room
      :else                                                        :unknown)))

;; =============================================================================
;; Pattern 3: Task type parsing → action template
;; =============================================================================

(defn parse-task-type [state]
  (keyword (str/replace (:task_type state) "_" "-")))

(defn parse-target-object
  "Extract the object type to find from the game observation's task line.

   Task line formats observed:
     'clean some soapbar and put it in cabinet.'
     'heat some apple and put it in garbagecan.'
     'cool some mug and put it in cabinet.'
     'put a pencil in shelf.'
     'put two keychain in safe.'
     'put a cool bread in countertop.'
     'examine the cd with the desklamp.'

   The object is the first noun after stripping determiners and adjectives."
  [state]
  (let [obs (:observation state)
        task-line (last (str/split obs #"\n"))
        ;; Strip 'Your task is to: '
        after-colon (second (str/split task-line #":\s*"))
        words (when after-colon
                (str/split (str/lower-case (str/trim after-colon)) #"\s+"))
        ;; Adjectives/determiners to skip
        skip? #{"a" "an" "the" "some" "two" "clean" "hot" "cool" "cold"
                "dirty" "used" "heated" "cooled" "cleaned"}]
    (when words
      (cond
        ;; "examine the X with the desklamp" — object is between "examine" and "with"
        (some #{"examine"} words)
        (let [idx (.indexOf words "examine")
              end-idx (let [wi (.indexOf words "with")] (if (pos? wi) wi (count words)))]
          (->> (subvec (vec words) (inc idx) end-idx)
               (remove skip?)
               first))

        ;; "look at X under the desklamp" — object is between "at" and "under"
        (and (some #{"look"} words) (some #{"at"} words))
        (let [idx (.indexOf words "at")
              end-idx (let [ui (.indexOf words "under")] (if (pos? ui) ui (count words)))]
          (->> (subvec (vec words) (inc idx) end-idx)
               (remove skip?)
               first))

        ;; "<verb> some X and put..." or "put a/two [adj] X in/on Y"
        :else
        (let [;; Skip the verb (clean/heat/cool/put/find)
              after-verb (rest words)]
          (->> after-verb
               (remove skip?)
               ;; Remove trailing prepositions and punctuation
               (remove #{"and" "in" "on" "put" "it" "them" "with" "then"
                          "under" "at" "look"})
               first
               ;; Remove trailing period
               (#(when % (str/replace % #"\." "")))))))))

(defn parse-target-receptacle
  "Extract the target receptacle type from the task description.
   Target is always the last word after the last 'in' or 'on'."
  [state]
  (let [obs (:observation state)
        task-line (last (str/split obs #"\n"))
        after-colon (second (str/split task-line #":\s*"))
        words (when after-colon
                (str/split (str/lower-case (str/trim after-colon)) #"\s+"))]
    (when words
      ;; Find the last "in" or "on" and take the word after it
      (let [idx (last (keep-indexed #(when (#{"in" "on"} %2) %1) words))]
        (when (and idx (< (inc idx) (count words)))
          (str/replace (nth words (inc idx)) #"\." ""))))))

;; =============================================================================
;; Pattern 2: Verb → appliance mapping
;; =============================================================================

(defn verb-appliance
  "Map task verb to the appliance needed for transformation."
  [task-type]
  (case task-type
    :pick-clean-then-place-in-recep "sinkbasin"
    :pick-heat-then-place-in-recep  "microwave"
    :pick-cool-then-place-in-recep  "fridge"
    nil))

;; =============================================================================
;; Pattern 1 + 5: Object location priors + systematic fallback
;; =============================================================================

(def room-search-order
  "Per-room-type ordered search lists. High-probability categories first."
  {:kitchen     ["countertop" "shelf" "cabinet" "drawer" "sinkbasin"
                 "stoveburner" "microwave" "fridge" "coffeemachine" "toaster"
                 "garbagecan"]
   :bedroom     ["desk" "shelf" "bed" "drawer" "safe" "laundryhamper"
                 "garbagecan"]
   :bathroom    ["countertop" "toilet" "sinkbasin" "cabinet" "shelf"
                 "towelholder" "handtowelholder" "toiletpaperhanger"
                 "garbagecan"]
   :living-room ["armchair" "sofa" "sidetable" "dresser" "shelf" "cabinet"
                 "drawer" "garbagecan"]
   :unknown     ["countertop" "shelf" "cabinet" "desk" "drawer" "table"
                 "armchair" "sofa" "bed" "sinkbasin"]})

(defn object-specific-priors
  "Boost certain receptacles based on the target object type."
  [object-type room-type]
  (let [boosts (case object-type
                 ("pillow" "remote" "newspaper")     ["armchair" "sofa" "bed"]
                 ("mug" "cup")                       ["coffeemachine" "countertop" "shelf" "cabinet"]
                 ("knife" "fork" "spoon" "spatula"
                  "butterknife")                     ["countertop" "drawer"]
                 ("potato" "tomato" "apple" "lettuce"
                  "egg" "bread")                     ["countertop" "fridge"]
                 ("soapbar" "soapbottle" "cloth"
                  "spraybottle")                     ["countertop" "sinkbasin" "cabinet"]
                 ("toiletpaper")                     ["toilet" "toiletpaperhanger" "countertop" "cabinet"]
                 ("saltshaker" "peppershaker")       ["countertop" "shelf" "cabinet" "drawer"]
                 ("cd" "book" "pencil" "pen"
                  "creditcard" "cellphone"
                  "keychain" "alarmclock" "laptop")  ["desk" "shelf" "drawer" "sidetable"]
                 ("bowl" "plate" "pan" "pot")        ["countertop" "cabinet" "sinkbasin" "stoveburner"]
                 ("vase" "statue" "houseplant"
                  "candle")                          ["shelf" "countertop" "dresser" "sidetable"]
                 ("towel" "handtowel")               ["towelholder" "handtowelholder" "countertop"]
                 ;; Default: use room order
                 nil)]
    (if boosts
      ;; Put boosted receptacles first, then the rest of the room order
      (let [room-order (get room-search-order room-type
                            (get room-search-order :unknown))
            remaining (remove (set boosts) room-order)]
        (concat boosts remaining))
      (get room-search-order room-type
           (get room-search-order :unknown)))))

(defn build-search-plan
  "Build ordered list of specific receptacles to visit, based on priors."
  [object-type room-type available-recs]
  (let [category-order (object-specific-priors object-type room-type)]
    ;; For each category, find matching receptacles sorted by number
    (->> category-order
         (mapcat (fn [cat]
                   (->> available-recs
                        (filter #(str/starts-with? % cat))
                        sort)))
         ;; Remove duplicates (a receptacle might match multiple categories)
         distinct
         vec)))

;; =============================================================================
;; Pattern 7: Mental map
;; =============================================================================

(defn update-mental-map
  "Parse observation to update mental map of what's at each location."
  [mental-map location observation]
  (let [;; "On the countertop 1, you see a mug 1, a bowl 2, and a fork 1."
        ;; "The cabinet 1 is closed."
        ;; "You arrive at shelf 2. On the shelf 2, you see a glassbottle 3, and a saltshaker 1."
        items (when (str/includes? observation "you see")
                (->> (re-seq #"a (\w+ \d+)" observation)
                     (map second)))]
    (if items
      (assoc mental-map location (set items))
      (if (str/includes? observation "is closed")
        (assoc mental-map location :closed)
        (assoc mental-map location #{})))))

(defn find-in-mental-map
  "Check if the mental map already knows where an object type is."
  [mental-map object-type]
  (some (fn [[loc items]]
          (when (and (set? items)
                     (some #(str/starts-with? % object-type) items))
            {:location loc
             :item (first (filter #(str/starts-with? % object-type) items))}))
        mental-map))

;; =============================================================================
;; Pattern 9: Closed container handling
;; =============================================================================

(defn handle-closed-container
  "If we arrived at a closed container, open it and return the new state."
  [state location]
  (if (str/includes? (:observation state) "is closed")
    (do
      (trace "Container closed, opening:" location)
      (step! (str "open " location)))
    state))

;; =============================================================================
;; Action helpers
;; =============================================================================

(defn find-admissible
  "Find an admissible command matching a pattern."
  [state pattern]
  (first (filter #(str/includes? % pattern) (:admissible_commands state))))

(defn find-take-command
  "Find a 'take X from Y' command for the given object."
  [state object-name]
  (first (filter #(and (str/starts-with? % "take ")
                       (str/includes? % object-name))
                 (:admissible_commands state))))

(defn find-object-at-location
  "Find an object matching the target type at the current location."
  [state object-type]
  (let [take-cmds (filter #(str/starts-with? % "take ") (:admissible_commands state))]
    ;; "take mug 1 from countertop 2" → extract "mug 1"
    (some (fn [cmd]
            (let [parts (str/split cmd #" ")
                  ;; take <obj> <num> from <loc>...
                  obj-name (when (>= (count parts) 4)
                             (str (nth parts 1) " " (nth parts 2)))]
              (when (and obj-name (str/starts-with? obj-name object-type))
                obj-name)))
          take-cmds)))

(defn find-move-command
  "Find a 'move X to Y' command."
  [state object-name]
  (first (filter #(and (str/starts-with? % "move ")
                       (str/includes? % object-name))
                 (:admissible_commands state))))

(defn find-receptacle-instance
  "Find a specific receptacle instance matching a type from admissible go-to commands."
  [state rec-type]
  (let [go-cmds (filter #(str/starts-with? % "go to ") (:admissible_commands state))]
    (first (filter #(str/includes? % rec-type) go-cmds))))

;; =============================================================================
;; Core game loop
;; =============================================================================

(defn search-for-object!
  "Search for an object using the prior-ordered search plan.
   Returns {:found true :object 'mug 1' :state state :mental-map map :steps n}
   or {:found false} if not found."
  [initial-state object-type search-plan mental-map]
  (loop [plan search-plan
         state initial-state
         mmap mental-map
         steps 0]
    (if (empty? plan)
      {:found false :mental-map mmap :steps steps :state state}
      (let [target-rec (first plan)]
        ;; Check mental map first
        (if-let [known (find-in-mental-map mmap object-type)]
          (do
            (trace "Mental map hit:" (:item known) "at" (:location known))
            ;; Go there
            (let [go-cmd (str "go to " (:location known))
                  result (step! go-cmd)
                  result (handle-closed-container result (:location known))
                  mmap (update-mental-map mmap (:location known) (:observation result))]
              {:found true
               :object (or (find-object-at-location result object-type) (:item known))
               :state result
               :mental-map mmap
               :steps (inc steps)}))
          ;; Check if we can go there
          (if-let [go-cmd (find-admissible state (str "go to " target-rec))]
            (let [result (step! go-cmd)
                  obs (:observation result)
                  ;; Handle closed container
                  result (if (str/includes? obs "is closed")
                           (let [opened (step! (str "open " target-rec))]
                             (trace "Opened" target-rec)
                             opened)
                           result)
                  mmap (update-mental-map mmap target-rec (:observation result))
                  obj (find-object-at-location result object-type)
                  extra-steps (if (str/includes? obs "is closed") 2 1)]
              (if obj
                (do
                  (trace "Found" obj "at" target-rec)
                  {:found true :object obj :state result :mental-map mmap
                   :steps (+ steps extra-steps)})
                (do
                  (trace "Not at" target-rec)
                  (recur (rest plan) result mmap (+ steps extra-steps)))))
            ;; This receptacle doesn't exist in the room, skip
            (recur (rest plan) state mmap steps)))))))

(defn play-game!
  "Play one ALFWorld game deterministically. Returns result map."
  []
  (let [initial (reset!)
        task-type (parse-task-type initial)
        recs (receptacles initial)
        room-type (identify-room recs)
        object-type (parse-target-object initial)
        target-rec-type (parse-target-receptacle initial)
        appliance (verb-appliance task-type)
        two-objects? (= task-type :pick-two-obj-and-place)
        examine-light? (= task-type :look-at-obj-in-light)]

    (trace "Task type:" task-type)
    (trace "Room:" room-type)
    (trace "Object:" object-type)
    (trace "Target:" target-rec-type)
    (trace "Appliance:" appliance)

    (let [search-plan (build-search-plan object-type room-type recs)
          _ (trace "Search plan:" (take 5 search-plan) "...")

          ;; === Phase 1: Find and take the object ===
          search-result (search-for-object! initial object-type search-plan {})]
      (if-not (:found search-result)
        (let [state (:state search-result)]
          (println "  FAILED: Could not find" object-type "(giving up on this game)")
          {:won false
           :done true
           :score 0.0
           :steps (:step state)
           :task-type task-type
           :room-type room-type
           :object object-type
           :target target-rec-type
           :fail-reason :could-not-find-object})

        (let [obj-name (:object search-result)
              state (:state search-result)
              mmap (:mental-map search-result)

              ;; Take the object
              take-cmd (find-take-command state obj-name)
              _ (trace "Taking:" take-cmd)
              state (if take-cmd (step! take-cmd) state)

              ;; === Phase 2: Transform if needed ===
              state (if appliance
                      (let [;; Go to appliance
                            go-cmd (str "go to " appliance " 1")
                            _ (trace "Going to appliance:" go-cmd)
                            result (step! go-cmd)
                            result (handle-closed-container result (str appliance " 1"))
                            ;; Find the transform command
                            verb (case task-type
                                   :pick-clean-then-place-in-recep "clean"
                                   :pick-heat-then-place-in-recep "heat"
                                   :pick-cool-then-place-in-recep "cool"
                                   nil)
                            transform-cmd (find-admissible result (str verb " " obj-name))
                            _ (trace "Transform:" transform-cmd)]
                        (if transform-cmd
                          (step! transform-cmd)
                          (do (trace "WARNING: no transform command found") result)))
                      state)

              ;; === Phase 3: Place at target (or use desklamp) ===
              state (if examine-light?
                      ;; For look_at_obj_in_light: find desklamp and use it
                      (let [;; Check mental map for desklamp
                            lamp-loc (find-in-mental-map mmap "desklamp")
                            state (if lamp-loc
                                    (do (trace "Desklamp known at:" (:location lamp-loc))
                                        (step! (str "go to " (:location lamp-loc))))
                                    ;; Search desks for desklamp
                                    (let [desks (filter #(str/starts-with? % "desk") recs)]
                                      (trace "Searching desks for lamp:" desks)
                                      (loop [ds desks
                                             s state]
                                        (if (empty? ds)
                                          s
                                          (let [result (step! (str "go to " (first ds)))]
                                            (if (find-admissible result "use desklamp")
                                              result
                                              (recur (rest ds) result)))))))
                            use-cmd (find-admissible state "use desklamp")]
                        (if use-cmd
                          (do (trace "Using lamp:" use-cmd)
                              (step! use-cmd))
                          (do (trace "WARNING: no use desklamp command found") state)))

                      ;; Normal placement: go to target receptacle and place
                      (let [;; Find a target receptacle instance
                            target-recs (->> recs
                                             (filter #(str/starts-with? % (or target-rec-type "")))
                                             sort)
                            _ (trace "Target receptacles:" target-recs)
                            ;; Go to first target
                            go-cmd (when (seq target-recs)
                                     (str "go to " (first target-recs)))
                            _ (trace "Going to target:" go-cmd)
                            state (if go-cmd
                                    (let [result (step! go-cmd)]
                                      (handle-closed-container result (first target-recs)))
                                    state)
                            ;; Place the object
                            move-cmd (find-move-command state obj-name)
                            _ (trace "Placing:" move-cmd)]
                        (if move-cmd
                          (step! move-cmd)
                          (do (trace "WARNING: no move command found") state))))

              ;; === Phase 4: Second object for pick_two tasks ===
              result (if two-objects?
                       (let [_ (trace "=== Second object ===")
                             search2 (search-for-object! state object-type search-plan mmap)]
                         (if-not (:found search2)
                           (let [state2 (:state search2)]
                             (println "  FAILED: Could not find second" object-type "(giving up on this game)")
                             {:won false
                              :done true
                              :score 0.0
                              :steps (:step state2)
                              :task-type task-type
                              :room-type room-type
                              :object object-type
                              :target target-rec-type
                              :fail-reason :could-not-find-second-object})

                           (let [obj2 (:object search2)
                                 state2 (:state search2)
                                 take-cmd2 (find-take-command state2 obj2)
                                 _ (trace "Taking second:" take-cmd2)
                                 state2 (if take-cmd2 (step! take-cmd2) state2)
                                 ;; Go to same target
                                 target-recs (->> (receptacles state2)
                                                  (filter #(str/starts-with? % (or target-rec-type "")))
                                                  sort)
                                 go-cmd2 (when (seq target-recs)
                                           (str "go to " (first target-recs)))
                                 state2 (if go-cmd2
                                          (let [r (step! go-cmd2)]
                                            (handle-closed-container r (first target-recs)))
                                          state2)
                                 move-cmd2 (find-move-command state2 obj2)
                                 _ (trace "Placing second:" move-cmd2)
                                 state2 (if move-cmd2
                                          (step! move-cmd2)
                                          (do (trace "WARNING: no move command for second object") state2))]
                             {:won (:won state2)
                              :done (:done state2)
                              :score (:score state2)
                              :steps (:step state2)
                              :task-type task-type
                              :room-type room-type
                              :object object-type
                              :target target-rec-type})))

                       ;; Non-two-object tasks: normal report from state
                       {:won (:won state)
                        :done (:done state)
                        :score (:score state)
                        :steps (:step state)
                        :task-type task-type
                        :room-type room-type
                        :object object-type
                        :target target-rec-type})]

          result)))))

;; =============================================================================
;; Main
;; =============================================================================

(let [n-games (or (some-> (first *command-line-args*)
                          (parse-long))
                  1)]
  (println (str "ALFWorld Deterministic Runner — " n-games " game(s)"))
  (println "Patterns: object-location-priors, verb-to-appliance-mapping,")
  (println "  plan-then-execute, admissible-commands-are-ground-truth,")
  (println "  systematic-search-fallback, single-carry-economy,")
  (println "  remember-what-you-see, room-type-from-receptacles,")
  (println "  closed-containers-need-opening, search-dominates-execution")
  (println)

  (let [results (vec (for [i (range n-games)]
                       (let [_ (print (str "Game " (inc i) ": "))
                             _ (flush)
                             result (play-game!)]
                         (println (str (if (:won result) "WON" "LOST")
                                       " | " (:steps result) " steps"
                                       " | " (name (:task-type result))
                                       " | " (name (:room-type result))
                                       " | " (:object result)
                                       " → " (:target result)))
                         result)))
        wins (count (filter :won results))
        total (count results)
        total-steps (reduce + (map :steps results))
        avg-steps (when (pos? total) (double (/ total-steps total)))]
    (println)
    (println (str "Results: " wins "/" total " wins"
                  " (" (when (pos? total)
                         (int (* 100 (/ wins total)))) "%)"
                  " | " total-steps " total steps"
                  " | " (format "%.1f" (or avg-steps 0.0)) " avg steps/game"))))
