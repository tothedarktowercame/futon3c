# G4 for Zai seats: the inject.py contract (shared by both packets)

    python3 -I /home/joe/code/minus40/scripts/inject.py --agent <agent-id>   < packet text on stdin

stdout, always one JSON object, exit 0 always (errors go to
$MINUS40_HOOK_LOG and give the empty answer):

    {"enabled": bool,            # agent listed in ~/code/data/minus40/inject-enabled.txt
                                 # (one id per line, or "all"); false -> nothing else done
     "memories": [{"id": "cc:-home-joe/agency-send-mode-brief-trap",
                   "score": 0.5276}],          # at most 3, best first, distinct ids
     "block": "minus40 memories injected before round 1 for this turn ...",  # "" when none
     "elapsed-ms": 5180}

Budget: 15 s wall, after which it gives the empty answer.
