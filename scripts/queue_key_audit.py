#!/usr/bin/env python3
r"""Audit codex-sorry-queue.edn for GENUINE duplicate top-level keys.

WHY A DEDICATED SCRIPT (2026-07-31). Counting duplicate keys with a regex over
the row text does not work, and got the answer wrong twice in opposite
directions on the same night:

  - `re.findall(r':[\w-]+', row)` also matches keyword-shaped VALUES. In this
    queue `:duree`, `:resolved`, `:blocked-mathlib-frontier` and `:hard-proof-step`
    are values, so a naive count reported "64 of 88 rows" including many rows
    whose only "duplicate" was a repeated value keyword.
  - An earlier hand count in the same session reported "~10 duplicates across 8
    rows, mostly :receipt", which understated the real figure by more than an
    order of magnitude.

The truth (this script): 59 of 88 rows, 306 extra key occurrences.

Keys are the tokens at DEPTH 1 in even position, so the row must be walked with
a real tokeniser that skips over strings and nested collections. That is what
top_level_keys does.

WHAT THE CORRECTED SHAPE REVEALS, which the wrong numbers hid: the duplicated
keys are almost entirely `:job-id`, `:dispatched-at`, `:dispatch-mode`,
`:dispatched-by`, `:runner`, `:attempts` -- dispatch bookkeeping, duplicated
ONCE PER RE-DISPATCH (a01A07 has five `:job-id`s). The bookkeeping path APPENDS
where it should SET. EDN readers keep a single occurrence (edn_format takes the
last), so behaviour is correct-by-luck while each row's dispatch history is
silently shadowed.

Do NOT "fix" this with a regex sweep -- that corrupted the queue once already
(four rows got wrong receipts, one was emptied). Make the bookkeeping path
set-not-append, then re-emit once with nothing in flight, verifying row count
and field values.
"""
import sys,re,collections
sys.path.insert(0,'/home/joe/code/futon3c/scripts')
from refresh_statement_hints import iter_rows
from pathlib import Path

def top_level_keys(row):
    """Keys of the row map: tokens at depth 1 in even (key) position."""
    assert row.startswith('{') and row.rstrip().endswith('}')
    s=row.strip()[1:-1]
    keys=[]; i=0; n=len(s); depth=0; slot=0   # slot 0 = key, 1 = value
    while i<n:
        ch=s[i]
        if ch.isspace(): i+=1; continue
        if ch=='"':                       # string literal
            j=i+1
            while j<n and not (s[j]=='"' and s[j-1]!='\\'): j+=1
            tok=s[i:j+1]; i=j+1
        elif ch in '[{(':                 # nested collection
            d=1; j=i+1
            while j<n and d:
                if s[j]=='"':
                    j+=1
                    while j<n and not (s[j]=='"' and s[j-1]!='\\'): j+=1
                elif s[j] in '[{(': d+=1
                elif s[j] in ']})': d-=1
                j+=1
            tok=s[i:j]; i=j
        else:
            j=i
            while j<n and not s[j].isspace(): j+=1
            tok=s[i:j]; i=j
        if slot==0: keys.append(tok)
        slot^=1
    return keys

t=Path('/home/joe/code/futon3c/data/codex-sorry-queue.edn').read_text()
bad=0; tot=0; worst=[]
for lo,hi in iter_rows(t):
    row=t[lo:hi]; tot+=1
    ks=top_level_keys(row)
    dup={k:v for k,v in collections.Counter(ks).items() if v>1}
    if dup:
        rid=re.search(r':id "([^"]+)"',row)
        worst.append((sum(dup.values())-len(dup), rid.group(1) if rid else '?', dup)); bad+=1
worst.sort(reverse=True)
print(f"rows with GENUINE duplicate keys: {bad} of {tot}")
for extra,rid,dup in worst[:12]: print(f"  +{extra:2d}  {rid[:44]:44s} {dup}")
print(f"total extra key occurrences: {sum(w[0] for w in worst)}")
