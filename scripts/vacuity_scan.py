#!/usr/bin/env python3
r"""Corpus-wide vacuity scan for the apm-lean problem set.

WHY THIS EXISTS (2026-07-31). Axiom-cleanliness is not non-vacuity. `a95J03` is
counted CLEAN and proves nothing: its target is stated over
`def windingNumber (_g : R -> C) (_z : C) : Z := 0`. Every gate this loop runs --
sorry count, `#print axioms`, signature diffing, the transitive-sorryAx census --
passes on a theorem about a constant function. The only signal was a docstring,
read by a human.

The check this implements has two parts, and BOTH are needed:

  0a. is the CONCLUSION itself `True` (or ends in `True`)?      [a95J06]
  0b. does the conclusion's SUBJECT unfold to a bare constant?  [a95J03, a96A02]

0b was added after a96A02, which 0a alone passes: its conclusions are substantive
quantified inequalities, but their subject is `spikeFunction := 0`, under which
one target is trivially TRUE and the other is FALSE. **A placeholder subject does
not only make positive statements trivial, it makes negative statements false.**

TWO PARSING TRAPS, both of which produced a FALSE ALL-CLEAR on the first run:

  1. A `def` regex of the form `def (\w+)[^:=]*:=` cannot match any definition
     with TYPED BINDERS, because those contain `:`. The first version of this
     scan reported "0 flagged" across 145 files while a95J03 sat in the corpus.
     Definitions and signatures must be delimited by a DEPTH-0 scan for `:=`,
     not by a character-class exclusion.
  2. Classifying a file as sorried with `(?<![\w.])sorry(?![\w])` matches the
     word in PROSE. a95J03's own docstring says "had a sorry in its definition",
     so the scan filed the one known vacuous CLEAN problem under "sorried" and
     the headline "0 clean problems flagged" was vacuously true. Use the lexical
     `^[ \t]*sorry\b`, as refresh_statement_hints.py does.

ALWAYS SANITY-CHECK AGAINST THE KNOWN POSITIVES (a95J03, a96A02, a01J06) before
believing an all-clear -- that is what caught both traps.

Result of the 2026-07-31 run over 145 problem files: exactly ONE clean problem
carries a vacuity signal (a95J03, already known and flagged
`:counts-toward-clean :disputed`), and no new instance exists among the sorried
problems beyond a01J06 / a95J04 / a95J06 / a96A02, all already blocked.
"""
import re, json, sys
from pathlib import Path
sys.path.insert(0,str(Path(__file__).resolve().parent))
from refresh_statement_hints import sorry_positions
from pathlib import Path
ROOT=Path('/home/joe/code/apm-lean/problems')

def depth0_scan(s, start, stop_tok=':='):
    d=0; i=start
    while i < len(s):
        ch=s[i]
        if ch in '([{': d+=1
        elif ch in ')]}': d-=1
        elif d==0 and s.startswith(stop_tok,i): return i
        elif ch=='\n' and s[i+1:i+2] not in (' ','\t','') and d==0: return -1
        i+=1
    return -1

def last_depth0_colon(sig):
    d=0; last=-1
    for i,ch in enumerate(sig):
        if ch in '([{': d+=1
        elif ch in ')]}': d-=1
        elif ch==':' and d==0 and sig[i+1:i+2]!='=': last=i
    return sig[last+1:].strip() if last>=0 else sig.strip()

DEFHEAD=re.compile(r'^(?:private\s+|protected\s+|noncomputable\s+)*def\s+(\w+)',re.M)
DECLHEAD=re.compile(r'^(?:private\s+|protected\s+|noncomputable\s+)*(?:theorem|lemma)\s+(\w+)',re.M)
TRIVIAL=re.compile(r'^\(?\s*(?:-?\d+(?:\.\d+)?|True|False|fun\s+[^=]*=>\s*-?\d+(?:\.\d+)?)\s*\)?$')

rows=[]; files=sorted(ROOT.glob('*/lean/Main.lean'))
for main in files:
    prob=main.parts[-3]; src=main.read_text(errors='replace')
    has_sorry=bool(sorry_positions(src))
    ph={}
    for m in DEFHEAD.finditer(src):
        j=depth0_scan(src,m.end())
        if j<0: continue
        rhs=src[j+2:].split('\n\n')[0].strip()
        rhs=rhs.split('\n')[0].strip() if '\n' in rhs and rhs.split('\n')[0].strip() else rhs
        if TRIVIAL.match(rhs): ph[m.group(1)]=rhs
    trueb=re.findall(r'\((\w+)\s*:\s*True\)',src)
    truefield=re.findall(r'^\s*(\w+)\s*:\s*True\s*$',src,re.M)
    hits=[]
    for m in DECLHEAD.finditer(src):
        j=depth0_scan(src,m.end())
        if j<0: continue
        sig=re.sub(r'\s+',' ',src[m.end():j]).strip()
        concl=last_depth0_colon(sig)
        a=bool(re.search(r'(?<![\w.])True\s*$',concl))
        subj=[p for p in ph if re.search(r'(?<![\w.])'+re.escape(p)+r'(?![\w])',concl)]
        if a or subj: hits.append({'decl':m.group(1),'true_concl':a,'subj':subj,'concl':concl[:140]})
    if hits or ph or trueb or truefield:
        rows.append({'problem':prob,'clean':not has_sorry,'ph':ph,'binders':trueb,
                     'true_fields':truefield,'hits':hits})
json.dump(rows,open('/tmp/vacuity_scan.json','w'),indent=1)
print(f"scanned {len(files)} files; flagged {len(rows)}")
for known in ('a95J03','a96A02','a01J06'):
    r=[x for x in rows if x['problem']==known]
    print(f"  SANITY {known}: {'FLAGGED' if r else '*** MISSED ***'}" + (f"  ph={r[0]['ph']}" if r else ''))
print("\n=== CLEAN problems flagged (counted as solved):")
n=0
for r in rows:
    if not r['clean']: continue
    n+=1
    print(f"  {r['problem']:10s} ph={r['ph'] or '-'} binders={r['binders'] or '-'} fields={r['true_fields'] or '-'}")
    for h in r['hits']: print(f"       {h['decl']}: True-concl={h['true_concl']} subj={h['subj']}")
    if not r['hits']: print(f"       (no target touches them -- placeholder likely INERT)")
print(f"  -> {n} clean problems flagged")
print("\n=== SORRIED problems flagged (pre-dispatch blockers):")
for r in rows:
    if r['clean']: continue
    tgt=[h for h in r['hits']]
    print(f"  {r['problem']:10s} ph={r['ph'] or '-'} binders={r['binders'] or '-'} hits={len(tgt)}")
    for h in tgt: print(f"       {h['decl']}: True-concl={h['true_concl']} subj={h['subj']}")
