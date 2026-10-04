#!/usr/bin/env python3
"""Read-only census: how often does an agent's bracketed target ("㊟ (your
point on X) ...") match something without a model?  Reads the 象 turn store
(~/.emacs-graph/session-turn-analysis) and futon1b evidence (:7073).  Writes
nothing except its own JSON report to the path given as argv[1], and prints
a one-line summary.  Usage: python3 scripts/xiang_bracket_census.py OUT.json
First run 2026-10-04 (daxiang_live §9 step 1): 1,772 targets, 28 found in
the operator turn they answer."""
import glob, json, os, re, sys, collections, urllib.request, urllib.parse

STORE = os.path.expanduser('~/.emacs-graph/session-turn-analysis')
SINCE = '2026-09-30'
MARKS = "㊩🈖㊢🈯㊟㊣🈚㊮🈹🈲🈕㊫㊭㊝🈘🈝㊯🈡🈸🈰㊬㊥🈳"
OPENING_MARKS = set("🈸㊭🈯㊩㊬🈲㊯")          # agent paragraphs whose kind opens a port
OPENING_INTENTS = {'propose', 'ask-action', 'offer', 'delegate', 'promise',
                   'report-problem', 'clarify', 'constrain', 'verify'}
MIN = 8

def squash(s): return re.sub(r'\s+', ' ', (s or '').lower()).strip()

def paragraphs(t): return [p.strip() for p in re.split(r'\n\s*\n', t or '') if p.strip()]

def bracket(after):
    """Text of a leading (...) in AFTER, balanced; None when absent."""
    s = after.lstrip()
    if s.startswith(':'): s = s[1:].lstrip()
    if not s.startswith('('): return None, after
    depth = 0
    for i, ch in enumerate(s):
        depth += ch == '('
        depth -= ch == ')'
        if depth == 0: return s[1:i].strip(), s[i+1:]
    return None, after

def marked(text):
    out = []
    for p in paragraphs(text):
        if p and p[0] in MARKS:
            tgt, body = bracket(p[1:])
            out.append({'mark': p[0], 'target': tgt, 'body': body.strip(), 'para': p})
    return out

def get(url):
    req = urllib.request.Request(url, headers={'Accept': 'application/json'})
    with urllib.request.urlopen(req, timeout=120) as r: return json.load(r)

def session_evidence(sid):
    out, cur = [], None
    while True:
        q = {'session-id': sid, 'limit': '1000', 'since': SINCE + 'T00:00:00Z'}
        if cur: q.update({'cursor-at': cur['at'], 'cursor-id': cur['id']})
        d = get('http://localhost:7073/api/alpha/evidence?' + urllib.parse.urlencode(q))
        out += d['entries']
        cur = d.get('next-cursor')
        if not cur or not d['entries']: return out

# --- operator records -------------------------------------------------------
recs = []
for f in glob.glob(os.path.join(STORE, 'turn-*.json')):
    if os.path.basename(f).count('.') > 1: continue
    try: r = json.load(open(f))
    except Exception: continue
    if not isinstance(r, dict) or (r.get('created_at') or '') < SINCE: continue
    if (r.get('origin') or 'operator') != 'operator': continue
    a = f + '.analysis.json'
    r['_frags'] = []
    if os.path.exists(a):
        try: r['_frags'] = [fr for s in json.load(open(a)).get('sentences', []) for fr in s.get('fragments', [])]
        except Exception: pass
    r['_file'] = os.path.basename(f)
    recs.append(r)
by_sess = collections.defaultdict(list)
for r in recs: by_sess[r['session_id']].append(r)

# --- replies ---------------------------------------------------------------
rows, sessions_used, seats = [], set(), collections.Counter()
for sid, rs in by_sess.items():
    by_ev = {r.get('evidence_id'): r for r in rs if r.get('evidence_id')}
    if not by_ev: continue
    try: ev = session_evidence(sid)
    except Exception as e:
        print('session read failed', sid, e, file=sys.stderr); continue
    replies = []
    for e in ev:
        b = e.get('evidence/body')
        if not (isinstance(b, dict) and b.get('role') == 'assistant'): continue
        if b.get('segment-final') is False: continue
        op = by_ev.get(e.get('evidence/in-reply-to'))
        if not op: continue
        replies.append((op['created_at'], op, b.get('unified-text') or b.get('text') or '', b.get('turn-id')))
    replies.sort(key=lambda x: x[0])
    if replies: sessions_used.add(sid)
    seen_paras = []          # (squashed body, turn-id) of earlier reply paragraphs
    seen_open = []           # squashed text of earlier opening moves (agent or operator)
    for at, op, text, tid in replies:
        seats[op.get('agent_id')] += 1
        op_text = squash(op.get('source_text'))
        cur_open = [squash(fr.get('text')) for fr in op['_frags'] if fr.get('intent') in OPENING_INTENTS]
        ms = marked(text)
        for m in ms:
            row = {'turn': tid or op.get('turn_id'), 'op_turn': op.get('turn_id'), 'agent': op.get('agent_id'),
                   'at': at, 'mark': m['mark'], 'target': m['target']}
            t = squash(m['target'])
            if m['target'] is not None and len(t) >= MIN:
                row['a'] = t in op_text
                hits = [x for x in seen_paras if t in x[0]]
                row['b'] = len(hits) == 1
                row['b_any'] = len(hits)
                row['c'] = any(t in x for x in seen_open + cur_open)
            rows.append(row)
        for m in ms:
            seen_paras.append((squash(m['body']), tid))
            if m['mark'] in OPENING_MARKS: seen_open.append(squash(m['body']))
        seen_open += cur_open

json.dump({'sessions': len(sessions_used), 'seats': seats, 'rows': rows,
           'records': len(recs)}, open(sys.argv[1], 'w'), ensure_ascii=False)
print('records', len(recs), 'sessions', len(sessions_used), 'rows', len(rows))

# --- summary ---------------------------------------------------------------
# Generic: the same target text on >= 5 paragraphs (a label, no referent).
# Classes over the rest (>= MIN chars), exclusive in the order a > b > c > d > e:
#   a  occurs in the operator turn the reply answers
#   b  occurs in exactly one earlier reply paragraph of the session
#   c  occurs in an earlier opening move (agent 🈸㊭🈯㊩㊬🈲㊯ paragraph, or an
#      operator fragment with an opening intent); not checked for still-open
#   d  addressed by wording ("your …", "my …", "earlier …")
#   e  none of these
withb = [r for r in rows if r['target'] is not None]
freq = collections.Counter(squash(r['target']) for r in withb)
generic = {t for t, n in freq.items() if n >= 5}
pool = [r for r in withb if 'a' in r and squash(r['target']) not in generic]
excl = collections.Counter()
for r in pool:
    t = squash(r['target'])
    excl['a' if r['a'] else 'b' if r['b'] else 'c' if r['c'] else
         'd' if re.match(r"^(your|you|my|our|earlier|the earlier|the operator|joe)\b", t)
                or re.search(r"\byour (point|question|turn|message|ask|request|note|proposal)", t)
         else 'e'] += 1
print(json.dumps({'replies': len({(r['turn'], r['op_turn']) for r in rows}),
                  'marked': len(rows), 'with_target': len(withb),
                  'generic': sum(1 for r in withb if squash(r['target']) in generic),
                  'short': sum(1 for r in withb if 'a' not in r and squash(r['target']) not in generic),
                  'pool': len(pool), 'classes': dict(excl)}, ensure_ascii=False))
