#!/usr/bin/env python3
"""Census: how would label brackets link, under candidate rules?  Read-only.

Replays, in Python, the existing quote rule (turn-acts/quoted-targets +
link-quoted-brackets) and two candidate rules over the turn records with
reply_text, and reports how many opening operator fragments each would answer,
and how the labelled claude-15 session (CHECKPOINT-2026-10-11-false-open-ports)
would come out row by row.

  A  label-overlap : a bracket with no quote links to the unique kinded
                     fragment of the same turn sharing >= 2 content words
  B  sole-port     : a reply paragraph of a "says" kind (report, explain,
                     verify, qualify, report-problem) with no target answers
                     the turn's only opening fragment
Only the current turn's fragments are candidates, as in link-quoted-brackets.
"""
import json, glob, re, sys, collections
DIR = '/home/joe/.emacs-graph/session-turn-analysis'
MARKS = {'㊥':'gist','㊭':'propose','㊣':'approve','🈚':'disagree','㊟':'qualify','🈖':'explain',
         '🈯':'clarify','㊢':'report','㊩':'report-problem','㊬':'verify','🈹':'retract','🈳':'unresolved',
         '🈲':'constrain','🈸':'ask-action','㊯':'delegate','㊝':'prioritize','㊮':'collect','🈕':'extend',
         '🈰':'continue','🈝':'defer','🈘':'redirect','㊫':'explore','🈡':'withdraw'}
OPENING = {'propose','ask-action','offer','delegate','promise','report-problem','clarify','constrain','verify'}
SAYS = {'report','explain','verify','qualify','report-problem'}
# adjacency (logic/xiang.clj, 2026-10-11): opener -> {answer: effect}
ADJ = {'propose':{'approve':'closes','disagree':'closes','defer':'closes','redirect':'closes','extend':'closes','commit':'closes','withdraw':'closes','retract':'closes'},
       'ask-action':{'accept':'closes','disagree':'closes','qualify':'keeps','offer':'keeps','report':'closes','explain':'closes'},
       'offer':{'accept':'closes','disagree':'closes','defer':'closes','retract':'closes','withdraw':'closes'},
       'delegate':{'promise':'closes','report-problem':'keeps','retract':'closes','disagree':'closes','report':'closes'},
       'promise':{'fulfil':'closes','release':'closes','lapse':'closes','report-problem':'keeps','qualify':'keeps'},
       'report-problem':{'verify':'closes','explain':'closes','redirect':'closes','commit':'closes','collect':'keeps'},
       'clarify':{'explain':'closes','report':'closes'},
       'constrain':{'withdraw':'closes','rule-withdraw':'closes','qualify':'keeps','commit':'keeps'},
       'verify':{'report':'closes','approve':'closes','explain':'closes','report-problem':'closes'}}
# B*: sole-port, but any reply paragraph kind, and the link counts as a close only when ADJ types the pair :closes
NOKIND = {'gist','unresolved'}
STOP = set('''the a an and or of to in on for with that this these those is are was were be been being it its
we you i they he she our your their my me us them not no yes do does did done have has had will would can could
should may might just also then than so as at by from into over about what which who how when where why let lets
please ok okay well right sure thing things something anything some any all more most much very really there here
one two first next now still again back out up down off like want need think know see use used using make made
going go get got take went come came say said work works working way ways'''.split())
def squash(s): return re.sub(r'\s+',' ',(s or '').lower()).strip()
def words(s): return {w for w in re.findall(r"[a-z0-9][a-z0-9'\-]{2,}", squash(s)) if w not in STOP and len(w)>=4}
def load():
    recs=[]
    for p in glob.glob(DIR+'/turn-*.json'):
        if p.rsplit('/',1)[1].count('.')>1: continue
        try: d=json.load(open(p))
        except Exception: continue
        recs.append((d.get('created_at',''),p.rsplit('/',1)[1][:-5],d))
    recs.sort(); return recs
def frags(id):
    try: an=json.load(open(f'{DIR}/{id}.json.analysis.json'))
    except Exception: return []
    out=[]
    for s in an.get('sentences',[]):
        for i,f in enumerate(s.get('fragments') or []):
            k=f.get('intent')
            if k and k not in NOKIND: out.append({'id':f"{s.get('id')}-{i}",'kind':k,'text':f.get('text','')})
    return out
def paras(reply):
    out=[]
    for x in re.split(r'\n\s*\n',reply or ''):
        x=x.strip()
        if not x or x[0] not in MARKS: continue
        intent=MARKS[x[0]]
        if intent in NOKIND: continue
        body=re.sub(r'^:\s*','',x[1:].lstrip())
        m=re.match(r'^\s*\(([^)]*)\)',body)
        bracket=m.group(1) if m else None
        quotes=[squash(q) for q in re.findall(r'["“]([^"”]+)["”]',bracket or '') if len(squash(q))>=8]
        out.append({'kind':intent,'bracket':bracket,'quotes':quotes,'text':body})
    return out
def link(fr, ps, rules):
    """targets per paragraph index under the rule set; returns {pidx: fid}"""
    t={}
    sq=[(f['id'],squash(f['text'])) for f in fr]
    for i,p in enumerate(ps):
        if p['quotes']:
            ids={fid for q in p['quotes'] for fid,ft in sq if ft and (q in ft or ft in q)}
            if len(ids)==1: t[i]=ids.pop(); continue
        if 'A' in rules and p['bracket'] is not None and not p['quotes']:
            lw=words(p['bracket'])
            if len(lw)>=2:
                ids={fid for fid,ft in sq if len(lw & words(ft))>=2}
                if len(ids)==1: t[i]=ids.pop(); continue
        if 'B' in rules and p['kind'] in SAYS:
            op=[f for f in fr if f['kind'] in OPENING]
            if len(op)==1: t[i]=op[0]['id']
        if 'B*' in rules:
            op=[f for f in fr if f['kind'] in OPENING]
            if len(op)==1 and ADJ.get(op[0]['kind'],{}).get(p['kind'])=='closes': t[i]=op[0]['id']
    return t
def main():
    recs=load()
    by_sess=collections.defaultdict(list)
    for c,id,d in recs:
        if d.get('origin')=='operator' and d.get('analysis_status')=='analyzed' and d.get('reply_text'):
            by_sess[d.get('session_id')].append((c,id,d))
    print('sessions with reply text:',len(by_sess),' records:',sum(len(v) for v in by_sess.values()))
    tot=collections.Counter(); amb=collections.Counter()
    rows=[]  # labelled session
    LAB='ec3a9b31-6656-4530-90db-53a22762c17f'
    for sess,items in by_sess.items():
        for c,id,d in items:
            fr=frags(id); ps=paras(d['reply_text'])
            op=[f for f in fr if f['kind'] in OPENING]
            tot['opening fragments']+=len(op); tot['marked paragraphs']+=len(ps)
            tot['turns']+=1; tot['turns with exactly one opening fragment']+= (len(op)==1)
            closed={}
            for name,rules in (('quote',set()),('quote+A',{'A'}),('quote+B',{'B'}),('quote+A+B',{'A','B'}),('quote+B*',{'B*'})):
                t=link(fr,ps,rules); hit={fid for fid in t.values() if fid in {f['id'] for f in op}}
                closed[name]=hit; tot[f'{name}: opening fragments answered']+=len(hit)
            if sess==LAB or d.get('session_id','').startswith('claude-15'):
                for f in op:
                    rows.append((c[5:16].replace('T',' '),f['kind'],f['text'][:55],*(('closed' if f['id'] in closed[n] else '-') for n in ('quote','quote+A','quote+B','quote+A+B','quote+B*'))))
    for k,v in sorted(tot.items()): print(f'{v:6d}  {k}')
    print('\nlabelled session (claude-15), opening fragments by rule:  quote | +A | +B | +A+B | +B*(table-typed)')
    for r in rows: print(' | '.join(map(str,r)))
if __name__=='__main__': main()
