"""Audit frozen materials and actual Student/tool receipt joins, not theorem validity."""
from pathlib import Path
import hashlib
import json

BASE = Path(__file__).resolve().parent

def read(path):
    return json.loads(path.read_text())

freeze = read(BASE / 'freeze.json')
for name, expected in freeze['files'].items():
    assert hashlib.sha256((BASE / name).read_bytes()).hexdigest() == expected, name
patterns = read(BASE / 'materials/patterns.json')['patterns']
for pattern in patterns:
    assert hashlib.sha256(pattern['text'].encode()).hexdigest() == pattern['sha256']

turns = []
for send_path in sorted((BASE / 'receipts').glob('*-send.json')):
    label = send_path.name.removesuffix('-send.json')
    job_path = BASE / f'receipts/{label}-job.json'
    if not job_path.exists():
        continue
    send, job = read(send_path), read(job_path)['job']
    assert send['job-id'] == job['job-id']
    assert job['agent-id'] == 'zai-student-transfer-20260910'
    assert job['state'] == 'done', (label, job['state'])
    snapshot = read(BASE / f'receipts/{label}-messages.json')
    messages = snapshot['messages']
    assert all('reasoning_content' not in m for m in messages)
    user_index = max(i for i,m in enumerate(messages) if m['role']=='user')
    prompt = (BASE / f'prompts/{label}.txt').read_text()
    assert prompt.strip() in messages[user_index]['content'], label
    calls = {}
    for index, message in enumerate(messages[user_index+1:], user_index+1):
        for call in message.get('tool_calls', []):
            assert call['id'] not in calls
            calls[call['id']] = {'message_index':index, 'name':call['function']['name'],
                                 'arguments':json.loads(call['function']['arguments'])}
        if message['role']=='tool':
            assert message['tool_call_id'] in calls
            call = calls[message['tool_call_id']]
            assert message['name']==call['name']
            call['result_index'] = index
            call['result_sha256'] = hashlib.sha256(message['content'].encode()).hexdigest()
            call['ok'] = json.loads(message['content']).get('ok')
    assert all('result_index' in call for call in calls.values())
    entries=read(BASE / f'receipts/{label}-evidence.json')['entries']
    start=[e['evidence/body'] for e in entries if e.get('evidence/body',{}).get('dispatch-id')==job['job-id'] and e['evidence/body'].get('event')=='turn-start']
    assert len(start)==1, (label,len(start))
    turn_id=start[0]['turn-id']
    rounds=[e['evidence/body'] for e in entries if e.get('evidence/body',{}).get('turn-id')==turn_id and e['evidence/body'].get('event')=='turn-round']
    assert rounds and any(r.get('final') for r in rounds), label
    usage={k:sum(r.get(k,0) or 0 for r in rounds) for k in ['cost/input-tokens','cost/output-tokens','cost/cached-input-tokens','cost/total-tokens']}
    turns.append({'case_turn':label,'job_id':job['job-id'],'turn_id':turn_id,'session_id':job['session-id'],
                  'started_at':job['started-at'],'finished_at':job['finished-at'],'model':snapshot['model'],
                  'tool_calls':list(calls.values()),'round_count':len(rounds),'reported_usage_sum':usage,
                  'receipt':str((BASE / f'receipts/{label}-messages.json').relative_to(BASE))})
assert len({t['session_id'] for t in turns})==1
report={'scope':'Receipt and frozen-hash audit only; mathematical adjudication is TA paper review. Tool outputs are actual; private reasoning omitted.',
        'frozen_files_verified':len(freeze['files']),'pattern_text_hashes_verified':len(patterns),'turns':turns}
(BASE / 'receipt-audit.json').write_text(json.dumps(report,ensure_ascii=False,indent=2)+'\n')
print(f'PASS: {len(freeze["files"])} frozen files, {len(patterns)} patterns, {len(turns)} completed Student turns; prompt/tool/result/evidence joins.')
