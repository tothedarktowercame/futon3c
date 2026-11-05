import json,sys
for r in sys.argv[1:]:
    d=json.load(open(f'input/{r}.input.json'))
    print('==',r,d['turn_id'],d['created_at'])
    for f in d['fragments']: print('FRAG', f['id'], f['intent'], '|', f['text'][:150])
    for a in d.get('reply_acts',[]): print('REPLY', a['id'], '|', a['text'][:240].replace('\n',' ¶ '))
    for a in d.get('previous_reply_acts',[]): print('PREV', a['id'], '|', a['text'][:60].replace('\n',' '))
