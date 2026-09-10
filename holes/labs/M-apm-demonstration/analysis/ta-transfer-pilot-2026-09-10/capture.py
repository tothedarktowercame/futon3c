import json,subprocess,urllib.request,sys
from pathlib import Path
b=Path('/home/joe/code/futon3c/holes/labs/M-apm-demonstration/analysis/ta-transfer-pilot-2026-09-10')
label=sys.argv[1]
send=json.load(open(b/f'receipts/{label}-send.json'))
r=json.load(urllib.request.urlopen('http://127.0.0.1:7070'+send['status-url']))
(b/f'receipts/{label}-job.json').write_text(json.dumps(r,indent=2)+'\n')
raw=subprocess.check_output(['bash','scripts/proof-eval.sh','-f',str(b/'capture-messages.clj')],text=True)
Path('/tmp/transfer-message-export.edn').write_text(raw)
expr='(require (quote clojure.edn)) (let [r (clojure.edn/read-string (slurp "/tmp/transfer-message-export.edn"))] (assert (:ok r)) (assert (string? (:value r))) (print (:value r)))'
m=subprocess.check_output(['bb','-e',expr],text=True)
(b/f'receipts/{label}-messages.json').write_text(m+'\n')
sid=Path('/tmp/futon-zai-session-id-zai-student-transfer-20260910').read_text().strip()
e=json.load(urllib.request.urlopen('http://127.0.0.1:7070/api/alpha/evidence?session-id='+sid+'&limit=1000'))
(b/f'receipts/{label}-evidence.json').write_text(json.dumps(e,indent=2)+'\n')
print(label,r['job']['state'],'messages',len(json.loads(m)['messages']))
