## Kind A5: rehome

The item depends on futon1a (port 7071), which is superseded by futon1b (port 7073, code at
/home/joe/code/futon1b). Your question: what exactly does the item take from futon1a, and what
in futon1b answers to each dependency?

Output per item:
```json
{"stem": "<id>", "kind": "rehome",
 "dependencies": [
   {"what": {"value": "<the futon1a thing the item uses: endpoint, gate, census, default port, file>", "cite": "path:line", "quote": "..."},
    "futon1b": {"value": "<the futon1b equivalent: path or endpoint>", "cite": "path:line in /home/joe/code/futon1b", "quote": "..."} OR {"unsure": "no equivalent found where I looked: <paths>"}}
 ],
 "port-size": {"value": "small|medium|large", "cite": "path:line", "quote": "..."} OR {"unsure": "..."},
 "confidence": "high|medium|low"}
```
