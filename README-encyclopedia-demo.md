# Futon3c Encyclopedia Demo (First Refactor Demo)

This captures the first `futon3` replacement demo in `futon3c`:
- `GET /fulab/encyclopedia/corpuses`
- `GET /fulab/encyclopedia/:corpus/entries?limit=&offset=`
- `GET /fulab/encyclopedia/:corpus/entry/:id`

The endpoints are compatible with the legacy Futon3 encyclopedia response
shape and use local PlanetMath EDN corpuses.

## Prerequisites

- Local corpus directory exists (default): `/home/joe/code/planetmath`
- Merged corpus example: `/home/joe/code/planetmath/planetmath.edn`

## Run Demo Server

```bash
cd /home/joe/code/futon3c
clojure -M -m scripts.encyclopedia-demo
```

Optional environment overrides:

```bash
FUTON3C_PORT=5050 FUTON3C_PLANETMATH_ROOT=/home/joe/code/planetmath clojure -M -m scripts.encyclopedia-demo
```

## Smoke Check

```bash
curl -s http://localhost:5050/fulab/encyclopedia/corpuses | jq .
curl -s 'http://localhost:5050/fulab/encyclopedia/planetmath/entries?limit=3&offset=0' | jq .
curl -s 'http://localhost:5050/fulab/encyclopedia/planetmath/entry/Derangement' | jq .
```

## Notes

- Entry IDs in the route are URL-decoded, so encoded IDs (for example with `/`)
  are supported.
- Corpus loading is cached with mtime invalidation for iterative local work.
