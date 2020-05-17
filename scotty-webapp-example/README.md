Scotty Webapp Example
=====================

Example application implementing RESTful endpoints using the [Scotty framework](https://hackage.haskell.org/package/scotty).

Created after the example found on the tutorial [How to write a Haskell web service (from scratch)](https://dev.to/parambirs/how-to-write-a-haskell-web-servicefrom-scratch---part-3-5en6)

# Build and Run

```bash
cabal clean
cabal build
cabal run
```

# Test

```bash
curl -X GET http://localhost:3000/article

curl -H "Content-Type: application/json" -X POST http://localhost:3000/article -d '{"id":1, "title":"Test", "bodyText":"body"}'
```
