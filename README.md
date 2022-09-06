# cicero-pipe — Stream facts to Cicero
Creates Cicero facts from whitespace-separated JSON on stdin
  
To include an artifact, send a `!` before the JSON and send `NN:Bs` afterward,
where `NN` matches attoparsec's `decimal @Int` parser, and `Bs` is `NN` raw bytes

```
Usage: cicero-pipe [--cicero-url CICERO_URL] [--user USER --password PASS]

Available options:
  --cicero-url CICERO_URL  URL of the cicero server
                           (default: http://localhost:8080)
  --user USER              User name for BASIC authentication with cicero server
                           (default $CICERO_USER)
  --password PASS          Password for BASIC authentication with cicero server
                           (default $CICERO_PASS)
  -h,--help                Show this help text
```