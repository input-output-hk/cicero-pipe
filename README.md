# cicero-pipe — Stream facts to Cicero
Creates Cicero facts from whitespace-separated JSON on stdin
  
To include an artifact, send a `!` before the JSON and send `NN:Bs` afterward,
where `NN` matches attoparsec's `decimal @Int` parser, and `Bs` is `NN` raw bytes.

The resulting fact UUIDs will be emitted on `stdout`

```
Usage: cicero-pipe [--cicero-url CICERO_URL] [--netrc-file NETRC]
                   [--run-id RUN_ID] [--disable-artifacts] [--debug-mode]

Available options:
  --cicero-url CICERO_URL  URL of the cicero server
                           (default: http://localhost:8080)
  --netrc-file NETRC       the path to a netrc(5) file for credentials
  --run-id RUN_ID          the ID of the run to associate the facts with
  --disable-artifacts      Prevent artifacts from being posted
  --debug-mode             Print results to stderr instead of posting to Cicero
  -h,--help                Show this help text
```
