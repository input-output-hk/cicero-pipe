{-# LANGUAGE OverloadedStrings #-}
module Args where

import Control.Exception
import IOHK.Cicero.API.Run
import Options.Applicative hiding (empty)
import Options.Applicative.Help.Pretty
import Servant.Client.Core.BaseUrl

import Parse

data Args = Args
  { ciceroURL :: !BaseUrl
  , netrcFile :: !(Maybe FilePath)
  , runId :: !(Maybe RunID)
  , allowArtifacts :: !AllowArtifacts
  , debug :: !Bool
  }

baseUrlReader :: ReadM BaseUrl
baseUrlReader = do
  urlStr <- str
  case parseBaseUrl urlStr of
    Left e -> case fromException e of
      Just (InvalidBaseUrlException s) -> readerError $ "invalid URL '" ++ urlStr ++ "': " ++ s
      Nothing -> readerError $ "exception parsing '" ++ urlStr ++ "' as a URL: " ++ displayException e
    Right b -> pure b

argsParser :: Parser Args
argsParser = Args
  <$> option baseUrlReader
        ( long "cicero-url"
       <> metavar "CICERO_URL"
       <> help "URL of the cicero server"
       <> showDefaultWith showBaseUrl
       <> (value $ BaseUrl Http "localhost" 8080 "")
        )
  <*> optional (strOption
        ( long "netrc-file"
       <> metavar "NETRC"
       <> help "the path to a netrc(5) file for credentials"
        ))
  <*> optional (option (maybeReader runIdFromString)
        ( long "run-id"
       <> metavar "RUN_ID"
       <> help "the ID of the run to associate the facts with"
        ))
  <*> flag YesArtifacts NoArtifacts
        ( long "disable-artifacts"
       <> help "Prevent artifacts from being posted"
        )
  <*> flag False True
        ( long "debug-mode"
       <> help "Print results to stderr instead of posting to Cicero"
        )

argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
 <> header "cicero-pipe — Stream facts to Cicero"
 <> (progDescDoc . Just $ vcat
   [ "Creates Cicero facts from whitespace-separated JSON on stdin"
   , empty
   , (  "To include an artifact,"
    </> "send a '!' before the JSON and send \"NN:Bs\" afterward,"
    </> "where NN matches attoparsec's `decimal @Int` parser, and Bs is NN raw bytes."
     )
   , empty
   , "The resulting fact UUIDs will be emitted on `stdout`"
   ])
  )
