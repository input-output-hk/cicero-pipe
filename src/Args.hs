{-# LANGUAGE OverloadedStrings #-}
module Args where

import Control.Exception
import Data.String
import Options.Applicative hiding (empty)
import Options.Applicative.Help.Pretty
import Servant.API.BasicAuth
import Servant.Client.Core.BaseUrl

type EnvUsername = String
type EnvPassword = String

data Args = Args
  { ciceroURL :: !BaseUrl
  , ciceroAuth :: !(Maybe BasicAuthData)
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

basicAuthParser :: Maybe EnvUsername -> Maybe EnvPassword -> Parser BasicAuthData
basicAuthParser envUsername envPassword = BasicAuthData
  <$> option str
        ( long "user"
       <> metavar "USER"
       <> help "User name for BASIC authentication with cicero server (default $CICERO_USER)"
       <> maybe mempty (value . fromString) envUsername
        )
  <*> option str
        ( long "password"
       <> metavar "PASS"
       <> help "Password for BASIC authentication with cicero server (default $CICERO_PASS)"
       <> maybe mempty (value . fromString) envPassword
        )

argsParser :: Maybe EnvUsername -> Maybe EnvPassword -> Parser Args
argsParser envUsername envPassword = Args
  <$> option baseUrlReader
        ( long "cicero-url"
       <> metavar "CICERO_URL"
       <> help "URL of the cicero server"
       <> showDefaultWith showBaseUrl
       <> (value $ BaseUrl Http "localhost" 8080 "")
        )
  <*> optional (basicAuthParser envUsername envPassword)
  <*> flag False True
        ( long "debug-mode"
       <> help "Print results to stderr instead of posting to Cicero"
        )

argsInfo :: Maybe EnvUsername -> Maybe EnvPassword -> ParserInfo Args
argsInfo envUsername envPassword = info (argsParser envUsername envPassword <**> helper)
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
