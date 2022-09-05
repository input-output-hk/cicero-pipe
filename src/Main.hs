{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Exception
import Data.Aeson.Parser hiding (value)
import Data.Attoparsec.ByteString hiding (option)
import Data.Attoparsec.ByteString.Char8 as Atto
import Data.ByteString as BS
import Data.Proxy
import Data.String
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Options.Applicative as Optparse
import Options.Applicative.Help.Pretty as Pretty
import Servant.Client hiding (manager)
import Servant.Client.Core.BaseUrl
import Servant.Client.Core.BasicAuth
import Servant.API.BasicAuth
import System.Environment
import System.IO
import IOHK.Cicero.API
import IOHK.Cicero.API.Fact hiding (API, value)

type EnvUsername = String
type EnvPassword = String

data Args = Args
  { ciceroURL :: !BaseUrl
  , ciceroAuth :: !(Maybe BasicAuthData)
  }

baseUrlReader :: ReadM BaseUrl
baseUrlReader = do
  urlStr <- str
  case parseBaseUrl urlStr of
    Left e -> case fromException e of
      Just (InvalidBaseUrlException s) -> readerError $ "invalid URL '" ++ urlStr ++ "': " ++ s
      Nothing -> readerError $ "exception parsing '" ++ urlStr ++ "' as a URL: " ++ displayException e
    Right b -> pure b

basicAuthParser :: Maybe EnvUsername -> Maybe EnvPassword -> Optparse.Parser BasicAuthData
basicAuthParser envUsername envPassword = BasicAuthData
  <$> Optparse.option str
        ( long "user"
       <> metavar "USER"
       <> help "User name for BASIC authentication with cicero server (default $CICERO_USER)"
       <> maybe mempty (value . fromString) envUsername
        )
  <*> Optparse.option str
        ( long "password"
       <> metavar "PASS"
       <> help "Password for BASIC authentication with cicero server (default $CICERO_PASS)"
       <> maybe mempty (value . fromString) envPassword
        )
  
argsParser :: Maybe EnvUsername -> Maybe EnvPassword -> Optparse.Parser Args
argsParser envUsername envPassword = Args
  <$> Optparse.option baseUrlReader
        ( long "cicero-url"
       <> metavar "CICERO_URL"
       <> help "URL of the cicero server"
       <> showDefaultWith showBaseUrl
       <> (value $ BaseUrl Http "localhost" 8080 "")
        )
  <*> optional (basicAuthParser envUsername envPassword)

argsInfo :: Maybe EnvUsername -> Maybe EnvPassword -> ParserInfo Args
argsInfo envUsername envPassword = info (argsParser envUsername envPassword <**> helper)
  ( fullDesc
 <> header "cicero-pipe — Stream facts to Cicero"
 <> (progDescDoc . Just $ vcat
   [ "Creates Cicero facts from whitespace-separated JSON on stdin"
   , Pretty.empty
   , ("To include an artifact," </> "send a '!' before the JSON and send \"NN:Bs\" afterward," </> "where NN matches attoparsec's `decimal @Int` parser, and Bs is NN raw bytes")
   ])
  )

data ParseException = ParseException
  { leftover :: !ByteString
  , contexts :: ![String]
  , message :: !String
  } deriving stock (Show)

instance Exception ParseException

-- | Parse a fact with an optional artifact.
--
-- Yields 'Nothing' if EOF is reached after only
-- spaces have been consumed.
--
-- Parses a straight JSON value as a fact with no
-- artifact.
--
-- Parses '!' followed by a JSON value followed by
-- NN:Bs where NN matches 'decimal' @'Int' and Bs
-- is NN bytes as a fact with an artifact.
createFactParser :: Atto.Parser (Maybe CreateFactV1)
createFactParser = do
  skipSpace

  eof <- atEnd

  case eof of
    True -> pure Nothing
    False -> Just <$> do
      hasArtifact <- Atto.option False $
        Atto.char '!' *> pure True

      v <- jsonNoDup' <?> "fact JSON"

      art <- case hasArtifact of
        False -> pure Nothing
        True -> do
          skipSpace
          byteCount <- decimal @Int <?> "artifact size"
          _ <- Atto.char ':' <?> "artifact size/content separator"
          Just . fromStrict <$> (Atto.take byteCount <?> "artifact content")

      pure $ CreateFact v art

-- | Parse facts from stdin
parseFacts :: Handle -> Chan (Maybe CreateFactV1) -> IO ()
parseFacts inH chan = go Nothing
  where
    bufsiz = 2048 -- Why not

    startParse = Just . parse createFactParser

    go (Just (Fail l ctx err)) =
      throwIO $ ParseException l ctx err
    go (Just (Done l res)) = do
      writeChan chan res
      case res of
        Nothing -> pure ()
        Just _ -> go $ case BS.null l of
          True -> Nothing
          False -> startParse l
    go st = do
      bs <- hGetSome inH bufsiz
      case st of
        Nothing -> go $ startParse bs
        Just st' -> go . Just $ feed st' bs

-- | Post facts from input channel to Cicero
postFacts :: Chan (Maybe CreateFactV1) -> ClientEnv -> IO ()
postFacts chan cEnv = go
  where
    createFact = (client $ Proxy @API).fact.create

    go = readChan chan >>= \case
      Nothing -> pure ()
      Just cf ->
        runClientM (createFact cf) cEnv >>= \case
          Left e -> throw e
          Right res -> do
            -- TODO better logging
            hPutStrLn stderr $ "Created new fact: " ++ show res.id.uuid
            go

main :: IO ()
main = do
  -- Parse inputs
  envUsername <- lookupEnv "CICERO_USER"
  envPassword <- lookupEnv "CICERO_PASS"
  args <- execParser $ argsInfo envUsername envPassword

  -- Set up plumbing for parser
  let inH = stdin

  -- Set up plumbing for poster
  manager <- newTlsManagerWith $
    tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
  let
    cEnv = mkClientEnv manager args.ciceroURL
    cEnv' = case args.ciceroAuth of
      Nothing -> cEnv
      Just ba -> cEnv
        { makeClientRequest = \u -> defaultMakeClientRequest u . basicAuthReq ba
        }

  -- Pipe parse results into poster
  chan <- newChan
  concurrently_ (parseFacts inH chan) (postFacts chan cEnv')
