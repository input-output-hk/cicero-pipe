{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import Control.Exception
import Data.ByteString
import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Options.Applicative (execParser)
import Servant.Client hiding (manager)
import Servant.Client.Core.BasicAuth
import System.Environment
import System.IO
import IOHK.Cicero.API
import IOHK.Cicero.API.Fact hiding (API)

import Args
import Parse

-- | Post a fact to Cicero
postFact :: ClientEnv -> CreateFactV1 -> IO ()
postFact cEnv cf = runClientM (createFact cf) cEnv >>= \case
    Left e -> throw e
    Right res -> do
      hPutStrLn stdout $ show res.id.uuid
      hFlush stdout
  where
    createFact = (client $ Proxy @API).fact.create

main :: IO ()
main = do
  -- Parse inputs
  envUsername <- lookupEnv "CICERO_USER"
  envPassword <- lookupEnv "CICERO_PASS"
  args <- execParser $ argsInfo envUsername envPassword

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

  let bufsiz = 2048 -- Why not
  parseFacts $ ParseFacts (hGetSome stdin bufsiz) (postFact cEnv')
