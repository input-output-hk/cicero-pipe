{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Data.ByteString
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Servant.Client hiding (manager)
import Servant.Client.Core.BasicAuth
import System.Environment
import System.IO

import Args
import Parse
import Post

main :: IO ()
main = do
  -- Parse inputs
  envUsername <- lookupEnv "CICERO_USER"
  envPassword <- lookupEnv "CICERO_PASS"
  args <- execParser $ argsInfo envUsername envPassword

  pf <- case args.debug of
    True -> pure debugPostFact
    False -> do
      manager <- newTlsManagerWith $
        tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
      let
        cEnv = mkClientEnv manager args.ciceroURL
        cEnv' = case args.ciceroAuth of
          Nothing -> cEnv
          Just ba -> cEnv
            { makeClientRequest = \u -> defaultMakeClientRequest u . basicAuthReq ba
            }
      pure $ realPostFact cEnv' args.runId

  let bufsiz = 2048 -- Why not
  parseFacts $ ParseFacts (hGetSome stdin bufsiz) (postFact pf)
