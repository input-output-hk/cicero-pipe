{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.ByteString as BS hiding (foldr)
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Options.Applicative
import Servant.Client hiding (manager)
import Servant.Client.Core.BasicAuth
import Servant.API.BasicAuth
import System.IO
import Text.Parsec.ByteString
import Network.NetRc
import Data.Monoid

import Args
import Parse
import Post

main :: IO ()
main = do
  args <- execParser argsInfo

  pf <- case args.debug of
    True -> pure debugPostFact
    False -> do
      manager <- newTlsManagerWith $
        tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
      auth <- case args.netrcFile of
        Just p -> parseFromFile netRcParsec p >>= \case
          Left e -> do
            hPutStrLn stderr $ "warning: failed to parse netrc file " ++ (show e)
            pure Nothing
          Right netrc -> do
            let urlbs = BS8.pack args.ciceroURL.baseUrlHost
                credsIfMatch :: NetRcHost -> First (ByteString, ByteString)
                credsIfMatch netrcHost = First $ if netrcHost.nrhName == urlbs || BS.null netrcHost.nrhName
                  then Just (netrcHost.nrhLogin, netrcHost.nrhPassword)
                  else Nothing
                match = getFirst (foldr (\netrcHost -> (credsIfMatch netrcHost <>)) mempty netrc.nrHosts)
            case match of
              Nothing -> do
                hPutStrLn stderr $ "warning: failed to find a matching entry in netrc file"
                pure Nothing
              Just (login, pass)
                | BS.null login || BS.null pass -> pure Nothing
                | otherwise -> pure . Just $ BasicAuthData login pass
        Nothing -> pure Nothing
      let
        cEnv = mkClientEnv manager args.ciceroURL
        cEnv' = case auth of
          Nothing -> cEnv
          Just ba -> cEnv
            { makeClientRequest = \u -> defaultMakeClientRequest u . basicAuthReq ba
            }
      pure $ realPostFact cEnv' args.runId

  let bufsiz = 2048 -- Why not
  parseFacts args.allowArtifacts $
    ParseFacts (hGetSome stdin bufsiz) (postFact pf)
