{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
module Post where

import Control.Monad.Catch
import Data.Proxy
import Data.Time.LocalTime
import Data.UUID.V4
import IOHK.Cicero.API
import IOHK.Cicero.API.Fact hiding (API)
import Servant.Client
import System.IO

-- | Capablities needed to 'postFact'.
data PostFact m = PostFact
  { -- | Send off the fact
    post :: !(CreateFactV1 -> m (Either ClientError FactV1))
  , -- | Report the fact ID
    report :: !(FactID -> m ())
  }

-- | Report facts by newline-separated posting to `stdout`
stdoutReport :: FactID -> IO ()
stdoutReport i = do
  hPutStrLn stdout $ show i.uuid
  hFlush stdout

-- | 'PostFact' sending to the Cicero API
realPostFact :: ClientEnv -> PostFact IO
realPostFact cEnv = PostFact
    { post = flip runClientM cEnv . createFact
    , report = stdoutReport
    }
  where
    createFact = (client $ Proxy @API).fact.create

-- If we have proper logging this should probably just be noopPostFact
-- | 'PostFact' printing to stderr.
debugPostFact :: PostFact IO
debugPostFact = PostFact
  { post = \cf -> do
      putStr "Fact: "
      print cf.fact
      case cf.artifact of
        Nothing -> putStrLn "No artifact"
        Just art -> do
          putStr "Artifact: "
          print art
      fid <- FactID <$> nextRandom
      now <- getZonedTime
      pure . Right $ Fact
        { id = fid
        , runId = Nothing
        , createdAt = now
        , value = cf.fact
        , binaryHash = Nothing -- Not right, oh well
        }
  , report = stdoutReport
  }

-- | Post a fact to Cicero
postFact :: (MonadThrow m) => PostFact m -> CreateFactV1 -> m ()
postFact (PostFact {..}) cf = post cf >>= \case
    Left e -> throwM e
    Right res -> report $ res.id
