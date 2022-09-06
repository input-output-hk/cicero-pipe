{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}
module Post where

import Control.Monad.Catch
import Data.Proxy
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

-- | Post a fact to Cicero
postFact :: (MonadThrow m) => PostFact m -> CreateFactV1 -> m ()
postFact (PostFact {..}) cf = post cf >>= \case
    Left e -> throwM e
    Right res -> report $ res.id
