{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
module Parse where

import Prelude hiding (null, take)

import Control.Monad.Catch
import Data.Aeson.Parser
import Data.Attoparsec.ByteString.Char8
import Data.ByteString hiding (take)
import IOHK.Cicero.API.Fact

data ParseException = ParseException
  { leftover :: !ByteString
  , contexts :: ![String]
  , message :: !String
  } deriving stock (Show)

instance Exception ParseException

-- | Config flag determining if artifacts are allowed in facts.
data AllowArtifacts
  = YesArtifacts
  | NoArtifacts

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
createFactParser :: AllowArtifacts -> Parser (Maybe CreateFactV1)
createFactParser allowArts = do
    skipSpace

    eof <- atEnd

    case eof of
      True -> pure Nothing
      False -> Just <$> parseReal allowArts
  where
    parseReal YesArtifacts = do
      hasArtifact <- option False $
        char '!' *> pure True

      v <- jsonNoDup' <?> "fact JSON"

      art <- case hasArtifact of
        False -> pure Nothing
        True -> do
          skipSpace
          byteCount <- decimal <?> "artifact size"
          _ <- char ':'
            <?> "artifact size/content separator"
          Just . fromStrict <$>
            (take byteCount <?> "artifact content")

      pure $ CreateFact v art
    parseReal NoArtifacts = do
      v <- jsonNoDup' <?> "fact JSON"
      pure $ CreateFact v Nothing

-- | Capablities needed to 'parseFacts'
data ParseFacts m = ParseFacts
  { -- | Get more data to parse.
    --
    -- Yields 'Data.ByteString.empty' when done.
    getSome :: !(m ByteString)
  , -- | Post the new fact
    post :: !(CreateFactV1 -> m ())
  }

-- | Parse facts from input and pass on to continuation
parseFacts :: (MonadThrow m) => AllowArtifacts -> ParseFacts m -> m ()
parseFacts allowArts ParseFacts {..} = go initState
  where
    startParse = parse (createFactParser allowArts)

    initState = Partial startParse

    go (Fail l ctx err) =
      throwM $ ParseException l ctx err
    go (Done _ Nothing) = pure ()
    go (Done l (Just cf)) = do
      post cf
      go $ case null l of
        True -> initState
        False -> startParse l
    go (Partial k) =
      getSome >>= go . k
