module YTM.VideoId where

import Data.Either
import Prelude
import Undefined
import YTM.Types

import Control.Alternative ((<|>))
import Data.String (trim)
import Text.Parsing.StringParser (Parser(..), runParser)
import Text.Parsing.StringParser.CodeUnits (eof, regex, string)

-- | Should accept inputs in the following formats:
-- |
-- | ```
-- | https://www.youtube.com/watch?v=4ggdVMdWYOw&list=UUTtp2hieIlrFBC3Kb_jA9FQ&index=3
-- | https://www.youtube.com/watch?v=4ggdVMdWYOw&list=UUTtp2hieIlrFBC3Kb_jA9FQ
-- | https://www.youtube.com/watch?v=4ggdVMdWYOw
-- | https://youtu.be/4ggdVMdWYOw
-- | 4ggdVMdWYOw
-- | ```
parseVideoId :: VideoURLInput -> Either InvalidVideoURL VideoId
parseVideoId input = note input $ hush $ runParser videoIdParser (trim input)

videoIdParser :: Parser VideoId
videoIdParser =
  (id <* eof) <|> shortLink <|> longLink

id :: Parser VideoId
id = VideoId <$> regex "[a-zA-Z0-9_-]{11}"

shortLink :: Parser VideoId
shortLink = do
  void $ string "https://youtu.be/"
  res <- id
  eof
  pure res

longLink :: Parser VideoId
longLink = do
  void $ string "https://www.youtube.com/watch?v="
  res <- id
  void $ regex ".*"
  eof
  pure res
