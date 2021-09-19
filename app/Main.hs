module Main where
import Web.Scotty
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text as T
import Data.Char (isSpace)
import Parser
import RunByteCode

main :: IO ()
main = scotty 3000 $ do
  post "/" $ do
    b <- body
    let tBody = decodeUtf8 b
    let expression = handlePostRequest $ toStrict tBody
    text $ fromStrict expression

handlePostRequest :: T.Text -> T.Text
handlePostRequest input = runByteCode bc
    where textLines = T.lines input
          nonBlankLines = [line | line<- textLines, (not . allWhitespacePredicate) line]
          bc = toByteCode nonBlankLines

allWhitespacePredicate :: T.Text -> Bool
allWhitespacePredicate txt = Prelude.all isSpace (T.unpack txt)