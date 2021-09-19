import Data.Text as T
import Data.Text.Internal.Search
import DataTypes
import Data.Maybe (isJust)
import Parser
import RunByteCode

main :: IO ()
main = do
  putStrLn "Running tests..."
  assert (foundExpectedResponse $ runByteCode byteCode) "passed runByteCode!" "FAIL: runByteCode"
  putStrLn "done"

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement

foundExpectedResponse :: T.Text -> Bool
foundExpectedResponse input = isJust (findTextMaybe "return (x + 1) * y" input)
                            && isJust (findTextMaybe "f() = 4.0" input)

findTextMaybe :: Text -> Text -> Maybe Int
findTextMaybe arg str = case indices arg str of
                          (idx:_) -> Just idx
                          _ -> Nothing

inputText :: [T.Text]
inputText = ["LOAD_VAL 1","WRITE_VAR 'x'","LOAD_VAL 2","WRITE_VAR 'y'",
            "READ_VAR 'x'","LOAD_VAL 1","ADD","READ_VAR 'y'","MULTIPLY","RETURN_VALUE"]

byteCode :: ByteCode
byteCode = toByteCode inputText

