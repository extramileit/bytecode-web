module Parser
  (
      toByteCode
  ) where

import Data.Text as T
import Data.Map.Ordered as MO
import qualified Data.List.Split as LS
import DataTypes

type MathOperationGrouping = [T.Text]

toByteCode :: [T.Text] -> ByteCode
toByteCode textLines = ByteCode varMap opCommands
    where  (variableAssignLines, operationLines) = splitIntoVarAssignAndOperations textLines
           varMap = buildVariableMap variableAssignLines
           mathOperationGroupings = groupByMathOperation operationLines
           opCommands = Prelude.map makeOperationCommand mathOperationGroupings

-- Assume that the body of text lines has the variable assignments coming first followed by operation commands
-- Fst of pair holds the lines used to set the variableMap in ByteCode
-- Snd of pair holds the lines used to set the operationCommands in ByteCode
splitIntoVarAssignAndOperations :: [T.Text] -> ([T.Text], [T.Text])
splitIntoVarAssignAndOperations textLines = Prelude.splitAt (findIndexLastWriteVarLine + 1) textLines
    where findIndexLastWriteVarLine = Prelude.maximum (fst writeVarLines)
              where writeVarLines = Prelude.unzip [(i, x) | (i, x) <- Prelude.zip [0..] textLines, T.isPrefixOf constWriteVar x]

-- splits the operation text lines into a list of groupings,
-- where each grouping contains the commands to make a partial or binomial operation (first grouping only)
-- In a stack based model, assume the math operation or RETURN_VALUE will mark the end of a grouping
groupByMathOperation :: [T.Text] -> [MathOperationGrouping]
groupByMathOperation = LS.split (LS.dropFinalBlank $ LS.keepDelimsR $ LS.whenElt isMathOperatorLine)
    where isMathOperatorLine textLine = T.isPrefixOf constAdd textLine
                                     || T.isPrefixOf constSubtract textLine
                                     || T.isPrefixOf constMultiply textLine
                                     || T.isPrefixOf constDivide textLine
                                     || T.isPrefixOf constReturnValue textLine

-- Parse a grouping of lines to build an OperationCommand data structure
-- Bases on line count, infer which type of OperationCommand to build
makeOperationCommand :: MathOperationGrouping -> OperationCommand
makeOperationCommand textLines  =
      case lineCount of 1 -> ReturnVal
                        2 -> Partial mathOpType (resolveInt firstLine)
                        3 -> Binomial mathOpType (resolveInt firstLine) (resolveInt secondLine)
                        _ -> error "Invalid line count"
        where lineCount = Prelude.length textLines
              firstLine = Prelude.head textLines
              secondLine = textLines !! 1
              lastLine = Prelude.last textLines
              mathOpType = resolveMathOp lastLine

              resolveInt textLine = if isLoadVal then secondWord
                                    else removeSingleQuoteChars secondWord
                                        where isLoadVal = T.isPrefixOf "LOAD_VAL" textLine
                                              secondWord = Prelude.last $ T.words textLine

              resolveMathOp textLine =
                  case textLine of "ADD" -> ADD
                                   "MULTIPLY" -> MULTIPLY
                                   "SUBTRACT" -> SUBTRACT
                                   "DIVIDE" -> DIVIDE
                                   "RETURN_VALUE" -> RETURN_VALUE
                                   _ -> error "Could not parse operation text"

buildVariableMap :: [T.Text] -> OMap T.Text Int
buildVariableMap textLines = MO.fromList $ findVariableAssignPairs textLines

findVariableAssignPairs :: [T.Text] -> [(T.Text, Int)]
findVariableAssignPairs textLines = do
    (line1, line2) <- allPairs textLines
    let variableAssignPair = (variableName line2, variableValue line1)
    return variableAssignPair
        where
              allPairs :: [T.Text] -> [(T.Text, T.Text)]
              allPairs [] = []
              allPairs items = (Prelude.head items, Prelude.head $ Prelude.tail items) : allPairs (Prelude.drop 2 items)

              lastWord textLine = Prelude.last (T.words textLine)
              variableName textLine = removeSingleQuoteChars $ lastWord textLine
              variableValue textLine = read $ T.unpack $ lastWord textLine


removeSingleQuoteChars :: T.Text -> T.Text
removeSingleQuoteChars = T.filter (/= constSingleQuoteCharacter)

constSingleQuoteCharacter :: Char
constSingleQuoteCharacter = '\''

constWriteVar :: T.Text
constWriteVar = "WRITE_VAR"

constAdd :: T.Text
constAdd = "ADD"

constSubtract :: T.Text
constSubtract = "SUBTRACT"

constMultiply :: T.Text
constMultiply = "MULTIPLY"

constDivide :: T.Text
constDivide = "DIVIDE"

constReturnValue :: T.Text
constReturnValue = "RETURN_VALUE"