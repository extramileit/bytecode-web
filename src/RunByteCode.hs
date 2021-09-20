module RunByteCode
  (
      runByteCode
  ) where

import Data.Text as T
import Data.Map.Ordered as MO
import Text.Read (readMaybe)
import Data.Maybe (isNothing, fromJust)
import DataTypes
  
runByteCode :: ByteCode -> T.Text
runByteCode bc = makeFunction bc
                 <> "\n"
                 <> "f() = "
                 <> T.pack (show (evaluateFunction bc))
                 <> "\n"
                   

makeFunction :: ByteCode -> T.Text
makeFunction bc = "function f() {\n"
                      <> makeVariableAssignments (MO.assocs (variableMap bc))
                      <> "\n"
                      <> "\t" <> makeReturnExpression bc
                      <> "\n}"

makeVariableAssignments :: [(T.Text, Int)] -> T.Text
makeVariableAssignments varPairs = T.intercalate "\n" (Prelude.map makeVariableAssignment varPairs)

makeVariableAssignment :: (T.Text, Int) -> T.Text
makeVariableAssignment (varName, varValue) = "\t" <> varName <> " = " <> T.pack (show varValue)

makeReturnExpression :: ByteCode -> T.Text
makeReturnExpression bc = Prelude.foldl applyCommand "" (operationCommands bc)
    where  
          appendExpressionFragment :: T.Text -> OperationCommand -> T.Text
          appendExpressionFragment accValue operationCmd =
              if T.null accValue then fragment
              else accValue <> " " <> fragment
                 where fragment = expressionFragment operationCmd

          isParensWrapperNeeded :: T.Text -> OperationCommand -> Bool
          isParensWrapperNeeded accValue nextCommand = isTrailingAddOrSubtract accValue && isMultiplyOrDivideCommand nextCommand
              where 
                lastOperator input = Prelude.last ([x | x <- input, x == '*' || x == '/' || x == '+' || x == '-'])
                isTrailingAddOrSubtract :: T.Text -> Bool
                isTrailingAddOrSubtract input = not (T.null input)
                            && ('+' == lastOperator (T.unpack input) || '-' == lastOperator (T.unpack input))
          
          displayMathOpSymbol :: MathOp -> T.Text
          displayMathOpSymbol ADD = "+"
          displayMathOpSymbol SUBTRACT = "-"
          displayMathOpSymbol MULTIPLY = "*"
          displayMathOpSymbol DIVIDE = "/"
          displayMathOpSymbol _ = ""
          
          expressionFragment :: OperationCommand -> T.Text
          expressionFragment (Binomial mathOp arg1 arg2) = arg1 <> constSpace <> displayMathOpSymbol mathOp <> constSpace <> arg2
          expressionFragment (Partial mathOp arg1) = displayMathOpSymbol mathOp <> constSpace <> arg1
          expressionFragment ReturnVal = "return"
             
          applyCommand accValue operationCmd
            | isReturnValueCommand operationCmd = expressionFragment operationCmd <> " " <> accValue
            | isParensWrapperNeeded accValue operationCmd = "(" <> accValue <> ")" <> " " <> expressionFragment operationCmd
            | otherwise = appendExpressionFragment accValue operationCmd

evaluateFunction :: ByteCode -> Float
evaluateFunction bc = Prelude.foldl applyCommand 0.0 opCommands
    where 
          opCommands = operationCommands bc
          varMap = variableMap bc
          
          convertToFloat :: Int -> Float
          convertToFloat intVal = Prelude.fromIntegral intVal :: Float
          
          resolveArgument :: T.Text -> Float 
          resolveArgument arg = convertToFloat getArgNumericValue
              where maybeNumber = readMaybe (T.unpack arg) :: Maybe Float
                    getArgNumericValue = if isNothing maybeNumber then fromJust $ MO.lookup arg varMap
                                         else Prelude.read $ T.unpack arg  
          
          evaluate :: Float -> Float -> MathOp -> Float
          evaluate num1 num2 mathOp = case mathOp of ADD -> num1 + num2
                                                     SUBTRACT -> num1 - num2
                                                     DIVIDE -> num1 / num2
                                                     MULTIPLY -> num1 * num2
                                                     _ -> error "mathOp not applicable to evaluate method" 

          applyCommand :: Float -> OperationCommand -> Float         
          applyCommand _ (Binomial mathOp arg1 arg2) = evaluate (resolveArgument arg1) (resolveArgument arg2) mathOp
          applyCommand accValue (Partial mathOp arg1) = evaluate accValue (resolveArgument arg1) mathOp
          applyCommand accValue ReturnVal = accValue


checkMultiplyOrDivide :: MathOp -> Bool
checkMultiplyOrDivide mathOp = MULTIPLY == mathOp || DIVIDE == mathOp

isMultiplyOrDivideCommand :: OperationCommand -> Bool
isMultiplyOrDivideCommand (Binomial mathOp _ _) = checkMultiplyOrDivide mathOp
isMultiplyOrDivideCommand (Partial mathOp _) = checkMultiplyOrDivide mathOp
isMultiplyOrDivideCommand _ = False

isReturnValueCommand ::OperationCommand -> Bool
isReturnValueCommand ReturnVal = True
isReturnValueCommand _ = False

constSpace :: T.Text
constSpace = " "
