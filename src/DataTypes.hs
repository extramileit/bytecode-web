module DataTypes where

import Data.Text as T
import Data.Map.Ordered as MO

data ByteCode = ByteCode {
    variableMap :: OMap T.Text Int
  , operationCommands :: [OperationCommand]
}

-- show is useful for debugging in GHCI
instance Show ByteCode where
    show byteCode = "Variable Pairs: " ++ dumpMapContents (variableMap byteCode)
                  ++ "\n"
                  ++ "Operation Commands: " ++ show (operationCommands byteCode) where
      dumpMapContents :: OMap T.Text Int -> String
      dumpMapContents = show . assocs

data MathOp = ADD | SUBTRACT | MULTIPLY | DIVIDE | RETURN_VALUE
    deriving (Show, Eq)

data OperationCommand = Binomial MathOp T.Text T.Text
                      | Partial MathOp T.Text
                      | ReturnVal
    deriving Show
