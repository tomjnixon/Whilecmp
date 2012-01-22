{-# LANGUAGE DeriveDataTypeable #-}
module While where
import Data.Generics hiding (Infix, Prefix)
import Data.Word
import Debug.Trace

type Var = Integer

data Variable = Variable { name :: String }
  deriving (Ord, Eq, Data, Typeable, Read, Show)

data Arithmetic = Add Arithmetic Arithmetic
                | Sub Arithmetic Arithmetic
                | Mul Arithmetic Arithmetic
                | AVariable Variable
                | Constant Var
  deriving (Data, Typeable, Read, Show)

data Boolean = BTrue
             | BFalse
             | Equal Arithmetic Arithmetic
             | LTE Arithmetic Arithmetic
             | And Boolean Boolean
             | Not Boolean
  deriving (Data, Typeable, Read, Show)

data Statement = Assignment Variable Arithmetic
               | Skip
               | Sequence Statement Statement
               | If Boolean Statement Statement
               | While Boolean Statement
               | Printf String [Arithmetic]
  deriving (Data, Typeable, Read, Show)
