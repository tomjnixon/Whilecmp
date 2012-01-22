{-# LANGUAGE DeriveDataTypeable #-}
module AbsMac (Instruction (..), Code, Var, Variable (..))where
import Data.Generics

import While

-- Abstract machione definition.
data Instruction = Push Var
                 | Add
                 | Mult
                 | Sub
                 | ITrue
                 | IFalse
                 | Eq
                 | Le
                 | And
                 | Neg
                 | Fetch Variable
                 | Store Variable
                 | Noop
                 | Branch Code Code
                 | Loop Code Code
  deriving (Data, Typeable, Read, Show, Eq)

type Code = [Instruction]


