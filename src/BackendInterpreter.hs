{-# LANGUAGE DeriveDataTypeable #-}
module BackendInterpreter where
import Data.Generics

import qualified Data.Map as M
import AbsMac

data StackVar = BoolVar Bool
              | IntVar  Var
  deriving (Data, Typeable, Read, Show, Eq)
type Stack = [StackVar]

type VarState = M.Map Variable Var

data State = State Code Stack VarState
           | Stopped State
  deriving (Data, Typeable, Read, Show, Eq)

newState code = State code [] M.empty

step (State ((Push n):c) e s) =
	State c ((IntVar n):e) s
step (State (Add:c) ((IntVar z1):(IntVar z2):e) s) =
	State c ((IntVar $ z1 + z2):e) s
step (State (Mult:c) ((IntVar z1):(IntVar z2):e) s) =
	State c ((IntVar $ z1 * z2):e) s
step (State (Sub:c) ((IntVar z1):(IntVar z2):e) s) =
	State c ((IntVar $ z1 - z2):e) s
step (State (ITrue:c) e s) =
	State c ((BoolVar True):e) s
step (State (IFalse:c) e s) =
	State c ((BoolVar False):e) s
step (State (Eq:c) ((IntVar z1):(IntVar z2):e) s) =
	State c ((BoolVar $ z1 == z2):e) s
step (State (Le:c) ((IntVar z1):(IntVar z2):e) s) =
	State c ((BoolVar $ z1 <= z2):e) s
step (State (And:c) ((BoolVar z1):(BoolVar z2):e) s) =
	State c ((BoolVar $ z1 && z2):e) s
step (State (Neg:c) ((BoolVar z):e) s) =
	State c ((BoolVar $ not z):e) s
step (State (Fetch x:c) e s) =
	case M.lookup x s of
		Just x' ->
			State c ((IntVar x'):e) s
		Nothing ->
			error $ "Undefined reference to " ++ name x
step (State (Store x:c) (IntVar z:e) s) =
	State c e (M.insert x z s)
step (State (Noop:c) e s) =
	State c e s
step (State (Branch c1 c2:c) (BoolVar t:e) s) =
	State ((if t then c1 else c2) ++ c) e s
step (State (Loop c1 c2:c) e s) =
	State (c1 ++ (Branch (c2 ++ [Loop c1 c2]) [Noop]:c)) e s
step s = Stopped s

finished (State _ _ _) = False
finished (Stopped _) = True

run = until finished step

show_state (Stopped (State _ _ s)) = mapM_ (putStrLn . showVar) $ M.toList s
	where
		showVar (Variable name, val) = name ++ " = " ++ show val
