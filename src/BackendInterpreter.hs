{-# LANGUAGE DeriveDataTypeable #-}
module BackendInterpreter where
import Control.Monad.State hiding (State)
import Data.Generics

import qualified Data.Map as M
import AbsMac

data StackVar = BoolVar Bool
              | IntVar  Var
  deriving (Data, Typeable, Read, Show, Eq)
type Stack = [StackVar]

type VarState = M.Map Variable Var

data StoppedReason = Finished
                   | Error String
  deriving (Data, Typeable, Read, Show, Eq)

data State = State Code Stack VarState
           | Stopped StoppedReason State
  deriving (Data, Typeable, Read, Show, Eq)

newState code = State code [] M.empty

type IOState = StateT State IO

step :: IOState ()
step = do
	state <- get
	case state of
		State ((Push n):c) e s ->
			put $ State c ((IntVar n):e) s
		State (Add:c) ((IntVar z1):(IntVar z2):e) s ->
			put $ State c ((IntVar $ z1 + z2):e) s
		State (Mult:c) ((IntVar z1):(IntVar z2):e) s ->
			put $ State c ((IntVar $ z1 * z2):e) s
		State (Sub:c) ((IntVar z1):(IntVar z2):e) s ->
			put $ State c ((IntVar $ z1 - z2):e) s
		State (ITrue:c) e s ->
			put $ State c ((BoolVar True):e) s
		State (IFalse:c) e s ->
			put $ State c ((BoolVar False):e) s
		State (Eq:c) ((IntVar z1):(IntVar z2):e) s ->
			put $ State c ((BoolVar $ z1 == z2):e) s
		State (Le:c) ((IntVar z1):(IntVar z2):e) s ->
			put $ State c ((BoolVar $ z1 <= z2):e) s
		State (And:c) ((BoolVar z1):(BoolVar z2):e) s ->
			put $ State c ((BoolVar $ z1 && z2):e) s
		State (Neg:c) ((BoolVar z):e) s ->
			put $ State c ((BoolVar $ not z):e) s
		State (Fetch x:c) e s ->
			case M.lookup x s of
				Just x' ->
					put $ State c ((IntVar x'):e) s
				Nothing ->
					put $ Stopped (Error $ "Undefined reference to " ++ name x) state
		State (Store x:c) (IntVar z:e) s ->
			put $ State c e (M.insert x z s)
		State (Noop:c) e s ->
			put $ State c e s
		State (Branch c1 c2:c) (BoolVar t:e) s ->
			put $ State ((if t then c1 else c2) ++ c) e s
		State (Loop c1 c2:c) e s ->
			put $ State (c1 ++ (Branch (c2 ++ [Loop c1 c2]) [Noop]:c)) e s
		State [] _ _ ->
			put $ Stopped Finished state
		s ->
			put $ Stopped (Error "Illegal instruction.") state

finished (State _ _ _) = False
finished (Stopped _ _) = True

run :: IOState ()
run = do
	state <- get
	if finished state
		then
			return ()
		else do
			step
			run

interpretCode program = 
	execStateT run $ newState program
