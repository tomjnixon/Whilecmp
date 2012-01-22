{-# LANGUAGE DeriveDataTypeable #-}
module BackendX86 (compile) where

import Data.Generics
import Control.Monad.State
import Data.List
import qualified Data.Map as M

import AbsMac

data CompilerState = CompilerState
                       { code :: [String]
                       , next_lbl :: Int
                       , stack_offset :: Int
                       , addr_map :: AddrMap
                       }
type Gen = Control.Monad.State.State CompilerState

type Label = String

-- Stuff for generating an address map.
type AddrMap = M.Map Variable Int

is_variable :: Variable -> Bool
is_variable variable = True

get_all_vars = nub . listify is_variable

allocate_vars :: Code -> Gen ()
allocate_vars code = do
	state <- get
	let vars = get_all_vars code
	let addresses =
		M.fromList $ zip (get_all_vars code) [4,8..]
	put $ state { stack_offset = 4 * (length vars)
	            , addr_map = addresses
	            }

-- Low-level code generation functions.
gen_label :: Gen (Label)
gen_label = do
	state <- get
	put $ state {next_lbl = (next_lbl state) + 1}
	return $ "lbl_" ++ (show $ next_lbl state)

put_line :: String -> Gen ()
put_line line = modify $ \state ->
	state {code = code state ++ [line]}

put_code line = put_line $ "    " ++ line
put_label lbl = put_line $ lbl ++ ":"

get_addr :: Variable -> Gen (Int)
get_addr var = gets $ (M.! var) . addr_map

get_code :: Gen (String)
get_code = gets $ unlines . code

compile' :: Code -> Gen String
compile' code = do
	allocate_vars code
	gen_preamble
	gen_list_int code
	gen_postscript
	get_code

compile :: Code -> String
compile code =
	evalState (compile' code) (CompilerState [] 0 0 M.empty)

-- Code generation!

gen_code' :: Instruction -> Gen ()
gen_code' ins = do
	put_code $ "; " ++ (show ins)
	gen_code ins

gen_code_int = gen_code
gen_list_int = mapM_ gen_code_int

gen_preamble :: Gen ()
gen_preamble = do
	state <- get
	put_code "section .data"
	
	forM_ (M.toList $ addr_map state) $ \(Variable name, _) -> do
		put_label $ "msg_" ++ name
		put_code $ "db \"" ++ name ++ " = %d\", 10, 0"
	
	put_code "section .text"
	put_code "global main"
	put_code "extern printf"
	put_label "main"
	put_code "push ebp"
	put_code "mov ebp, esp"
	put_code $ "sub esp, " ++ (show $ stack_offset state)

gen_postscript :: Gen ()
gen_postscript = do
	state <- get
	forM_ (M.toList $ addr_map state) $ \(Variable name, _) -> do
		gen_code_int $ Fetch $ Variable name
		put_code $ "push msg_" ++ name
		put_code "call printf"
	
	put_code "leave"
	put_code "ret"

gen_code :: Instruction -> Gen ()
gen_code (Fetch var) = do
	addr <- get_addr var
	put_code $ "mov eax, [ebp - " ++ (show addr) ++ "]"
	put_code "push eax"

gen_code (Push num) = do
	put_code $ "mov eax, " ++ (show num)
	put_code "push eax"

gen_code Add = do
	put_code "pop eax"
	put_code "pop ebx"
	put_code "add eax, ebx"
	put_code "push eax"

gen_code Mult = do
	put_code "pop eax"
	put_code "pop ebx"
	put_code "mul ebx"
	put_code "push eax"

gen_code Sub = do
	put_code "pop eax"
	put_code "pop ebx"
	put_code "sub eax, ebx"
	put_code "push eax"

gen_code ITrue = do
	put_code "mov eax, 1"
	put_code "push eax"

gen_code IFalse = do
	put_code "mov eax, 0"
	put_code "push eax"

gen_code Eq = do
	false <- gen_label
	end <- gen_label
	put_code "pop eax"
	put_code "pop ebx"
	put_code "cmp eax, ebx"
	put_code $ "jne " ++ false
	gen_code_int ITrue
	put_code $ "jmp " ++ end
	put_label false
	gen_code_int IFalse
	put_label end

gen_code Le = do
	false <- gen_label
	end <- gen_label
	put_code "pop eax"
	put_code "pop ebx"
	put_code "cmp eax, ebx"
	put_code $ "jg " ++ false
	gen_code_int ITrue
	put_code $ "jmp " ++ end
	put_label false
	gen_code_int IFalse
	put_label end

gen_code And = do
	put_code "pop eax"
	put_code "pop ebx"
	put_code "and eax, ebx"
	put_code "push ebx"

gen_code Neg = do
	put_code "pop eax"
	put_code "xor eax, 1"
	put_code "push eax"

gen_code (Store var) = do
	addr <- get_addr var
	put_code "pop eax"
	put_code $ "mov [ebp - " ++ (show addr) ++ "], eax"

gen_code Noop = do
	put_code "nop"

gen_code (Branch t f) = do
	false <- gen_label
	exit <- gen_label
	put_code "pop eax"
	put_code "cmp eax, 0"
	put_code $  "jeq " ++ false
	gen_list_int t
	put_code $ "jmp " ++ exit
	put_label false
	gen_list_int f
	put_label exit

gen_code (Loop c b) = do
	start <- gen_label
	exit <- gen_label
	put_label start
	gen_list_int c
	put_code "pop eax"
	put_code "cmp eax, 0"
	put_code $ "je " ++ exit
	gen_list_int b
	put_code $ "jmp " ++ start
	put_label exit
