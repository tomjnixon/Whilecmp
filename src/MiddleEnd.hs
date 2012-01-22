module MiddleEnd where
import qualified AbsMac as A

import While

ca (Constant x) = [A.Push x]
ca (AVariable x) = [A.Fetch x]
ca (Add a1 a2) = ca a2 ++ ca a1 ++ [A.Add]
ca (Mul a1 a2) = ca a2 ++ ca a1 ++ [A.Mult]
ca (Sub a1 a2) = ca a2 ++ ca a1 ++ [A.Sub]

cb BTrue = [A.ITrue]
cb BFalse = [A.IFalse]
cb (Equal a1 a2) = ca a2 ++ ca a1 ++ [A.Eq]
cb (LTE a1 a2) = ca a2 ++ ca a1 ++ [A.Le]
cb (And b1 b2) = cb b2 ++ cb b1 ++ [A.And]
cb (Not b) = cb b ++ [A.Neg]

cs (Assignment x a) = ca a ++ [A.Store x]
cs Skip = [A.Noop]
cs (Sequence s1 s2) = cs s1 ++ cs s2
cs (If b s1 s2) = cb b ++ [A.Branch (cs s1) (cs s2)]
cs (While b s) = [A.Loop (cb b) (cs s)]
cs (Printf s a) =
	(concat $ map ca $ reverse a) ++ [A.Printf s $ length a]
