{-# LANGUAGE ForeignFunctionInterface #-}
module Printf (printfHack) where

-- This module is an evil hack to get printf in haskell.
-- Text.Printf doesn't work, because variable numbers of arguments (at run
-- time) aren't possible (I think).

import Foreign
import Foreign.C
import Foreign.LibFFI

printfHack :: String -> [Int] -> IO Int
printfHack fmt args = do
	ret <- callFFI printf retCInt $
		argString fmt : map (argCInt . fromIntegral) args
	return $ fromIntegral ret


foreign import ccall "&printf"
	printf :: FunPtr (CString -> IO CInt)
