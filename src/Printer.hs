{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Printer where

import Control.Monad.State
import Node

newtype Printer a = Printer (State PrinterState a)
    deriving (Functor, Applicative, Monad, MonadState PrinterState)

data PrinterState = PrinterState
    { buffer :: String
    , indent :: Int
    , maxWidth :: Int
    }

runPrinter :: Node -> String
runPrinter n = buffer $ execState p initialState
  where
    initialState =
        PrinterState
            { buffer = ""
            , indent = 0
            , maxWidth = 80
            }
    (Printer p) = do
        printNode n

printNode :: Node -> Printer ()
printNode (Text t) = modify (\s -> s{buffer = buffer s ++ t})
printNode SpaceOrLine = modify (\s -> s{buffer = buffer s ++ " "})
printNode Line = do
    modify (\s -> s{buffer = buffer s ++ "\n"})
    modify (\s -> s{buffer = buffer s ++ replicate (indent s) ' '})
printNode (Indent nodes) = do
    modify (\s -> s{indent = indent s + 4})
    modify (\s -> s{buffer = buffer s ++ replicate (indent s) ' '})
    printNode (Nodes nodes)
    modify (\s -> s{indent = indent s - 4})
printNode (Nodes nodes) = mapM_ printNode nodes