module Main where

import Agda.Syntax.Concrete
import Agda.Syntax.Parser
import Agda.Syntax.Position
import Agda.Utils.FileName
import Control.Monad.Except
import Control.Monad.State (StateT (runStateT))
import Node
import Options.Applicative qualified as O
import Printer

data Options = Options
    { file :: FilePath
    , inPlace :: Bool
    , indent :: Int
    }

options :: O.Parser Options
options =
    Options
        <$> O.strArgument
            ( O.metavar "<file>"
                <> O.help "The file to format"
            )
        <*> O.switch
            ( O.long "inplace"
                <> O.short 'i'
                <> O.help "Format the file in place"
            )
        <*> O.option
            O.auto
            ( O.long "indent"
                <> O.help "Indentation width in spaces"
                <> O.value 4
                <> O.showDefault
                <> O.metavar "N"
            )

main :: IO ()
main = do
    opts <- O.execParser $ O.info (options O.<**> O.helper) O.fullDesc
    let path = mkAbsolute (file opts)
    let file = mkRangeFile path Nothing
    contents <- readFile $ filePath path
    (program, _) <-
        ( runStateT $
                runExceptT $
                    unPM $
                        parseFile moduleParser file contents
            )
            []
    case program of
        Left err -> print err
        Right ((modul, _), _) ->
            putStr $ runPrinter (node $ head decls)
          where
            decls = modDecls modul