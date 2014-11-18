module Main where

import Control.Applicative ( Applicative(..), (<**>), (<|>) )
import Data.Functor ( (<\$>) )
import Data.Monoid ( (<>), mempty )
import Options.Applicative
    (   CommandFields
    ,   Mod
    ,   Parser
    ,   command
    ,   customExecParser
    ,   execParser
    ,   flag'
    ,   help
    ,   helper
    ,   info
    ,   long
    ,   prefs
    ,   progDesc
    ,   short
    ,   showHelpOnError
    ,   subparser
    )
import System.Environment ( getArgs )

versionString :: String
versionString = "$executable_name;format="camel"$: $version_string$"

data Command = Version | ExampleCommand deriving (Show, Eq)

foldCommand :: a -> a -> Command -> a
foldCommand v e c = case c of
    Version         -> v
    ExampleCommand  -> e

commandParser :: Parser Command
commandParser =
        flag' Version (short 'v' <> long "version" <> help "version info.")
    <|> (subparser \$
            command' "example" "Example Description" (pure ExampleCommand)
    )

command' :: String -> String -> Parser a -> Mod CommandFields a
command' name description parser = command name (info (parser <**> helper) (progDesc description))

parseAndRun :: Parser a -> (a -> IO b) -> IO b
parseAndRun p f = do
    x <- getArgs
    case x of
        -- If there were no commands, and the flags (the only valids ones in this case should be --version/-v and --help)
        -- are not recognised, then show the help msg.
        []  -> customExecParser (prefs showHelpOnError) (info (p <**> helper) mempty) >>= f
        _   -> execParser (info (p <**> helper) mempty) >>= f

runCommand :: Command -> IO ()
runCommand = foldCommand printVersion (pure ())

printVersion :: IO ()
printVersion = putStrLn versionString

main :: IO ()
main = parseAndRun commandParser runCommand
