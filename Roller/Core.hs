module Roller.Core (
  module Roller.Parse
, module Roller.Types
, module Options.Applicative
, main
) where

import Roller.Types
import Roller.Parse

import System.Environment (getArgs)
import System.Random (randomRIO)
import Control.Applicative hiding (Const)
import Control.Monad
import Options.Applicative hiding (Const)

rolls :: Int -> Int -> IO [Int]
rolls n s = replicateM n . randomRIO $ (1,s)

roll :: DiceExp -> IO [[Int]]
roll de =
  case de of
    Sum e1 e2 -> (++) <$> roll e1 <*> roll e2
    Const n   -> return [[n]]
    Die n s   -> return <$> n `rolls` s

type CLI a = Bool -> Int -> [String] -> a

rollEm :: CLI (IO ())
rollEm verbose n args = maybe parseFail rollMany (parse input)
  where
    input        = concat args
    rollMany     = replicateM_ n . rollOnce
    rollOnce exp = fmap summary (roll exp) >>= putStrLn

    summary      = if verbose then show else show . sumRolls
    sumRolls     = sum . map sum
    parseFail    = putStrLn $ "Could not parse \"" ++ input ++ "\" as dice expression!"

main :: IO ()
main = join . withOpts $ rollEm

withOpts :: CLI a -> IO a
withOpts f = execParser . info (helper <*> handleOpts) $ infoMod
  where
  handleOpts =
    f <$> switch ( long "verbose"
                <> short 'v'
                <> help "List out each roll")
      <*> option ( long "nrolls"
                <> value 1
                <> short 'n'
                <> help "Number of times to roll the expression")
      <*> arguments1 str (metavar "EXPR.." <> help "Dice expressions")
  infoMod = (fullDesc <> progDesc "Roll some dice")

