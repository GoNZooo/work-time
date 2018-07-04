{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           WorkTime
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

main :: IO ()
main = do
  args     <- getArgs
  progName <- getProgName
  case args of
    [filename] -> do
      parseResults <- hoursFromFile filename
      case parseResults of
        Right x   -> printResults x
        Left  err -> print err
    _ -> putStrLn $ unwords ["Usage: ", progName, "<filename>"]

printResults :: Show b => [(Text, b)] -> IO ()
printResults = mapM_ printResult
 where
  printResult = TIO.putStrLn . applyTo [name, workHours]
  name        = T.justifyLeft 24 ' ' . fst
  workHours   = T.justifyRight 6 ' ' . T.pack . show . snd

applyTo :: Monoid b => [a -> b] -> a -> b
applyTo fs = mconcat . (<*>) fs . pure
