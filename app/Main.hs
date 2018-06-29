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

printResults :: [(Text, Double)] -> IO ()
printResults = mapM_ printResult
 where
  printResult r =
    TIO.putStrLn $ T.intercalate "\t" [fst r, T.pack . show $ snd r]
