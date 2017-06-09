{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Main where

import           Test.Framework.TH
import           Test.HUnit
import           Test.Framework.Providers.HUnit

import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (throwIO)
import           System.IO.Error (isDoesNotExistError, catchIOError)
import           System.Directory (doesFileExist, removeFile)
import           System.IO (hPutStrLn)
import           Data.Int

import Data.DiskHash

main :: IO ()
main = do
    removeFileIfExists outname
    $(defaultMainGenerator)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fp = removeFile fp `catchIOError` ignoreDoesNotExistError
    where
        ignoreDoesNotExistError e
                | isDoesNotExistError e = return ()
                | otherwise = throwIO e

outname :: FilePath
outname = "testing.dht"

case_smoke = do
    ht <- htOpenRW outname 15
    htInsert "key" (9 :: Int64) ht
    val <- htLookupRW "key" ht
    assertEqual "Lookup" (Just 9) val
    removeFileIfExists outname
