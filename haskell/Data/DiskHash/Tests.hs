{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Main where

import           Test.Framework.TH
import           Test.HUnit
import           Test.Framework.Providers.HUnit

import           Control.Exception (throwIO)
import           System.IO.Error (isDoesNotExistError, catchIOError)
import           System.Directory (doesFileExist, removeFile)
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
    s <- htSizeRW ht
    assertEqual "new table has size 0" s 0
    inserted <- htInsert "key" (9 :: Int64) ht
    assertBool "inserted should have return True" inserted
    reInserted <- htInsert "key" (9 :: Int64) ht
    assertBool "inserted should have return False (2nd time around)" (not reInserted)
    s' <- htSizeRW ht
    assertEqual "after insert table has size 1" s' 1
    val <- htLookupRW "key" ht
    assertEqual "Lookup" (Just 9) val
    removeFileIfExists outname
