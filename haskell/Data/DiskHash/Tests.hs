{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Main where

import           Test.Framework.TH
import           Test.HUnit (assertEqual, assertBool)
import           Test.QuickCheck (ASCIIString(..))
import           Test.QuickCheck.Property (Property, ioProperty)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import           Control.Arrow (first)
import           Control.Monad (forM, forM_)
import           Control.Exception (throwIO)
import           System.IO.Error (isDoesNotExistError, catchIOError)
import           System.Directory (removeFile)
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

case_open_close = do
    withDiskHashRW outname 15 $ \ht -> do
        s <- htSizeRW ht
        assertEqual "new table has size 0" s 0
        inserted <- htInsert "key" (9 :: Int64) ht
        assertBool "inserted should have return True" inserted
    ht <- htOpenRO outname 15
    assertEqual "read-only table after reopen" (htSizeRO ht) 1
    assertEqual "Lookup" (Just (9 :: Int64)) (htLookupRO "key" ht)
    removeFileIfExists outname

case_open_close_load = do
    withDiskHashRW outname 15 $ \ht -> do
        s <- htSizeRW ht
        assertEqual "new table has size 0" s 0
        inserted <- htInsert "key" (9 :: Int64) ht
        assertBool "inserted should have return True" inserted
    ht <- htLoadRO outname 15
    assertEqual "read-only table after reopen (load)" (htSizeRO ht) 1
    assertEqual "Lookup" (Just (9 :: Int64)) (htLookupRO "key" ht)
    removeFileIfExists outname

prop_insert_find :: [(ASCIIString, Int64)] -> Property
prop_insert_find args = ioProperty $ do
    let args' = normArgs args
    found <- withDiskHashRW outname 15 $ \ht -> do
        forM_ args' $ \(k,val) -> htInsert k val ht
        forM args' $ \(k, val) -> do
            v <- htLookupRW k ht
            return $! v == Just val
    removeFileIfExists outname
    return $! and found


normArgs :: [(ASCIIString, Int64)] -> [(B.ByteString, Int64)]
normArgs = normArgs' [] . map (first normKey)
    where
        normKey = B8.pack . (filter (/= '\0')) . getASCIIString
        normArgs' r [] = r
        normArgs' r (x@(k,_):xs)
            | k `elem` (map fst r) = normArgs' r xs
            | B.length k >= 15 = normArgs' r xs
            | otherwise = normArgs' (x:r) xs
