{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-|

Disk based hash table

The Haskell interface has two types, distinguishing between read-only and
read-write hash tables. Operations on the RW variant are in the IO monad, while
operations on RO tables are all pure (after the 'htOpenRO' call, naturally).
Using read-write hashtables with more than one thread is undefined behaviour,
but the read-only variant is perfectly thread safe.

All data structures are strict (naturally: they write to disk).

The Haskell API can be used to access diskhashes created from other languages
as long as the types are compatible.
-}

module Data.DiskHash
    ( DiskHashRO
    , DiskHashRW
    , htOpenRO
    , htLoadRO
    , htOpenRW
    , withDiskHashRW
    , htLookupRO
    , htLookupRW
    , htSizeRW
    , htSizeRO
    , htInsert
    , htModify
    , htReserve
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Control.Exception (throwIO)
import Control.Monad (when)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr, finalizeForeignPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.C.String (CString, peekCString)

type HashTable_t = ForeignPtr ()

-- | Represents a read-only diskhash storing type 'a'
newtype DiskHashRO a = DiskHashRO HashTable_t

-- | Represents a read-write diskhash storing type 'a'
newtype DiskHashRW a = DiskHashRW HashTable_t

foreign import ccall "dht_open2" c_dht_open2:: CString -> CInt -> CInt -> CInt -> Ptr CString -> IO (Ptr ())
foreign import ccall "dht_lookup" c_dht_lookup :: Ptr () -> CString -> IO (Ptr ())
foreign import ccall "dht_reserve" c_dht_reserve :: Ptr () -> CInt -> Ptr CString -> IO ()
foreign import ccall "dht_insert" c_dht_insert :: Ptr () -> CString -> Ptr () -> Ptr CString -> IO CInt
foreign import ccall "dht_size" c_dht_size :: Ptr () -> IO CSize
foreign import ccall "dht_load_to_memory" c_dht_load_to_memory :: Ptr () -> Ptr CString -> IO CInt
foreign import ccall "&dht_free" c_dht_free_p :: FunPtr (Ptr () -> IO ())

-- | Internal function to handle error message interface
--
-- If argument points to NULL, then return "No message"
-- Otherwise, return its contents and release memory
getError :: Ptr CString -> IO String
getError err = do
    err' <- peek err
    if err' == nullPtr
        then return "No message"
        else do
            m <- peekCString err'
            free err'
            return m

-- | open a hash table in read-write mode
htOpenRW :: forall a. (Storable a) => FilePath
                                        -- ^ file path
                                        -> Int
                                        -- ^ maximum key size
                                        -> IO (DiskHashRW a)
htOpenRW fpath maxk = DiskHashRW <$> open' (undefined :: a) fpath maxk 66 False

-- | open a hash table in read-only mode
--
-- The 'maxk' argument can be 0, in which case the value of the maximum key
-- will be taken from the disk file. If not zero, then it is checked against
-- the value on disk and an exception is raised if there is  a mismatch.
htOpenRO :: forall a. (Storable a) => FilePath
                                        -- ^ file path
                                        -> Int
                                        -- ^ maximum key size
                                        -> IO (DiskHashRO a)
htOpenRO fpath maxk = DiskHashRO <$> open' (undefined :: a) fpath maxk 0 False

-- | open a hash table in read-only mode and load it into memory
--
-- The 'maxk' argument can be 0, in which case the value of the maximum key
-- will be taken from the disk file. If not zero, then it is checked against
-- the value on disk and an exception is raised if there is  a mismatch.
--
-- @since 0.0.4.0
htLoadRO :: forall a. (Storable a) => FilePath
                                        -- ^ file path
                                        -> Int
                                        -- ^ maximum key size
                                        -> IO (DiskHashRO a)
htLoadRO fpath maxk = DiskHashRO <$> open' (undefined :: a) fpath maxk 0 True

open' :: forall a. (Storable a) => a -> FilePath -> Int -> CInt -> Bool -> IO HashTable_t
open' unused fpath maxk flags load = B.useAsCString (B8.pack fpath) $ \fpath' ->
    alloca $ \err -> do
        poke err nullPtr
        ht <- c_dht_open2 fpath' (fromIntegral maxk) (fromIntegral $ sizeOf unused) flags err
        if ht == nullPtr
            then do
                errmsg <- getError err
                throwIO $ userError ("Could not open hash table: " ++ show errmsg)
            else do
                when load $ do
                    e <- c_dht_load_to_memory ht err
                    when (e == 2) $ do
                        errmsg <- getError err
                        throwIO $ userError ("Could not load hash table into memory: " ++ show errmsg)
                newForeignPtr c_dht_free_p ht

-- | Open a hash table in read-write mode and pass it to an action
--
-- Once the action is is complete, the hashtable is closed (and sync'ed to disk).
withDiskHashRW :: (Storable a) => FilePath
                                    -- ^ file path
                                    -> Int
                                    -- ^ maximum key size
                                    -> (DiskHashRW a -> IO b) -> IO b
withDiskHashRW fp s act = do
    ht@(DiskHashRW ht') <- htOpenRW fp s
    r <- act ht
    finalizeForeignPtr ht'
    return r


-- | Retrieve the size of the hash table
htSizeRW :: DiskHashRW a -> IO Int
htSizeRW (DiskHashRW ht) = withForeignPtr ht $ \ht' -> fromIntegral <$> (c_dht_size ht')

-- | Retrieve the size of the hash table
htSizeRO :: DiskHashRO a -> Int
htSizeRO (DiskHashRO ht) = unsafeDupablePerformIO (htSizeRW (DiskHashRW ht))


-- | insert an element into the hash table
--
-- Returns whether an insertion took place (if an object with that key already
-- exists, no insertion is made).
--
-- This operation can fail (throwing an exception) if space could not be
-- allocated. You can pre-allocate space using 'htReserve'.
--
htInsert :: (Storable a) => B.ByteString
                            -- ^ key
                            -> a
                            -- ^ value
                            -> DiskHashRW a
                            -- ^ hash table
                            -> IO Bool
                            -- ^ True if inserted, False if not
htInsert key val (DiskHashRW ht) =
        withForeignPtr ht $ \ht' ->
            B.useAsCString key $ \key' ->
                alloca $ \val' ->
                    alloca $ \err -> do
                        poke err nullPtr
                        poke val' val
                        r <- c_dht_insert ht' key' (castPtr val') err
                        case r of
                            1 -> return True
                            0 -> return False
                            -1 -> do
                                errmsg <- getError err
                                throwIO $ userError ("insertion failed ("++errmsg++")")
                            _ -> do
                                errmsg <- getError err
                                throwIO $ userError ("Unexpected return from dht_insert: " ++ errmsg)
-- | Lookup by key
--
-- This is in the IO Monad to ensure ordering of operations.
htLookupRW :: (Storable a) => B.ByteString
                                    -- ^ key
                                    -> DiskHashRW a
                                    -> IO (Maybe a)
htLookupRW key (DiskHashRW ht) =
    withForeignPtr ht $ \ht' ->
        B.useAsCString key $ \key' -> do
            r <- c_dht_lookup ht' key'
            if r == nullPtr
                then return Nothing
                else Just <$> peek (castPtr r)

-- | Lookup by key
--
-- This is a pure operation
htLookupRO :: (Storable a) => B.ByteString -> DiskHashRO a -> Maybe a
htLookupRO key (DiskHashRO ht) = unsafeDupablePerformIO (htLookupRW key (DiskHashRW ht))

-- | Modify a value
htModify :: (Storable a) => B.ByteString -> (a -> a) -> DiskHashRW a -> IO Bool
htModify key f (DiskHashRW ht) =
    withForeignPtr ht $ \ht' ->
        B.useAsCString key $ \key' -> do
            r <- castPtr <$> c_dht_lookup ht' key'
            if r == nullPtr
                then return False
                else do
                    val <- peek r
                    poke r (f val)
                    return True

-- | Reserve space in the hash table
--
-- Reserving space can ensure that any subsequent 'htInsert' calls will not fail.
--
-- If the operation fails, an exception is raised
htReserve :: (Storable a) => Int -> DiskHashRW a -> IO Int
htReserve cap (DiskHashRW ht) =
    withForeignPtr ht $ \ht' ->
        alloca $ \err -> do
            poke err nullPtr
            cap' <- fromEnum <$> c_dht_reserve ht' (fromIntegral cap) err
            if cap' == 0
                then do
                    errmsg <- getError err
                    throwIO . userError $ "Could not change capacity: " ++ errmsg
                else return cap'

