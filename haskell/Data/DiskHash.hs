{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Data.DiskHash
    ( DiskHashRO
    , DiskHashRW
    , htOpenRO
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
import System.IO.Unsafe (unsafeDupablePerformIO)
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr, finalizeForeignPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.C.String (CString)

-- | Disk based hash table
--
-- The Haskell interface has two types, distinguishing between read-only and
-- read-write hash tables. Operations on the RW variant are in the IO monad,
-- while operations on RO tables are all pure (after opening the table,
-- naturally).
--
-- The datastructures are all strict.

type HashTable_t = ForeignPtr ()
newtype DiskHashRO a = DiskHashRO HashTable_t
newtype DiskHashRW a = DiskHashRW HashTable_t

foreign import ccall "dht_open2" c_dht_open2:: CString -> CInt -> CInt -> CInt -> IO (Ptr ())
foreign import ccall "dht_lookup" c_dht_lookup :: Ptr () -> CString -> IO (Ptr ())
foreign import ccall "dht_reserve" c_dht_reserve :: Ptr () -> CInt -> IO ()
foreign import ccall "dht_insert" c_dht_insert :: Ptr () -> CString -> Ptr () -> IO CInt
foreign import ccall "dht_size" c_dht_size :: Ptr () -> IO CSize
foreign import ccall "&dht_free" c_dht_free_p :: FunPtr (Ptr () -> IO ())

-- | open a hash table in read-write mode
htOpenRW :: forall a. (Storable a) => FilePath -> Int -> IO (DiskHashRW a)
htOpenRW fpath maxk = DiskHashRW <$> open' (undefined :: a) fpath maxk 66

-- | open a hash table in read-only mode
htOpenRO :: forall a. (Storable a) => FilePath -> Int -> IO (DiskHashRO a)
htOpenRO fpath maxk = DiskHashRO <$> open' (undefined :: a) fpath maxk 0

open' :: forall a. (Storable a) => a -> FilePath -> Int -> CInt -> IO HashTable_t
open' unused fpath maxk flags = B.useAsCString (B8.pack fpath) $ \fpath' -> do
    ht <- c_dht_open2 fpath' (fromIntegral maxk) (fromIntegral $ sizeOf unused) flags
    newForeignPtr c_dht_free_p ht

-- | Open a hash table in read-write mode and pass it to an action
--
-- Once the action is is complete, the hashtable is closed (and sync'ed to disk).
withDiskHashRW :: (Storable a) => FilePath -> Int -> (DiskHashRW a -> IO b) -> IO b
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
                alloca $ \val' -> do
                    poke val' val
                    r <- c_dht_insert ht' key' (castPtr val')
                    case r of
                        1 -> return True
                        0 -> return False
                        -1 -> throwIO $ userError "insertion failed (probably out of memory)"
                        _ -> throwIO $ userError "Unexpected return from dht_insert"
-- | Lookup by key
htLookupRW :: (Storable a) => B.ByteString -> DiskHashRW a -> IO (Maybe a)
htLookupRW key (DiskHashRW ht) =
    withForeignPtr ht $ \ht' ->
        B.useAsCString key $ \key' -> do
            r <- c_dht_lookup ht' key'
            if r == nullPtr
                then return Nothing
                else Just <$> peek (castPtr r)

-- | Lookup by key
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
-- If the operation fails, an exception is raised
htReserve :: (Storable a) => Int -> DiskHashRW a -> IO Int
htReserve cap (DiskHashRW ht) =
    withForeignPtr ht $ \ht' -> do
        cap' <- fromEnum <$> c_dht_reserve ht' (fromIntegral cap)
        if cap' == 0
            then throwIO $ userError "Could not change capacity"
            else return cap'

