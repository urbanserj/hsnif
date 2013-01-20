{-# LANGUAGE
  ForeignFunctionInterface, GeneralizedNewtypeDeriving, FlexibleInstances,
  UndecidableInstances, OverlappingInstances, TemplateHaskell #-}

module Foreign.Erlang.Nif where

import Data.Word
import Data.Int
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

#include <erl_nif.h>
#include <limits.h>

newtype ErlNifTerm = ErlNifTerm #{type ERL_NIF_TERM}
    deriving (Storable)
newtype ErlNifEnv = ErlNifEnv (Ptr ())
newtype ErlNifPid = ErlNifPid (Ptr ())
newtype ErlNifBinary = ErlNifBinary (Ptr ())
newtype ErlAtom = ErlAtom String deriving (Eq, Show)
newtype ErlBinary a = ErlBinary a
newtype ErlTuple a = ErlTuple [a] deriving (Eq, Show)

type ErlNifCharEncoding = #{type int}
#{enum ErlNifCharEncoding, , erl_nif_latin1=ERL_NIF_LATIN1}

data ErlNifSysInfo = ErlNifSysInfo {
  driver_major_version :: Int,
  driver_minor_version :: Int,
  erts_version :: String,
  otp_release :: String,
  thread_support :: Bool,
  smp_support :: Bool,
  async_threads :: Int,
  scheduler_threads :: Int,
  nif_major_version :: Int,
  nif_minor_version :: Int
} deriving (Show)


foreign import ccall unsafe enif_priv_data :: ErlNifEnv -> IO (Ptr ())
foreign import ccall unsafe enif_alloc :: #{type size_t} -> IO (Ptr ())
foreign import ccall unsafe enif_free :: Ptr () -> IO ()
foreign import ccall unsafe enif_is_atom :: ErlNifEnv -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_is_binary :: ErlNifEnv -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_is_ref :: ErlNifEnv -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_inspect_binary :: ErlNifEnv -> ErlNifTerm -> ErlNifBinary -> IO #{type int}
foreign import ccall unsafe enif_alloc_binary :: #{type size_t} -> ErlNifBinary -> IO #{type int}
foreign import ccall unsafe enif_realloc_binary :: ErlNifBinary -> #{type size_t} -> IO #{type int}
foreign import ccall unsafe enif_release_binary :: ErlNifBinary -> IO ()
foreign import ccall unsafe enif_get_int :: ErlNifEnv -> ErlNifTerm -> Ptr (#type int) -> IO #{type int}
foreign import ccall unsafe enif_get_ulong :: ErlNifEnv -> ErlNifTerm -> Ptr (#type unsigned long) -> IO #{type int}
foreign import ccall unsafe enif_get_double :: ErlNifEnv -> ErlNifTerm -> Ptr (#type double) -> IO #{type int}
foreign import ccall unsafe enif_get_list_cell :: ErlNifEnv -> ErlNifTerm -> Ptr (ErlNifTerm) -> Ptr (ErlNifTerm) -> IO #{type int}
foreign import ccall unsafe enif_get_tuple :: ErlNifEnv -> ErlNifTerm -> Ptr (#type int) -> Ptr (Ptr ErlNifTerm) -> IO #{type int}
foreign import ccall unsafe enif_is_identical :: ErlNifTerm -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_compare :: ErlNifTerm -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_make_binary :: ErlNifEnv -> ErlNifBinary -> IO ErlNifTerm
foreign import ccall unsafe enif_make_badarg :: ErlNifEnv -> IO ErlNifTerm
foreign import ccall unsafe enif_make_int :: ErlNifEnv -> #{type int} -> IO ErlNifTerm
foreign import ccall unsafe enif_make_ulong :: ErlNifEnv -> #{type unsigned long} -> IO ErlNifTerm
foreign import ccall unsafe enif_make_double :: ErlNifEnv -> #{type double} -> IO ErlNifTerm
foreign import ccall unsafe enif_make_atom :: ErlNifEnv -> CString -> IO ErlNifTerm
foreign import ccall unsafe enif_make_existing_atom :: ErlNifEnv -> CString -> Ptr ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_make_list_cell :: ErlNifEnv -> ErlNifTerm -> ErlNifTerm -> IO ErlNifTerm
foreign import ccall unsafe enif_make_string :: ErlNifEnv -> CString -> ErlNifCharEncoding -> IO ErlNifTerm
foreign import ccall unsafe enif_make_ref :: ErlNifEnv -> IO ErlNifTerm
foreign import ccall unsafe enif_realloc :: Ptr () -> #{type size_t} -> IO (Ptr ())
foreign import ccall unsafe enif_system_info :: Ptr ErlNifSysInfo -> #{type size_t} -> IO ()
foreign import ccall unsafe enif_inspect_iolist_as_binary :: ErlNifEnv -> ErlNifTerm -> ErlNifBinary -> IO #{type int}
foreign import ccall unsafe enif_make_sub_binary :: ErlNifEnv -> ErlNifTerm -> #{type size_t} -> #{type size_t} -> IO ErlNifTerm
foreign import ccall unsafe enif_get_string :: ErlNifEnv -> ErlNifTerm -> CString -> #{type unsigned} -> ErlNifCharEncoding -> IO #{type int}
foreign import ccall unsafe enif_get_atom :: ErlNifEnv -> ErlNifTerm -> CString -> #{type unsigned} -> ErlNifCharEncoding -> IO #{type int}
foreign import ccall unsafe enif_is_fun :: ErlNifEnv -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_is_pid :: ErlNifEnv -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_is_port :: ErlNifEnv -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_get_uint :: ErlNifEnv -> ErlNifTerm -> Ptr (#type unsigned) -> IO #{type int};
foreign import ccall unsafe enif_get_long :: ErlNifEnv -> ErlNifTerm -> Ptr (#type long) -> IO #{type int};
foreign import ccall unsafe enif_make_uint :: ErlNifEnv -> #{type unsigned} -> IO ErlNifTerm
foreign import ccall unsafe enif_make_long :: ErlNifEnv -> #{type long} -> IO ErlNifTerm
foreign import ccall unsafe enif_make_tuple_from_array :: ErlNifEnv -> Ptr ErlNifTerm -> #{type unsigned} -> IO ErlNifTerm
foreign import ccall unsafe enif_make_list_from_array :: ErlNifEnv -> Ptr ErlNifTerm -> #{type unsigned} -> IO ErlNifTerm
foreign import ccall unsafe enif_is_empty_list :: ErlNifEnv -> ErlNifTerm -> IO #{type int}
-- ERL_NIF_API_FUNC_DECL(ErlNifResourceType*,enif_open_resource_type,(ErlNifEnv*, const char* module_str, const char* name_str, void (*dtor)(ErlNifEnv*,void *), ErlNifResourceFlags flags, ErlNifResourceFlags* tried));
-- ERL_NIF_API_FUNC_DECL(void*,enif_alloc_resource,(ErlNifResourceType* type, size_t size));
-- ERL_NIF_API_FUNC_DECL(void,enif_release_resource,(void* obj));
-- ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_resource,(ErlNifEnv*, void* obj));
-- ERL_NIF_API_FUNC_DECL(int,enif_get_resource,(ErlNifEnv*, ERL_NIF_TERM term, ErlNifResourceType* type, void** objp));
-- ERL_NIF_API_FUNC_DECL(size_t,enif_sizeof_resource,(void* obj));
-- ERL_NIF_API_FUNC_DECL(void,enif_keep_resource,(void* obj));
-- ERL_NIF_API_FUNC_DECL(ERL_NIF_TERM,enif_make_resource_binary,(ErlNifEnv*,void* obj,const void* data, size_t size));
foreign import ccall unsafe enif_make_new_binary :: ErlNifEnv -> #{type size_t} -> Ptr ErlNifTerm -> IO (Ptr #{type unsigned char})
foreign import ccall unsafe enif_is_list :: ErlNifEnv -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_is_tuple :: ErlNifEnv -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_get_atom_length :: ErlNifEnv -> ErlNifTerm -> Ptr #{type unsigned} -> ErlNifCharEncoding -> IO #{type int}
foreign import ccall unsafe enif_get_list_length :: ErlNifEnv -> ErlNifTerm -> Ptr #{type unsigned} -> IO #{type int}
foreign import ccall unsafe enif_make_atom_len :: ErlNifEnv -> CString -> #{type size_t} -> IO ErlNifTerm
foreign import ccall unsafe enif_make_existing_atom_len :: ErlNifEnv -> CString -> #{type size_t} -> Ptr ErlNifTerm -> ErlNifCharEncoding -> IO #{type int}
foreign import ccall unsafe enif_make_string_len :: ErlNifEnv -> CString -> #{type size_t} -> ErlNifCharEncoding -> IO ErlNifTerm
foreign import ccall unsafe enif_alloc_env :: IO ErlNifEnv
foreign import ccall unsafe enif_free_env :: ErlNifEnv -> IO ()
foreign import ccall unsafe enif_clear_env :: ErlNifEnv -> IO ()
foreign import ccall unsafe enif_send :: ErlNifEnv -> ErlNifPid -> ErlNifEnv -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_make_copy :: ErlNifEnv -> ErlNifTerm -> IO ErlNifEnv
foreign import ccall unsafe enif_self :: ErlNifEnv -> ErlNifPid -> IO (ErlNifPid)
foreign import ccall unsafe enif_get_local_pid:: ErlNifEnv -> ErlNifTerm -> ErlNifPid -> IO #{type int}
#if SIZEOF_LONG != 8
foreign import ccall unsafe enif_get_int64 :: ErlNifEnv -> ErlNifTerm -> Ptr #{type ErlNifSInt64} -> IO #{type int}
foreign import ccall unsafe enif_get_uint64 :: ErlNifEnv -> ErlNifTerm -> Ptr #{type ErlNifUInt64} -> IO #{type int}
foreign import ccall unsafe enif_make_int64 :: ErlNifEnv -> #{type ErlNifSInt64} -> IO ErlNifTerm
foreign import ccall unsafe enif_make_uint64 :: ErlNifEnv -> #{type ErlNifUInt64} -> IO ErlNifTerm
#endif
foreign import ccall unsafe enif_is_exception :: ErlNifEnv -> ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_make_reverse_list :: ErlNifEnv -> ErlNifTerm -> Ptr ErlNifTerm -> IO #{type int}
foreign import ccall unsafe enif_is_number :: ErlNifEnv -> ErlNifTerm -> IO #{type int}


class ErlTerm a where
  toErlNifTerm :: ErlNifEnv -> a -> IO ErlNifTerm
  fromErlNifTerm :: ErlNifEnv -> ErlNifTerm -> IO a

instance (ErlTerm a) => ErlTerm (IO a) where
  toErlNifTerm env x = x >>= toErlNifTerm env
  fromErlNifTerm = undefined


instance ErlTerm ErlNifTerm where
  toErlNifTerm _ x = return x
  fromErlNifTerm _ x = return x

instance ErlTerm #{type int} where
  toErlNifTerm = enif_make_int
  fromErlNifTerm env x = alloca $ \ptr -> do
    ret <- enif_get_int env x ptr
    checkRc ret $ peek ptr

instance ErlTerm #{type uint} where
  toErlNifTerm = enif_make_uint
  fromErlNifTerm env x =  alloca $ \ptr -> do
    ret <- enif_get_uint env x ptr
    checkRc ret $ peek ptr

#if SIZEOF_INT != SIZEOF_LONG
instance ErlTerm #{type long} where
  toErlNifTerm = enif_make_long
  fromErlNifTerm env x = alloca $ \ptr -> do
    ret <- enif_get_long env x ptr
    checkRc ret $ peek ptr

instance ErlTerm #{type unsigned long} where
  toErlNifTerm = enif_make_ulong
  fromErlNifTerm env x = alloca $ \ptr -> do
    ret <- enif_get_ulong env x ptr
    checkRc ret $ peek ptr
#endif

instance ErlTerm #{type double} where
  toErlNifTerm = enif_make_double
  fromErlNifTerm env x = alloca $ \ptr -> do
    ret <- enif_get_double env x ptr
    checkRc ret $ peek ptr

#if SIZEOF_LONG != 8
instance ErlTerm #{type ErlNifSInt64} where
  toErlNifTerm = enif_make_int64
  fromErlNifTerm env x = alloca $ \ptr -> do
    ret <- enif_get_int64 env x ptr
    checkRc ret $ peek ptr

instance ErlTerm #{type ErlNifUInt64} where
  toErlNifTerm = enif_make_uint64
  fromErlNifTerm env x = alloca $ \ptr -> do
    ret <- enif_get_uint64 env x ptr
    checkRc ret $ peek ptr
#endif

instance (Integral a) => ErlTerm a where
  toErlNifTerm env x
#if SIZEOF_IMT != 4
    | toInteger x >= toInteger (minBound::Int32) && toInteger x <= toInteger (maxBound::Int32)
      = toErlNifTerm env (fromIntegral x :: Int32)
#endif
    | toInteger x >= toInteger (minBound::Int64) && toInteger x <= toInteger (maxBound::Int64)
      = toErlNifTerm env (fromIntegral x :: Int64)
    | otherwise
      = enif_make_badarg env
  fromErlNifTerm env x = fmap fromIntegral (fromErlNifTerm env x :: IO Int64)


instance ErlTerm CStringLen where
  toErlNifTerm env (str, len) = enif_make_string_len env str (fromIntegral len) erl_nif_latin1
  fromErlNifTerm env x =
    let size = 4096 in
    allocaBytes size $ \pbuf -> do
      rc <- enif_get_string env x pbuf (fromIntegral size) erl_nif_latin1
      checkRc rc $ if rc > 0
        then return (pbuf, fromIntegral rc)
        else (fromErlNifTerm env x :: IO String) >>= newCStringLen >>= \(str, len) -> return (str, len-1)


instance ErlTerm CString where
  toErlNifTerm env x = enif_make_string env x erl_nif_latin1
  fromErlNifTerm env x = do
    (str, size) <- fromErlNifTerm env x :: IO CStringLen
    checkNullTermination str size
    return str
    where
      checkNullTermination :: CString -> Int -> IO Int
      checkNullTermination _   (-1) = return 1
      checkNullTermination str size = do
        ch <- peekElemOff str size
        checkRc ch $ checkNullTermination str (size - 1)


instance ErlTerm Char where
  toErlNifTerm env x = toErlNifTerm env (fromEnum x)
  fromErlNifTerm env x = fmap toEnum (fromErlNifTerm env x :: IO Int)


instance (ErlTerm a) => ErlTerm [a] where
  toErlNifTerm env xs = do
    ys <- mapM (toErlNifTerm env) xs
    withArrayLen ys $ \cnt ptr ->
      enif_make_list_from_array env ptr (fromIntegral cnt)
  fromErlNifTerm env xs = do
    rc <- enif_is_list env xs
    checkRc rc $ enif_get_list_loop env xs [] >>=
      mapM (fromErlNifTerm env) >>= return . reverse
    where
      enif_get_list_loop :: ErlNifEnv -> ErlNifTerm -> [ErlNifTerm] -> IO [ErlNifTerm]
      enif_get_list_loop env xs acc =
        alloca $ \phead -> alloca $ \ptail -> do
          rc <- enif_get_list_cell env xs phead ptail
          if rc == 0
            then return acc
            else do
              head <- peek phead
              tail <- peek ptail
              enif_get_list_loop env tail (head : acc)


instance (ErlTerm a) => ErlTerm (ErlTuple a) where
  toErlNifTerm env (ErlTuple xs) = do
    ys <- mapM (toErlNifTerm env) xs
    withArrayLen ys $ \cnt ptr ->
      enif_make_tuple_from_array env ptr (fromIntegral cnt)
  fromErlNifTerm env x = do
    alloca $ \parity ->
      allocaBytes (#size ERL_NIF_TERM*) $ \parray -> do
      ret <- enif_get_tuple env x parity parray
      checkRc ret $ do
        arity <- peek parity; array <- peek parray
        xs <- peekArray (fromIntegral arity) array
        ys <- mapM (fromErlNifTerm env) xs
        return $ ErlTuple ys

instance ErlTerm () where
  toErlNifTerm env () = toErlNifTerm env $ (ErlTuple [] :: ErlTuple ErlNifTerm)
  fromErlNifTerm env x = do
    xs <- (fromErlNifTerm env x :: IO (ErlTuple ErlNifTerm))
    let ErlTuple ys = xs
    checkRt (length ys == 0) $ return ()

instance (ErlTerm a, ErlTerm b) => ErlTerm (a, b) where
  toErlNifTerm env (a0, a1) = do
    sequence [toErlNifTerm env a0, toErlNifTerm env a1] >>= \t ->
      toErlNifTerm env $ ErlTuple t
  fromErlNifTerm env x = do
    xs <- (fromErlNifTerm env x :: IO (ErlTuple ErlNifTerm))
    let ErlTuple ys = xs
    checkRt (length ys == 2) $ do
        let (a0:a1:[]) = ys
        b0 <- fromErlNifTerm env a0
        b1 <- fromErlNifTerm env a1
        return (b0, b1)

instance (ErlTerm a, ErlTerm b, ErlTerm c) => ErlTerm (a, b, c) where
  toErlNifTerm env (a0, a1, a2) =
    sequence
      [toErlNifTerm env a0, toErlNifTerm env a1, toErlNifTerm env a2] >>= \t ->
    toErlNifTerm env $ ErlTuple t
  fromErlNifTerm env x = do
    xs <- (fromErlNifTerm env x :: IO (ErlTuple ErlNifTerm))
    let ErlTuple ys = xs
    checkRt (length ys == 3) $ do
        let (a0:a1:a2:[]) = ys
        b0 <- fromErlNifTerm env a0
        b1 <- fromErlNifTerm env a1
        b2 <- fromErlNifTerm env a2
        return (b0, b1, b2)

instance (ErlTerm a, ErlTerm b, ErlTerm c, ErlTerm d) => ErlTerm (a, b, c, d) where
  toErlNifTerm env (a0, a1, a2, a3) =
    sequence
      [toErlNifTerm env a0, toErlNifTerm env a1,
       toErlNifTerm env a2, toErlNifTerm env a3] >>= \t ->
    toErlNifTerm env $ ErlTuple t
  fromErlNifTerm env x = do
    xs <- (fromErlNifTerm env x :: IO (ErlTuple ErlNifTerm))
    let ErlTuple ys = xs
    checkRt (length ys == 3) $ do
        let (a0:a1:a2:a3:[]) = ys
        b0 <- fromErlNifTerm env a0
        b1 <- fromErlNifTerm env a1
        b2 <- fromErlNifTerm env a2
        b3 <- fromErlNifTerm env a3
        return (b0, b1, b2, b3)


instance ErlTerm ErlAtom where
  toErlNifTerm env (ErlAtom str) = withCAStringLen str $
    \(pstr, len) -> enif_make_atom_len env pstr $ fromIntegral len
  fromErlNifTerm env x = do
    len <- alloca $ \plen -> do
      ret <- enif_get_atom_length env x plen erl_nif_latin1
      checkRc ret $ peek plen
    allocaBytes (fromIntegral $ len + 1) $ \ptr -> do
      ret <- enif_get_atom env x ptr len erl_nif_latin1
      checkRc ret $ do
        atom <- peekCString ptr
        return $ ErlAtom atom


withErlNifBinary :: ErlNifBinary -> (CStringLen -> IO a) -> IO a
withErlNifBinary (ErlNifBinary ptr) func = do
  bin_size <- (#peek ErlNifBinary, size) ptr
  bin_data <- (#peek ErlNifBinary, data) ptr
  func (bin_data, fromIntegral (bin_size :: #{type size_t}))

allocaErlNifBinary :: (ErlNifBinary -> IO a) -> IO a
allocaErlNifBinary func = allocaBytes (#size ErlNifBinary) $ func . ErlNifBinary


instance ErlTerm ErlNifBinary where
  toErlNifTerm = enif_make_binary
  fromErlNifTerm env x =
    allocaErlNifBinary $ \bin -> do
      rc <- enif_inspect_iolist_as_binary env x bin
      checkRc rc $ return bin


instance ErlTerm (ErlBinary CStringLen) where
  toErlNifTerm env (ErlBinary (pstr, len)) =
    allocaErlNifBinary $ \bin -> do
    rc <- enif_alloc_binary (fromIntegral len) bin
    checkCr env rc $ do
    withErlNifBinary bin $ \(bin_data, _) -> do
      copyArray bin_data pstr len
      toErlNifTerm env bin
  fromErlNifTerm env x = do
    bin <- fromErlNifTerm env x :: IO (ErlNifBinary)
    withErlNifBinary bin $ return . ErlBinary


instance ErlTerm (ErlBinary String) where
  toErlNifTerm env (ErlBinary str) =
    withCAStringLen str $ toErlNifTerm env . ErlBinary
  fromErlNifTerm env x = do
    (ErlBinary cstr) <- (fromErlNifTerm env x :: IO (ErlBinary CStringLen))
    peekCAStringLen cstr >>= return . ErlBinary


instance Storable ErlNifSysInfo where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = #size ErlNifSysInfo
  peek ptr = do
    driver_major_version <- (#peek ErlNifSysInfo, driver_major_version) ptr :: IO #{type int}
    driver_minor_version <- (#peek ErlNifSysInfo, driver_minor_version) ptr :: IO #{type int}
    erts_version <- (#peek ErlNifSysInfo, erts_version) ptr >>= peekCString
    otp_release <- (#peek ErlNifSysInfo, otp_release) ptr >>= peekCString
    thread_support <- (#peek ErlNifSysInfo, thread_support) ptr :: IO #{type int}
    smp_support <- (#peek ErlNifSysInfo, smp_support) ptr :: IO #{type int}
    async_threads <- (#peek ErlNifSysInfo, async_threads) ptr :: IO #{type int}
    scheduler_threads <- (#peek ErlNifSysInfo, scheduler_threads) ptr :: IO #{type int}
    nif_major_version <- (#peek ErlNifSysInfo, nif_major_version) ptr :: IO #{type int}
    nif_minor_version <- (#peek ErlNifSysInfo, nif_minor_version) ptr :: IO #{type int}
    return $ ErlNifSysInfo
      (fromIntegral driver_major_version)
      (fromIntegral driver_minor_version)
      erts_version
      otp_release
      (thread_support /= 0)
      (smp_support /= 0)
      (fromIntegral async_threads)
      (fromIntegral scheduler_threads)
      (fromIntegral nif_major_version)
      (fromIntegral nif_minor_version)
  poke = undefined

getErlNifSysInfo :: IO ErlNifSysInfo
getErlNifSysInfo = alloca $ \ptr ->
  enif_system_info ptr #{size ErlNifSysInfo} >> peek ptr


checkRc :: (Num a, Eq a) => a -> b -> b
checkRc 0 _ = error ""
checkRc _ x = x

checkCr :: ErlNifEnv -> #{type int} -> IO ErlNifTerm -> IO ErlNifTerm
checkCr env 0 _ = enif_make_badarg env
checkCr _   _ x = x

checkRt :: Bool -> b -> b
checkRt True x = x
checkRt False x = checkRc 0 x

-- ex: ft=haskell
