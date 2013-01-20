module Example where

import Foreign.Ptr
import Foreign.Erlang.Nif
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

id :: ErlNifTerm -> ErlNifTerm
id arg = arg

reverse :: ErlNifEnv -> ErlNifTerm -> IO ErlNifTerm
reverse env arg = alloca $ \ptr -> do
  rc <- enif_make_reverse_list env arg ptr
  checkCr env rc $ peek ptr

tratata :: ErlNifEnv -> IO ErlNifTerm
tratata env = enif_priv_data env >>= peek . castPtr

sum :: Int -> Int -> Int
sum x y = x + y

onLoad :: ErlNifEnv -> IO ErlNifTerm
onLoad env = toErlNifTerm env "tratata"

onUnload :: ErlNifEnv -> Ptr () -> IO ()
onUnload _ _ = return ()
