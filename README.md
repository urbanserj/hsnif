Hsnif allows to write Erlang NIF libraries in Haskell.

Intro
-----

Hsnif consists of two parts:

* Rebar plugin that compilates Haskell code to shared library
* Haskell library which is an interface to functions and types of Erlang NIF library

Rebar plugin
------------

Rebar plugin implements `compile` and `clean` commands.

To add rebar plugin to a new project, add following lines to rebar.config:

```erlang
{deps, [
  {hsnif, ".*", {git, "https://github.com/urbanserj/hsnif.git", "master"}}
]}.
{plugin_dir, "deps/hsnif/src"}.
{plugins, [hsnif]}.
```


For specifying Target, Source and Compilation options (the last is optional) these lines need to be added:

```erlang
{hsnif_spec, [
  {"priv/target.so", "hs_src/Source.hs", [
    {cflags, ["-O"]},
    {ldflags, []}
  ]}
]}.
```


Haskell code
------------

All exported from Source file functions will be NIF functions, and each of them should satisfy the following criteria:

* Each function's argument and return value should be an instance of the class `ErlTerm` (see below)
* First argument is optional, it should be `ErlNifEnv`

Example:

```haskell
id :: ErlNifTerm -> ErlNifTerm
sum :: Int -> Int -> Int
reverse :: ErlNifEnv -> ErlNifTerm -> IO ErlNifTerm
tratata :: ErlNifEnv -> IO ErlNifTerm
```


Foreign.Erlang.Nif
------------------

This haskell library is a part of hsnif and it is an interface to functions and types of Erlang NIF library.

To convert between Erlang and Haskell types class `ErlTerm` is used. Instance of the class `ErlTerm` must implement two functions:
`toErlNifTerm` (haskell to erlang term convertation) and `fromErlNifTerm` (vice versa).

```haskell
class ErlTerm a where
  toErlNifTerm :: ErlNifEnv -> a -> IO ErlNifTerm
  fromErlNifTerm :: ErlNifEnv -> ErlNifTerm -> IO a
```

Following instances already exist in the `Foreign.Erlang.Nif` library:

```haskell
ErlTerm Char
ErlTerm Double
ErlTerm Int32
ErlTerm Int64
ErlTerm Word32
ErlTerm Word64
ErlTerm ()
Integral a => ErlTerm a
ErlTerm CStringLen
ErlTerm CString
ErlTerm ErlAtom
ErlTerm ErlNifBinary
ErlTerm ErlNifTerm
ErlTerm a => ErlTerm [a]
ErlTerm a => ErlTerm (IO a)
ErlTerm a => ErlTerm (ErlTuple a)
ErlTerm (ErlBinary String)
ErlTerm (ErlBinary CStringLen)
(ErlTerm a, ErlTerm b) => ErlTerm (a, b)
(ErlTerm a, ErlTerm b, ErlTerm c) => ErlTerm (a, b, c)
(ErlTerm a, ErlTerm b, ErlTerm c, ErlTerm d) => ErlTerm (a, b, c, d)
```

To create a new instance of the class `ErlTerm` for arbitrary type add an instance for this type to source file.

Example:

```haskell
import Data.ByteString
import Foreign.C.String

instance ErlTerm (ByteString) where
  toErlNifTerm env x =
    useAsCStringLen x $ \cstr ->
    toErlNifTerm env (ErlBinary cstr)
  fromErlNifTerm env x = do
    ErlBinary cstr <- fromErlNifTerm env x :: IO (ErlBinary CStringLen)
    packCStringLen cstr
```

onLoad and onUnload
-------------------

You can specify two optional functions `onLoad` and `onUnload` in the source file. These functions will be called on loading and on unloading the module respectively and should be one of the following types:

```haskell
onLoad :: ErlNifEnv -> Ptr (Ptr ()) -> IO ErlNifTerm
onLoad :: ErlNifEnv -> IO ErlNifTerm
onLoad :: Ptr (Ptr ()) -> IO ErlNifTerm
onLoad :: IO ErlNifTerm

onUnload :: ErlNifEnv -> Ptr () -> IO ()
onUnload :: ErlNifEnv -> IO ()
onUnload :: Ptr () -> IO ()
onUnload :: IO ()
```

Look for semantics of these functions in Erlang NIF documentation.
