-module(hsnif).
-export([compile/2, clean/2]).

-include_lib("kernel/include/file.hrl").

-type funspec() :: {string(), non_neg_integer(), boolean()}.

compile(Config, _AppFile) ->
	Specs = rebar_config:get_local(Config, hsnif_spec, []),
	lists:foreach(fun build/1, Specs).


clean(Config, _AppFile) ->
	Specs = rebar_config:get_local(Config, hsnif_spec, []),
	[ begin
		Target = element(1, Spec),
		Source = element(2, Spec),
		HsDir = filename:dirname(Source),
		SourceModule = hs_module_name(Source),
		TempFiles = [ filename:join(HsDir, File) || File <- [
			"HsNif" ++ SourceModule ++ ".hs",
			"HsNif" ++ SourceModule ++ "_stub.h",
			"hsnif" ++ string:to_lower(SourceModule) ++ ".c"
		] ],
		[ file:delete(File) || File <- find(HsDir, ["*.o", "*.hi"]) ],
		[ file:delete(File) || File <- TempFiles ],
		file:delete(Target)
	end || Spec <- Specs ],
	[ file:delete( filename:rootname(File) ++ ".hs" )
		|| File <- find("hs_src", ["*.hsc"]) ],
	[ file:delete(File) || File <-
		find("hs_src", ["*.o", "*.hi", "*_stub.h"]) ],
	ok.


-spec build({file:name(), file:name()} |
	{file:name(), file:name(), list()}) -> ok.
build({Target, Source}) ->
	build({Target, Source, []});
build({Target, Source, Opts}) ->
	try
		{ok, SourceFI} = file:read_file_info(Source),
		{ok, TargetFI} = file:read_file_info(Target),
		case TargetFI#file_info.mtime =:= SourceFI#file_info.mtime of
			true -> rebar_log:log(info, "Skipped ~s~n", [Source])
		end
	catch
		_:_ -> do_build(Target, Source, Opts)
	end.

-spec do_build(file:name(), file:name(), list()) -> ok.
do_build(Target, Source, Opts) ->
	PluginDir = filename:dirname(filename:dirname(code:which(?MODULE))),
	PluginHsDir = filename:join(PluginDir, "hs_src") ++ "/",
	[ command(["hsc2hs", File]) || File <- find(PluginHsDir, ["*.hsc"]) ],
	GhcOpts = ["-i" ++ PluginHsDir],
	CFlags = GhcOpts ++ proplists:get_value(cflags, Opts, []),
	LDFlags = CFlags ++ proplists:get_value(ldflags, Opts, []),

	command(["ghc", Source | CFlags]),
	Exports = export_funs(Source, GhcOpts),
	rebar_log:log(info, "export: ~s~n", [
		string:join([Fun || {Fun,_,_} <- Exports], ", ")
	]),

	FFIModuleFile = hs_module(Source, Exports),
	CModuleFile = c_module(Source, Target, Exports),

	TargetDir = filename:dirname(Target),
	file:make_dir(TargetDir),
	command(["ghc", "--make", CModuleFile, FFIModuleFile, Source,
		"-shared", "-lHSrts", "-lffi", "-o", Target | LDFlags]),
	{ok, SourceFileInfo} = file:read_file_info(Source),
	file:write_file_info(Target,
		#file_info{mtime=SourceFileInfo#file_info.mtime}),
	io:format("Compiled ~s~n", [Source]).


-spec export_funs(file:name(), [string()]) -> [funspec()].
export_funs(Source, GhcOpts) ->
	QSource = lists:flatten( io_lib:format("~p", [Source]) ),
	SourceModule = hs_module_name(Source),
	SourceExportRaw = command(["ghc",
		"-e", ":load " ++ QSource,
		"-e", ":browse " ++ SourceModule
	| GhcOpts]),
	SourceExportList = lists:foldl(
		fun(" " ++ _ = Str, Acc) ->
			[hd(Acc) ++ Str|tl(Acc)];
		(Str, Acc) ->
			[Str|Acc] end,
	[], string:tokens(SourceExportRaw, "\n")),
	FT = [ {function_name(F, SourceModule), Type} || X <- SourceExportList,
		[F, Type] <- [re:split(X, "\s*::\s*", [{return, list}, {parts,2}])],
		lists:member($ , F) =:= false ],
	TypesInfo = command(["ghc", "-XTemplateHaskell",
		"-e", ":load " ++ QSource,
		"-e", "import Foreign.Erlang.Nif",
		"-e", "import Language.Haskell.TH",
		"-e", "import Language.Haskell.TH.Syntax",
		"-e", "let
			count ((ArrowT `AppT` x) `AppT` y) = 1 + count y;
			count (ForallT _ _ x) = count x;
			count _ = 0
		", "-e", "let
			first ((ArrowT `AppT` x) `AppT` _) = x;
			first (ForallT _ _ x) = first x;
			first x = x
		", "-e", "let info x = do
			t <- runQ x;
			e <- runQ [t| Foreign.Erlang.Nif.ErlNifEnv |];
			Prelude.putStr . show $ e == first t;
			Prelude.putStr \" \";
			Prelude.putStrLn . show $ count t
		", "-e", "
			" ++ listjoin(["info [t| " ++ Type ++ " |]" || {_F, Type} <- FT],
			" >>
			")
		| GhcOpts]),
	[ case Info of
		"True "  ++ Arity -> {Fun, list_to_integer(Arity) - 1, true};
		"False " ++ Arity -> {Fun, list_to_integer(Arity), false}
	end || {Info, Fun} <- lists:zip(
		string:tokens(TypesInfo, "\n"),
		[element(1, X) || X <- FT]
	) ].


-spec hs_module(file:name(), funspec()) -> file:name().
hs_module(Source, Exports) ->
	SourceModule = hs_module_name(Source),
	FunPrefix = "hsnif" ++ string:to_lower(SourceModule),
	FFIModule = "HsNif" ++ SourceModule,
	FFIModuleText = [ "
{-# LANGUAGE ForeignFunctionInterface #-}

module ", FFIModule, " where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Erlang.Nif
import Foreign.Marshal.Alloc
import Control.Exception as C
import qualified ", SourceModule, "

exhandler :: ErlNifEnv -> SomeException -> IO ErlNifTerm
exhandler env _ = enif_make_badarg env

",
	[ hs_fun(SourceModule, FunPrefix, {Fun, Arity, Env})
		|| {Fun, Arity, Env} <- Exports,
		Fun =/= "onLoad", Fun =/= "onUnload" ],

	case lists:keyfind("onLoad", 1, Exports) of
		false -> [];
		Spec -> hs_onload(SourceModule, FunPrefix, Spec)
	end,
	case lists:keyfind("onUnload", 1, Exports) of
		false -> [];
		Spec -> hs_onunload(SourceModule, FunPrefix, Spec)
	end
],
	HsDir = filename:dirname(Source),
	FFIModuleFile = filename:join(HsDir, FFIModule ++ ".hs"),
	ok = file:write_file(FFIModuleFile, FFIModuleText),
	FFIModuleFile.


-spec c_module(file:name(), file:name(), [funspec()]) -> file:name().
c_module(Source, Target, Exports) ->
	SourceModule = hs_module_name(Source),
	FunPrefix = "hsnif" ++ string:to_lower(SourceModule),
	QTarget = io_lib:format("~p", [Target]),
	MTarget = filename:basename(Target, ".so"),
	CModuleText = ["
#include <HsFFI.h>
#include <erl_nif.h>
",
	[ ["extern ERL_NIF_TERM ", FunPrefix, Fun,
		"(ErlNifEnv*, int, const ERL_NIF_TERM*);", "\n"]
	|| {Fun, _Arity, _Env} <- Exports,
	Fun =/= "onLoad", Fun =/= "onUnload" ],
	case lists:keyfind("onLoad", 1, Exports) of
		false -> "";
		_True -> ["extern int ", FunPrefix, "OnLoad(ErlNifEnv*, void**, ERL_NIF_TERM);\n"]
	end,
	case lists:keyfind("onUnload", 1, Exports) of
		false -> "";
		_True -> ["extern void ", FunPrefix, "OnUnload(ErlNifEnv*, void*);\n"]
	end, "

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
	static char *argv[] = { ", QTarget, ", NULL }, **argv_ = argv;
	static int argc = 1;
	hs_init(&argc, &argv_);
	",
	case lists:keyfind("onLoad", 1, Exports) of
		false -> "return 0;";
		_True -> [FunPrefix, "OnLoad(env, priv_data, load_info);"]
	end,
	"
}

void unload(ErlNifEnv* env, void* priv_data) {",
	case lists:keyfind("onUnload", 1, Exports) of
		false -> "";
		_True -> ["
	", FunPrefix, "OnUnload(env, priv_data);"]
	end,
	case lists:keyfind("onLoad", 1, Exports) of
		false -> "";
		_True -> "
	enif_free(priv_data);"
	end, "
	hs_exit();
}

static ErlNifFunc nif_funcs[] = {
",
	listjoin([
		["	{\"", Fun, "\", ", integer_to_list(Arity),
			", ", FunPrefix, Fun, "}"]
		|| {Fun, Arity, _Env} <- Exports,
		Fun =/= "onLoad", Fun =/= "onUnload"
	], ",\n"), "
};
ERL_NIF_INIT(", MTarget, ",nif_funcs,&load,NULL,NULL,&unload)
"],
	HsDir = filename:dirname(Source),
	CModuleFile = filename:join(HsDir, FunPrefix ++ ".c"),
	ok = file:write_file(CModuleFile, CModuleText),
	CModuleFile.


-spec hs_module_name(file:name()) -> string().
hs_module_name(Source) ->
	ModuleName = case get(Source) of
		undefined ->
			QSource = lists:flatten( io_lib:format("~p", [Source]) ),
			command(["ghc",
				"-e", ":m Language.Haskell.Parser Language.Haskell.Syntax",
				"-e", "readFile " ++ QSource ++ " >>= \\content ->
					let ParseOk (HsModule _ (Module name) _ _ _) =
							parseModule content
					in putStr name"]);
		Mn -> Mn
	end,
	put(Source, ModuleName),
	ModuleName.


-spec hs_fun(string(), string(), funspec()) -> iolist().
hs_fun(SourceModule, FunPrefix, {Fun, Arity, Env}) ->
	Arities = [integer_to_list(A) || A <- lists:seq(0, Arity - 1)], [
	FunPrefix, Fun, " :: ErlNifEnv -> CInt -> Ptr ErlNifTerm -> IO ErlNifTerm\n",
	FunPrefix, Fun, " env ", integer_to_list(Arity), " argv =",
		" (flip C.catch) (exhandler env) $ do", [["
	a", A, " <- peekElemOff argv ", A, " >>= fromErlNifTerm env"
	] || A <- Arities], "
	toErlNifTerm env $ ", SourceModule, ".", Fun,
		case Env of true -> " env"; false -> "" end,
		[" a" ++ A || A <- Arities], "\n",
	FunPrefix, Fun, " env _ _ = enif_make_badarg env\n",
	"foreign export ccall ", FunPrefix, Fun,
		" :: ErlNifEnv -> CInt -> Ptr ErlNifTerm -> IO ErlNifTerm

"].

-spec hs_onload(string(), string(), funspec()) -> iolist().
hs_onload(SourceModule, FunPrefix, {"onLoad", Arity, Env}) -> [
	FunPrefix, "OnLoad :: ErlNifEnv -> Ptr (Ptr ()) -> ErlNifTerm -> IO CInt\n",
	FunPrefix, "OnLoad env priv_data load_info = (flip C.catch) exhandler $ do
	x <- ", SourceModule, ".onLoad",
		case Env of true -> " env"; false -> "" end,
		case Arity of 0 -> ""; 1 -> " load_info" end, "
	ptr <- enif_alloc $ fromIntegral $ sizeOf x
	poke (castPtr ptr) x
	poke priv_data $ castPtr ptr
	return 0
		where
			exhandler :: SomeException -> IO CInt
			exhandler _ = return 1
",	"foreign export ccall ", FunPrefix, "OnLoad",
		" :: ErlNifEnv -> Ptr (Ptr ()) -> ErlNifTerm -> IO CInt

"].

-spec hs_onunload(string(), string(), funspec()) -> iolist().
hs_onunload(SourceModule, FunPrefix, {"onUnload", Arity, Env}) -> [
	FunPrefix, "OnUnload :: ErlNifEnv -> Ptr () -> IO ()\n",
	FunPrefix, "OnUnload env priv_data = (flip C.catch) exhandler $
	", SourceModule, ".onUnload",
		case Env of true -> " env"; false -> "" end,
		case Arity of 0 -> ""; 1 -> " priv_data" end, "
		where
			exhandler :: SomeException -> IO ()
			exhandler _ = return ()
",	"foreign export ccall ", FunPrefix, "OnUnload",
		" :: ErlNifEnv -> Ptr () -> IO ()

"].

-spec function_name(string(), string()) -> string().
function_name(Function, SourceModule) ->
	case string:tokens(Function, ".") of
		[SourceModule, F] -> F;
		[F] -> F
	end.


-spec find(file:name(), [string()]) -> [string()].
find(Dir, Patterns) ->
	{ok, List} = file:list_dir(Dir),
	L0 = [ filename:join(Dir, File) || Pattern <- Patterns,
			File <- filelib:wildcard(Pattern, Dir) ],
	L1 = lists:concat( [find(SubDir, Patterns) || L <- List,
			SubDir <- [filename:join(Dir, L)], filelib:is_dir(SubDir)] ),
	L0 ++ L1.


-spec command([string()]) -> string().
command(Args) ->
	Port = open_port({spawn_executable, "/usr/bin/env"},
		[{args, Args}, exit_status, binary, stderr_to_stdout]),
	command_loop(Args, Port, <<>>).

-spec command_loop([string()], port(), binary()) -> string().
command_loop(Args, Port, Buffer) ->
	receive
		{Port, {exit_status, Status}} ->
			Level = case Status of
				0 -> debug;
				_ -> error
			end,
			rebar_log:log(Level, "~s~n~s~n", [string:join(Args, " "), Buffer]),
			case Status of
				0 -> binary_to_list(Buffer);
				_ -> rebar_utils:abort()
			end;
		{Port, {data, Data}} ->
			command_loop(Args, Port, <<Buffer/binary, Data/binary>>);
		Msg ->
			rebar_log:log(info, "~p~n", [Msg]),
			command_loop(Args, Port, Buffer)
	end.


-spec listjoin([string()], string()) -> iolist().
listjoin([], _Sep) ->
	[];
listjoin([H|T], Sep) ->
	[H | [[Sep, X] || X <- T]].
