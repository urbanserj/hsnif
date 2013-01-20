-module(example).
-export([id/1, reverse/1, tratata/0, sum/2]).

-on_load(on_load/0).

-define(NIF_ERROR, erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})).

on_load() ->
	BaseDir = case code:priv_dir(?MODULE) of
		{error, bad_name} ->
			filename:join( [ filename:dirname( code:which(?MODULE) ), "..", "priv" ] );
		Dir ->
			Dir
		end,
	SoName = filename:join(BaseDir, atom_to_list(?MODULE)),
	erlang:load_nif(SoName, 0).

-spec id(A) -> A when A :: any().
id(_) ->
  ?NIF_ERROR.

-spec reverse(list()) -> list().
reverse(_) ->
  ?NIF_ERROR.

-spec tratata() -> string().
tratata() ->
  ?NIF_ERROR.

-spec sum(integer(), integer()) -> integer().
sum(_, _) ->
  ?NIF_ERROR.
