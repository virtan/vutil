-module(html_entities).
-export([encode/1, encode/2, decode/1, decode/2, nif_loader/0]).
-on_load(nif_loader/0).

encode(Bin) when is_binary(Bin) ->
    encode(Bin, html).

decode(Bin) when is_binary(Bin) ->
    decode(Bin, html).

encode(<<>>, _) -> <<>>;
encode(Bin, Doc) when is_binary(Bin) andalso (Doc =:= html orelse Doc =:= xml) ->
    lists:foldl(
      fun({What, With}, B) -> binary:replace(B, What, With, [global]) end, Bin,
      [
       {<<"&">>, <<"&amp;">>},
       {<<"\"">>, <<"&quot;">>},
       {<<"<">>, <<"&lt;">>},
       {<<">">>, <<"&gt;">>}
      ] ++ case Doc of html -> []; xml -> [{<<"'">>, <<"&apos;">>}] end).

decode(<<>>, _) -> <<>>;
decode(Bin, Doc) when is_binary(Bin) andalso (Doc =:= html orelse Doc =:= xml) ->
    lists:foldl(
      fun({What, With}, B) -> binary:replace(B, What, With, [global]) end, Bin,
      case Doc of html -> []; xml -> [{<<"&apos;">>, <<"'">>}] end ++ [
       {<<"&quot;">>, <<"\"">>},
       {<<"&lt;">>, <<"<">>},
       {<<"&gt;">>, <<">">>},
       {<<"&amp;">>, <<"&">>}
      ]).

nif_loader() ->
    SoName = case code:priv_dir(?MODULE) of
                 {error, bad_name} ->
                     case filelib:is_dir(filename:join(["..", priv])) of
                         true ->
                             filename:join(["..", priv, vutil]);
                         false ->
                             filename:join([priv, vutil])
                     end;
                 Dir ->
                     filename:join(Dir, vutil)
             end ++ "_drv",
    (catch erlang:load_nif(SoName, 0)),
    case erlang:system_info(otp_release) of
        "R13B03" -> true;
        _ -> ok
    end.
