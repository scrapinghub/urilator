-module(urilator_utils).


-export([pipe/2]).
-export([split/2, split/3]).


-type pipe_func() :: fun((term()) -> {ok, term()} | {error, term()}).


-spec pipe(FuncList, State) -> {ok, NewState} | {error, Label, Reason, NewState} when
      FuncList :: [{Label, pipe_func()}],
      State    :: term(),
      NewState :: term(),
      Reason   :: term(),
      Label    :: atom().
pipe([], State) ->
    {ok, State};
pipe([{Label, Func}|Rest], State) ->
    case Func(State) of
        {ok, NewState}            -> pipe(Rest, NewState);
        {error, Reason}           -> {error, Label, Reason, State};
        {error, Reason, NewState} -> {error, Label, Reason, NewState}
    end.



-spec split(Binary, Separator) -> Result when
      Binary    :: binary(),
      Separator :: binary(),
      Result    :: [binary()].
split(Binary, Separator) ->
    split(Binary, Separator, -1).



-spec split(Binary, Separator, Times) -> Result when
      Binary    :: binary(),
      Separator :: binary(),
      Times     :: integer(),
      Result    :: [binary()].
split(Binary, Separator, Times) ->
    split(Binary, Separator, size(Separator), Times, <<>>, []).


-spec split(Binary, Separator, SeparatorSize, Times, Buf, Acc) -> Result when
      Binary        :: binary(),
      Separator     :: binary(),
      SeparatorSize :: non_neg_integer(),
      Times         :: integer(),
      Buf           :: binary(),
      Acc           :: [binary()],
      Result        :: [binary()].
split(Bin, _Separator, _SeparatorSize, Times, _Buf, Acc) when Times == 0 ->
    lists:reverse([Bin|Acc]);
split(<<>>, _Separator, _SeparatorSize, _Times, Buf, Acc) ->
    lists:reverse([Buf|Acc]);
split(Bin, Separator, SeparatorSize, Times, Buf, Acc) ->
    {Rest, NewTimes, NewBuf, NewAcc} = case Bin of
        <<Separator:SeparatorSize/binary, R/binary>> -> {R, Times - 1, <<>>, [Buf|Acc]};
        <<Char:1/binary, R/binary>>                  -> {R, Times, <<Buf/binary, Char:1/binary>>, Acc}
    end,
    split(Rest, Separator, SeparatorSize, NewTimes, NewBuf, NewAcc).



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


split_test_() ->
    [
        {"empty binary", fun() ->
            ?assertEqual([<<>>], split(<<>>, <<"w">>))
        end},
        {"splitting whole binary", fun() ->
            ?assertEqual([<<"a">>, <<>>, <<"b">>, <<>>, <<"c">>], split(<<"a::::b::::c">>, <<"::">>))
        end},
        {"splitting only once", fun() ->
            ?assertEqual([<<"a">>, <<"::b::::c">>], split(<<"a::::b::::c">>, <<"::">>, 1))
        end},
        {"binary consists only of separator", fun() ->
            ?assertEqual([<<>>, <<>>, <<>>], split(<<"::::">>, <<"::">>))
        end},
        {"no splits", fun() ->
            ?assertEqual([<<";;:;">>], split(<<";;:;">>, <<"::">>))
        end}
    ].

-endif.
