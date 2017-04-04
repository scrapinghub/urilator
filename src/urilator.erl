-module(urilator).

%% RFC link: https://www.ietf.org/rfc/rfc3986.txt

%% ADT exports
-export([scheme/1, scheme/2]).
-export([user_info/1, user_info/2]).
-export([host/1, host/2]).
-export([port/1, port/2]).
-export([path/1, path/2]).
-export([query_string/1, query_string/2]).
-export([fragment/1, fragment/2]).

%% API exports
-export([parse/1]).
-export([from_kvlist/1]).
-export([export/1]).


-record(uri, {
    scheme       = <<>> :: binary(),
    user_info    = <<>> :: binary(),
    host         = <<>> :: binary(),
    port                :: undefined | non_neg_integer(),
    path         = <<>> :: binary(),
    query_string = <<>> :: binary(),
    fragment     = <<>> :: binary()
}).


%% parse state
-record(ps, {
    uri :: uri(),
    bin :: binary()
}).


-opaque uri() :: #uri{}.


-export_type([uri/0]).


%% ADT funcs

-spec scheme(Uri) -> Scheme when
      Uri    :: uri(),
      Scheme :: binary().
scheme(#uri{scheme = Scheme}) -> Scheme.


-spec scheme(Scheme, Uri) -> NewUri when
      Scheme :: binary(),
      Uri    :: uri(),
      NewUri :: uri().
scheme(Scheme, Uri) when is_binary(Scheme) -> Uri#uri{scheme = Scheme}.



-spec user_info(Uri) -> UserInfo when
      Uri      :: uri(),
      UserInfo :: binary().
user_info(#uri{user_info = UserInfo}) -> UserInfo.


-spec user_info(UserInfo, Uri) -> NewUri when
      UserInfo :: binary(),
      Uri      :: uri(),
      NewUri   :: uri().
user_info(UserInfo, Uri) when is_binary(UserInfo) -> Uri#uri{user_info = UserInfo}.


-spec host(Uri) -> Host when
      Uri  :: uri(),
      Host :: binary().
host(#uri{host = Host}) -> Host.


-spec host(Host, Uri) -> NewUri when
      Host   :: binary(),
      Uri    :: uri(),
      NewUri :: uri().
host(Host, Uri) when is_binary(Host) -> Uri#uri{host = Host}.


-spec port(Uri) -> Port when
      Uri  :: uri(),
      Port :: undefined | non_neg_integer().
port(#uri{port = Port}) -> Port.


-spec port(Port, Uri) -> NewUri when
      Port   :: undefined | non_neg_integer(),
      Uri    :: uri(),
      NewUri :: uri().
port(Port, Uri) when is_integer(Port) orelse Port == undefined -> Uri#uri{port = Port}.



-spec path(Uri) -> Path when
      Uri  :: uri(),
      Path :: binary().
path(#uri{path = Path}) -> Path.


-spec path(Path, Uri) -> NewUri when
      Path   :: binary(),
      Uri    :: uri(),
      NewUri :: uri().
path(Path, Uri) when is_binary(Path) -> Uri#uri{path = Path}.



-spec query_string(Uri) -> QueryString when
      Uri         :: uri(),
      QueryString :: binary().
query_string(#uri{query_string = QueryString}) -> QueryString.


-spec query_string(QueryString, Uri) -> NewUri when
      QueryString :: binary(),
      Uri         :: uri(),
      NewUri      :: uri().
query_string(QueryString, Uri) when is_binary(QueryString) -> Uri#uri{query_string = QueryString}.



-spec fragment(Uri) -> Fragment when
      Uri    :: uri(),
      Fragment :: binary().
fragment(#uri{fragment = Fragment}) -> Fragment.


-spec fragment(Fragment, Uri) -> NewUri when
      Fragment :: binary(),
      Uri      :: uri(),
      NewUri   :: uri().
fragment(Fragment, Uri) when is_binary(Fragment) -> Uri#uri{fragment = Fragment}.




%% API funcs

-spec parse(RawUri) -> {ok, Uri} | {error, Reason} when
      RawUri :: binary(),
      Uri    :: uri(),
      Reason :: {atom(), term()}.
parse(Uri) when is_binary(Uri) ->
    FuncList = [
        {parse_scheme,       fun parse_scheme/1},
        {parse_authority,    fun parse_authority/1},
        {parse_path,         fun parse_path/1},
        {parse_query_string, fun parse_query_string/1},
        {parse_fragment,     fun parse_fragment/1}
    ],
    case urilator_utils:pipe(FuncList, #ps{uri = #uri{}, bin = Uri}) of
        {ok, #ps{uri = ParsedUri}} -> {ok, ParsedUri};
        {error, Label, Reason, _}  -> {error, {Label, Reason}}
    end.


-spec from_kvlist(KVList) -> uri() when
      KVList  :: [Element],
      Element :: {scheme, binary()}
               | {user_info, binary()}
               | {host, binary()}
               | {port, undefined | non_neg_integer()}
               | {path, binary()}
               | {query_string, binary()}
               | {fragment, binary()}.
from_kvlist(KVList) ->
    FoldFun = fun
        ({scheme, Scheme}, Uri)  -> scheme(Scheme, Uri);
        ({user_info, UI}, Uri)   -> user_info(UI, Uri);
        ({host, Host}, Uri)      -> host(Host, Uri);
        ({port, Port}, Uri)      -> port(Port, Uri);
        ({path, Path}, Uri)      -> path(Path, Uri);
        ({query_string, Q}, Uri) -> query_string(Q, Uri);
        ({fragment, Frag}, Uri)  -> fragment(Frag, Uri)
    end,
    lists:foldl(FoldFun, #uri{}, KVList).



-spec export(uri()) -> binary().
export(#uri{scheme = S, user_info = UI, host = H, port = Po, path = Pa, query_string = QS, fragment = F}) ->
    HostPort = case Po of
        undefined -> H;
        _         -> <<H/binary, ":", (integer_to_binary(Po))/binary>>
    end,
    UserInfo = case UI of
        <<>> -> <<>>;
        _    -> <<UI/binary, "@">>
    end,
    Scheme = case S of
        <<>> -> <<>>;
        _    -> <<S/binary, "://">>
    end,
    QString = case QS of
        <<>> -> <<>>;
        _    -> <<"?", QS/binary>>
    end,
    Fragment = case F of
        <<>> -> <<>>;
        _    -> <<"#", F/binary>>
    end,
    <<Scheme/binary, UserInfo/binary, HostPort/binary, Pa/binary, QString/binary, Fragment/binary>>.


%% internal funcs


-spec parse_scheme(ParseState) -> {ok, NewParseState} when
      ParseState    :: #ps{},
      NewParseState :: #ps{}.
parse_scheme(#ps{uri = Uri, bin = Bin}) ->
    {Scheme, Rest} = parse_scheme(Bin, <<>>),
    {ok, #ps{uri = scheme(Scheme, Uri), bin = Rest}}.


-spec parse_scheme(Bin, Acc) -> {Scheme, Rest} when
      Bin    :: binary(),
      Acc    :: binary(),
      Scheme :: binary(),
      Rest   :: binary().
parse_scheme(Bin = <<":", _/binary>>, <<>>) ->
    %% scheme should contain at least 1 char
    {<<>>, Bin};
parse_scheme(<<":", Rest/binary>>, Acc) ->
    {Acc, Rest};
parse_scheme(<<Char:1/binary, Rest/binary>>, Acc) ->
    parse_scheme(Rest, <<Acc/binary, Char:1/binary>>);
parse_scheme(<<>>, Acc) ->
    %% scheme should be delimeted by ":", which is not the case here
    {<<>>, Acc}.


-spec parse_authority(ParseState) -> {ok, NewParseState} | {error, Reason} when
      ParseState    :: #ps{},
      NewParseState :: #ps{},
      Reason        :: bad_port.
parse_authority(#ps{uri = Uri, bin = Bin}) ->
    %% authority should be preceded by a double slash ("//")
    ParsePort = fun
        (<<>>)    -> undefined;
        (BinPort) -> catch binary_to_integer(BinPort)
    end,
    {UserInfo, Host, RawPort, Rest} = case Bin of
        <<"//", RawAuthority/binary>> -> parse_authority_uihp(RawAuthority);
        _                             -> {<<>>, <<>>, <<>>, Bin}
    end,
    case ParsePort(RawPort) of
        {'EXIT', _} ->
            {error, bad_port};
        Port ->
            UpdatedUri = user_info(UserInfo, port(Port, host(Host, Uri))),
            {ok, #ps{uri = UpdatedUri, bin = Rest}}
    end.


-spec parse_authority_uihp(Bin) -> {UserInfo, Host, RawPort, Rest} when
      Bin       :: binary(),
      UserInfo  :: binary(),
      Host      :: binary(),
      RawPort   :: binary(),
      Rest      :: binary().
%% @private
%% parse authority to by picking out user info/host/port
%% @end
parse_authority_uihp(Bin) ->
    {UserInfo, HostPortRest} = case urilator_utils:split(Bin, <<"@">>, 1) of
        [UInfo, HPRest] -> {UInfo, HPRest};
        [HPRest]        -> {<<>>, HPRest}
    end,
    {HostPort, Rest} = case urilator_utils:split(HostPortRest, <<"/">>, 1) of
        [HP, R] -> {HP, <<"/", R/binary>>};
        [HP]    -> {HP, <<>>}
    end,
    {Host, RawPort} = case urilator_utils:split(HostPort, <<":">>, 1) of
        [H, P] -> {H, P};
        [H]    -> {H, <<>>}
    end,
    {UserInfo, Host, RawPort, Rest}.



-spec parse_path(ParseState) -> {ok, NewParseState} when
      ParseState    :: #ps{},
      NewParseState :: #ps{}.
parse_path(#ps{uri = Uri, bin = Bin}) ->
    {Path, Rest} = parse_path(Bin, <<>>),
    {ok, #ps{uri = path(Path, Uri), bin = Rest}}.


-spec parse_path(Bin, Acc) -> {Path, Rest} when
      Bin  :: binary(),
      Acc  :: binary(),
      Path :: binary(),
      Rest :: binary().
parse_path(<<"?", Rest/binary>>, Acc) ->
    {Acc, Rest};
parse_path(<<>>, Acc) ->
    {Acc, <<>>};
parse_path(<<Char:1/binary, Rest/binary>>, Acc) ->
    parse_path(Rest, <<Acc/binary, Char:1/binary>>).


-spec parse_query_string(ParseState) -> {ok, NewParseState} when
      ParseState    :: #ps{},
      NewParseState :: #ps{}.
parse_query_string(#ps{uri = Uri, bin = Bin}) ->
    {QueryString, Rest} = parse_query_string(Bin, <<>>),
    {ok, #ps{uri = query_string(QueryString, Uri), bin = Rest}}.


-spec parse_query_string(Bin, Acc) -> {QueryString, Rest} when
      Bin         :: binary(),
      Acc         :: binary(),
      QueryString :: binary(),
      Rest        :: binary().
parse_query_string(<<>>, Acc) ->
    {Acc, <<>>};
parse_query_string(<<"#", Rest/binary>>, Acc) ->
    %% first "#" should be discarded
    {Acc, Rest};
parse_query_string(<<Char:1/binary, Rest/binary>>, Acc) ->
    parse_query_string(Rest, <<Acc/binary, Char:1/binary>>).



-spec parse_fragment(ParseState) -> {ok, NewParseState} when
      ParseState    :: #ps{},
      NewParseState :: #ps{}.
parse_fragment(#ps{uri = Uri, bin = Bin}) ->
    {ok, #ps{uri = fragment(Bin, Uri), bin = <<>>}}.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_scheme_test_() ->
    [
        {"scheme should contain at least 1 char", fun() ->
            ?assertEqual({<<>>, <<":bin">>}, parse_scheme(<<":bin">>, <<>>))
        end},
        {"scheme should be delimited by ':' from the rest of input", fun() ->
            ?assertEqual({<<>>, <<"bin">>}, parse_scheme(<<"bin">>, <<>>))
        end},
        {"valid scheme, only one ':' should be consumed", fun() ->
            ?assertEqual({<<"b">>, <<"::in">>}, parse_scheme(<<"b:::in">>, <<>>))
        end},
        {"empty input is valid", fun() ->
            ?assertEqual({<<>>, <<>>}, parse_scheme(<<>>, <<>>))
        end}
    ].


parse_authority_uihp_test_() ->
    [
        {"empty input is valid", fun() ->
            ?assertEqual({<<>>, <<>>, <<>>, <<>>}, parse_authority_uihp(<<>>))
        end},
        {"slash, which indicates end of authority should not be consumed or discarded", fun() ->
            ?assertEqual(
                {<<"user:password">>, <<"host">>, <<"port">>, <<"/path">>},
                parse_authority_uihp(<<"user:password@host:port/path">>)
            )
        end},
        {"no user info, empty port, path", fun() ->
            ?assertEqual(
                {<<>>, <<"host">>, <<>>, <<"///path">>},
                parse_authority_uihp(<<"host:///path">>)
            )
        end},
        {"only user info", fun() ->
            ?assertEqual(
                {<<"userinfo">>, <<>>, <<>>, <<>>},
                parse_authority_uihp(<<"userinfo@">>)
            )
        end},
        {"only host", fun() ->
            ?assertEqual(
                {<<>>, <<"host">>, <<>>, <<>>},
                parse_authority_uihp(<<"host">>)
            )
        end},
        {"only port", fun() ->
            ?assertEqual(
                {<<>>, <<>>, <<"port">>, <<>>},
                parse_authority_uihp(<<":port">>)
            )
        end},
        {"user info contains '/' and ':', path contains '@' and ':'", fun() ->
            ?assertEqual(
                {<<"userinfo:/">>, <<"host">>, <<"port">>, <<"/path#@:">>},
                parse_authority_uihp(<<"userinfo:/@host:port/path#@:">>)
            )
        end},
        {"empty authority is valid(only path)", fun() ->
            ?assertEqual({<<>>, <<>>, <<>>, <<"/">>}, parse_authority_uihp(<<"/">>))
        end}
    ].


parse_path_test_() ->
    [
        {"empty input is valid", fun() ->
            ?assertEqual({<<>>, <<>>}, parse_path(<<>>, <<>>))
        end},
        {"parse should consume data until first '?', which should be discarded", fun() ->
            ?assertEqual({<<>>, <<>>}, parse_path(<<"?">>, <<>>)),
            ?assertEqual({<<"path">>, <<"rest">>}, parse_path(<<"path?rest">>, <<>>)),
            ?assertEqual({<<"path">>, <<"?rest">>}, parse_path(<<"path??rest">>, <<>>))
        end}
    ].

-endif.
