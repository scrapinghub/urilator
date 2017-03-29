%%% @doc
%%% Urilator: A URI handling function library.
%%%
%%% An ADT utility for URIs. Includes import, export and validation functions.
%%%
%%% TODO:
%%%     * Host IPv4 parsing
%%%     * Host IPv6 parsing
%%%     * Punycode conversion ( https://tools.ietf.org/html/rfc5891#section-4.4 )
%%%     * URI-safe escapes    ( https://tools.ietf.org/html/rfc7320 )
%%%     * UTF-8 readable, non-escaped output.
%%%     * Canonicalize the record fields in accordance with above RFCs.
%%% @end

-module(urilator).
-vsn("0.1.0").

-export([%% API
         new/0, new/1,
         export/1,
         protocol/1, protocol/2,
         username/1, username/2,
         password/1, password/2,
         hostname/1, hostname/2,
         port/1, port/2,
         path/1, path/2,
         qs/1, qs/2, update_qs/3, drop_qs/2,
         construct_uri/2,
         root_domain/1,
         get_parent_domain/1,
         is_tld/1,
         %% utils
         strip_domain/1,
         check_custom_port/2]).

-export_type([uri/0, qs/0, segment/0]).

-record(uri,
        {protocol = <<"">>  :: binary(),
         username = <<"">>  :: binary(),
         password = <<"">>  :: binary(),
         hostname = []      :: [binary()],
         port     = default :: default
                             | inet:port_number(),
         path     = []      :: [binary()],
         qs       = []      :: [qs()],
         fragment = <<"">>  :: binary(),
         original = <<"">>  :: binary()}).
%        utf8     = <<"">>  :: binary(),
%        punycode = <<"">>  :: binary(),
%        escaped  = <<"">>  :: binary()}).


-type uri()     :: #uri{}.
-type qs()      :: {Label :: binary(), Value :: binary()}.
-type segment() :: scheme
                 | user_or_hostname
                 | hostname_or_port
                 | password_or_port
                 | hostname
                 | port
                 | path
                 | querystring
                 | fragment.



%%% Interface functions

-spec new() -> uri().
%% @doc
%% Create a new, blank URI structure.

new() ->
    #uri{}.


-spec new(URI) -> Result
    when URI     :: binary(),
         Result  :: {ok, uri()}
                  | {error, Reason},
         Reason  :: not_a_binary
                  | segment().
%% @doc
%% Accept a URI as a binary and return a urilator:uri() structure.
%% If the input binary is not a valid URI `{error, FunName, Args}' is returned.

new(URI) when is_binary(URI) ->
    consume_protocol(<<"">>, URI, #uri{original = URI});
new(_) ->
    {error, not_a_binary}.


consume_protocol(<<"">>, <<"/", Rest/binary>>, URI) ->
    %% empty hostname
    consume_path(<<"">>, [], Rest, URI);
consume_protocol(Acc, <<"">>, URI) ->
    Hostname = Acc,
    {ok, URI#uri{protocol = <<"http">>, hostname = [Hostname]}};
consume_protocol(Acc, <<".", Rest/binary>>, URI) ->
    consume_hostname(<<"">>, [Acc], Rest, URI#uri{protocol = <<"http">>});
consume_protocol(Acc, <<"/", Rest/binary>>, URI) ->
    Hostname = Acc,
    consume_path(<<"">>, [], Rest, URI#uri{protocol = <<"http">>, hostname = [Hostname]});
consume_protocol(Acc, <<"://", Rest/binary>>, URI) ->
    Protocol = to_lower(Acc),
    consume_username(<<"">>, Rest, URI#uri{protocol = Protocol});
consume_protocol(Acc, <<Letter:1/binary , Rest/binary>>, URI)
        when <<"a">> =< Letter, Letter =< <<"z">>;
             <<"A">> =< Letter, Letter =< <<"Z">>;
             <<"0">> =< Letter, Letter =< <<"9">>;
             <<"-">> =< Letter ->
    consume_protocol(<<Acc/binary, Letter/binary>>, Rest, URI);
consume_protocol(_Acc, _Rest, _Uri) ->
    {error, scheme}.


consume_username(<<"">>, <<"">>, _) ->
    {error, user_or_hostname};
consume_username(Acc, <<"">>, URI) ->
    Hostname = binary:split(to_lower(Acc), <<".">>, [global]),
    {ok, URI#uri{hostname = Hostname}};
consume_username(Acc, <<"/", Rest/binary>>, URI) ->
    Hostname = binary:split(to_lower(Acc), <<".">>, [global]),
    consume_path(<<"">>, [], Rest, URI#uri{hostname = Hostname});
consume_username(Acc, <<":", Rest/binary>>, URI) ->
    Username = Acc,
    consume_password(<<"">>, Rest, URI#uri{username = Username});
consume_username(Acc, <<"@", Rest/binary>>, URI) ->
    Username = Acc,
    consume_hostname(<<"">>, [], Rest, URI#uri{username = Username});
consume_username(Acc, <<Letter:1/binary, Rest/binary>>, URI)
        when <<"a">> =< Letter, Letter =< <<"z">>;
             <<"A">> =< Letter, Letter =< <<"Z">>;
             <<"0">> =< Letter, Letter =< <<"9">>;
             Letter == <<"-">>; Letter == <<"_">>;
             Letter == <<"=">>; Letter == <<".">> ->
    consume_username(<<Acc/binary, Letter/binary>>, Rest, URI);
consume_username(_, _, _) ->
    {error, user_or_hostname}.


consume_password(<<"">>, <<"">>, #uri{username = <<"">>}) ->
    {error, password_or_port};
consume_password(<<"">>, <<"">>, URI = #uri{username = Username}) ->
    Hostname = binary:split(to_lower(Username), <<".">>, [global]),
    {ok, URI#uri{username = <<"">>, hostname = Hostname}};
consume_password(<<"">>, <<"/", Rest/binary>>, URI = #uri{username = Username}) ->
    Hostname = binary:split(to_lower(Username), <<".">>, [global]),
    consume_path(<<"">>, <<"">>, Rest, URI#uri{username = <<"">>, hostname = Hostname});
consume_password(Acc, <<"">>, URI = #uri{username = Username}) ->
     try binary_to_integer(Acc) of
        Port ->
            Hostname = binary:split(to_lower(Username), <<".">>, [global]),
            {ok, URI#uri{username = <<"">>, hostname = Hostname, port = Port}}
     catch
        error:badarg -> {error, port}
    end;
consume_password(Acc, <<"/" , Rest/binary>>, URI = #uri{username = Username}) ->
     try binary_to_integer(Acc) of
        Port ->
            Hostname = binary:split(to_lower(Username), <<".">>, [global]),
            NewURI = URI#uri{username = <<"">>, hostname = Hostname, port = Port},
            consume_path(<<"">>, [], Rest, NewURI)
     catch
         error:badarg -> {error, port}
    end;
consume_password(Acc, <<"@", Rest/binary>>, URI) ->
    Password = Acc,
    consume_hostname(<<"">>, [], Rest, URI#uri{password = Password});
consume_password(Acc, <<Letter:1/binary, Rest/binary>>, URI)
        when <<"a">> =< Letter, Letter =< <<"z">>;
             <<"A">> =< Letter, Letter =< <<"Z">>;
             <<"0">> =< Letter, Letter =< <<"9">>;
             Letter == <<"-">>; Letter == <<"_">> ->
    consume_password(<<Acc/binary, Letter/binary>>, Rest, URI);
consume_password(_, _, _) ->
    {error, password_or_port}.


consume_hostname(Acc, Parts, <<"">>, URI) ->
    Part = Acc,
    NewParts = [Part | Parts],
    Hostname = lists:reverse(NewParts),
    {ok, URI#uri{hostname = Hostname, path = []}};
consume_hostname(Acc, Parts, <<".", Rest/binary>>, URI) ->
    Part = Acc,
    consume_hostname(<<"">>, [Part | Parts], Rest, URI);
consume_hostname(Acc, Parts, <<":" ,Rest/binary>>, URI) ->
    Part = Acc,
    NewParts = [Part | Parts],
    Hostname = lists:reverse(NewParts),
    consume_port(<<"">>, Rest, URI#uri{hostname = Hostname});
consume_hostname(Acc, Parts, <<"/", Rest/binary>>, URI) ->
    Part = Acc,
    NewParts = [Part | Parts],
    Hostname = lists:reverse(NewParts),
    consume_path(<<"">>, [], Rest, URI#uri{hostname = Hostname});
consume_hostname(Acc, Parts, <<Letter:1/binary, Rest/binary>>, URI)
        when <<"a">> =< Letter, Letter =< <<"z">>;
             <<"A">> =< Letter, Letter =< <<"Z">>;
             <<"0">> =< Letter, Letter =< <<"9">>;
             Letter == <<"-">>; Letter == <<"_">> ->
    consume_hostname(<<Acc/binary, Letter/binary>>, Parts, Rest, URI);
consume_hostname(_, _, _, _) ->
    {error, hostname}.


consume_port(Acc, <<"">>, URI) ->
    Port = list_to_integer(Acc),
    {ok, URI#uri{port = Port}};
consume_port(Acc, <<"/", Rest/binary>>, URI) ->
    Port = list_to_integer(Acc),
    consume_path(<<"">>, [], Rest, URI#uri{port = Port});
consume_port(Acc, <<"?", Rest/binary>>, URI) ->
    Port = list_to_integer(Acc),
    consume_qs(Rest, URI#uri{port = Port});
consume_port(Acc, <<"#", Rest/binary>>, URI) ->
    Port = list_to_integer(Acc),
    consume_fragment(<<"">>, Rest, URI#uri{port = Port});
consume_port(Acc, <<Letter:1/binary, Rest/binary>>, URI)
        when <<"0">> =< Letter, Letter =< <<"9">> ->
    consume_port(<<Acc/binary, Letter/binary>>, Rest, URI);
consume_port(_, _, _) ->
    {error, port}.


consume_path(<<"">>, Parts, <<"">>, URI) ->
    {ok, URI#uri{path = lists:reverse(Parts)}};
consume_path(Acc, Parts, <<"">>, URI) ->
    Part = Acc,
    NewParts = [Part | Parts],
    Path = lists:reverse(NewParts),
    {ok, URI#uri{path = Path}};
consume_path(Acc, Parts, <<"//", Rest/binary>>, URI) ->
    %% repeated slash in path
    %% becomes a component instead of division
    %% this removes 1 slash at a time to avoid errors
    consume_path(<<Acc/binary, "//">>, Parts, Rest, URI);
consume_path(<<"">>, _, <<"/", Rest/binary>>, URI) ->
    %% empty path
    consume_path(<<"">>, [], Rest, URI);
consume_path(Acc, Parts, <<"/", Rest/binary>>, URI) ->
    Part = Acc,
    consume_path(<<"">>, [Part | Parts], Rest, URI);
consume_path(Acc, Parts, <<"?", Rest/binary>>, URI) ->
    Part = Acc,
    NewParts = [Part | Parts],
    Path = lists:reverse(NewParts),
    consume_qs(Rest, URI#uri{path = Path});
consume_path(Acc, Parts, <<"#", Rest/binary>>, URI) ->
    Part = Acc,
    NewParts = [Part | Parts],
    Path = lists:reverse(NewParts),
    consume_fragment(<<"">>, Rest, URI#uri{path = Path});
consume_path(Acc, Parts, <<Letter:1/binary, Rest/binary>>, URI)
        when <<"a">> =< Letter, Letter =< <<"z">>;
             <<"A">> =< Letter, Letter =< <<"Z">>;
             <<"0">> =< Letter, Letter =< <<"9">>;
             Letter == <<"-">>; Letter == <<"_">>;
             Letter == <<".">>; Letter == <<",">>;
             Letter == <<"%">>; Letter == <<"+">>;
             Letter == <<":">>; Letter == <<"=">> ->
    consume_path(<<Acc/binary, Letter/binary>>, Parts, Rest, URI);
consume_path(_Acc, _Parts, _Rest, _URI) ->
    {error, path}.


consume_qs(<<"">>, URI) ->
    {ok, URI};
consume_qs(<<"#", Rest/binary>>, URI) ->
    consume_fragment(<<"">>, Rest, URI);
consume_qs(Rest, URI) ->
    consume_qs(<<"">>, [], Rest, URI).


consume_qs(<<"">>, Parts, <<"">>, URI) ->
    QS = lists:reverse(Parts),
    {ok, URI#uri{qs = QS}};
consume_qs(Acc, Parts, <<"">>, URI) ->
    Q = Acc,
    Part = {Q, <<"">>},
    QS = lists:reverse([Part | Parts]),
    {ok, URI#uri{qs = QS}};
consume_qs(<<"">>, Parts, <<"#", Rest/binary>>, URI) ->
    QS = lists:reverse(Parts),
    consume_fragment(<<"">>, Rest, URI#uri{qs = QS});
consume_qs(<<"">>, _Parts, <<"&", Rest/binary>>, URI) ->
    consume_qs(<<"">>, [{<<>>,<<>>}], Rest, URI);
consume_qs(Acc, Parts, <<"#", Rest/binary>>, URI) ->
    Q = Acc,
    Part = {Q, <<"">>},
    QS = lists:reverse([Part | Parts]),
    consume_fragment(<<"">>, Rest, URI#uri{qs = QS});
consume_qs(<<"">>, _, <<"=" , _/binary>>, _) ->
    error;
consume_qs(Acc, Parts, <<"=", Rest/binary>>, URI) ->
    consume_qs(<<"">>, Acc, Parts, Rest, URI);
consume_qs(Acc, Parts, <<Letter:1/binary, Rest/binary>>, URI)
        when <<"a">> =< Letter, Letter =< <<"z">>;
             <<"A">> =< Letter, Letter =< <<"Z">>;
             <<"0">> =< Letter, Letter =< <<"9">>;
             Letter == <<"-">>; Letter == <<"_">>;
             Letter == <<".">>; Letter == <<",">>;
             Letter == <<"%">>; Letter == <<"+">>;
             Letter == <<"{">>; Letter == <<"}">>;
             Letter == <<"[">>; Letter == <<"]">>;
             Letter == <<":">>; Letter == <<"@">>;
             Letter == <<"/">> ->
    consume_qs(<<Acc/binary, Letter/binary>>, Parts, Rest, URI);
consume_qs(_, _, _, _) ->
    {error, querystring}.


consume_qs(Acc, Q, Parts, <<"">>, URI) ->
    S = Acc,
    QS = lists:reverse([{Q, S} | Parts]),
    {ok, URI#uri{qs = QS}};
consume_qs(Acc, Q, Parts, <<"&", Rest/binary>>, URI) ->
    S = Acc,
    consume_qs(<<"">>, [{Q, S} | Parts], Rest, URI);
consume_qs(Acc, Q, Parts, <<";", Rest/binary>>, URI) ->
    S = Acc,
    consume_qs(<<"">>, [{Q, S} | Parts], Rest, URI);
consume_qs(Acc, Q, Parts, <<"#", Rest/binary>>, URI) ->
    S = Acc,
    QS = lists:reverse([{Q, S} | Parts]),
    consume_fragment(<<"">>, Rest, URI#uri{qs = QS});
consume_qs(Acc, Q, Parts, <<Letter:1/binary, Rest/binary>>, URI) ->
    consume_qs(<<Acc/binary, Letter/binary>>, Q, Parts, Rest, URI).


consume_fragment(_, Fragment, URI) ->
    {ok, URI#uri{fragment = Fragment}}.
% TODO: Escaping. The version above simply doesn't escape anything.
%consume_fragment(Acc, <<"">>, URI) ->
%    Fragment = lists:reverse(Acc),
%    {ok, URI#uri{fragment = Fragment}};
%consume_fragment(Acc, [Letter | Rest], URI) ->
%    consume_fragment([Letter | Acc], Rest, URI).


-spec export(URI) -> Binary
    when URI    :: uri(),
         Binary :: binary().
%% @doc
%% Accept a URI datatype and export a URI-safe encoded string.
%% FIXME: This currently returns an unsafe URI.

export(URI = #uri{protocol = <<"">>, hostname = []}) ->
    cat_path(<<"">>, URI);
export(URI = #uri{protocol = Protocol}) ->
    Hostname = hostname(URI),
    cat_port(<<Protocol/binary, "://", Hostname/binary>>, URI).


cat_port(Acc, URI = #uri{port = default}) ->
    cat_path(Acc, URI);
cat_port(Acc, URI = #uri{port = Number}) ->
    Port = integer_to_binary(Number),
    cat_path(<<Acc/binary, ":", Port/binary>>, URI).


cat_path(Acc, URI = #uri{path = []}) ->
    cat_qs(Acc, URI);
cat_path(Acc, URI = #uri{path = Parts}) ->
%%    Path = string:join(Parts, "/"),
    Path = binary_join(Parts, <<"/">>),
    cat_qs(<<Acc/binary, "/", Path/binary>>, URI).


cat_qs(Acc, URI = #uri{qs = []}) ->
    cat_fragment(Acc, URI);
cat_qs(Acc, URI = #uri{qs = Parts}) ->
    %% the following lines are useful in to preserve order of & (when there's an empty item before)
    CatQSFun = fun({<<>>, <<>>}, Acc) -> [<<"">> | Acc];
                  ({Q,S}, Acc)        -> [<<Q/binary, "=", S/binary>> | Acc]
               end,
    QSList = lists:foldr(CatQSFun, [], Parts),
    QS = binary_join(QSList, <<"&">>),
    cat_fragment(<<Acc/binary, "?", QS/binary>>, URI).


cat_fragment(Acc, #uri{fragment = <<"">>}) ->
    Acc;
cat_fragment(Acc, #uri{fragment = Fragment}) ->
    <<Acc/binary, "#", Fragment/binary>>.


-spec protocol(URI) -> Protocol
    when URI      :: uri(),
         Protocol :: binary().
%% @doc
%% Accept a URI datatype and return the protocol.

protocol(#uri{protocol = Protocol}) -> Protocol.


-spec protocol(Binary, URI) -> NewURI
    when Binary :: binary(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a protocol label and a URI, and return the URI updated with the
%% new protocol.
%% FIXME: Will currently accept any invalid input -- should only accept [:alpha:]

protocol(Protocol, URI) ->
    URI#uri{protocol = Protocol}.


-spec username(URI) -> Username
    when URI      :: uri(),
         Username :: binary().
%% @doc
%% Accept a URI datatype and return the username.

username(#uri{username = Username}) ->
    Username.


-spec username(Binary, URI) -> NewURI
    when Binary :: binary(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a username and a URI, and return the URI updated with the new username.

username(Binary, URI) ->
    URI#uri{username = Binary}.


-spec password(URI) -> Password
    when URI      :: uri(),
         Password :: binary().
%% @doc
%% Accept a URI datatype and return the password.

password(#uri{password = Password}) ->
    Password.


-spec password(Binary, URI) -> NewURI
    when Binary :: binary(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a password and a URI, and return the URI updated with the new password.

password(Binary, URI) ->
    URI#uri{password = Binary}.


-spec hostname(URI) -> Hostname
    when URI      :: uri(),
         Hostname :: binary().
%% @doc
%% Accept a URI datatype and return the hostname.

hostname(#uri{hostname = Parts}) ->
    binary_join(Parts, <<".">>).
%%    string:join(Parts, ".").


-spec hostname(Binary, URI) -> NewURI
    when Binary :: binary(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a hostname and a URI, and return the URI updated with the new hostname.
%% FIXME: Currently accepts invalid input and performs no IDN conversions.

hostname(Binary, URI) ->
    Hostname = binary:split(Binary, <<".">>, [global]),
    URI#uri{hostname = Hostname}.


-spec port(URI) -> Port
    when URI  :: uri(),
         Port :: default
               | inet:port_number().
%% @doc
%% Accept a URI datatype and return the port or `default' if it is undefined.

port(#uri{port = Port}) -> Port.


-spec port(Port, URI) -> NewURI
    when Port   :: default
                 | inet:port_number(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a port number and a URI, and return the URI updated with the
%% new port.

port(default, URI) ->
    URI#uri{port = default};
port(Port, URI)
        when 0 =< Port, Port =< 65535 ->
    URI#uri{port = Port}.


-spec path(URI) -> Path
    when URI  :: uri(),
         Path :: binary().
%% @doc
%% Accept a URI datatype and return the path as a binary.

path(#uri{path = Parts}) ->
    binary_join(Parts, <<"/">>).


-spec path(Path, URI) -> NewURI
    when Path   :: binary(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a path and a URI, and return the URI updated with the new path.
%% FIXME: This currently accepts invalid input and could return an invalid
%%        result in certain cases.

path(Binary, URI) ->
    URI#uri{path = binary:split(Binary, <<".">>, [global])}.


-spec qs(URI) -> Queries
    when URI     :: uri(),
         Queries :: [qs()].
%% @doc
%% Accept a URI datatype and return its list of queries.

qs(#uri{qs= QS}) -> [KV || KV <- QS, KV =/= {<<>>, <<>>}].


-spec qs(Queries, URI) -> NewURI
    when Queries :: [qs()],
         URI     :: uri(),
         NewURI  :: uri().
%% @doc
%% Accept a list of queries and a URI, and return the URI updated with the
%% new queries, replacing all of the original ones.

qs(QS, URI) ->
    URI#uri{qs = QS}.


-spec update_qs(Label, Value, URI) -> NewURI
    when Label  :: binary(),
         Value  :: binary(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a Label, Value and a URI, upsert the query indicated by the label,
%% and return the new URI.

update_qs(Label, Value, URI = #uri{qs = QS}) ->
    NewQS = lists:keystore(Label, 1, QS, {Label, Value}),
    URI#uri{qs = NewQS}.


-spec drop_qs(Label, URI) -> NewURI
    when Label  :: binary(),
         URI    :: uri(),
         NewURI :: uri().
%% @doc
%% Accept a label and a URI, and drop the query indicated by the label, if it
%% exists.

drop_qs(Label, URI = #uri{qs = QS}) ->
    NewQS = lists:keydelete(Label, 1, QS),
    URI#uri{qs = NewQS}.


-spec construct_uri(UriBinary, Defaults) -> Result
    when UriBinary :: binary(),
         Defaults  :: [qs()],
         Result    :: {ok, uri()}
                    | {error, bad_uri}.
%% @doc
%% Accept a URI string and a proplist of default query values, and return
%% a normalized URI string, incorporating the default query values.

construct_uri(UriBinary, Defaults) ->
    case new(UriBinary) of
        {ok, URI} ->
            QS = qs(URI),
            FullQS = lists:foldl(fun update_listkey/2, Defaults, QS),
            NewURI = qs(FullQS, URI),
            {ok, NewURI};
        Error ->
            Error
    end.


-spec update_listkey({Key, Value}, AccList) -> NewAccList
    when Key        :: binary(),
         Value      :: binary(),
         AccList    :: list(),
         NewAccList :: list().
%% @private
%% A helper function for using a fold to override default header values with a list
%% list of current header value overrides. Called by the foldl in construct_uri/2.
         
update_listkey({Key, Value}, QueryList) ->
    lists:keystore(Key, 1, QueryList, {Key, Value}).


-spec root_domain(URI) -> RootDomain
    when URI        :: uri(),
         RootDomain :: binary().
%% @doc
%% Get root domain for any hostname
%% Example: `bar.baz.foo.example.com` -> example.com

root_domain(#uri{hostname = Hostname}) ->
    RootDomain = lists:nthtail(length(Hostname) - 2, Hostname),
    binary_join(RootDomain, <<".">>).



-spec get_parent_domain(URI) -> ParentHostname
    when URI            :: uri(),
         ParentHostname :: binary().
%% @doc
%% Get parent domain for any hostname
%% Example: `foo.example.com`     -> example.com
%%          `bar.foo.example.com` -> foo.example.com

get_parent_domain(#uri{hostname = [_ParentHostname]}) ->
    <<"">>;
get_parent_domain(#uri{hostname = [_H|ParentHostname]}) ->
    binary_join(ParentHostname, <<".">>).


-spec is_tld(URI) -> Result
    when URI    :: uri(),
         Result :: boolean().
%% @doc
%% Check if the Domain is a Top Level Domain
%% It uses the bd providen in
%% http://mxr.mozilla.org/mozilla-central/source/netwerk/dns/effective_tld_names.dat?raw=1

is_tld(URI) ->
    Hostname = hostname(URI),
    %% FIXME: we use binary conversion
    publicsuffix:is_tld(binary_to_list(Hostname)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec check_custom_port(Scheme, Port) -> Result
    when Scheme    :: http_uri:scheme(),
         Port      :: pos_integer(),
         Result    :: {Scheme, MaybePort},
         MaybePort :: Port
                    | default.
%% @doc
%% Accepts a Scheme and Port argument, and returns the scheme plus a custom
%% port number, or the scheme and the atom 'default'.
%%
%% Output of this function is consumed by format_uri/5.
%% @see urilator:format_uri/5


check_custom_port(http,     80) -> {http,   default};
check_custom_port(https,   443) -> {https,  default};
check_custom_port(ftp,      21) -> {ftp,    default};
check_custom_port(ssh,      22) -> {ssh,    default};
check_custom_port(scp,      22) -> {scp,    default};
check_custom_port(sftp,     22) -> {sftp,   default};
check_custom_port(telnet,   23) -> {telnet, default};
check_custom_port(smtp,     25) -> {smtp,   default};
check_custom_port(dns,      53) -> {smtp,   default};
check_custom_port(tftp,     69) -> {tftp,   default};
check_custom_port(gopher,   70) -> {gopher, default};
check_custom_port(Scheme, Port) -> {Scheme, Port}.


-spec strip_domain(Domain) -> StrippedDomain
when Domain        :: binary(),
    StrippedDomain :: binary().
%% @doc
%% @private
%% Get stripped version of any domain
%% Example: `.example.com` -> example.com

strip_domain(Domain) ->
    case Domain of
        <<".", Rest/binary>> -> Rest;
        _                    -> Domain
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_lower(Binary) -> <<<<(string:to_lower(C))/utf8>> || <<C/utf8>> <= Binary >>.


%% https://coderwall.com/p/746dgg/joining-a-list-of-binaries-in-erlang-92b2817f-28ce-4cb9-aaa0-73d6c00098f7
-spec binary_join(List :: list(binary()), Separator :: binary()) -> binary().
binary_join([], _Separator) ->
    <<>>;
binary_join([Item], _Separator) ->
    Item;
binary_join([H | List], Separator) ->
    Rest = binary_join(List, Separator),
    <<H/binary, Separator/binary, Rest/binary>>.
