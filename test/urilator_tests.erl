-module(urilator_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% API
urilator_test_() ->
    [
        {"not binary, badard", fun() ->
            ?assertMatch({'EXIT', _}, catch urilator:parse("https://example.net"))
        end},
        {"common url", fun() ->
            {ok, URI} = urilator:parse(<<"https://example.net">>),

            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"example.net">>, urilator:host(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"https://example.net">>, urilator:export(URI))
        end},
        {"regular path", fun() ->
            {ok, URI} = urilator:parse(<<"https://example.net/some/path/">>),

            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"example.net">>, urilator:host(URI)),
            ?assertEqual(<<"/some/path/">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"https://example.net/some/path/">>, urilator:export(URI))
        end},
        {"CasE path", fun() ->
            {ok, URI} = urilator:parse(<<"https://example.net/secure/Dashboard.jspa">>),
            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"example.net">>, urilator:host(URI)),
            ?assertEqual(<<"/secure/Dashboard.jspa">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"https://example.net/secure/Dashboard.jspa">>, urilator:export(URI))
        end},
        {"user_info and password", fun() ->
            {ok, URI} = urilator:parse(<<"https://u:p@example.net">>),
            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"u:p">>, urilator:user_info(URI)),
            ?assertEqual(<<"example.net">>, urilator:host(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"https://u:p@example.net">>, urilator:export(URI))
        end},
        {"user_info and empty password", fun() ->
            {ok, URI} = urilator:parse(<<"https://u:@example.net">>),
            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"u:">>, urilator:user_info(URI)),
            ?assertEqual(<<"example.net">>, urilator:host(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"https://u:@example.net">>, urilator:export(URI))
        end},
        {"port", fun() ->
            {ok, URI} = urilator:parse(<<"https://example.net:8080">>),

            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"example.net">>, urilator:host(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(8080, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"https://example.net:8080">>, urilator:export(URI))
        end},
        {"bad port", fun() ->
            {error, {parse_authority, bad_port}} = urilator:parse(<<"https://example.net:808a">>)
        end},
        {"no path with trailing slash", fun() ->
            {ok, URI} = urilator:parse(<<"https://example.net/">>),
            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"example.net">>, urilator:host(URI)),
            ?assertEqual(<<"/">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"https://example.net/">>, urilator:export(URI))
        end},
        {"IP host", fun() ->
            {ok, URI} = urilator:parse(<<"http://10.0.2.2:8000/api/v1/user-agents">>),
            ?assertEqual(<<"http">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"10.0.2.2">>, urilator:host(URI)),
            ?assertEqual(<<"/api/v1/user-agents">>, urilator:path(URI)),
            ?assertEqual(8000, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"http://10.0.2.2:8000/api/v1/user-agents">>, urilator:export(URI))
        end},
        {"dashed netloc", fun() ->
            {ok, URI} = urilator:parse(<<"https://example-develop.atlassian.net">>),
            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"example-develop.atlassian.net">>, urilator:host(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"https://example-develop.atlassian.net">>, urilator:export(URI))
        end},
        {"subdomain", fun() ->
            {ok, URI} = urilator:parse(<<"https://crawlera-nubela.scrapinghub.com/curl">>),
            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"crawlera-nubela.scrapinghub.com">>, urilator:host(URI)),
            ?assertEqual(<<"/curl">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"https://crawlera-nubela.scrapinghub.com/curl">>, urilator:export(URI))
        end},
        {"splitted path ", fun() ->
            {ok, URI1} = urilator:parse(<<"https://example-develop.atlassian.net/path/path1">>),
            {ok, URI2} = urilator:parse(<<"https://example-develop.atlassian.net/path/path1/path2">>),
            {ok, URI3} = urilator:parse(<<"https://example-develop.atlassian.net/path/path1/path2/path3">>),
            ?assertEqual(<<"/path/path1">>, urilator:path(URI1)),
            ?assertEqual(<<"/path/path1/path2">>, urilator:path(URI2)),
            ?assertEqual(<<"/path/path1/path2/path3">>, urilator:path(URI3))
        end},
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Incomplete URLS
        {"root path only", fun() ->
            {ok, URI} = urilator:parse(<<"/">>),

            ?assertEqual(<<"">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"">>, urilator:host(URI)),
            ?assertEqual(<<"/">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<"">>, urilator:query_string(URI)),
            ?assertEqual(<<"/">>, urilator:export(URI))
        end},
        {"relative path", fun() ->
            {ok, URI} = urilator:parse(<<"/relative/path">>),
            ?assertEqual(<<"">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"">>, urilator:host(URI)),
            ?assertEqual(<<"/relative/path">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"/relative/path">>, urilator:export(URI))
        end},
        {"path with numbers", fun() ->
            {ok, URI} = urilator:parse(<<"/sessions/273490241">>),
            ?assertEqual(<<"">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"">>, urilator:host(URI)),
            ?assertEqual(<<"/sessions/273490241">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"/sessions/273490241">>, urilator:export(URI))
        end},
        {"path without hostname or scheme only", fun() ->
            {ok, URI} = urilator:parse(<<"localhost">>),

            ?assertEqual(<<"">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"">>, urilator:host(URI)),
            ?assertEqual(<<"localhost">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"localhost">>, urilator:export(URI))
        end},
        {"no scheme -> everything counts as path", fun() ->
            {ok, URI} = urilator:parse(<<"example.net/relative/path">>),

            ?assertEqual(<<"">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"">>, urilator:host(URI)),
            ?assertEqual(<<"example.net/relative/path">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"example.net/relative/path">>, urilator:export(URI))
        end},
        {"complex url with url in path", fun() ->
            {ok, URI} = urilator:parse(<<"http://contact.meta.ua/register/http://contact.meta.ua/6630420/forum">>),

            ?assertEqual(<<"http">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"contact.meta.ua">>, urilator:host(URI)),
            ?assertEqual(<<"/register/http://contact.meta.ua/6630420/forum">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<>>, urilator:query_string(URI)),
            ?assertEqual(<<"http://contact.meta.ua/register/http://contact.meta.ua/6630420/forum">>, urilator:export(URI))
        end},
        {"complex url with `=` in path", fun() ->
            RawUri = <<"https://www.amazon.com/gp/help/customer/display.html/ref=footer_cou?ie=UTF8&nodeId=508088">>,
            {ok, URI} = urilator:parse(RawUri),

            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"www.amazon.com">>, urilator:host(URI)),
            ?assertEqual(<<"/gp/help/customer/display.html/ref=footer_cou">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<"ie=UTF8&nodeId=508088">>, urilator:query_string(URI)),
            ?assertEqual(RawUri, urilator:export(URI))
        end},
        {"complex url with empty QS between `?` and `&`", fun() ->
            {ok, URI} = urilator:parse(<<"https://www.amazon.com/b/?&node=5160028011">>),
            ?assertEqual(<<"https">>, urilator:scheme(URI)),
            ?assertEqual(<<"">>, urilator:user_info(URI)),
            ?assertEqual(<<"www.amazon.com">>, urilator:host(URI)),
            ?assertEqual(<<"/b/">>, urilator:path(URI)),
            ?assertEqual(undefined, urilator:port(URI)),
            ?assertEqual(<<"https://www.amazon.com/b/?&node=5160028011">>, urilator:export(URI)),
            ?assertEqual(<<"&node=5160028011">>, urilator:query_string(URI))
        end}

    ].

