-module(urilator_tests).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% API
urilator_test_() ->
    [
        {"not binary", fun() ->
            {error, not_a_binary} = urilator:new("https://example.net")
        end},
        {"common url", fun() ->
            {ok, URI} = urilator:new(<<"https://example.net">>),

            ?assertEqual(<<"https">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"example.net">>, urilator:hostname(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"https://example.net">>, urilator:export(URI))
        end},
        {"regular path", fun() ->
            {ok, URI} = urilator:new(<<"https://example.net/some/path/">>),

            ?assertEqual(<<"https">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"example.net">>, urilator:hostname(URI)),
            ?assertEqual(<<"some/path">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            %% FIXME: trailing '/' in path
            ?assertEqual(<<"https://example.net/some/path">>, urilator:export(URI))
        end},
        {"CasE path", fun() ->
            {ok, URI} = urilator:new(<<"https://example.net/secure/Dashboard.jspa">>),
            ?assertEqual(<<"https">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"example.net">>, urilator:hostname(URI)),
            ?assertEqual(<<"secure/Dashboard.jspa">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            %% FIXME: trailing '/' in path
            ?assertEqual(<<"https://example.net/secure/Dashboard.jspa">>, urilator:export(URI))
        end},

        {"username and password", fun() ->
            {ok, URI} = urilator:new(<<"https://u:p@example.net">>),

            ?assertEqual(<<"https">>, urilator:protocol(URI)),
            ?assertEqual(<<"u">>, urilator:username(URI)),
            ?assertEqual(<<"p">>, urilator:password(URI)),
            ?assertEqual(<<"example.net">>, urilator:hostname(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"https://example.net">>, urilator:export(URI))
        end},

        {"username and empty password", fun() ->
            {ok, URI} = urilator:new(<<"https://u:@example.net">>),

            ?assertEqual(<<"https">>, urilator:protocol(URI)),
            ?assertEqual(<<"u">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"example.net">>, urilator:hostname(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"https://example.net">>, urilator:export(URI))
        end},

        {"port", fun() ->
            {ok, URI} = urilator:new(<<"https://example.net:8080">>),

            ?assertEqual(<<"https">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"example.net">>, urilator:hostname(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(8080, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"https://example.net:8080">>, urilator:export(URI))
        end},
        {"bad port", fun() ->
            {error, port} = urilator:new(<<"https://example.net:808a">>)
        end},

        {"no path with trailing slash", fun() ->
            {ok, URI} = urilator:new(<<"https://example.net/">>),

            ?assertEqual(<<"https">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"example.net">>, urilator:hostname(URI)),
            %% FIXME: path is empty regading of trailing `/`
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"https://example.net">>, urilator:export(URI))
        end},

        {"IP host", fun() ->
            {ok, URI} = urilator:new(<<"http://10.0.2.2:8000/api/v1/user-agents">>),
    
            ?assertEqual(<<"http">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"10.0.2.2">>, urilator:hostname(URI)),
            ?assertEqual(<<"api/v1/user-agents">>, urilator:path(URI)),
            ?assertEqual(8000, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"http://10.0.2.2:8000/api/v1/user-agents">>, urilator:export(URI))
        end},

        {"dashed netloc", fun() ->
            {ok, URI} = urilator:new(<<"https://example-develop.atlassian.net">>),

            ?assertEqual(<<"https">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"example-develop.atlassian.net">>, urilator:hostname(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"https://example-develop.atlassian.net">>, urilator:export(URI))
        end},
        
        {"subdomain", fun() ->
            {ok, URI} = urilator:new(<<"https://crawlera-nubela.scrapinghub.com/curl">>),

            ?assertEqual(<<"https">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"crawlera-nubela.scrapinghub.com">>, urilator:hostname(URI)),
            ?assertEqual(<<"curl">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"https://crawlera-nubela.scrapinghub.com/curl">>, urilator:export(URI))
        end},

        {"multiple / in path", fun() ->
            {ok, URI1} = urilator:new(<<"https://example-develop.atlassian.net//path">>),
            {ok, URI2} = urilator:new(<<"https://example-develop.atlassian.net///path">>),
            {ok, URI3} = urilator:new(<<"https://example-develop.atlassian.net////path">>),
            {ok, URI4} = urilator:new(<<"https://example-develop.atlassian.net/////path">>),
            {ok, URI5} = urilator:new(<<"https://example-develop.atlassian.net//////path">>),

            ?assertEqual(<<"path">>, urilator:path(URI1)),
            ?assertEqual(<<"path">>, urilator:path(URI2)),
            ?assertEqual(<<"path">>, urilator:path(URI3)),
            ?assertEqual(<<"path">>, urilator:path(URI4)),
            ?assertEqual(<<"path">>, urilator:path(URI5))
        end},

        {"splitted path ", fun() ->
            {ok, URI1} = urilator:new(<<"https://example-develop.atlassian.net//path/path1">>),
            {ok, URI2} = urilator:new(<<"https://example-develop.atlassian.net///path/path1/path2">>),
            {ok, URI3} = urilator:new(<<"https://example-develop.atlassian.net////path/path1/path2/path3">>),

            ?assertEqual(<<"path/path1">>, urilator:path(URI1)),
            ?assertEqual(<<"path/path1/path2">>, urilator:path(URI2)),
            ?assertEqual(<<"path/path1/path2/path3">>, urilator:path(URI3))
        end},

        %% wrong inputs
        {"extra :", fun() ->
            {error, password_or_port} = urilator:new(<<"https://a:b:c@example-develop.atlassian.net">>)
        end},

        {"invalid chars in username", fun() ->
            {error, user_or_hostname} = urilator:new(<<"https://%@$@#!:b@example.net">>)
        end},

        {"invalid chars in hostname", fun() ->
            {error, hostname} = urilator:new(<<"https://a:b@example].atlassian.net">>)
        end},

        {"invalid chars in username and hostname", fun() ->
            {error, user_or_hostname} = urilator:new(<<"https://%@$@#!:b@example].atlassian.net">>)
        end},

        {"invalid chars in username and hostname", fun() ->
            {error, user_or_hostname} = urilator:new(<<"https://%@$@#!:b@example].atlassian.net">>)
        end},

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %% Incomplete URLS
        {"root path only", fun() ->
            {ok, URI} = urilator:new(<<"/">>),

            ?assertEqual(<<"">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"">>, urilator:hostname(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"">>, urilator:export(URI))
        end},
        {"relative path", fun() ->
            {ok, URI} = urilator:new(<<"/relative/path">>),

            ?assertEqual(<<"">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"">>, urilator:hostname(URI)),
            ?assertEqual(<<"relative/path">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"/relative/path">>, urilator:export(URI))
        end},
        {"path with numbers", fun() ->
            {ok, URI} = urilator:new(<<"/sessions/273490241">>),

            ?assertEqual(<<"">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"">>, urilator:hostname(URI)),
            ?assertEqual(<<"sessions/273490241">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"/sessions/273490241">>, urilator:export(URI))
        end},
        {"host only", fun() ->
            {ok, URI} = urilator:new(<<"localhost">>),

            ?assertEqual(<<"http">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"localhost">>, urilator:hostname(URI)),
            ?assertEqual(<<"">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"http://localhost">>, urilator:export(URI))
        end},
        {"no scheme with path", fun() ->
            {ok, URI} = urilator:new(<<"example.net/relative/path">>),

            ?assertEqual(<<"http">>, urilator:protocol(URI)),
            ?assertEqual(<<"">>, urilator:username(URI)),
            ?assertEqual(<<"">>, urilator:password(URI)),
            ?assertEqual(<<"example.net">>, urilator:hostname(URI)),
            ?assertEqual(<<"relative/path">>, urilator:path(URI)),
            ?assertEqual(default, urilator:port(URI)),
            ?assertEqual([], urilator:qs(URI)),
            ?assertEqual(<<"http://example.net/relative/path">>, urilator:export(URI))
        end}

    ].

