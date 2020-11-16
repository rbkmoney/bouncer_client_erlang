-module(bouncer_client_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-export([all/0]).
-export([init/1]).

-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([
    judge/1
]).

-type test_case_name() :: atom().

-behaviour(supervisor).

%% tests descriptions

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [test_case_name()].
all() ->
    [
        {group, default}
    ].

-spec groups() -> [{atom(), list(), [test_case_name()]}].
groups() ->
    [
        {default, [], [
            judge
        ]}
    ].

-type config() :: [{atom(), any()}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Apps =
        genlib_app:start_application_with(bouncer_client, [
            {service_clients, #{
                bouncer => #{
                    url => <<"http://bouncer:8022/">>,
                    retries => #{
                        'Judge' => {linear, 3, 1000},
                        '_' => finish
                    }
                }
            }}
        ]),
    [{apps, Apps}] ++ Config.

-spec end_per_suite(config()) -> _.
end_per_suite(Config) ->
    [application:stop(App) || App <- proplists:get_value(apps, Config)],
    Config.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    [{test_sup, start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) -> config().
end_per_testcase(_Name, C) ->
    stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%

-spec judge(config()) -> _.
judge(C) ->
    mock_services(
        [
            {bouncer, fun('Judge', _) -> {ok, #bdcs_Judgement{resolution = allowed}} end}
        ],
        C
    ),
    WoodyContext = woody_context:new(),
    allowed = bouncer_client:judge(#{builders => []}, WoodyContext).

%%

start_mocked_service_sup(Module) ->
    {ok, SupPid} = supervisor:start_link(Module, []),
    _ = unlink(SupPid),
    SupPid.

-spec stop_mocked_service_sup(pid()) -> _.
stop_mocked_service_sup(SupPid) ->
    exit(SupPid, shutdown).

-define(APP, bouncer_client).
-define(HOST_IP, "::").
-define(HOST_PORT, 8080).
-define(HOST_NAME, "localhost").
-define(HOST_URL, ?HOST_NAME ++ ":" ++ integer_to_list(?HOST_PORT)).

mock_services(Services, SupOrConfig) ->
    maps:map(fun set_cfg/2, mock_services_(Services, SupOrConfig)).

set_cfg(Service = bouncer, Url) ->
    {ok, Clients} = application:get_env(?APP, service_clients),
    #{Service := BouncerCfg} = Clients,
    ok = application:set_env(
        ?APP,
        service_clients,
        Clients#{Service => BouncerCfg#{url => Url}}
    ).

mock_services_(Services, Config) when is_list(Config) ->
    mock_services_(Services, ?config(test_sup, Config));
mock_services_(Services, SupPid) when is_pid(SupPid) ->
    Name = lists:map(fun get_service_name/1, Services),

    Port = get_random_port(),
    {ok, IP} = inet:parse_address(?HOST_IP),
    ChildSpec = woody_server:child_spec(
        {dummy, Name},
        #{
            ip => IP,
            port => Port,
            event_handler => scoper_woody_event_handler,
            handlers => lists:map(fun mock_service_handler/1, Services)
        }
    ),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),

    lists:foldl(
        fun(Service, Acc) ->
            ServiceName = get_service_name(Service),
            case ServiceName of
                bouncer ->
                    Acc#{ServiceName => make_url(ServiceName, Port)}
            end
        end,
        #{},
        Services
    ).

get_service_name({ServiceName, _Fun}) ->
    ServiceName;
get_service_name({ServiceName, _WoodyService, _Fun}) ->
    ServiceName.

mock_service_handler({ServiceName, Fun}) ->
    mock_service_handler(ServiceName, get_service_modname(ServiceName), Fun);
mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {bouncer_client_dummy_service, #{function => Fun}}}}.

get_service_modname(bouncer) ->
    {bouncer_decisions_thrift, 'Arbiter'}.

% TODO not so failproof, ideally we need to bind socket first and then give to a ranch listener
get_random_port() ->
    rand:uniform(32768) + 32767.

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).
