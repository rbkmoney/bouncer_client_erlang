-module(bouncer_client).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

%% API

-export([judge/2]).
-export([make_env_context_builder/0]).
-export([make_auth_context_builder/2]).
-export([make_user_context_builder/2]).
-export([make_requester_context_builder/1]).

%%

-define(BLANK_CONTEXT, #bctx_v1_ContextFragment{}).

%%

-type woody_context() :: woody_context:ctx().
-type auth_method() :: binary().
-type timestamp() :: pos_integer().
-type ip() :: string() | undefined.
-type user_id() :: binary().

-type context_fragment_id() :: binary().
-type context_fragment() :: bouncer_context_v1_thrift:'ContextFragment'().
-type context_builder() :: fun((context_fragment()) -> {context_fragment_id(), context_fragment()}).

-type judge_context() :: #{
    builders := [context_builder()]
}.

-type service_name() :: atom().

-export_type([service_name/0]).
-export_type([judge_context/0]).
-export_type([context_builder/0]).

-spec judge(judge_context(), woody_context()) ->
    boolean().

judge(JudgeContext, WoodyContext) ->
    case judge_(JudgeContext, WoodyContext) of
        {ok, Judgement} ->
            Judgement;
        Error ->
            erlang:error(Error)
    end.

-spec judge_(judge_context(), woody_context()) ->
    {ok, boolean()}
    | {error,
        {ruleset, notfound | invalid}
        | {context, invalid}}.

judge_(JudgeContext, WoodyContext) ->
    Context = collect_judge_context(JudgeContext),
    case bouncer_client_woody:call(bouncer, 'Judge', {<<"service/authz/api">>, Context}, WoodyContext) of
        {ok, Judgement} ->
            {ok, is_judgement_allows(Judgement)};
        {exception, #bdcs_RulesetNotFound{}} ->
            {error, {ruleset, notfound}};
        {exception, #bdcs_InvalidRuleset{}} ->
            {error, {ruleset, invalid}};
        {exception, #bdcs_InvalidContext{}} ->
            {error, {context, invalid}}
    end.

%%

collect_judge_context(#{builders := Builders}) ->
    Type = {struct, struct, {bouncer_context_v1_thrift, 'ContextFragment'}},
    Fragments = lists:foldl(fun(Builder, Acc0) ->
        {FragmentID, Fragment} = Builder(?BLANK_CONTEXT),
        Acc0#{FragmentID => #bctx_ContextFragment{
            type = v1_thrift_binary,
            content = encode_context_fragment(Type, Fragment)
        }}
    end, #{}, Builders),
    #bdcs_Context{fragments = Fragments}.

%% Builders

-spec make_env_context_builder() ->
    context_builder().
make_env_context_builder() ->
    fun(BlankContext) ->
        {<<"env">>, BlankContext#bctx_v1_ContextFragment{
            env = #bctx_v1_Environment{
                now = genlib_rfc3339:format(genlib_time:unow(), second)
            }
        }}
    end.

-spec make_auth_context_builder(auth_method(), timestamp() | undefined) ->
    context_builder().
make_auth_context_builder(Method, Expiration) ->
    fun(BlankContext) ->
        {<<"auth">>, BlankContext#bctx_v1_ContextFragment{
            auth = #bctx_v1_Auth{
                method = Method,
                expiration = maybe_format_time(Expiration)
            }
        }}
    end.

maybe_format_time(undefined) ->
    undefined;
maybe_format_time(Expiration) ->
    genlib_rfc3339:format(Expiration, second).

-spec make_user_context_builder(user_id(), woody_context()) ->
    context_builder().
make_user_context_builder(UserID, _WoodyContext) ->
    %% TODO add org managment call here
    fun(BlankContext) ->
        {<<"user">>, BlankContext#bctx_v1_ContextFragment{
            user = #bctx_v1_User{
                id = UserID,
                orgs = [
                    #bctx_v1_Organization{
                        %% UserID = PartyID = OrganizationID
                        id = UserID,
                        owner = #bctx_v1_Entity{
                            %% User is organization owner
                            id = UserID
                        }
                    }
                ]
            }
        }}
    end.

-spec make_requester_context_builder(ip()) ->
    context_builder().
make_requester_context_builder(IP0) ->
    IP1 = case IP0 of
        undefined ->
            undefined;
        IP0 ->
            list_to_binary(IP0)
    end,
    fun(BlankContext) ->
        {<<"requester">>, BlankContext#bctx_v1_ContextFragment{
            requester = #bctx_v1_Requester{
                ip = IP1
            }
        }}
    end.

%%

is_judgement_allows(#bdcs_Judgement{resolution = allowed}) ->
    true;
is_judgement_allows(#bdcs_Judgement{resolution = forbidden}) ->
    false.

%%

encode_context_fragment(Type, ContextFragment) ->
    Codec = thrift_strict_binary_codec:new(),
    case thrift_strict_binary_codec:write(Codec, Type, ContextFragment) of
        {ok, Codec1} ->
            thrift_strict_binary_codec:close(Codec1)
    end.
