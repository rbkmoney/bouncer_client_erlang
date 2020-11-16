-module(bouncer_client).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_thrift.hrl").

% -include_lib("org_management_proto/include/orgmgmt_auth_context_provider_thrift.hrl").

%% API

-export([judge/2]).
-export([make_env_context_fragment/0]).
-export([make_env_context_fragment/1]).
-export([make_auth_context_fragment/2]).
-export([make_default_user_context_fragment/1]).
-export([make_requester_context_fragment/1]).
-export([get_user_context_fragment/2]).

%%

-type auth_method() :: binary().
-type timestamp() :: non_neg_integer().
-type ip() :: string().
-type user_id() :: binary().
-type datetime() :: binary().
-type woody_context() :: woody_context:ctx().

-type context_fragment_id() :: binary().
-type bouncer_fragment() :: bouncer_context_v1_thrift:'ContextFragment'().
-type encoded_bouncer_fragment() :: bouncer_context_thrift:'ContextFragment'().
-type context_fragment() ::
    {fragment, bouncer_fragment()}
    | {encoded_fragment, encoded_bouncer_fragment()}.

-type judge_context() :: #{
    fragments => #{context_fragment_id() => context_fragment()}
}.

-type judgement() :: allowed | forbidden.

-type service_name() :: atom().

-export_type([service_name/0]).
-export_type([judgement/0]).
-export_type([judge_context/0]).
-export_type([context_fragment/0]).

-spec judge(judge_context(), woody_context()) -> judgement().
judge(JudgeContext, WoodyContext) ->
    case judge_(JudgeContext, WoodyContext) of
        {ok, Judgement} ->
            Judgement;
        {error, Reason} ->
            erlang:error({bouncer_judgement_failed, Reason})
    end.

-spec judge_(judge_context(), woody_context()) ->
    {ok, judgement()}
    | {error,
        {ruleset, notfound | invalid}
        | {context, invalid}}.
judge_(JudgeContext, WoodyContext) ->
    Context = collect_judge_context(JudgeContext),
    case bouncer_client_woody:call(bouncer, 'Judge', {<<"service/authz/api">>, Context}, WoodyContext) of
        {ok, Judgement} ->
            {ok, parse_judgement(Judgement)};
        {exception, #bdcs_RulesetNotFound{}} ->
            {error, {ruleset, notfound}};
        {exception, #bdcs_InvalidRuleset{}} ->
            {error, {ruleset, invalid}};
        {exception, #bdcs_InvalidContext{}} ->
            {error, {context, invalid}}
    end.

%%

collect_judge_context(JudgeContext) ->
    #bdcs_Context{fragments = collect_fragments(JudgeContext, #{})}.

collect_fragments(#{fragments := Fragments}, Context) ->
    maps:fold(fun collect_fragments_/3, Context, Fragments);
collect_fragments(_, Context) ->
    Context.

collect_fragments_(FragmentID, {encoded_fragment, EncodedFragment}, Acc0) ->
    Acc0#{FragmentID => EncodedFragment};
collect_fragments_(FragmentID, {fragment, Fragment}, Acc0) ->
    Acc0#{
        FragmentID => #bctx_ContextFragment{
            type = v1_thrift_binary,
            content = encode_context_fragment(Fragment)
        }
    }.

%% Fragment builders

-spec make_env_context_fragment() -> context_fragment().
make_env_context_fragment() ->
    make_env_context_fragment(genlib_rfc3339:format(genlib_time:unow(), second)).

-spec make_env_context_fragment(datetime()) -> context_fragment().
make_env_context_fragment(Datetime) ->
    {fragment, #bctx_v1_ContextFragment{
        env = #bctx_v1_Environment{
            now = Datetime
        }
    }}.

-spec make_auth_context_fragment(auth_method(), timestamp() | undefined) -> context_fragment().
make_auth_context_fragment(Method, Expiration) ->
    {fragment, #bctx_v1_ContextFragment{
        auth = #bctx_v1_Auth{
            method = Method,
            expiration = maybe_format_time(Expiration)
        }
    }}.

maybe_format_time(undefined) ->
    undefined;
maybe_format_time(Expiration) ->
    genlib_rfc3339:format(Expiration, second).

-spec make_default_user_context_fragment(user_id()) -> context_fragment().
make_default_user_context_fragment(UserID) ->
    {fragment, #bctx_v1_ContextFragment{
        user = #bctx_v1_User{
            id = UserID
        }
    }}.

-spec make_requester_context_fragment(ip() | undefined) -> context_fragment().
make_requester_context_fragment(IP0) ->
    IP1 =
        case IP0 of
            undefined ->
                undefined;
            IP0 ->
                list_to_binary(IP0)
        end,
    {fragment, #bctx_v1_ContextFragment{
        requester = #bctx_v1_Requester{
            ip = IP1
        }
    }}.

-spec get_user_context_fragment(user_id(), woody_context()) -> {ok, context_fragment()} | {error, {user, notfound}}.
get_user_context_fragment(UserID, WoodyContext) ->
    ServiceName = org_management,
    case bouncer_client_woody:call(ServiceName, 'GetUserContext', {UserID}, WoodyContext) of
        {ok, EncodedFragment} ->
            {ok, {encoded_fragment, convert_fragment(ServiceName, EncodedFragment)}};
        {exception, {orgmgmt_UserNotFound}} ->
            {error, {user, notfound}}
    end.

%%

parse_judgement(#bdcs_Judgement{resolution = allowed}) ->
    allowed;
parse_judgement(#bdcs_Judgement{resolution = forbidden}) ->
    forbidden.

convert_fragment(org_management, {bctx_ContextFragment, Type = v1_thrift_binary, Content}) when is_binary(Content) ->
    #bctx_ContextFragment{
        type = Type,
        content = Content
    }.

%%

encode_context_fragment(ContextFragment) ->
    Type = {struct, struct, {bouncer_context_v1_thrift, 'ContextFragment'}},
    Codec = thrift_strict_binary_codec:new(),
    case thrift_strict_binary_codec:write(Codec, Type, ContextFragment) of
        {ok, Codec1} ->
            thrift_strict_binary_codec:close(Codec1)
    end.
