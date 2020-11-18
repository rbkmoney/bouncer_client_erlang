-module(bouncer_context_helpers).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_thrift.hrl").

-export([make_env_context_fragment/0]).
-export([make_env_context_fragment/1]).
-export([make_auth_context_fragment/2]).
-export([make_default_user_context_fragment/1]).
-export([make_requester_context_fragment/1]).
-export([get_user_context_fragment/2]).

-type auth_method() :: binary().
-type timestamp() :: non_neg_integer().
-type ip() :: string().
-type user_id() :: binary().
-type datetime() :: binary().
-type context_fragment() :: bouncer_client:context_fragment().
-type woody_context() :: woody_context:ctx().

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

convert_fragment(org_management, {bctx_ContextFragment, Type = v1_thrift_binary, Content}) when is_binary(Content) ->
    #bctx_ContextFragment{
        type = Type,
        content = Content
    }.
