-module(rebar3_applications_plugin).

-behaviour(provider).

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, do/1, format_error/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------------------------------------------------------
-define(PROVIDER, applications).

-define(DEPS, [compile]).

-define(LEAST_DEPENDENCIES, [kernel, stdlib]).

-define(APPLICATIONS(List), [__Application || {__Application, _} <- List]).

-define(APPLICATION(AppInfo), binary_to_atom(rebar_app_info:name(AppInfo), utf8)).

-define(DIRECTORY(AppInfo), filename:dirname(rebar_app_info:ebin_dir(AppInfo))).

-define(WRITE_FILE_IF_CONTENTS_DIFFER(Filename, Format, Data), rebar_file_utils:write_file_if_contents_differ(Filename, io_lib:format(Format, Data))).

-define(MATCH(Expr, Pattern), case Expr of Pattern -> true; _ -> false end).

-define(PARTITION(Fun, Pattern, List), lists:partition(fun (__Elem) -> ?MATCH(Fun(__Elem), Pattern) end, List)).

-define(NOT_IS(Term), fun (__Term) -> __Term =/= Term end).

-define(DEBUG(Format, Args), begin ok = rebar_api:debug(Format, Args) end).

-define(ASSERT(BoolExpr, Format, Args), (BoolExpr) orelse rebar_api:abort(Format, Args)).

%%----------------------------------------------------------------------------------------------------------------------
%% 'provider' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @hidden
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {desc, "Automatically generate the applications in the application resource file."}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

%% @hidden
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, Xref} = xref:start([{xref_mode, modules}]),
    ProjectApps = rebar_state:project_apps(State),
    _ = [ok = add_application(Xref, AppInfo) || AppInfo <- rebar_state:all_deps(State) ++ ProjectApps],
    Applications = ?APPLICATIONS(sort([], [], maps:to_list(lists:foldl(fun (AppInfo, Map) -> update(Map, AppInfo) end, map(Xref, ProjectApps), ProjectApps)))),
    ?DEBUG("Applications: ~p~n", [Applications]),
    _ = [ok = rewrite_applications(Xref, Applications, AppInfo) || AppInfo <- ProjectApps],
    xref:stop(Xref),
    {ok, State}.

%% @hidden
-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec add_application(Xref :: pid(), AppInfo :: rebar_app_info:t()) -> ok.
add_application(Xref, AppInfo) ->
    Application = ?APPLICATION(AppInfo),
    {ok, Application} = xref:add_application(Xref, ?DIRECTORY(AppInfo), [{name, Application}]),
    ok.

-spec map(Xref :: pid(), ProjectApps :: [rebar_app_info:t()]) -> Map when
      Map          :: #{Application => {Sources :: Dependencies, Destinations :: Dependencies}},
      Application  :: atom(),
      Dependencies :: ordsets:ordset(Application).
map(Xref, ProjectApps) ->
    Map = maps:from_list([tuple(Xref, ProjectApps, AppInfo) || AppInfo <- ProjectApps]),
    ?DEBUG("Map: ~p~n", [Map]),
    Map.

-spec tuple(Xref :: pid(), ProjectApps :: [AppInfo], AppInfo) -> Tuple when
      AppInfo      :: rebar_app_info:t(),
      Tuple        :: {Application, {Sources :: Dependencies, Destinations :: Dependencies}},
      Application  :: atom(),
      Dependencies :: ordsets:ordset(Application).
tuple(Xref, ProjectApps, AppInfo) ->
    Application = ?APPLICATION(AppInfo),
    Applications = applications(AppInfo),
    ?DEBUG("Application: ~p, Applications: ~p~n", [Application, Applications]),
    Dependencies = dependencies(Xref, ProjectApps, Application),
    ?DEBUG("Application: ~p, Dependencies: ~p~n", [Application, Dependencies]),
    Destinations = ordsets:union(Applications, Dependencies),
    ?DEBUG("Application: ~p, Destinations: ~p~n", [Application, Destinations]),
    {Application, {ordsets:new(), Destinations}}.

-spec dependencies(Xref :: pid(), ProjectApps :: [rebar_app_info:t()], Application) -> Dependencies when
      Application  :: atom(),
      Dependencies :: ordsets:ordset(Application).
dependencies(Xref, ProjectApps, Application) ->
    ordsets:filter(fun (Dependency) -> not ordsets:is_element(Application, applications(ProjectApps, Dependency)) end, analyze_dependencies(Xref, Application)).

-spec applications(ProjectApps :: [rebar_app_info:t()], Application) -> Applications when
      Application  :: atom(),
      Applications :: ordsets:ordset(Application).
applications(ProjectApps, Application) ->
    case rebar_app_utils:find(atom_to_binary(Application, utf8), ProjectApps) of {ok, AppInfo} -> applications(AppInfo); _ -> ordsets:new() end.

-spec applications(AppInfo :: rebar_app_info:t()) -> Applications :: ordsets:ordset(atom()).
applications(AppInfo) -> ordsets:from_list(rebar_app_info:applications(AppInfo)).

-spec update(Map, AppInfo :: rebar_app_info:t()) -> Map when
      Map          :: #{Application => {Sources :: Dependencies, Destinations :: Dependencies}},
      Application  :: atom(),
      Dependencies :: ordsets:ordset(Application).
update(Map, AppInfo) ->
    Application = ?APPLICATION(AppInfo),
    Applications = ordsets:from_list(maps:keys(Map)),
    Sources = sources(Map, Application, Applications),
    ?DEBUG("Application: ~p, Sources: ~p~n", [Application, Sources]),
    Destinations = destinations(Map, Application, Applications),
    ?DEBUG("Application: ~p, Destinations: ~p~n", [Application, Destinations]),
    ?ASSERT(ordsets:is_disjoint(Sources, Destinations), "Detect circular dependencies: ~p~n", [[{Application, {ordsets:intersection(Sources, Destinations)}}]]),
    maps:update(Application, {Sources, Destinations}, Map).

-spec sources(Map, Application, Applications) -> Dependencies when
      Map          :: #{Application => {Sources :: Dependencies, Destinations :: Dependencies}},
      Application  :: atom(),
      Applications :: ordsets:ordset(Application),
      Dependencies :: Applications.
sources(Map, Application, Applications) ->
    ordsets:intersection(Applications, ordsets:from_list(maps:keys(maps:filter(fun (_, {_, Destinations}) -> ordsets:is_element(Application, Destinations) end, Map)))).

-spec destinations(Map, Application, Applications) -> Dependencies when
      Map          :: #{Application => {Sources :: Dependencies, Destinations :: Dependencies}},
      Application  :: atom(),
      Applications :: ordsets:ordset(Application),
      Dependencies :: Applications.
destinations(Map, Application, Applications) ->
    {_, Destinations} = maps:get(Application, Map),
    ordsets:intersection(Applications, Destinations).

-spec sort(Head :: List, Tail :: List, List) -> List when
      List         :: [Tuple],
      Tuple        :: {Application, {Sources :: Dependencies, Destinations :: Dependencies}},
      Application  :: atom(),
      Dependencies :: ordsets:ordset(Application).
sort(Head, Tail, []) -> Head ++ Tail;
sort(Head, Tail, List) ->
    ?DEBUG("Head: ~p~nTail: ~p~nList: ~p~n", [Head, Tail, List]),
    Ordered = ordsets:union(ordsets:from_list(?APPLICATIONS(Head)), ordsets:from_list(?APPLICATIONS(Tail))),
    Unordered = fun ({Application, {Sources, Destinations}}) -> {Application, {ordsets:subtract(Sources, Ordered), ordsets:subtract(Destinations, Ordered)}} end,
    {T, Left} = ?PARTITION(Unordered, {_, {[],  _}}, lists:sort(fun (A, B) -> ordering(Unordered(A), Unordered(B)) end, List)),
    ?DEBUG("T: ~p~nLeft: ~p~n", [T, Left]),
    {H, Rest} = ?PARTITION(Unordered, {_, { _, []}}, Left),
    ?DEBUG("H: ~p~nRest: ~p~n", [H, Rest]),
    ?ASSERT([] =/= List -- Rest, "Detect circular dependencies: ~p~n", [List]),
    sort(Head ++ H, T ++ Tail, Rest).

-spec ordering(Tuple, Tuple) -> boolean() when
      Tuple        :: {Application, {Sources :: Dependencies, Destinations :: Dependencies}},
      Application  :: atom(),
      Dependencies :: ordsets:ordset(Application).
ordering({A, {[],  _}}, {B, {[],  _}}) -> A =< B;
ordering({_, {[],  _}},             _) -> false;
ordering(            _, {_, {[],  _}}) -> true;
ordering({A, { _, []}}, {B, { _, []}}) -> A =< B;
ordering({_, { _, []}},             _) -> true;
ordering(            _, {_, { _, []}}) -> false;
ordering({A,        _}, {B,        _}) -> A =< B.

-spec rewrite_applications(Xref :: pid(), Applications :: [atom()], AppInfo :: rebar_app_info:t()) -> ok.
rewrite_applications(Xref, Applications, AppInfo) ->
    Application = ?APPLICATION(AppInfo),
    AppFile = rebar_app_info:app_file(AppInfo),
    {ok, [{application, Application, List}]} = file:consult(AppFile),
    ?WRITE_FILE_IF_CONTENTS_DIFFER(AppFile, "~p.\n", [{application, Application, replace_applications(Xref, Applications, Application, List)}]).

-spec replace_applications(Xref :: pid(), Applications, Application, list()) -> list() when
      Application  :: atom(),
      Applications :: [Application].
replace_applications(Xref, Applications, Application, List) ->
    lists:keystore(applications, 1, List, {applications, append_dependencies(Xref, Application, Applications, proplists:get_value(applications, List, []))}).

-spec append_dependencies(Xref :: pid(), Application, Applications, Dependencies) -> Dependencies when
      Application  :: atom(),
      Applications :: [Application],
      Dependencies :: Applications.
append_dependencies(Xref, Application, Applications, Dependencies) ->
    normalize_dependencies(Dependencies ++ ordsets:subtract(linearize_dependencies(Xref, Application, Applications), ordsets:from_list(Dependencies)), []).

-spec normalize_dependencies(Dependencies, Dependencies) -> Dependencies when Dependencies :: [atom()].
normalize_dependencies(Dependencies, Dependencies) -> ?LEAST_DEPENDENCIES ++ Dependencies;
normalize_dependencies(Dependencies, _) -> normalize_dependencies(Dependencies -- ?LEAST_DEPENDENCIES, Dependencies).

-spec linearize_dependencies(Xref :: pid(), Application, Applications) -> Dependencies when
      Application  :: atom(),
      Applications :: [Application],
      Dependencies :: ordsets:ordset(Application).
linearize_dependencies(Xref, Application, Applications) ->
    ordsets:subtract(analyze_dependencies(Xref, Application), ordsets:from_list(lists:dropwhile(?NOT_IS(Application), Applications))).

-spec analyze_dependencies(Xref :: pid(), Application) -> Dependencies when
      Application  :: atom(),
      Dependencies :: ordsets:ordset(Application).
analyze_dependencies(Xref, Application) ->
    {ok, Applications} = xref:analyze(Xref, {application_call, Application}),
    Dependencies = ordsets:del_element(Application, ordsets:from_list(Applications)),
    ?DEBUG("Application: ~p, Dependencies: ~p~n", [Application, Dependencies]),
    Dependencies.
