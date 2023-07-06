%%% @doc
%%% Test suite for unsplit
%%% @copyright Nimbuzz B.V.
%%% @author Ahmed Omar <omar@nimbuzz.nl>
%%% @end

-module(unsplit_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(TABLE, test1).
-define(NODES, ['cn1', 'cn2']).
-define(DISCONNECT_TIME_MS, 2000).
-define(UNSPLIT_TIMEOUT_MS, 2000).
-record(?TABLE, {key, modified=erlang:timestamp(), value}).

-define(TEST_CASE_SPLIT_ALL_REMOTE_KEYS_NO_ACTION, all_remote_keys_no_action).
-define(TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_MODIFIED, all_remote_keys_last_modified).
-define(TEST_CASE_SPLIT_ALL_REMOTE_KEYS_BAG, all_remote_keys_bag).
-define(TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_VERSION, all_remote_keys_last_version).

all() -> [
    ?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_NO_ACTION,
    ?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_MODIFIED,
    ?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_BAG,
    ?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_VERSION
].

init_per_suite(Conf) ->
    net_kernel:start(['test@127.0.0.1', longnames]),
    erl_boot_server:start(['127.0.0.1']),
    application:ensure_all_started(unsplit),

    Nodes = ct:get_config(nodes, ?NODES),
    DisconnectTime = ct:get_config(disconnect_time, ?DISCONNECT_TIME_MS),
    UnsplitTimeout = ct:get_config(unsplit_timeout, ?UNSPLIT_TIMEOUT_MS),
    Host = "127.0.0.1",

    StartNode = fun(Node, {NodeNamesAcc, PeersAcc})->
        ct:print("starting node ~p, on host ~p",[Node, Host]),

        {ok, Peer, StartedNode} = ?CT_PEER(#{host => Host, longnames => true, name => Node, args => [
            "-pa", "../../_build/test/lib/unsplit/ebin",
            "-pa", "../../_build/test/lib/unsplit/test",
            "-kernel", "prevent_overlapping_partitions", "false",
            "-kernel", "dist_auto_connect", "once",
            "-config", "../../test/sys.config",
            "-loader", "inet",
            "-hosts", Host,
            "-setcookie", atom_to_list(erlang:get_cookie())
        ]}),
        unlink(Peer),
        {[StartedNode|NodeNamesAcc], [{Node, Peer}|PeersAcc]}
    end,

    {NodeNames, Peers} = lists:foldl(StartNode, {[], []}, Nodes),

    ct:print("started things: ~p peers: ~p",[NodeNames, Peers]),

    [
        {disconnect_time, DisconnectTime},
        {unsplit_timeout, UnsplitTimeout},
        {peers, Peers},
        {nodes, NodeNames}|Conf
    ].

end_per_suite(Conf) ->
    Nodes = ct:get_config(nodes, ?NODES),
    Peers = proplists:get_value(peers, Conf),

    ct:print("stop nodes: ~p",[Nodes]),

    StopNode = fun(Node)->
        {_, Peer} = proplists:lookup(Node, Peers),
        ok = peer:stop(Peer)
    end,
    lists:map(StopNode, Nodes),
    ok.

init_per_testcase(Case, Conf0) ->
    UserProperties = case Case of
        ?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_NO_ACTION -> [
                {unsplit_strategy, all_remote_keys},
                {unsplit_method, {unsplit_lib, no_action, []}} | Conf0
            ];
        ?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_MODIFIED -> [
            {unsplit_strategy, all_remote_keys},
            {unsplit_method, {unsplit_lib, last_modified, []}} | Conf0
        ];
        ?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_BAG -> [
            {unsplit_strategy, all_remote_keys},
            {unsplit_method, {unsplit_lib, bag, []}} | Conf0
        ];
        ?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_VERSION -> [
            {unsplit_strategy, all_remote_keys},
            {unsplit_method, {unsplit_lib, last_version, [modified]}} | Conf0
        ];
        _ ->
            []
    end,

    Conf = [{user_properties, UserProperties}|Conf0],

    ct:print("Test case ~p started ...", [Case]),
    init_nodes(Conf),
    Conf.

end_per_testcase(Case, Conf) ->
    ct:print("Test case ~p finished ...", [Case]),
    terminate_nodes(proplists:get_value(nodes, Conf)),
    Conf.

?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_NO_ACTION(Conf) ->
    {Master, Slave} = split_test(?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_NO_ACTION, Conf),
    ?assertEqual(3, length(Master)),
    ?assertEqual(3, length(Slave)),
    ?assertEqual(ok, test_values(b, Master)).

?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_MODIFIED(Conf) ->
    {Master, Slave} = split_test(?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_MODIFIED, Conf),
    ?assertEqual(4, length(Master)),
    ?assertEqual(Master, Slave),
    ?assertEqual(ok, test_values(b, Master)).

?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_BAG(Conf) ->
    {Master, Slave} = split_test(?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_MODIFIED, Conf),
    ?assertEqual(4, length(Master)),
    ?assertEqual(Master, Slave),
    ?assertEqual(ok, test_values(b, Master)).

?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_VERSION(Conf) ->
    {Master, Slave} = split_test(?TEST_CASE_SPLIT_ALL_REMOTE_KEYS_LAST_VERSION, Conf),
    ?assertEqual(4, length(Master)),
    ?assertEqual(Master, Slave),
    ?assertEqual(ok, test_values(b, Master)).

split_test(_Case, Conf)->
    DisconnectTime = proplists:get_value(disconnect_time, Conf),
    UnsplitTimeout = proplists:get_value(unsplit_timeout, Conf),
    Nodes = [M, S|_Rest] = proplists:get_value(nodes, Conf),

    ct:print("Initial tables ..."),
    {MasterContent0, SlaveContent0} = print_tables(Nodes, ?TABLE),
    ?assertEqual([], MasterContent0),
    ?assertEqual([], SlaveContent0),

    ct:print("inserting records ..."),
    ?assertEqual({atomic, ok}, write(M, [
        #?TABLE{key=1, value=a1},
        #?TABLE{key=2, value=a2}
    ])),

    {MasterContent1, SlaveContent1} = print_tables(Nodes, ?TABLE),
    ?assertEqual(MasterContent1, SlaveContent1),
    ?assertEqual(2, length(MasterContent1)),

    ct:print("disconnecting node ~p from ~p", [M, S]),
    disconnect(M, S),

    ct:print("updating records on the nodes, while they are disconected ..."),
    timer:sleep(DisconnectTime),

    ?assertEqual({atomic, ok}, write(M, [
        #?TABLE{key=1, modified=erlang:timestamp(), value=b1},
        #?TABLE{key=2, modified=erlang:timestamp(), value=b2},
        #?TABLE{key=3, modified=erlang:timestamp(), value=b3}
    ])),

    ?assertEqual({atomic, ok}, write(S, [
        #?TABLE{key=4, modified=erlang:timestamp(), value=b4}
    ])),

    {MasterContent2, SlaveContent2} = print_tables(Nodes, ?TABLE),
    ?assertNotEqual(MasterContent2, SlaveContent2),
    ?assertEqual(3, length(MasterContent2)),
    ?assertEqual(3, length(SlaveContent2)),

    ct:print("reconnecting nodes ..."),

    connect(M, S),
    timer:sleep(UnsplitTimeout),
    print_tables(Nodes, ?TABLE).

% internals

print_tables([M,S|_], Table)->
    MasterTable = rpc:call(M, ets, tab2list, [Table]),
    SlaveTable = rpc:call(S, ets, tab2list, [Table]),
    ct:print("master table = ~p", [MasterTable]),
    ct:print("slave table = ~p", [SlaveTable]),
    {MasterTable, SlaveTable}.

terminate_nodes(Nodes)->
    rpc:multicall(Nodes, mnesia, del_table_copy, [?TABLE]),
    rpc:multicall(Nodes, application, stop, [unsplit]),
    rpc:multicall(Nodes, mnesia, stop, []),
    rpc:call(hd(Nodes), mnesia, delete_schema, [Nodes]).

init_nodes(Conf)->
    Nodes = proplists:get_value(nodes, Conf),
    FirstNode = hd(Nodes),
    rpc:call(FirstNode, mnesia, create_schema, [Nodes]),

    {[R1|_],[]} = rpc:multicall(Nodes, application, ensure_all_started, [unsplit]),
    ?assertMatch({ok, _}, R1),

    rpc:call(FirstNode, mnesia, create_table, [?TABLE, [
        {ram_copies, Nodes},
        {attributes, record_info(fields, ?TABLE)},
        {user_properties, proplists:get_value(user_properties, Conf)}
    ]]).

disconnect(Master, Slave)->
    true = rpc:call(Master, erlang, disconnect_node, [Slave]),
    NodeList = rpc:call(Master, erlang, nodes, []),
    ct:print("node list is now: ~p", [NodeList]).

connect(Master, Slave)->
    true = rpc:call(Master, net_kernel, connect_node, [Slave]),
    NodeList = rpc:call(Master, erlang, nodes, []),
    ct:print("node list is now: ~p", [NodeList]).

write(Node, Records)->
    rpc:call(Node, ?MODULE, write, [Records]).

write(Records)->
    mnesia:transaction(fun()-> lists:foreach(fun(Record)-> mnesia:write(Record) end, Records) end).

test_values(Tag, TableContent) ->
    TagAsList = atom_to_list(Tag),
    lists:foreach(fun(#?TABLE{key = K, value = V}) ->
        ExpectedV = list_to_atom(TagAsList ++ integer_to_list(K)),
        ?assertEqual(ExpectedV, V)
    end, TableContent).
