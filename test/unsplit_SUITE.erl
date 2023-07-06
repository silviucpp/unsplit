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

all() -> [
    split1
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

init_per_testcase(Case, Conf) ->
    ct:print("Test case ~p started ...", [Case]),
    init_nodes(proplists:get_value(nodes, Conf)),
    Conf.

end_per_testcase(Case, Conf) ->
    ct:print("Test case ~p finished ...", [Case]),
    terminate_nodes(proplists:get_value(nodes, Conf)),
    Conf.

split1(Conf)->
    DisconnectTime = proplists:get_value(disconnect_time, Conf),
    UnsplitTimeout = proplists:get_value(unsplit_timeout, Conf),
    Nodes = [M, S|_Rest] = proplists:get_value(nodes, Conf),

    ct:print("Initial tables ..."),
    {MasterContent0, SlaveContent0} = print_tables(Nodes, ?TABLE),
    ?assertEqual([], MasterContent0),
    ?assertEqual([], SlaveContent0),

    ct:print("inserting records ..."),
    ?assertEqual({atomic, ok}, write(M, [#?TABLE{key=1, value=a}, #?TABLE{key=2, value=a}])),
    {MasterContent1, SlaveContent1} = print_tables(Nodes, ?TABLE),
    ?assertEqual(MasterContent1, SlaveContent1),
    ?assertEqual(2, length(MasterContent1)),

    ct:print("disconnecting node ~p from ~p", [M, S]),
    disconnect(M, S),

    ct:print("updating records on one node, while the other one is disconnected ..."),
    timer:sleep(DisconnectTime),

    ?assertEqual({atomic, ok}, write(M, [#?TABLE{key=1, modified=erlang:timestamp() ,value=c}, #?TABLE{key=2, modified=erlang:timestamp(), value=d}])),
    {MasterContent2, SlaveContent2} = print_tables(Nodes, ?TABLE),
    ?assertNotEqual(MasterContent2, SlaveContent2),

    ct:print("reconnecting nodes ..."),

    connect(M, S),
    timer:sleep(UnsplitTimeout),
    {MasterContent3, SlaveContent3} = print_tables(Nodes, ?TABLE),
    ?assertEqual(MasterContent3, SlaveContent3),
    true.

% internals

print_tables([M,S|_], Table)->
    MasterTable = rpc:call(M, ets, tab2list, [Table]),
    SlaveTable = rpc:call(S, ets, tab2list, [Table]),
    ct:print("master table = ~p", [MasterTable]),
    ct:print("slave table = ~p", [SlaveTable]),
    {MasterTable, SlaveTable}.

terminate_nodes(Nodes)->
    rpc:multicall(Nodes, application, stop, [unsplit]),
    rpc:multicall(Nodes, mnesia, stop, []),
    rpc:call(hd(Nodes), mnesia, delete_schema, [Nodes]).

init_nodes(Nodes)->
    FirstNode = hd(Nodes),
    rpc:call(FirstNode, mnesia, create_schema, [Nodes]),

    {[R1|_],[]} = rpc:multicall(Nodes, application, ensure_all_started, [unsplit]),
    ?assertMatch({ok, _}, R1),

    rpc:call(FirstNode, mnesia, create_table, [?TABLE, [
        {ram_copies, Nodes},
        {attributes, record_info(fields, ?TABLE)},
        {user_properties, [
            {unsplit_method, {unsplit_lib, last_modified, []}}]
        }]
    ]).

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
