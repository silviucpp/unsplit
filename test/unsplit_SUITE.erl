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
-define(DISCONNECT_TIME, 2000).
-define(UNSPLIT_TIMEOUT, 2000).
-record(?TABLE, {key, modified=erlang:timestamp(), value}).

all() -> [
    split1
].

init_per_suite(Conf) ->
    net_kernel:start(['test@127.0.0.1', longnames]),
    erl_boot_server:start(['127.0.0.1']),
    application:ensure_all_started(unsplit),

    Nodes = ct:get_config(nodes, ?NODES),
    DisconnectTime = ct:get_config(disconnect_time, ?DISCONNECT_TIME),
    UnsplitTimeout = ct:get_config(unsplit_timeout, ?UNSPLIT_TIMEOUT),
    Host = '127.0.0.1',

    StartNode = fun(Node)->
        ct:print("starting node ~p, on host ~p",[Node, Host]),
        {ok, StartedNode} = slave:start(Host, Node, "-pa ../../_build/test/lib/*/ebin -pa ../../_build/test/lib/unsplit/test -kernel prevent_overlapping_partitions false -kernel dist_auto_connect once -config ../../test/sys.config -loader inet -hosts 127.0.0.1 -setcookie " ++ atom_to_list(erlang:get_cookie())),
        StartedNode
    end,

    NodeNames = lists:map(StartNode, Nodes),

    ct:print("started things: ~p",[NodeNames]),

    [
        {disconnect_time, DisconnectTime},
        {unsplit_timeout, UnsplitTimeout},
        {nodes, NodeNames}|Conf
    ].

end_per_suite(_Conf) ->
    Nodes = ct:get_config(nodes, ?NODES),
    StopNode = fun(Node)-> ok = slave:stop(Node) end,
    lists:map(StopNode, Nodes),
    ok.

init_per_testcase(Case, Conf) ->
    ct:print("Test case ~p started ...", [Case]),
    init_nodes(get_conf(nodes, Conf)),
    Conf.

end_per_testcase(Case, Conf) ->
    ct:print("Test case ~p finished ...", [Case]),
    terminate_nodes(get_conf(nodes, Conf)),
    Conf.

split1(Conf)->
    DisconnectTime = get_conf(disconnect_time, Conf),
    UnsplitTimeout = get_conf(unsplit_timeout, Conf),
    Nodes = [M, S|_Rest] = get_conf(nodes, Conf),

    ct:print("Initial tables ..."),
    {MasterContent0, SlaveContent0} = print_tables(Nodes, ?TABLE),
    ?assertEqual(MasterContent0, SlaveContent0),

    ct:print("inserting records ..."),
    ?assertEqual({atomic, ok}, write(M, [#?TABLE{key=1, value=a}, #?TABLE{key=2, value=a}])),
    {MasterContent1, SlaveContent1} = print_tables(Nodes, ?TABLE),
    ?assertEqual(MasterContent1, SlaveContent1),

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

get_conf(Key, Conf)->
    proplists:get_value(Key, Conf).

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
        {attributes, [key, modified, value]},
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
    Trans = fun()->
        lists:foreach(fun(Record)-> mnesia:write(Record) end, Records)
    end,
    mnesia:transaction(Trans).
