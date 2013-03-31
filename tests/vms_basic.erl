-module(vms_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0]).

-define(UUID1, <<"TEST1">>).
-define(UUID2, <<"TEST2">>).

-define(HV, <<"HYPERVISOR">>).

confirm() ->
    ?assertEqual(ok, libsniffle:start()),
    lager:info("Deploy node to test command line"),
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    F = fun(_) ->
                Servers = libsniffle:servers(),
                lager:debug("Looking for sniffle servers found: ~p", [Servers]),
                [] =/= Servers
        end,
    ?assertEqual(ok, rt:wait_until(Node, F)),
    empty_test(Node),
    register_test(Node, ?UUID1),
    list_test(Node),
    read_test(Node),
    set3_test(Node),
    set3_read_test(Node),
    set2_test(Node),
    set2_read_test(Node),
    register_test(Node, ?UUID2),
    list2_test(Node),
    unregister_test(Node, ?UUID2),
    list_test(Node),
    pass.

empty_test(_Node) ->
    lager:debug("First test we have those servers: ~p", [libsniffle:servers()]),
    ?assertEqual({ok, []}, libsniffle:vm_list()),
    ok.

register_test(_Node, UUID) ->
    ?assertEqual(ok, libsniffle:vm_register(UUID, ?HV)),
    ok.

unregister_test(_Node, UUID) ->
    ?assertEqual(ok, libsniffle:vm_unregister(UUID)),
    ok.

list_test(_Node) ->
    ?assertEqual({ok, [?UUID1]}, libsniffle:vm_list()),
    ok.

list2_test(_Node) ->
    {ok, R} = libsniffle:vm_list(),
    ?assertEqual([?UUID1, ?UUID2], lists:sort(R)),
    ok.

read_test(_Node) ->
    ?assertEqual({ok, [{<<"hypervisor">>,?HV},{<<"uuid">>,?UUID1}]},libsniffle:vm_get(?UUID1)),
    ok.

set3_test(_Node) ->
    ?assertEqual(ok, libsniffle:vm_set(?UUID1, <<"key">>, 1)),
    ok.

set3_read_test(_Node) ->
    ?assertEqual({ok, [{<<"hypervisor">>,?HV},
                       {<<"key">>, 1},
                       {<<"uuid">>,?UUID1}]},
                 libsniffle:vm_get(?UUID1)),
    ok.

set2_test(_Node) ->
    ?assertEqual(ok, libsniffle:vm_set(?UUID1, [{<<"key">>, 2}])),
    ?assertEqual(ok, libsniffle:vm_set(?UUID1, [{<<"key1">>, 1}, {<<"key2">>, 2}])),
    ok.

set2_read_test(_Node) ->
    ?assertEqual({ok, [{<<"hypervisor">>,?HV},
                       {<<"key">>, 2},
                       {<<"key1">>, 1},
                       {<<"key2">>, 2},
                       {<<"uuid">>,?UUID1}]},
                 libsniffle:vm_get(?UUID1)),
    ok.
