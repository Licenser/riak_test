-module(vms_basic).
-include_lib("eunit/include/eunit.hrl").

-behavior(riak_test).
-export([confirm/0,
         full_test/1,
         register_test/2]).

-define(UUID1, <<"TEST1">>).
-define(UUID2, <<"TEST2">>).

-define(HV, <<"HYPERVISOR">>).

confirm() ->
    lager:info("Deploy node to test command line"),
    [Node] = rt:deploy_nodes(1),
    ?assertEqual(ok, rt:wait_until_nodes_ready([Node])),
    full_test(Node),
    pass.


%%
%% This test runs through creating, modyfing and deleting a VM
%%
full_test(Node) ->
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
    list_test(Node).

empty_test(Node) ->
    ?assertEqual({ok, []}, rt_sniffle:vm_list(Node)),
    ok.

register_test(Node, UUID) ->
    ?assertEqual(ok, rt_sniffle:vm_register(Node, UUID, ?HV)),
    ok.

unregister_test(Node, UUID) ->
    ?assertEqual(ok, rt_sniffle:vm_unregister(Node, UUID)),
    ok.

list_test(Node) ->
    ?assertEqual({ok, [?UUID1]}, rt_sniffle:vm_list(Node)),
    ok.

list2_test(Node) ->
    {ok, R} = rt_sniffle:vm_list(Node),
    ?assertEqual([?UUID1, ?UUID2], lists:sort(R)),
    ok.

read_test(Node) ->
    ?assertEqual({ok, [{<<"hypervisor">>,?HV},{<<"uuid">>,?UUID1}]},rt_sniffle:vm_get(Node, ?UUID1)),
    ok.

set3_test(Node) ->
    ?assertEqual(ok, rt_sniffle:vm_set(Node, ?UUID1, <<"key">>, 1)),
    ok.

set3_read_test(Node) ->
    ?assertEqual({ok, [{<<"hypervisor">>,?HV},
                       {<<"key">>, 1},
                       {<<"uuid">>,?UUID1}]},
                 rt_sniffle:vm_get(Node, ?UUID1)),
    ok.

set2_test(Node) ->
    ?assertEqual(ok, rt_sniffle:vm_set(Node, ?UUID1, [{<<"key">>, 2}])),
    ?assertEqual(ok, rt_sniffle:vm_set(Node, ?UUID1, [{<<"key1">>, 1}, {<<"key2">>, 2}])),
    ok.

set2_read_test(Node) ->
    ?assertEqual({ok, [{<<"hypervisor">>,?HV},
                       {<<"key">>, 2},
                       {<<"key1">>, 1},
                       {<<"key2">>, 2},
                       {<<"uuid">>,?UUID1}]},
                 rt_sniffle:vm_get(Node, ?UUID1)),
    ok.
