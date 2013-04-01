-module(rt_sniffle).


-export([node_endpoing/1, call/2]).


-export([
         vm_register/3,
         vm_unregister/2,
         vm_update/4,
         vm_set/3,
         vm_set/4,
         vm_log/3,
         vm_snapshot/3,
         vm_delete_snapshot/3,
         vm_rollback_snapshot/3,
         vm_list/1,
         vm_list/2,
         vm_get/2,
         vm_start/2,
         vm_stop/2,
         vm_reboot/2,
         vm_stop/3,
         vm_reboot/3,
         vm_delete/2
        ]).


node_endpoing(Node) ->
    {ok, IP} = rpc:call(Node, application, get_env, [mdns_server_lib, ip]),
    {ok, Port} = rpc:call(Node, application, get_env, [mdns_server_lib, port]),
    {IP, Port}.





call(Node, Msg) ->
    {IP, Port} = node_endpoing(Node),
    lager:debug("~s:~p <- ~p", [IP, Port, Msg]),
    {ok, Socket} = gen_tcp:connect(IP, Port, [binary, {active,false}, {packet,4}], 100),
    ok = gen_tcp:send(Socket, term_to_binary(Msg)),
    {ok, Repl} = gen_tcp:recv(Socket, 0),
    {reply, Res} = binary_to_term(Repl),
    lager:debug("~s:~p -> ~p", [IP, Port, Res]),
    gen_tcp:close(Socket),
    Res.


%%%===================================================================
%%% VM Functions
%%%===================================================================

vm_register(Node, VM, Hypervisor) when
      is_binary(VM),
      is_binary(Hypervisor) ->
    call(Node, {vm, register, VM, Hypervisor}).

vm_unregister(Node, VM) when
      is_binary(VM) ->
    call(Node, {vm, unregister, VM}).


vm_start(Node, VM) when
      is_binary(VM) ->
    call(Node, {vm, start, VM}).

vm_stop(Node, VM) when is_binary(VM) ->
    vm_stop(Node, VM, []).

vm_stop(Node, VM, []) when
      is_binary(VM)->
    call(Node, {vm, stop, VM});

vm_stop(Node, VM, [force]) when
      is_binary(VM)->
    call(Node, {vm, stop, force, VM}).

vm_get(Node, VM) when
      is_binary(VM) ->
    call(Node, {vm, get, VM}).

vm_reboot(Node, VM) when
      is_binary(VM) ->
    vm_reboot(Node, VM, []).

vm_reboot(Node, VM, []) when
      is_binary(VM) ->
    call(Node, {vm, reboot, VM});

vm_reboot(Node, VM, [force]) when
      is_binary(VM) ->
    call(Node, {vm, reboot, force, VM}).

vm_delete(Node, VM) when
      is_binary(VM) ->
    call(Node, {vm, delete, VM}).

vm_update(Node, VM, Package, Config) when
      is_binary(VM),
      is_list(Config) ->
    call(Node, {vm, update, VM, Package, Config}).

vm_set(Node, VM, Attribute, Value) when
      is_binary(VM) ->
    call(Node, {vm, set, VM, Attribute, Value}).

vm_set(Node, VM, Attributes) when
      is_binary(VM) ->
    call(Node, {vm, set, VM, [{K, V} || {K, V} <- Attributes,
                                        is_binary(K)]}).
vm_log(Node, VM, Log) ->
    call(Node, {vm, log, VM, Log}).

vm_snapshot(Node, VM, Comment) ->
    call(Node, {vm, snapshot, VM, Comment}).

vm_delete_snapshot(Node, VM, UUID) ->
    call(Node, {vm, snapshot, delete, VM, UUID}).


vm_rollback_snapshot(Node, VM, UUID) ->
    call(Node, {vm, snapshot, rollback, VM, UUID}).

vm_list(Node) ->
    call(Node, {vm, list}).

vm_list(Node, Reqs) ->
    call(Node, {vm, list, Reqs}).
