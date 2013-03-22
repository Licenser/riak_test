-module(rt_cascading).
-compile(export_all).
-behavior(riak_test).

-include_lib("eunit/include/eunit.hrl").

-export([confirm/0]).

% cluster_mgr port = 10006 + 10n where n is devN

confirm() ->
    case eunit:test(?MODULE, [verbose]) of
        ok ->
            pass;
        error ->
            % at the time this is written, the return value isn't acutally
            % checked, the only way to fail is to crash the process.
            % i leave the fail here in hopes a future version will actually
            % do what the documentation says.
            exit(error),
            fail
    end.

-record(simple_state, {
    beginning = [] :: [node()],
    middle = [] :: [node()],
    ending = [] :: [node()]
}).

simple_test_() ->
    {timeout, 60000, {setup, fun() ->
        Conf = conf(),
        [BeginNode, MiddleNode, EndNode] = Nodes = rt:deploy_nodes(3, Conf),
        repl_util:make_cluster([BeginNode]),
        repl_util:make_cluster([MiddleNode]),
        repl_util:make_cluster([EndNode]),
        repl_util:name_cluster(BeginNode, "beginning"),
        [repl_util:wait_until_is_leader(N) || N <- Nodes],
        repl_util:name_cluster(MiddleNode, "middle"),
        repl_util:name_cluster(EndNode, "end"),
        #simple_state{beginning = BeginNode, middle = MiddleNode,
            ending = EndNode}
    end,
    fun(State) ->
        Nodes = [State#simple_state.beginning, State#simple_state.middle,
            State#simple_state.ending],
        rt:clean_cluster(Nodes)
    end,
    fun(State) -> [

        {"connecting Beginning to Middle", fun() ->
            repl_util:connect_cluster(State#simple_state.beginning, "127.0.0.1", 10026),
            repl_util:enable_realtime(State#simple_state.beginning, "middle"),
            repl_util:start_realtime(State#simple_state.beginning, "middle")
        end},

        {"connection Middle to End", fun() ->
            repl_util:connect_cluster(State#simple_state.middle, "127.0.0.1", 10036),
            repl_util:enable_realtime(State#simple_state.middle, "end"),
            repl_util:start_realtime(State#simple_state.middle, "end")
        end},

        {"cascade a put from beginning down to ending", fun() ->
            BeginningClient = rt:pbc(State#simple_state.beginning),
            Bin = <<"cascading realtime">>,
            Obj = riakc_obj:new(<<"objects">>, Bin, Bin),
            riakc_pb_socket:put(BeginningClient, Obj, [{w,1}]),
            riakc_pb_socket:stop(BeginningClient),
            ?assertEqual(Bin, maybe_eventually_exists(State#simple_state.middle, <<"objects">>, Bin)),
            ?assertEqual(Bin, maybe_eventually_exists(State#simple_state.ending, <<"objects">>, Bin))
        end}

    ] end}}.

big_circle_test_() ->
    {timeout, 10000, {setup, fun() ->
        Conf = conf(),
        Nodes = rt:deploy_nodes(6, Conf),
        [repl_util:make_cluster([N]) || N <- Nodes],
        [repl_util:wait_until_is_leader(N) || N <- Nodes],
        Names = ["1", "2", "3", "4", "5", "6"],
        [repl_util:name_cluster(Node, Name) || {Node, Name} <- lists:zip(Nodes, Names)],
        [NameHd | NameTail] = Names,
        ConnectTo = NameTail ++ [NameHd],
        Connect = fun({Node, ConnectToName}) ->
            Port = list_to_integer("100" ++ ConnectToName ++ "6"),
            connect_rt(Node, Port, ConnectToName)
        end,
        Res = lists:map(Connect, lists:zip(Nodes, ConnectTo)),
        ?debugFmt("der res: ~p", [Res]),
        Nodes
    end,
    fun(Nodes) ->
        rt:clean_cluster(Nodes)
    end,
    fun(Nodes) -> [

        {"circle it", timeout, 10000, fun() ->
            [One | _] = Nodes,
            C = rt:pbc(One),
            Bin = <<"goober">>,
            Bucket = <<"objects">>,
            Obj = riakc_obj:new(Bucket, Bin, Bin),
            riakc_pb_socket:put(C, Obj, [{w,1}]),
            riakc_pb_socket:stop(C),
            [begin
                ?debugFmt("Checking ~p", [Node]),
                ?assertEqual(Bin, maybe_eventually_exists(Node, Bucket, Bin))
            end || Node <- Nodes]
        end},

        {"2 way repl, and circle it", timeout, 10000, fun() ->
            ConnectTo = ["6", "1", "2", "3", "4", "5"],
            Connect = fun({Node, ConnectToName}) ->
                Port = list_to_integer("100" ++ ConnectToName ++ "6"),
                connect_rt(Node, Port, ConnectToName)
            end,
            lists:map(Connect, lists:zip(Nodes, ConnectTo)),
            C = rt:pbc(hd(Nodes)),
            Bin = <<"2 way repl">>,
            Bucket = <<"objects">>,
            Obj = riakc_obj:new(Bucket, Bin, Bin),
            riakc_pb_socket:put(C, Obj, [{w,1}]),
            lists:map(fun(N) ->
                ?debugFmt("Testing ~p", [N]),
                ?assertEqual(Bin, maybe_eventually_exists(N, Bucket, Bin))
            end, Nodes)
            % there will be duplicate writes, but due to size of the circle,
            % there's not going to be a lot. Also, it's very difficult to
            % determine when/where a duplicate may start/occur.
            % a full breakdown:
            % "1" forwards to "2" and "6", noting its local forwards.
            % so we have two flows going. Assuming both sides flow at the same
            % rate:
            %     1
            %    / \
            %   6   2:   6 has [1, 2, 6]; 2 has [1, 2, 6]
            %   5   3:   5 has [1,2,5,6]; 3 has [1,2,3,6]
            %   4   4:   4 has [1,2,4,5,6]; 4 has [1,2,3,4,6] ! double write
            %   3   5:   3 has [1,2,3,4,5,6]; 5 has [1,2,3,4,5,6] ! double write
            %
            % let's explore the flow with 10 clusters:
            %      1
            %     / \
            %    10  2  10: [1,2,10]; 2: [1,2,10]
            %    9   3  9: [1,2,9,10]; 3: [1,2,3,10]
            %    8   4  8: [1,2,8,9,10]; 4: [1,2,3,4,10]
            %    7   5  7: [1,2,7,8,9,10]; 5: [1,2,3,4,5,10]
            %    6   6  6: [1,2,6,7,8,9,10]; 6: [1,2,3,4,5,6,10] !!
            %    5   7  5: [1,2,5..10]; 7: [1..7,10] !!
            %    4   8  4: [1,2,4..10]; 8: [1..8,10] !!
            %    3   9  3: [1..10]; 9: [1..10] !!
            % so, by adding 4 clusters, we've added 2 overlaps.
            % best guess based on what's above is:
            %  NumDuplicateWrites = ceil(NumClusters/2 - 1.5)
        end}

    ] end}}.

circle_test_() ->
    {timeout, 10000, {setup, fun() ->
        Conf = conf(),
        [One, Two, Three] = Nodes = rt:deploy_nodes(3, Conf),
        [repl_util:make_cluster([N]) || N <- Nodes],
        [repl_util:wait_until_is_leader(N) || N <- Nodes],
        Names = ["one", "two", "three"],
        [repl_util:name_cluster(Node, Name) || {Node, Name} <- lists:zip(Nodes, Names)],

        Connections = [
            {One, 10026, "two"},
            {Two, 10036, "three"},
            {Three, 10016, "one"}
        ],
        lists:map(fun({Node, Port, Name}) ->
            connect_rt(Node, Port, Name)
        end, Connections),
        Nodes
    end,
    fun(Nodes) ->
        rt:clean_cluster(Nodes)
    end,
    fun(Nodes) -> [

        {"cascade all the way to the other end, but no further", timeout, 6000, fun() ->
            Client = rt:pbc(hd(Nodes)),
            Bin = <<"cascading">>,
            Obj = riakc_obj:new(<<"objects">>, Bin, Bin),
            riakc_pb_socket:put(Client, Obj, [{w,1}]),
            ?assertEqual(Bin, maybe_eventually_exists(lists:last(Nodes), <<"objects">>, Bin)),
            % we want to ensure there's not a cascade back to the beginning, so
            % there's no event we can properly wait for. All we can do is wait
            % and make sure we didn't update/write the object.
            timer:sleep(1000),
            Status = rpc:call(hd(Nodes), riak_repl2_rt, status, []),
            [SinkData] = proplists:get_value(sinks, Status, [[]]),
            ?assertEqual(undefined, proplists:get_value(expect_seq, SinkData))
        end},

        {"cascade starting at a different point", timeout, 6000, fun() ->
            [One, Two | _] = Nodes,
            Client = rt:pbc(Two),
            Bin = <<"start_at_two">>,
            Obj = riakc_obj:new(<<"objects">>, Bin, Bin),
            riakc_pb_socket:put(Client, Obj, [{w,1}]),
            ?assertEqual(Bin, maybe_eventually_exists(One, <<"objects">>, Bin)),
            timer:sleep(1000),
            Status = rpc:call(Two, riak_repl2_rt, status, []),
            [SinkData] = proplists:get_value(sinks, Status, [[]]),
            ?assertEqual(2, proplists:get_value(expect_seq, SinkData))
        end}

    ] end}}.

pyramid_test_() ->
    {timeout, 60000, {setup, fun() ->
        Conf = conf(),
        [Top, Left, _Left2, Right, _Right2] = Nodes = rt:deploy_nodes(5, Conf),
        [repl_util:make_cluster([N]) || N <- Nodes],
        [repl_util:wait_until_is_leader(N) || N <- Nodes],
        Names = ["top", "left", "left2", "right", "right2"],
        [repl_util:name_cluster(Node, Name) || {Node, Name} <- lists:zip(Nodes, Names)],
        connect_rt(Top, 10026, "left"),
        connect_rt(Left, 10036, "left2"),
        connect_rt(Top, 10046, "right"),
        connect_rt(Right, 10056, "right2"),
        Nodes
    end,
    fun(Nodes) ->
        rt:clean_cluster(Nodes)
    end,
    fun(Nodes) -> [

        {"Cascade to both kids", fun() ->
            [Top | _] = Nodes,
            Client = rt:pbc(Top),
            Bucket = <<"objects">>,
            Bin = <<"pyramid_top">>,
            Obj = riakc_obj:new(Bucket, Bin, Bin),
            riakc_pb_socket:put(Client, Obj, [{w,1}]),
            lists:map(fun(N) ->
                ?debugFmt("Checking ~p", [N]),
                ?assertEqual(Bin, maybe_eventually_exists(N, Bucket, Bin))
            end, Nodes)
        end}

    ] end}}.

diamond_test_() ->
    % A pretty cluster of clusters:
    %                      +-----+
    %     +--------------->| top |
    %     | loop added     +-----+
    %     |               /       \
    %     |              V         V
    %     |    +---------+         +----------+
    %     ^    | midleft |         | midright |
    %     |    +---------+         +----------+
    %     |               \       /
    %     |                V     V
    %     |               +--------+
    %     +-------<-------| bottom |
    %                     +--------+
    {timeout, 60000, {setup, fun() ->
        Conf = conf(),
        [Top, MidLeft, MidRight, _Bottom] = Nodes = rt:deploy_nodes(4, Conf),
        [repl_util:make_cluster([N]) || N <- Nodes],
        Names = ["top", "midleft", "midright", "bottom"],
        [repl_util:name_cluster(Node, Name) || {Node, Name} <- lists:zip(Nodes, Names)],
        [repl_util:wait_until_is_leader(N) || N <- Nodes],
        connect_rt(Top, 10026, "midleft"),
        connect_rt(MidLeft, 10046, "bottom"),
        connect_rt(MidRight, 10046, "bottom"),
        connect_rt(Top, 10036, "midright"),
        Nodes
    end,
    fun(Nodes) ->
        rt:clean_cluster(Nodes)
    end,
    fun(Nodes) -> [

        {"unfortunate double write", timeout, 10000, fun() ->
            [Top, MidLeft, MidRight, Bottom] = Nodes,
            Client = rt:pbc(Top),
            Bin = <<"start_at_top">>,
            Obj = riakc_obj:new(<<"objects">>, Bin, Bin),
            riakc_pb_socket:put(Client, Obj, [{w,1}]),
            timer:sleep(100000),
            ?assertEqual(Bin, maybe_eventually_exists(MidLeft, <<"objects">>, Bin)),
            ?assertEqual(Bin, maybe_eventually_exists(MidRight, <<"objects">>, Bin)),
            ?assertEqual(Bin, maybe_eventually_exists(Bottom, <<"objects">>, Bin)),
            %timer:sleep(1000),
            Status = rpc:call(Bottom, riak_repl2_rt, status, []),
            [SinkOne, SinkTwo] = proplists:get_value(sinks, Status, [[], []]),
            ?assertEqual(proplists:get_value(expect_seq, SinkOne), proplists:get_value(expect_seq, SinkTwo))
        end},

        {"connect bottom to top", fun() ->
            [Top, _MidLeft, _MidRight, Bottom] = Nodes,
            connect_rt(Bottom, 10016, "top"),
            WaitFun = fun(N) ->
                Status = rpc:call(N, riak_repl2_rt, status, []),
                Sinks = proplists:get_value(sinks, Status, []),
                length(Sinks) == 1
            end,
            ?assertEqual(ok, rt:wait_until(Top, WaitFun))
        end},

        {"start at midright", timeout, 10000, fun() ->
            [Top, MidLeft, MidRight, Bottom] = Nodes,
            % To ensure a write doesn't happen to MidRight when it originated
            % on midright, we're going to compare the expect_seq before and
            % after.
            Status = rpc:call(MidRight, riak_repl2_rt, status, []),
            [Sink] = proplists:get_value(sinks, Status, [[]]),
            ExpectSeq = proplists:get_value(expect_seq, Sink),

            Client = rt:pbc(MidRight),
            Bin = <<"start at midright">>,
            Bucket = <<"objects">>,
            Obj = riakc_obj:new(Bucket, Bin, Bin),
            riakc_pb_socket:put(Client, Obj, [{w,1}]),
            [begin
                ?debugFmt("Checking ~p", [N]),
                ?assertEqual(Bin, maybe_eventually_exists(N, Bucket, Bin))
            end || N <- [Bottom, Top, MidLeft]],
            %?assertEqual(Bin, maybe_eventually_exists(MidLeft, Bucket, Bin)),

            Status2 = rpc:call(MidRight, riak_repl2_rt, status, []),
            [Sink2] = proplists:get_value(sinks, Status2, [[]]),
            GotSeq = proplists:get_value(expect_seq, Sink2),
            ?assertEqual(ExpectSeq, GotSeq)
        end}

    ] end}}.

circle_and_spurs_test_() ->
    %                        +------------+
    %                        | north_spur |
    %                        +------------+
    %                               ^
    %                               |
    %                           +-------+
    %                     +---> | north | ---+
    %                     |     +-------+    |
    %                     |                  V
    % +-----------+    +------+           +------+    +-----------+
    % | west_spur | <- | west | <-------- | east | -> | east_spur |
    % +-----------+    +------+           +------+    +-----------+
    {timeout, 60000, {setup, fun() ->
        Conf = conf(),
        [North, East, West, _NorthSpur, _EastSpur, _WestSpur] = Nodes = rt:deploy_nodes(6, Conf),
        [repl_util:make_cluster([N]) || N <- Nodes],
        Names = ["north", "east", "west", "north_spur", "east_spur", "west_spur"],
        [repl_util:name_cluster(Node, Name) || {Node, Name} <- lists:zip(Nodes, Names)],
        [repl_util:wait_until_is_leader(N) || N <- Nodes],
        connect_rt(North, 10026, "east"),
        connect_rt(East, 10036, "west"),
        connect_rt(West, 10016, "north"),
        connect_rt(North, 10046, "north_spur"),
        connect_rt(East, 10056, "east_spur"),
        connect_rt(West, 10066, "west_spur"),
        Nodes
    end,
    fun(Nodes) ->
        rt:clean_cluster(Nodes)
    end,
    fun(Nodes) -> [

        {"start at north", fun() ->
            [North | _Rest] = Nodes,
            Client = rt:pbc(North),
            Bin = <<"start at north">>,
            Bucket = <<"objects">>,
            Obj = riakc_obj:new(Bucket, Bin, Bin),
            riakc_pb_socket:put(Client, Obj, [{w,1}]),
            [begin
                ?debugFmt("Checking ~p", [N]),
                ?assertEqual(Bin, maybe_eventually_exists(N, Bucket, Bin))
            end || N <- Nodes, N =/= North]
        end},

        {"Start at west", fun() ->
            [_North, _East, West | _Rest] = Nodes,
            Client = rt:pbc(West),
            Bin = <<"start at west">>,
            Bucket = <<"objects">>,
            Obj = riakc_obj:new(Bucket, Bin, Bin),
            riakc_pb_socket:put(Client, Obj, [{w,1}]),
            [begin
                ?debugFmt("Checking ~p", [N]),
                ?assertEqual(Bin, maybe_eventually_exists(N, Bucket, Bin))
            end || N <- Nodes, N =/= West]
        end},

        {"spurs don't replicate back", timeout, 30000, fun() ->
            [_North, _East, _West, NorthSpur | _Rest] = Nodes,
            Client = rt:pbc(NorthSpur),
            Bin = <<"start at north_spur">>,
            Bucket = <<"objects">>,
            Obj = riakc_obj:new(Bucket, Bin, Bin),
            riakc_pb_socket:put(Client, Obj, [{w,1}]),
            [begin
                ?debugFmt("Checking ~p", [N]),
                ?assertEqual({error, notfound}, maybe_eventually_exists(N, Bucket, Bin))
            end || N <- Nodes, N =/= NorthSpur]
        end}

    ] end}}.

%% =====
%% utility functions for teh happy
%% ====

conf() ->
    [{riak_repl, [
        {fullsync_on_connect, false},
        {fullsync_interval, disabled},
        {diff_batch_size, 10}
    ]}].

connect_rt(SourceNode, SinkPort, SinkName) ->
    repl_util:connect_cluster(SourceNode, "127.0.0.1", SinkPort),
    repl_util:enable_realtime(SourceNode, SinkName),
    repl_util:start_realtime(SourceNode, SinkName).

maybe_eventually_exists(Node, Bucket, Key) ->
    maybe_eventually_exists(Node, Bucket, Key, 10, 1000).

maybe_eventually_exists(Node, Bucket, Key, MaxTries, WaitMs) ->
    Pid = rt:pbc(Node),
    Got = riakc_pb_socket:get(Pid, Bucket, Key),
    maybe_eventually_exists(Got, Pid, Node, Bucket, Key, MaxTries - 1, WaitMs).

maybe_eventually_exists({error, notfound}, Pid, Node, Bucket, Key, MaxTries, WaitMs) when MaxTries > 0 ->
    timer:sleep(WaitMs),
    Got = riakc_pb_socket:get(Pid, Bucket, Key),
    maybe_eventually_exists(Got, Pid, Node, Bucket, Key, MaxTries - 1, WaitMs);

maybe_eventually_exists({ok, RiakObj}, _Pid, _Node, _Bucket, _Key, _MaxTries, _WaitMs) ->
    riakc_obj:get_value(RiakObj);

maybe_eventually_exists(Got, _Pid, _Node, _Bucket, _Key, _MaxTries, _WaitMs) ->
    Got.

wait_for_rt_started(Node, ToName) ->
    Fun = fun(_) ->
        Status = rpc:call(Node, riak_repl2_rt, status, []),
        Started = proplists:get_value(started, Status, []),
        lists:member(ToName, Started)
    end,
    rt:wait_until(Node, Fun).
