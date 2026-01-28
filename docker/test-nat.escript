#!/usr/bin/env escript
%% -*- erlang -*-

-mode(compile).

main(_Args) ->
    io:format("=== erlang-nat Docker Test ===~n~n"),

    %% Add code paths (escript doesn't expand wildcards)
    LibDir = "_build/default/lib",
    case file:list_dir(LibDir) of
        {ok, Libs} ->
            lists:foreach(fun(Lib) ->
                Ebin = filename:join([LibDir, Lib, "ebin"]),
                code:add_patha(Ebin)
            end, Libs);
        {error, _} ->
            io:format("Warning: ~s not found~n", [LibDir])
    end,

    %% Start required applications
    io:format("Starting applications...~n"),
    {ok, _} = application:ensure_all_started(nat),
    timer:sleep(500),

    %% Test 1: Discovery
    io:format("~n[Test 1] Discovering NAT gateway...~n"),
    case nat:discover() of
        ok ->
            io:format("  OK: NAT gateway discovered~n"),
            {ok, Ctx} = nat:get_context(),
            io:format("  Context: ~p~n", [Ctx]);
        no_nat ->
            io:format("  FAIL: No NAT gateway found~n"),
            halt(1);
        {error, Reason} ->
            io:format("  FAIL: ~p~n", [Reason]),
            halt(1)
    end,

    %% Test 2: Get addresses
    io:format("~n[Test 2] Getting addresses...~n"),
    {ok, ExtAddr} = nat:get_external_address(),
    {ok, IntAddr} = nat:get_internal_address(),
    {ok, DevAddr} = nat:get_device_address(),
    io:format("  External IP: ~s~n", [ExtAddr]),
    io:format("  Internal IP: ~s~n", [IntAddr]),
    io:format("  Device IP:   ~s~n", [DevAddr]),

    %% Test 3: Add port mapping
    io:format("~n[Test 3] Adding TCP port mapping (8080 -> 8080)...~n"),
    case nat:add_port_mapping(tcp, 8080, 8080, 3600) of
        {ok, {Since, IntPort, ExtPort, Lifetime}} ->
            io:format("  OK: Mapping created~n"),
            io:format("  Since: ~p, Internal: ~p, External: ~p, Lifetime: ~ps~n",
                     [Since, IntPort, ExtPort, Lifetime]);
        {error, Reason2} ->
            io:format("  FAIL: ~p~n", [Reason2]),
            halt(1)
    end,

    %% Test 4: Add UDP port mapping
    io:format("~n[Test 4] Adding UDP port mapping (9000 -> 9000)...~n"),
    case nat:add_port_mapping(udp, 9000, 9000, 3600) of
        {ok, {Since2, IntPort2, ExtPort2, Lifetime2}} ->
            io:format("  OK: Mapping created~n"),
            io:format("  Since: ~p, Internal: ~p, External: ~p, Lifetime: ~ps~n",
                     [Since2, IntPort2, ExtPort2, Lifetime2]);
        {error, Reason3} ->
            io:format("  FAIL: ~p~n", [Reason3]),
            halt(1)
    end,

    %% Test 5: List mappings
    io:format("~n[Test 5] Listing managed mappings...~n"),
    Mappings = nat:list_mappings(),
    io:format("  Found ~p mapping(s):~n", [length(Mappings)]),
    lists:foreach(fun({Proto, IntP, ExtP, Expires}) ->
        io:format("    ~p ~p -> ~p (expires: ~p)~n", [Proto, IntP, ExtP, Expires])
    end, Mappings),

    %% Test 6: Event registration
    io:format("~n[Test 6] Testing event registration...~n"),
    ok = nat:reg_pid(self()),
    io:format("  OK: Registered for events~n"),
    ok = nat:unreg_pid(self()),
    io:format("  OK: Unregistered~n"),

    %% Test 7: Delete mappings
    io:format("~n[Test 7] Deleting port mappings...~n"),
    ok = nat:delete_port_mapping(tcp, 8080, 8080),
    io:format("  OK: TCP 8080 deleted~n"),
    ok = nat:delete_port_mapping(udp, 9000, 9000),
    io:format("  OK: UDP 9000 deleted~n"),

    %% Verify deletion
    FinalMappings = nat:list_mappings(),
    io:format("  Remaining mappings: ~p~n", [length(FinalMappings)]),

    io:format("~n=== All tests passed! ===~n"),
    halt(0).
