# Advanced Usage

This guide covers advanced topics for users who need fine-grained control over NAT operations.

## Direct Protocol Access

While `nat.erl` provides a unified interface, you can use protocol modules directly for specific needs.

### Using a Specific Protocol

```erlang
%% Force UPnP v2
case natupnp_v2:discover() of
    {ok, Ctx} ->
        {ok, ExtIp} = natupnp_v2:get_external_address(Ctx),
        natupnp_v2:add_port_mapping(Ctx, tcp, 8080, 8080, 3600);
    {error, _} ->
        {error, upnp_v2_not_available}
end.

%% Force NAT-PMP
case natpmp:discover() of
    {ok, Gateway} ->
        {ok, ExtIp} = natpmp:get_external_address(Gateway),
        natpmp:add_port_mapping(Gateway, tcp, 8080, 8080, 3600);
    {error, _} ->
        {error, natpmp_not_available}
end.
```

### Listing Router Mappings (UPnP Only)

UPnP allows listing all mappings on the router:

```erlang
{ok, Ctx} = natupnp_v1:discover().

%% List all mappings (up to 100)
{ok, Mappings} = natupnp_v1:list_port_mappings(Ctx).

%% Process mappings
lists:foreach(fun(M) ->
    io:format("~s:~p -> ~s:~p (~p) - ~s~n", [
        M#port_mapping.remote_host,
        M#port_mapping.external_port,
        M#port_mapping.internal_client,
        M#port_mapping.internal_port,
        M#port_mapping.protocol,
        M#port_mapping.description
    ])
end, Mappings).
```

### Checking Router Status (UPnP Only)

```erlang
{ok, Ctx} = natupnp_v1:discover().
{Status, LastError, Uptime} = natupnp_v1:status_info(Ctx).
io:format("Status: ~s, Uptime: ~p seconds~n", [Status, Uptime]).
```

## Manual Renewal Management

If you need custom renewal logic instead of `nat_server`'s automatic renewal:

```erlang
%% Don't use nat:add_port_mapping, use protocol directly
{ok, Gateway} = natpmp:discover().
{ok, Since, Int, Ext, Life} = natpmp:add_port_mapping(Gateway, tcp, 8080, 8080, 3600).

%% Set up your own renewal timer
RenewalTime = Life div 2 * 1000, % 50% of lifetime in ms
erlang:send_after(RenewalTime, self(), {renew, Gateway, tcp, Int, Ext, Life}).

%% Handle renewal
handle_info({renew, Gateway, Proto, Int, Ext, Life}, State) ->
    case natpmp:add_port_mapping(Gateway, Proto, Int, Ext, Life) of
        {ok, _, _, _, NewLife} ->
            RenewalTime = NewLife div 2 * 1000,
            erlang:send_after(RenewalTime, self(), {renew, Gateway, Proto, Int, Ext, NewLife});
        {error, Reason} ->
            %% Handle failure
            logger:error("Renewal failed: ~p", [Reason])
    end,
    {noreply, State}.
```

## Working with IPv6 (PCP)

PCP supports IPv6 natively:

```erlang
%% PCP handles IPv6 addresses
{ok, Gateway} = natpcp:discover().

%% If on IPv6 network, addresses will be IPv6
{ok, ExtIp} = natpcp:get_external_address(Gateway).
%% ExtIp could be "2001:db8::1" on IPv6 networks

%% Port mappings work the same way
{ok, _, _, _, _} = natpcp:add_port_mapping(Gateway, tcp, 8080, 8080).
```

## Handling Multiple NAT (Double NAT)

In double-NAT scenarios, you may need to handle both layers:

```erlang
discover_all_nats() ->
    %% Get all potential gateways
    Gateways = natpmp:system_gateways(),
    
    %% Try each one
    Results = lists:filtermap(fun(Gw) ->
        case natpmp:get_external_address(Gw) of
            {ok, ExtIp} ->
                %% Check if external IP is private (indicates double NAT)
                case inet_ext:is_private_address(ExtIp) of
                    true ->
                        {true, {Gw, ExtIp, double_nat}};
                    false ->
                        {true, {Gw, ExtIp, single_nat}}
                end;
            {error, _} ->
                false
        end
    end, Gateways),
    
    Results.
```

## Custom Discovery Timeout

```erlang
%% The discovery timeout is defined in nat.hrl
%% DISCOVER_TIMEOUT = 10000 (10 seconds)

%% For custom timeout, implement your own discovery wrapper
discover_with_timeout(Timeout) ->
    Ref = make_ref(),
    Parent = self(),
    
    Pid = spawn(fun() ->
        Result = nat:discover(),
        Parent ! {Ref, Result}
    end),
    
    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        {error, timeout}
    end.
```

## Monitoring NAT Device Health

```erlang
-module(nat_health_monitor).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_info/2]).

-define(CHECK_INTERVAL, 30000). % 30 seconds

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Initial check
    self() ! check_health,
    {ok, #{last_ip => undefined, failures => 0}}.

handle_info(check_health, #{failures := Failures} = State) ->
    NewState = case nat:get_external_address() of
        {ok, Ip} ->
            check_ip_change(Ip, State),
            State#{last_ip => Ip, failures => 0};
        {error, Reason} ->
            NewFailures = Failures + 1,
            if NewFailures >= 3 ->
                logger:error("NAT device unhealthy: ~p failures", [NewFailures]),
                trigger_recovery();
               true ->
                ok
            end,
            State#{failures => NewFailures}
    end,
    
    erlang:send_after(?CHECK_INTERVAL, self(), check_health),
    {noreply, NewState}.

check_ip_change(Ip, #{last_ip := undefined}) ->
    ok;
check_ip_change(Ip, #{last_ip := Ip}) ->
    ok;
check_ip_change(NewIp, #{last_ip := OldIp}) ->
    logger:warning("IP changed: ~s -> ~s", [OldIp, NewIp]),
    notify_ip_change(OldIp, NewIp).

trigger_recovery() ->
    spawn(fun() ->
        timer:sleep(5000),
        nat:discover()
    end).

notify_ip_change(Old, New) ->
    %% Implement your notification logic
    ok.
```

## Graceful Shutdown

Properly clean up mappings on application shutdown:

```erlang
-module(my_app).
-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

start(_Type, _Args) ->
    %% Start your supervisor
    my_sup:start_link().

prep_stop(State) ->
    %% Clean up NAT mappings before stopping
    logger:info("Cleaning up NAT mappings..."),
    Mappings = nat:list_mappings(),
    lists:foreach(fun({Proto, Int, Ext, _Expires}) ->
        nat:delete_port_mapping(Proto, Int, Ext)
    end, Mappings),
    State.

stop(_State) ->
    ok.
```

## Error Recovery Patterns

### Retry with Backoff

```erlang
add_mapping_with_retry(Proto, Int, Ext, Retries) ->
    add_mapping_with_retry(Proto, Int, Ext, Retries, 1000).

add_mapping_with_retry(_Proto, _Int, _Ext, 0, _Delay) ->
    {error, max_retries};
add_mapping_with_retry(Proto, Int, Ext, Retries, Delay) ->
    case nat:add_port_mapping(Proto, Int, Ext) of
        {ok, _, _, _, _} = Result ->
            Result;
        {error, no_context} ->
            %% Need to discover first
            case nat:discover() of
                ok ->
                    add_mapping_with_retry(Proto, Int, Ext, Retries - 1, Delay);
                _ ->
                    timer:sleep(Delay),
                    add_mapping_with_retry(Proto, Int, Ext, Retries - 1, Delay * 2)
            end;
        {error, _Reason} ->
            timer:sleep(Delay),
            add_mapping_with_retry(Proto, Int, Ext, Retries - 1, Delay * 2)
    end.
```

### Fallback Protocols

```erlang
discover_with_fallback() ->
    %% Try protocols in order of preference
    Protocols = [
        {natupnp_v2, "UPnP v2"},
        {natupnp_v1, "UPnP v1"},
        {natpcp, "PCP"},
        {natpmp, "NAT-PMP"}
    ],
    
    discover_with_fallback(Protocols).

discover_with_fallback([]) ->
    {error, no_protocol_available};
discover_with_fallback([{Module, Name} | Rest]) ->
    case Module:discover() of
        {ok, Ctx} ->
            logger:info("Using ~s", [Name]),
            {ok, {Module, Ctx}};
        {error, _} ->
            discover_with_fallback(Rest)
    end.
```

## Integration with Other Libraries

### With Ranch (TCP Acceptor)

```erlang
start_server(Port) ->
    %% Create NAT mapping
    case nat:discover() of
        ok ->
            case nat:add_port_mapping(tcp, Port, Port) of
                {ok, _, _, ExtPort, _} ->
                    {ok, ExtIp} = nat:get_external_address(),
                    logger:info("Server accessible at ~s:~p", [ExtIp, ExtPort]),
                    
                    %% Start Ranch listener
                    ranch:start_listener(my_server, ranch_tcp,
                        [{port, Port}], my_protocol, []);
                {error, Reason} ->
                    {error, {nat_mapping_failed, Reason}}
            end;
        no_nat ->
            %% No NAT, start directly
            ranch:start_listener(my_server, ranch_tcp,
                [{port, Port}], my_protocol, []);
        {error, Reason} ->
            {error, {nat_discovery_failed, Reason}}
    end.
```

### With gen_tcp

```erlang
start_listening(Port) ->
    %% Set up NAT mapping first
    ok = nat:discover(),
    {ok, _, _, ExtPort, _} = nat:add_port_mapping(tcp, Port, Port),
    {ok, ExtIp} = nat:get_external_address(),
    
    %% Start listening
    {ok, ListenSock} = gen_tcp:listen(Port, [binary, {active, true}]),
    
    %% Return connection info
    {ok, #{
        listen_socket => ListenSock,
        external_ip => ExtIp,
        external_port => ExtPort,
        internal_port => Port
    }}.
```
