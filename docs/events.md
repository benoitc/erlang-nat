# Event System

The `nat_server` provides an event system to notify applications of NAT-related changes.

## Event Types

```erlang
-type nat_event() ::
    %% Mapping successfully renewed
    {mapping_renewed, Protocol, InternalPort, ExternalPort, NewLifetime} |
    
    %% Mapping renewal failed after retries
    {mapping_failed, Protocol, InternalPort, ExternalPort, Reason} |
    
    %% External IP address changed
    {ip_changed, OldIp :: string(), NewIp :: string()} |
    
    %% NAT context lost (device unreachable)
    {context_lost, Reason}.
```

## Subscribing to Events

### Process Registration

Register a process to receive `{nat_event, Event}` messages:

```erlang
%% Register current process
ok = nat:reg_pid(self()).

%% Or register another process
ok = nat:reg_pid(WorkerPid).
```

Registered processes are monitored. If a process exits, it is automatically unregistered.

### Callback Registration

Register a callback function:

```erlang
%% Register a function
{ok, Ref} = nat:reg_fun(fun(Event) ->
    logger:info("NAT event: ~p", [Event])
end).

%% The function is called asynchronously (spawned)
```

## Unsubscribing

```erlang
%% Unregister process
ok = nat:unreg_pid(self()).

%% Unregister callback (using reference from reg_fun)
ok = nat:unreg_fun(Ref).
```

## Event Details

### mapping_renewed

Sent when a port mapping is successfully renewed before expiry.

```erlang
{mapping_renewed, tcp, 8080, 8080, 3600}
%% Protocol: tcp
%% Internal port: 8080
%% External port: 8080
%% New lifetime: 3600 seconds
```

**When it occurs:**

- At 50% of mapping lifetime (or 60 seconds before expiry, whichever is later)
- After successful renewal request to NAT device

### mapping_failed

Sent when a mapping renewal fails after all retry attempts.

```erlang
{mapping_failed, tcp, 8080, 8080, timeout}
%% Protocol: tcp
%% Internal port: 8080
%% External port: 8080
%% Reason: timeout (or other error)
```

**When it occurs:**

- After 3 failed renewal attempts with exponential backoff
- The mapping is considered expired and removed from management

**Recommended action:**

- Log the failure
- Attempt to re-create the mapping
- Notify users if service availability is affected

### ip_changed

Sent when the external IP address changes.

```erlang
{ip_changed, "203.0.113.42", "198.51.100.23"}
%% Old IP: 203.0.113.42
%% New IP: 198.51.100.23
```

**When it occurs:**

- NAT-PMP/PCP: Multicast announcement received, or epoch reset detected
- UPnP: Polling detected IP change (every 60 seconds)

**Recommended action:**

- Update DNS records if using dynamic DNS
- Re-announce services to peers
- Update any external service registrations

### context_lost

Sent when the NAT device becomes unreachable.

```erlang
{context_lost, timeout}
%% Reason: timeout (device not responding)
```

**When it occurs:**

- NAT device stops responding to requests
- Network connectivity lost

**Recommended action:**

- Trigger re-discovery with `nat:discover()`
- Wait and retry after a delay

## Example: Event Handler

### Using gen_server

```erlang
-module(nat_event_handler).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_info/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    ok = nat:reg_pid(self()),
    {ok, #{}}.

handle_info({nat_event, Event}, State) ->
    handle_nat_event(Event),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_nat_event({mapping_renewed, Proto, Int, Ext, Life}) ->
    logger:info("Mapping ~p:~p->~p renewed for ~ps", 
                [Proto, Ext, Int, Life]);

handle_nat_event({mapping_failed, Proto, Int, Ext, Reason}) ->
    logger:error("Mapping ~p:~p->~p failed: ~p", 
                 [Proto, Ext, Int, Reason]),
    %% Try to recreate
    nat:add_port_mapping(Proto, Int, Ext);

handle_nat_event({ip_changed, OldIp, NewIp}) ->
    logger:warning("External IP changed: ~s -> ~s", [OldIp, NewIp]),
    %% Update dynamic DNS
    update_dns(NewIp);

handle_nat_event({context_lost, Reason}) ->
    logger:error("NAT context lost: ~p", [Reason]),
    %% Schedule re-discovery
    timer:apply_after(5000, nat, discover, []).
```

### Using Callback Function

```erlang
%% Simple logging callback
{ok, _Ref} = nat:reg_fun(fun(Event) ->
    case Event of
        {mapping_renewed, _, _, _, _} ->
            ok; % Normal operation, don't log
        {mapping_failed, P, I, E, R} ->
            error_logger:error_msg("NAT mapping failed: ~p ~p->~p: ~p~n",
                                   [P, E, I, R]);
        {ip_changed, Old, New} ->
            error_logger:warning_msg("IP changed: ~s -> ~s~n", [Old, New]);
        {context_lost, R} ->
            error_logger:error_msg("NAT context lost: ~p~n", [R])
    end
end).
```

### Using receive loop

```erlang
start_event_loop() ->
    ok = nat:reg_pid(self()),
    event_loop().

event_loop() ->
    receive
        {nat_event, {mapping_renewed, P, I, E, L}} ->
            io:format("[NAT] ~p mapping ~p->~p renewed (~ps)~n", [P, E, I, L]),
            event_loop();
            
        {nat_event, {mapping_failed, P, I, E, R}} ->
            io:format("[NAT] ~p mapping ~p->~p FAILED: ~p~n", [P, E, I, R]),
            %% Recreate mapping
            spawn(fun() -> nat:add_port_mapping(P, I, E) end),
            event_loop();
            
        {nat_event, {ip_changed, Old, New}} ->
            io:format("[NAT] IP changed: ~s -> ~s~n", [Old, New]),
            %% Handle IP change
            handle_ip_change(New),
            event_loop();
            
        {nat_event, {context_lost, R}} ->
            io:format("[NAT] Context lost: ~p, rediscovering...~n", [R]),
            timer:sleep(5000),
            nat:discover(),
            event_loop();
            
        stop ->
            nat:unreg_pid(self()),
            ok
    end.
```

## Best Practices

1. **Always handle mapping_failed**: Mappings can fail due to network issues or NAT device restarts.

2. **React to ip_changed promptly**: Update any external references to your IP address.

3. **Implement retry logic for context_lost**: The NAT device may come back online.

4. **Don't block in event handlers**: Use spawned processes or async operations.

5. **Monitor critical mappings**: Log or alert on mapping failures for important services.
