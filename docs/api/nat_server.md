# nat_server Module

The `nat_server` module is a `gen_server` that manages NAT port mappings with automatic renewal, IP change detection, and event dispatch.

## Overview

`nat_server` is started automatically as part of the `nat` application supervision tree. It provides:

- **Context Storage**: Stores the discovered NAT context
- **Auto-renewal**: Automatically renews port mappings before expiry
- **IP Monitoring**: Detects external IP address changes
- **Event Dispatch**: Notifies registered subscribers of changes

## Architecture

```
nat_sup (one_for_one)
  |
  +-- nat_server (gen_server, {local, nat_server})
        - Stores discovered NAT context
        - Manages port mappings with auto-renewal timers
        - Monitors external IP (multicast + polling)
        - Dispatches events to subscribers
```

## Server State

The server maintains the following state:

```erlang
#nat_state{
    nat_ctx       :: undefined | {Module, Context},
    external_ip   :: undefined | string(),
    last_epoch    :: undefined | integer(),
    mappings      :: map(),      %% Key: {Protocol, InternalPort}
    reg_pids      :: map(),      %% Registered process monitors
    reg_funs      :: map(),      %% Registered callback functions
    ip_timer      :: undefined | reference(),
    listener_pid  :: undefined | pid(),
    opts          :: map()
}
```

## Renewal Strategy

Port mappings are automatically renewed using the following strategy:

1. **Renewal Time**: At 50% of the lifetime (minimum 60 seconds before expiry)
2. **Retry Logic**: On failure, exponential backoff (5s, 10s, 20s...)
3. **Max Retries**: 3 attempts before dispatching `mapping_failed` event
4. **Timer Management**: Each mapping has its own renewal timer

```
Lifetime: 3600s
          |
          |-------- 1800s --------|-------- 1800s --------|
          ^                       ^                       ^
        Created               Renewal                  Expiry
                             (at 50%)
```

## IP Change Detection

The server monitors for external IP address changes:

### NAT-PMP/PCP

- Listens on multicast address `224.0.0.1:5350` for announcements
- Checks epoch field in responses (reset indicates IP change)

### UPnP

- Polls `GetExternalIPAddress` every 60 seconds
- Compares with stored external IP

When an IP change is detected, the server:

1. Dispatches `{ip_changed, OldIp, NewIp}` event
2. Updates stored external IP
3. Applications should re-announce their services

## API Functions

All functions are delegated from `nat.erl`. See [nat module](nat.md) for full documentation.

### start_link/0

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

Start the server. Called automatically by `nat_sup`.

### discover/0

```erlang
-spec discover() -> ok | no_nat | {error, term()}.
```

Discover a NAT gateway and store the context.

### get_context/0

```erlang
-spec get_context() -> {ok, {Module, Context}} | {error, no_context}.
```

Get the stored NAT context.

### add_port_mapping/3,4

```erlang
-spec add_port_mapping(Protocol, InternalPort, ExternalPort) -> Result.
-spec add_port_mapping(Protocol, InternalPort, ExternalPort, Lifetime) -> Result.
```

Add a port mapping with auto-renewal.

### delete_port_mapping/3

```erlang
-spec delete_port_mapping(Protocol, InternalPort, ExternalPort) -> ok | {error, term()}.
```

Delete a port mapping and cancel its renewal timer.

### get_external_address/0, get_device_address/0, get_internal_address/0

```erlang
-spec get_external_address() -> {ok, string()} | {error, term()}.
-spec get_device_address() -> {ok, string()} | {error, term()}.
-spec get_internal_address() -> {ok, string()} | {error, term()}.
```

Get network addresses.

### list_mappings/0

```erlang
-spec list_mappings() -> [{Protocol, InternalPort, ExternalPort, ExpiresAt}].
```

List all managed port mappings.

### reg_pid/1, unreg_pid/1

```erlang
-spec reg_pid(pid()) -> ok.
-spec unreg_pid(pid()) -> ok.
```

Register/unregister a process for events.

### reg_fun/1, unreg_fun/1

```erlang
-spec reg_fun(fun()) -> {ok, reference()}.
-spec unreg_fun(reference()) -> ok.
```

Register/unregister a callback function for events.

## Internal Messages

The server handles the following internal messages:

### Timer Messages

- `{renew_mapping, Key}` - Trigger mapping renewal
- `poll_ip` - Trigger IP address poll (UPnP only)

### Monitor Messages

- `{'DOWN', Ref, process, Pid, _}` - Registered process exited

### Multicast Messages

- NAT-PMP announcement packets on `224.0.0.1:5350`

## Configuration

The server uses these default constants (defined in `nat.hrl`):

| Constant | Value | Description |
|----------|-------|-------------|
| `RECOMMENDED_MAPPING_LIFETIME_SECONDS` | 3600 | Default mapping lifetime |
| `RENEWAL_FACTOR` | 0.5 | Renew at 50% of lifetime |
| `MIN_RENEWAL_INTERVAL` | 60 | Minimum seconds before expiry to renew |
| `MAX_RENEWAL_RETRIES` | 3 | Max renewal retry attempts |
| `INITIAL_RETRY_DELAY_MS` | 5000 | Initial retry delay (doubles each retry) |
| `IP_POLL_INTERVAL_MS` | 60000 | UPnP IP polling interval |

## Supervision

The server is supervised by `nat_sup` with a `one_for_one` strategy:

```erlang
%% nat_sup.erl
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    ChildSpecs = [
        #{id => nat_server,
          start => {nat_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [nat_server]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
```

## Example: Monitoring Server State

While the server state is internal, you can observe its behavior through events:

```erlang
%% Monitor all NAT events
nat:reg_pid(self()),
loop() ->
    receive
        {nat_event, {mapping_renewed, Proto, Int, Ext, Life}} ->
            io:format("[~p] Renewed ~p:~p->~p for ~ps~n", 
                      [calendar:local_time(), Proto, Int, Ext, Life]),
            loop();
        {nat_event, {mapping_failed, Proto, Int, Ext, Reason}} ->
            io:format("[~p] FAILED ~p:~p->~p: ~p~n", 
                      [calendar:local_time(), Proto, Int, Ext, Reason]),
            loop();
        {nat_event, {ip_changed, Old, New}} ->
            io:format("[~p] IP CHANGED: ~s -> ~s~n", 
                      [calendar:local_time(), Old, New]),
            loop();
        {nat_event, {context_lost, Reason}} ->
            io:format("[~p] CONTEXT LOST: ~p~n", 
                      [calendar:local_time(), Reason]),
            loop()
    end.
```
