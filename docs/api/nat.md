# nat Module

The `nat` module provides the main public API for NAT traversal operations.

## Types

```erlang
-type nat_ctx() :: {module(), term()}.
-type nat_protocol() :: tcp | udp.
```

## Discovery Functions

### discover/0

```erlang
-spec discover() -> ok | no_nat | {error, term()}.
```

Discover a NAT gateway and store the context in `nat_server`.

The discovery process tries multiple protocols in parallel:

1. UPnP IGD v2
2. UPnP IGD v1
3. NAT-PMP
4. PCP

The first protocol to successfully respond is used.

**Returns:**

- `ok` - NAT gateway discovered and context stored
- `no_nat` - No NAT gateway found (may indicate direct internet connection)
- `{error, Reason}` - Discovery failed

**Example:**

```erlang
case nat:discover() of
    ok ->
        io:format("NAT discovered~n");
    no_nat ->
        io:format("No NAT found~n");
    {error, timeout} ->
        io:format("Discovery timed out~n")
end.
```

### get_context/0

```erlang
-spec get_context() -> {ok, nat_ctx()} | {error, no_context}.
```

Get the stored NAT context for advanced operations.

**Returns:**

- `{ok, {Module, Context}}` - The NAT context tuple
- `{error, no_context}` - No context stored (call `discover/0` first)

**Example:**

```erlang
{ok, {natupnp_v2, UpnpCtx}} = nat:get_context().
```

## Port Mapping Functions

### add_port_mapping/3

```erlang
-spec add_port_mapping(Protocol, InternalPort, ExternalPort) -> Result when
    Protocol :: nat_protocol(),
    InternalPort :: non_neg_integer(),
    ExternalPort :: non_neg_integer(),
    Result :: {ok, Since, InternalPort, ExternalPort, Lifetime} | {error, term()}.
```

Add a port mapping with the default lifetime (3600 seconds).

The mapping is automatically renewed before expiry.

**Parameters:**

- `Protocol` - `tcp` or `udp`
- `InternalPort` - Local port number to map
- `ExternalPort` - Requested external port (0 for any available port)

**Returns:**

- `{ok, Since, InternalPort, ExternalPort, Lifetime}` - Mapping created
    - `Since` - NAT epoch timestamp when mapping was created
    - `InternalPort` - Actual internal port mapped
    - `ExternalPort` - Actual external port assigned
    - `Lifetime` - Mapping lifetime in seconds
- `{error, Reason}` - Mapping failed

**Example:**

```erlang
{ok, _Since, 8080, 8080, 3600} = nat:add_port_mapping(tcp, 8080, 8080).
```

### add_port_mapping/4

```erlang
-spec add_port_mapping(Protocol, InternalPort, ExternalPort, Lifetime) -> Result when
    Protocol :: nat_protocol(),
    InternalPort :: non_neg_integer(),
    ExternalPort :: non_neg_integer(),
    Lifetime :: non_neg_integer(),
    Result :: {ok, Since, InternalPort, ExternalPort, Lifetime} | {error, term()}.
```

Add a port mapping with a specific lifetime.

**Parameters:**

- `Protocol` - `tcp` or `udp`
- `InternalPort` - Local port number to map
- `ExternalPort` - Requested external port (0 for any available port)
- `Lifetime` - Requested lifetime in seconds

**Example:**

```erlang
%% Request 2-hour lifetime
{ok, _, _, _, ActualLifetime} = nat:add_port_mapping(tcp, 8080, 8080, 7200).
```

!!! warning "Lifetime may differ"
    The actual lifetime granted may be less than requested, depending on the 
    NAT device's configuration. Always check the returned lifetime.

### delete_port_mapping/3

```erlang
-spec delete_port_mapping(Protocol, InternalPort, ExternalPort) -> ok | {error, term()}.
```

Delete a port mapping and cancel its auto-renewal timer.

**Parameters:**

- `Protocol` - `tcp` or `udp`
- `InternalPort` - Internal port of the mapping
- `ExternalPort` - External port of the mapping

**Example:**

```erlang
ok = nat:delete_port_mapping(tcp, 8080, 8080).
```

### list_mappings/0

```erlang
-spec list_mappings() -> [{Protocol, InternalPort, ExternalPort, ExpiresAt}].
```

List all port mappings managed by `nat_server`.

**Returns:**

A list of tuples containing:

- `Protocol` - `tcp` or `udp`
- `InternalPort` - Internal port number
- `ExternalPort` - External port number
- `ExpiresAt` - Unix timestamp when the mapping expires

**Example:**

```erlang
Mappings = nat:list_mappings().
%% => [{tcp, 8080, 8080, 1706500000}, {udp, 9000, 9000, 1706500000}]
```

## Address Functions

### get_external_address/0

```erlang
-spec get_external_address() -> {ok, string()} | {error, term()}.
```

Get the external (public) IP address as reported by the NAT device.

**Example:**

```erlang
{ok, "203.0.113.42"} = nat:get_external_address().
```

### get_device_address/0

```erlang
-spec get_device_address() -> {ok, string()} | {error, term()}.
```

Get the NAT gateway's IP address.

**Example:**

```erlang
{ok, "192.168.1.1"} = nat:get_device_address().
```

### get_internal_address/0

```erlang
-spec get_internal_address() -> {ok, string()} | {error, term()}.
```

Get the local device's IP address (the address used to communicate with the NAT gateway).

**Example:**

```erlang
{ok, "192.168.1.100"} = nat:get_internal_address().
```

## Event Registration Functions

### reg_pid/1

```erlang
-spec reg_pid(pid()) -> ok.
```

Register a process to receive `{nat_event, Event}` messages.

The registered process is monitored; if it exits, it is automatically unregistered.

**Example:**

```erlang
ok = nat:reg_pid(self()).
receive
    {nat_event, Event} -> handle_event(Event)
end.
```

### unreg_pid/1

```erlang
-spec unreg_pid(pid()) -> ok.
```

Unregister a process from receiving events.

**Example:**

```erlang
ok = nat:unreg_pid(self()).
```

### reg_fun/1

```erlang
-spec reg_fun(fun((Event :: term()) -> any())) -> {ok, reference()}.
```

Register a callback function for NAT events.

The function is called asynchronously (spawned) for each event.

**Returns:**

- `{ok, Ref}` - Reference to use for unregistration

**Example:**

```erlang
{ok, Ref} = nat:reg_fun(fun(Event) ->
    logger:info("NAT event: ~p", [Event])
end).
```

### unreg_fun/1

```erlang
-spec unreg_fun(reference()) -> ok.
```

Unregister a callback function by its reference.

**Example:**

```erlang
ok = nat:unreg_fun(Ref).
```

## Error Handling

Common error reasons:

| Error | Description |
|-------|-------------|
| `no_context` | No NAT context stored (call `discover/0` first) |
| `timeout` | Operation timed out |
| `not_authorized` | NAT device rejected the request |
| `no_resources` | NAT device has no available ports |
| `{http_error, Code, Body}` | UPnP SOAP error |

**Example error handling:**

```erlang
case nat:add_port_mapping(tcp, 80, 80) of
    {ok, _, _, ExtPort, _} ->
        {ok, ExtPort};
    {error, no_context} ->
        %% Need to discover first
        nat:discover(),
        nat:add_port_mapping(tcp, 80, 80);
    {error, not_authorized} ->
        %% Port 80 may be restricted
        nat:add_port_mapping(tcp, 80, 8080);
    {error, Reason} ->
        {error, Reason}
end.
```
