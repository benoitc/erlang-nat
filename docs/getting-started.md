# Getting Started

This guide walks you through installing erlang-nat and using it to create port mappings.

## Installation

### Using rebar3

Add `nat` to your `rebar.config` dependencies:

```erlang
{deps, [
    {nat, "0.5.0"}
]}.
```

Or from GitHub:

```erlang
{deps, [
    {nat, {git, "https://github.com/benoitc/erlang-nat.git", {tag, "0.5.0"}}}
]}.
```

### Using mix (Elixir)

Add to your `mix.exs`:

```elixir
defp deps do
  [
    {:nat, "~> 0.5.0"}
  ]
end
```

## Basic Usage

### 1. Start the Application

```erlang
%% Start nat and its dependencies
{ok, _} = application:ensure_all_started(nat).
```

### 2. Discover NAT Gateway

```erlang
%% Discover and store NAT context
case nat:discover() of
    ok ->
        io:format("NAT gateway discovered~n");
    no_nat ->
        io:format("No NAT gateway found (direct connection?)~n");
    {error, Reason} ->
        io:format("Discovery failed: ~p~n", [Reason])
end.
```

### 3. Get Network Addresses

```erlang
%% External (public) IP address
{ok, ExternalIp} = nat:get_external_address().
io:format("External IP: ~s~n", [ExternalIp]).

%% NAT gateway IP address
{ok, DeviceIp} = nat:get_device_address().
io:format("Gateway IP: ~s~n", [DeviceIp]).

%% Local device IP address
{ok, InternalIp} = nat:get_internal_address().
io:format("Internal IP: ~s~n", [InternalIp]).
```

### 4. Create Port Mappings

```erlang
%% Map external port 8080 to internal port 8080 (TCP)
%% Default lifetime is 3600 seconds (1 hour)
{ok, Since, InternalPort, ExternalPort, Lifetime} = 
    nat:add_port_mapping(tcp, 8080, 8080).

io:format("Mapped ~p:~p -> ~p:~p for ~p seconds~n",
          [ExternalIp, ExternalPort, InternalIp, InternalPort, Lifetime]).

%% Map with custom lifetime (2 hours)
{ok, _, _, _, _} = nat:add_port_mapping(udp, 9000, 9000, 7200).
```

!!! note "Auto-renewal"
    Port mappings created through `nat:add_port_mapping/3,4` are automatically 
    renewed before expiry. The renewal happens at 50% of the lifetime (minimum 60 seconds).

### 5. List Active Mappings

```erlang
%% Get all managed mappings
Mappings = nat:list_mappings().
%% Returns: [{Protocol, InternalPort, ExternalPort, ExpiresAt}, ...]

lists:foreach(fun({Proto, IntPort, ExtPort, Expires}) ->
    io:format("~p: ~p -> ~p (expires: ~p)~n", 
              [Proto, IntPort, ExtPort, Expires])
end, Mappings).
```

### 6. Delete Port Mappings

```erlang
%% Remove specific mapping
ok = nat:delete_port_mapping(tcp, 8080, 8080).
ok = nat:delete_port_mapping(udp, 9000, 9000).
```

## Event Handling

Register to receive notifications about mapping changes and IP changes:

```erlang
%% Register current process for events
ok = nat:reg_pid(self()).

%% Handle events
handle_nat_events() ->
    receive
        {nat_event, {mapping_renewed, Proto, IntPort, ExtPort, Lifetime}} ->
            io:format("Renewed: ~p ~p->~p for ~ps~n", 
                      [Proto, IntPort, ExtPort, Lifetime]),
            handle_nat_events();
            
        {nat_event, {mapping_failed, Proto, IntPort, ExtPort, Reason}} ->
            io:format("Failed: ~p ~p->~p: ~p~n", 
                      [Proto, IntPort, ExtPort, Reason]),
            handle_nat_events();
            
        {nat_event, {ip_changed, OldIp, NewIp}} ->
            io:format("IP changed: ~s -> ~s~n", [OldIp, NewIp]),
            %% Re-announce services, update DNS, etc.
            handle_nat_events();
            
        {nat_event, {context_lost, Reason}} ->
            io:format("NAT context lost: ~p~n", [Reason]),
            %% Trigger re-discovery
            handle_nat_events()
    end.

%% Unregister when done
ok = nat:unreg_pid(self()).
```

## Complete Example

Here's a complete example of a simple server that opens a port through NAT:

```erlang
-module(nat_example).
-export([start/0, stop/0]).

start() ->
    %% Start application
    {ok, _} = application:ensure_all_started(nat),
    
    %% Discover NAT
    case nat:discover() of
        ok ->
            %% Get addresses
            {ok, ExtIp} = nat:get_external_address(),
            {ok, IntIp} = nat:get_internal_address(),
            
            %% Create mapping
            Port = 8333,
            case nat:add_port_mapping(tcp, Port, Port) of
                {ok, _, _, ExtPort, Lifetime} ->
                    io:format("Server accessible at ~s:~p~n", [ExtIp, ExtPort]),
                    io:format("Mapping valid for ~p seconds (auto-renewed)~n", [Lifetime]),
                    
                    %% Start your server listening on Port
                    %% ...
                    
                    {ok, #{external_ip => ExtIp,
                           internal_ip => IntIp,
                           port => ExtPort}};
                           
                {error, Reason} ->
                    {error, {mapping_failed, Reason}}
            end;
            
        no_nat ->
            io:format("No NAT detected, using direct connection~n"),
            {ok, direct};
            
        {error, Reason} ->
            {error, {discovery_failed, Reason}}
    end.

stop() ->
    %% Delete mapping (optional - nat_server will clean up on shutdown)
    nat:delete_port_mapping(tcp, 8333, 8333),
    ok.
```

## Next Steps

- Learn about [Events](events.md) for monitoring NAT changes
- Explore the [API Reference](api/nat.md) for all available functions
- Understand the [Protocols](protocols/index.md) supported by erlang-nat
- Set up the [Docker Testing](docker.md) environment for development
