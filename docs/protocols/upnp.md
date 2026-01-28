# UPnP IGD Protocol

Universal Plug and Play Internet Gateway Device (UPnP IGD) is a protocol suite for NAT traversal using HTTP/SOAP over the local network.

## Overview

erlang-nat supports both UPnP IGD versions:

| Module | Version | Service |
|--------|---------|---------|
| `natupnp_v1` | IGD v1 | WANIPConnection:1 |
| `natupnp_v2` | IGD v2 | WANIPConnection:2 |

## Discovery

UPnP discovery uses SSDP (Simple Service Discovery Protocol):

1. Send M-SEARCH multicast to `239.255.255.250:1900`
2. Wait for response with Location header
3. Fetch device description XML
4. Parse for WANIPConnection service URL

```erlang
%% UPnP v1 discovery
{ok, Ctx} = natupnp_v1:discover().

%% UPnP v2 discovery  
{ok, Ctx} = natupnp_v2:discover().
```

### Context Structure

```erlang
#nat_upnp{
    service_url :: string(),  %% SOAP endpoint URL
    ip :: string()            %% Local IP used for discovery
}
```

## Port Mapping

### AddPortMapping (v1)

UPnP v1 uses `AddPortMapping` action:

```erlang
{ok, Since, IntPort, ExtPort, Lifetime} = 
    natupnp_v1:add_port_mapping(Ctx, tcp, 8080, 8080).
```

If the requested external port is unavailable, the call fails.

### AddAnyPortMapping (v2)

UPnP v2 uses `AddAnyPortMapping` which allows dynamic port allocation:

```erlang
%% Request port 8080, but accept any available port
{ok, Since, IntPort, ExtPort, Lifetime} = 
    natupnp_v2:add_port_mapping(Ctx, tcp, 8080, 0).
%% ExtPort may differ from requested port
```

### Lifetime Handling

Some routers only support permanent leases. erlang-nat handles this automatically:

```erlang
%% If error 725 (OnlyPermanentLeasesSupported) is returned,
%% erlang-nat retries with lifetime=0 (permanent)
```

## Listing Mappings

UPnP allows listing existing port mappings:

```erlang
%% Get all mappings (up to 100)
{ok, Mappings} = natupnp_v1:list_port_mappings(Ctx).

%% Get with custom limit
{ok, Mappings} = natupnp_v1:list_port_mappings(Ctx, 50).

%% Get specific mapping by index
{ok, Mapping} = natupnp_v1:get_generic_port_mapping_entry(Ctx, 0).

%% Get mapping by external port
{ok, IntPort, IntAddr} = natupnp_v1:get_port_mapping(Ctx, tcp, 8080).
```

### Port Mapping Record

```erlang
#port_mapping{
    remote_host     :: string(),         %% Usually empty
    external_port   :: non_neg_integer(),
    protocol        :: tcp | udp,
    internal_port   :: non_neg_integer(),
    internal_client :: string(),         %% Internal IP
    enabled         :: boolean(),
    description     :: string(),
    lease_duration  :: non_neg_integer() %% 0 = permanent
}
```

## Status Information

```erlang
%% Get router connection status
{Status, LastError, Uptime} = natupnp_v1:status_info(Ctx).
%% Status: "Connected", "Disconnected", etc.
%% Uptime: seconds since connection established
```

## SOAP Actions

The following UPnP actions are used:

| Action | Purpose |
|--------|---------|
| `GetExternalIPAddress` | Get public IP |
| `AddPortMapping` | Create mapping (v1) |
| `AddAnyPortMapping` | Create mapping with dynamic port (v2) |
| `DeletePortMapping` | Remove mapping |
| `GetGenericPortMappingEntry` | List mapping by index |
| `GetSpecificPortMappingEntry` | Get mapping by port |
| `GetPortMappingNumberOfEntries` | Count mappings |
| `GetStatusInfo` | Get connection status |

## Error Codes

Common UPnP error codes:

| Code | Description |
|------|-------------|
| 401 | Invalid Action |
| 402 | Invalid Args |
| 501 | Action Failed |
| 713 | SpecifiedArrayIndexInvalid |
| 714 | NoSuchEntryInArray |
| 715 | WildCardNotPermittedInSrcIP |
| 716 | WildCardNotPermittedInExtPort |
| 718 | ConflictInMappingEntry |
| 724 | SamePortValuesRequired |
| 725 | OnlyPermanentLeasesSupported |
| 726 | RemoteHostOnlySupportsWildcard |
| 727 | ExternalPortOnlySupportsWildcard |

## Direct Usage Example

```erlang
%% Direct UPnP v2 usage
case natupnp_v2:discover() of
    {ok, Ctx} ->
        %% Get external IP
        {ok, ExtIp} = natupnp_v2:get_external_address(Ctx),
        io:format("External IP: ~s~n", [ExtIp]),
        
        %% Add port mapping
        case natupnp_v2:add_port_mapping(Ctx, tcp, 8080, 8080, 3600) of
            {ok, _, IntPort, ExtPort, Life} ->
                io:format("Mapped ~p -> ~p for ~ps~n", [ExtPort, IntPort, Life]);
            {error, Reason} ->
                io:format("Mapping failed: ~p~n", [Reason])
        end,
        
        %% List all mappings
        {ok, Mappings} = natupnp_v2:list_port_mappings(Ctx),
        lists:foreach(fun(M) ->
            io:format("~p: ~p -> ~s:~p~n", 
                [M#port_mapping.protocol,
                 M#port_mapping.external_port,
                 M#port_mapping.internal_client,
                 M#port_mapping.internal_port])
        end, Mappings);
        
    {error, Reason} ->
        io:format("UPnP v2 not available: ~p~n", [Reason])
end.
```

## Troubleshooting

### Discovery Fails

1. **UPnP Disabled**: Enable UPnP in router settings
2. **Firewall**: Allow UDP 1900 (SSDP) and HTTP to router
3. **IGMP Snooping**: May block multicast on some switches

### Mapping Fails

1. **Port Conflict**: Another device may have the port mapped
2. **Restricted Ports**: Ports < 1024 may be blocked
3. **NAT-RSIP Disabled**: Required for IGD v2 (check router settings)

### Check Router Support

```erlang
%% Try both versions to see what's supported
V1 = natupnp_v1:discover(),
V2 = natupnp_v2:discover(),
io:format("UPnP v1: ~p~n", [V1]),
io:format("UPnP v2: ~p~n", [V2]).
```
