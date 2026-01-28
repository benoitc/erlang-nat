# NAT-PMP Protocol

NAT Port Mapping Protocol (NAT-PMP) is a lightweight protocol defined in RFC 6886 for creating port mappings on NAT devices.

## Overview

| Property | Value |
|----------|-------|
| Module | `natpmp` |
| RFC | RFC 6886 |
| Transport | UDP |
| Port | 5351 |
| IPv6 | No (use PCP instead) |

## Discovery

NAT-PMP discovery sends requests to the default gateway:

```erlang
{ok, Gateway} = natpmp:discover().
%% Gateway is the IP address of the NAT device
```

### Discovery Process

1. Get system gateways from routing table
2. Send NAT-PMP request to each gateway on port 5351
3. First valid response wins

```erlang
%% Get potential gateways
Gateways = natpmp:system_gateways().
%% => ["192.168.1.1"]

%% Or use potential private network gateways
Potential = natpmp:potential_gateways().
%% => [{192,168,1,1}, {10,0,0,1}, ...]
```

## Port Mapping

### Add Mapping

```erlang
%% Add TCP mapping with default lifetime (3600s)
{ok, Since, IntPort, ExtPort, Lifetime} = 
    natpmp:add_port_mapping(Gateway, tcp, 8080, 8080).

%% Add with custom lifetime
{ok, Since, IntPort, ExtPort, Lifetime} = 
    natpmp:add_port_mapping(Gateway, udp, 9000, 9000, 7200).
```

### Delete Mapping

```erlang
%% Delete specific mapping
ok = natpmp:delete_port_mapping(Gateway, tcp, 8080, 8080).

%% Delete ALL mappings for a protocol
ok = natpmp:delete_all_mappings(Gateway, tcp).
```

### Dynamic Port Allocation

Request external port 0 for automatic allocation:

```erlang
{ok, Since, IntPort, ExtPort, Lifetime} = 
    natpmp:add_port_mapping(Gateway, tcp, 8080, 0).
%% ExtPort will be assigned by the NAT device
```

## Address Functions

```erlang
%% Get external (public) IP
{ok, ExtIp} = natpmp:get_external_address(Gateway).

%% Get external IP with epoch time
{ok, ExtIp, Epoch} = natpmp:get_external_address_with_epoch(Gateway).

%% Get gateway address
{ok, GwIp} = natpmp:get_device_address(Gateway).

%% Get local address used to reach gateway
{ok, IntIp} = natpmp:get_internal_address(Gateway).
```

## Protocol Details

### Packet Format

NAT-PMP uses a simple binary protocol:

**Request (External Address):**
```
 0                   1
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|    Version    |    Opcode     |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

**Request (Port Mapping):**
```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|    Version    |    Opcode     |          Reserved             |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|        Internal Port          |     Suggested External Port   |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                     Requested Lifetime                        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

### Opcodes

| Opcode | Purpose |
|--------|---------|
| 0 | External Address Request |
| 1 | Map UDP |
| 2 | Map TCP |
| 128+ | Response (opcode + 128) |

### Epoch Time

NAT-PMP responses include an epoch field indicating seconds since the NAT device booted. If the epoch decreases between requests, all previous mappings may have been lost (device rebooted).

```erlang
%% Check for epoch reset
{ok, Ip1, Epoch1} = natpmp:get_external_address_with_epoch(Gw),
%% ... later ...
{ok, Ip2, Epoch2} = natpmp:get_external_address_with_epoch(Gw),
if Epoch2 < Epoch1 ->
    %% Device rebooted, re-create mappings
    recreate_mappings();
   true ->
    ok
end.
```

## Error Handling

### Error Types

```erlang
-type natpmp_error() ::
    unsupported_version |  %% Version not supported
    not_authorized      |  %% Unauthorized request
    network_failure     |  %% Network/NAT failure
    out_of_resource     |  %% No resources available
    unsupported_opcode  |  %% Opcode not supported
    bad_response        |  %% Malformed response
    timeout.               %% No response received
```

### Result Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Unsupported Version |
| 2 | Not Authorized |
| 3 | Network Failure |
| 4 | Out of Resources |
| 5 | Unsupported Opcode |

## IP Change Notification

NAT-PMP supports multicast announcements for IP changes:

- Gateway sends to `224.0.0.1:5350` when external IP changes
- `nat_server` listens for these announcements automatically

```erlang
%% Manual multicast listening (for reference)
{ok, Sock} = gen_udp:open(5350, [
    {reuseaddr, true},
    {ip, {224,0,0,1}},
    {multicast_ttl, 1},
    {add_membership, {{224,0,0,1}, {0,0,0,0}}}
]).
```

## Retry Strategy

NAT-PMP uses exponential backoff for retries:

1. Initial timeout: 250ms
2. Double on each retry: 250, 500, 1000, 2000, 4000, 8000, 16000, 32000, 64000ms
3. Total timeout: ~128 seconds after 9 retries

erlang-nat uses a shorter strategy:

- Initial: 125ms
- Max retries: 3
- Total: ~1 second

## Direct Usage Example

```erlang
%% Direct NAT-PMP usage
case natpmp:discover() of
    {ok, Gateway} ->
        %% Get network info
        {ok, ExtIp} = natpmp:get_external_address(Gateway),
        {ok, IntIp} = natpmp:get_internal_address(Gateway),
        io:format("External: ~s, Internal: ~s~n", [ExtIp, IntIp]),
        
        %% Create mapping
        case natpmp:add_port_mapping(Gateway, tcp, 8333, 8333, 3600) of
            {ok, Since, Int, Ext, Life} ->
                io:format("Mapped since epoch ~p: ~p -> ~p (~ps)~n", 
                          [Since, Ext, Int, Life]);
            {error, out_of_resource} ->
                io:format("No ports available~n");
            {error, Reason} ->
                io:format("Failed: ~p~n", [Reason])
        end;
        
    {error, Reason} ->
        io:format("NAT-PMP not available: ~p~n", [Reason])
end.
```

## Compatibility

NAT-PMP is primarily supported by:

- Apple AirPort devices
- Apple Time Capsule
- Some third-party routers (check documentation)
- pfSense/OPNsense with NAT-PMP enabled
- MiniUPnPd (open-source implementation)
