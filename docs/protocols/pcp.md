# PCP Protocol

Port Control Protocol (PCP) is defined in RFC 6887 as the IPv6-capable successor to NAT-PMP.

## Overview

| Property | Value |
|----------|-------|
| Module | `natpcp` |
| RFC | RFC 6887 |
| Transport | UDP |
| Port | 5351 |
| IPv6 | Yes |

PCP supports both IPv4 and IPv6, with IPv4 addresses encoded as IPv4-mapped IPv6 addresses.

## Discovery

```erlang
{ok, Gateway} = natpcp:discover().
%% Gateway is the IP address of the PCP server
```

Discovery uses the same process as NAT-PMP:

1. Get gateways from system routing table
2. Send PCP ANNOUNCE request to each on port 5351
3. First valid response wins

## Port Mapping

### Add Mapping

```erlang
%% Add TCP mapping with default lifetime (3600s)
{ok, Since, IntPort, ExtPort, Lifetime} = 
    natpcp:add_port_mapping(Gateway, tcp, 8080, 8080).

%% Add with custom lifetime
{ok, Since, IntPort, ExtPort, Lifetime} = 
    natpcp:add_port_mapping(Gateway, udp, 9000, 9000, 7200).
```

### Delete Mapping

```erlang
%% Delete mapping by setting lifetime to 0
ok = natpcp:delete_port_mapping(Gateway, tcp, 8080, 8080).
```

### Dynamic Port Allocation

```erlang
%% Request any available external port
{ok, Since, IntPort, ExtPort, Lifetime} = 
    natpcp:add_port_mapping(Gateway, tcp, 8080, 0).
```

## Address Functions

```erlang
%% Get external IP (via MAP request with lifetime 0)
{ok, ExtIp} = natpcp:get_external_address(Gateway).

%% Get gateway address
{ok, GwIp} = natpcp:get_device_address(Gateway).

%% Get local address
{ok, IntIp} = natpcp:get_internal_address(Gateway).
```

## Protocol Details

### Version

PCP uses version 2 (distinguishing it from NAT-PMP version 0):

```erlang
-define(PCP_VERSION, 2).
```

### Opcodes

| Opcode | Name | Purpose |
|--------|------|---------|
| 0 | ANNOUNCE | Verify PCP support |
| 1 | MAP | Create/delete port mapping |
| 2 | PEER | Create mapping for specific peer |

### Packet Format

**Request Header:**
```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|  Version = 2  |R|   Opcode    |         Reserved              |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                 Requested Lifetime (32 bits)                  |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                               |
|           PCP Client's IP Address (128 bits)                  |
|                                                               |
|                                                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                               |
~                  Opcode-Specific Information                  ~
|                                                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

**MAP Opcode Payload:**
```
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                               |
|                 Mapping Nonce (96 bits)                       |
|                                                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|   Protocol    |          Reserved (24 bits)                   |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|        Internal Port          |    Suggested External Port    |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                                                               |
|           Suggested External IP Address (128 bits)            |
|                                                               |
|                                                               |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

### IPv4-Mapped IPv6

PCP uses 128-bit addresses. IPv4 addresses are encoded as IPv4-mapped IPv6:

```
::ffff:192.168.1.100  →  {0,0,0,0,0,65535,49320,356}
```

erlang-nat handles this conversion automatically:

```erlang
%% Internal conversion functions (exported for testing)
Bin = natpcp:ip_to_pcp_binary({192,168,1,100}).
%% => <<0,0,0,0,0,0,0,0,0,0,255,255,192,168,1,100>>

Ip = natpcp:pcp_binary_to_ip(Bin).
%% => {192,168,1,100}
```

### Nonce

Each MAP request includes a random 12-byte nonce. The response must echo the same nonce to prevent spoofing.

```erlang
Nonce = crypto:strong_rand_bytes(12).
```

## Error Handling

### Error Types

```erlang
-type pcp_error() ::
    unsupp_version     |  %% Version not supported
    not_authorized     |  %% Unauthorized
    malformed_request  |  %% Request malformed
    unsupp_opcode      |  %% Opcode not supported
    unsupp_option      |  %% Option not supported
    malformed_option   |  %% Option malformed
    network_failure    |  %% Network failure
    no_resources       |  %% Out of resources
    unsupp_protocol    |  %% Protocol not supported
    user_ex_quota      |  %% User exceeded quota
    cannot_provide_external | %% Cannot provide external address
    address_mismatch   |  %% Address mismatch
    excessive_remote_peers | %% Too many remote peers
    bad_response       |  %% Malformed response
    timeout.              %% No response
```

### Result Codes

| Code | Name | Description |
|------|------|-------------|
| 0 | SUCCESS | Success |
| 1 | UNSUPP_VERSION | Unsupported version |
| 2 | NOT_AUTHORIZED | Not authorized |
| 3 | MALFORMED_REQUEST | Malformed request |
| 4 | UNSUPP_OPCODE | Unsupported opcode |
| 5 | UNSUPP_OPTION | Unsupported option |
| 6 | MALFORMED_OPTION | Malformed option |
| 7 | NETWORK_FAILURE | Network failure |
| 8 | NO_RESOURCES | Out of resources |
| 9 | UNSUPP_PROTOCOL | Unsupported protocol |
| 10 | USER_EX_QUOTA | User exceeded quota |
| 11 | CANNOT_PROVIDE_EXTERNAL | Cannot provide external |
| 12 | ADDRESS_MISMATCH | Address mismatch |
| 13 | EXCESSIVE_REMOTE_PEERS | Too many remote peers |

## Epoch and IP Change

Like NAT-PMP, PCP includes an epoch field. A reset indicates:

- Server rebooted
- IP address changed
- All mappings may be lost

The `nat_server` monitors epoch changes and emits `{ip_changed, Old, New}` events.

## Direct Usage Example

```erlang
%% Direct PCP usage
case natpcp:discover() of
    {ok, Gateway} ->
        %% Get addresses
        {ok, ExtIp} = natpcp:get_external_address(Gateway),
        {ok, IntIp} = natpcp:get_internal_address(Gateway),
        io:format("External: ~s, Internal: ~s~n", [ExtIp, IntIp]),
        
        %% Create mapping
        case natpcp:add_port_mapping(Gateway, tcp, 8333, 8333, 3600) of
            {ok, Since, Int, Ext, Life} ->
                io:format("Mapped: ext:~p -> int:~p (~ps, epoch:~p)~n",
                          [Ext, Int, Life, Since]);
            {error, no_resources} ->
                io:format("No ports available~n");
            {error, Reason} ->
                io:format("Failed: ~p~n", [Reason])
        end;
        
    {error, Reason} ->
        io:format("PCP not available: ~p~n", [Reason])
end.
```

## Compatibility

PCP is supported by:

- Modern carrier-grade NAT (CGNAT) devices
- Enterprise NAT gateways
- MiniUPnPd (when compiled with PCP support)
- Some modern consumer routers
- Apple devices (as successor to NAT-PMP)

## PCP vs NAT-PMP

| Feature | NAT-PMP | PCP |
|---------|---------|-----|
| Version | 0 | 2 |
| IPv6 | No | Yes |
| Nonce | No | Yes (12 bytes) |
| Address size | 32-bit | 128-bit |
| PEER opcode | No | Yes |
| Options | No | Yes |
