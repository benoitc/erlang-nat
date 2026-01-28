# Protocol Overview

erlang-nat supports multiple NAT traversal protocols to ensure maximum compatibility with different router implementations.

## Protocol Comparison

| Feature | UPnP IGD v1 | UPnP IGD v2 | NAT-PMP | PCP |
|---------|-------------|-------------|---------|-----|
| RFC | - | - | RFC 6886 | RFC 6887 |
| Transport | HTTP/SOAP | HTTP/SOAP | UDP | UDP |
| Port | 1900 (SSDP) | 1900 (SSDP) | 5351 | 5351 |
| IPv6 | No | No | No | Yes |
| Dynamic Port Alloc | Limited | Yes | Yes | Yes |
| Lease Duration | Yes | Yes | Yes | Yes |
| Router Support | Wide | Growing | Apple/Some | Modern |

## Discovery Priority

When `nat:discover/0` is called, protocols are tried in parallel. The first successful response wins:

```
                    discover()
                        |
        +-------+-------+-------+-------+
        |       |       |       |       |
     UPnP v2  UPnP v1  NAT-PMP   PCP
        |       |       |       |
        +-------+-------+-------+
                |
          First Success
                |
            Store Context
```

## Protocol Selection

The discovered protocol is automatically selected based on what the NAT device supports. You don't need to choose a specific protocol.

However, if you need to use a specific protocol, you can use the protocol modules directly:

```erlang
%% Use NAT-PMP specifically
case natpmp:discover() of
    {ok, Gateway} ->
        natpmp:add_port_mapping(Gateway, tcp, 8080, 8080);
    {error, _} ->
        %% NAT-PMP not available
        error
end.
```

## Protocol Modules

Each protocol has its own module with a consistent interface:

| Module | Protocol | Description |
|--------|----------|-------------|
| `natupnp_v1` | UPnP IGD v1 | Universal Plug and Play (legacy) |
| `natupnp_v2` | UPnP IGD v2 | Universal Plug and Play (modern) |
| `natpmp` | NAT-PMP | NAT Port Mapping Protocol |
| `natpcp` | PCP | Port Control Protocol |

## Common Interface

All protocol modules implement these functions:

```erlang
%% Discovery
-spec discover() -> {ok, Context} | {error, term()}.

%% Address retrieval
-spec get_external_address(Context) -> {ok, string()} | {error, term()}.
-spec get_device_address(Context) -> {ok, string()} | {error, term()}.
-spec get_internal_address(Context) -> {ok, string()}.

%% Port mapping
-spec add_port_mapping(Context, Protocol, InternalPort, ExternalPort) -> Result.
-spec add_port_mapping(Context, Protocol, InternalPort, ExternalPort, Lifetime) -> Result.
-spec delete_port_mapping(Context, Protocol, InternalPort, ExternalPort) -> ok | {error, term()}.
```

## Router Compatibility

### High Compatibility (UPnP)

Most consumer routers support UPnP IGD:

- Linksys/Cisco
- Netgear
- ASUS
- TP-Link
- D-Link
- Most ISP-provided routers

### Apple Devices (NAT-PMP)

Apple routers and AirPort devices use NAT-PMP:

- AirPort Express
- AirPort Extreme
- Time Capsule

### Modern Routers (PCP)

Newer routers may support PCP, especially those with IPv6 support:

- Carrier-grade NAT (CGNAT)
- Enterprise routers
- Some modern consumer routers

## Troubleshooting

### No NAT Discovered

1. **UPnP Disabled**: Check router settings to enable UPnP
2. **Firewall**: Ensure SSDP (UDP 1900) and HTTP are not blocked
3. **Multiple NATs**: Double-NAT scenarios may not work
4. **Direct Connection**: You may not be behind NAT

### Port Mapping Fails

1. **Port Restricted**: Some routers block privileged ports (<1024)
2. **Port in Use**: Another mapping may exist for that port
3. **Quota Exceeded**: Router may limit number of mappings
4. **Protocol Disabled**: Specific protocol may be disabled on router

### IP Address Changes

External IP may change due to:

1. **DHCP Renewal**: ISP assigned new IP
2. **Router Reboot**: NAT device restarted
3. **CGNAT**: Carrier-grade NAT reassignment

Use the [event system](../events.md) to detect IP changes.
