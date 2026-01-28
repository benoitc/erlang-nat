# Docker Test Environment for erlang-nat

This directory contains a Docker-based test environment for testing NAT traversal protocols (UPnP IGD, NAT-PMP, PCP) without requiring an actual NAT router.

## Architecture

```
                    External Network (172.20.0.0/24)
                              |
                    +---------+---------+
                    |   nat-gateway     |
                    |   (miniupnpd)     |
                    |   172.20.0.2      |
                    +---------+---------+
                              |
                    Internal Network (172.21.0.0/24)
                              |
                    +---------+---------+
                    |    nat-client     |
                    |   (erlang-nat)    |
                    |   172.21.0.10     |
                    +-------------------+
```

## Components

### Gateway (nat-gateway)

- Built from source miniupnpd with all protocol support
- Configured with TEST-NET external IP (203.0.113.42) for testing
- Supports: UPnP IGD v1/v2, NAT-PMP, PCP
- Uses iptables-legacy for compatibility

### Client (nat-client)

- Erlang/OTP 26 Alpine image
- Contains the erlang-nat library
- Includes test script and debugging tools

## Usage

### Quick Test

```bash
# Start both containers and run tests
docker compose up -d
docker exec nat-client ./docker/test-nat.escript
```

### Interactive Shell

```bash
# Start an Erlang shell in the client
docker compose up -d
docker exec -it nat-client rebar3 shell

# In the shell:
1> nat:discover().
ok
2> nat:get_external_address().
{ok,"203.0.113.42"}
3> nat:add_port_mapping(tcp, 8080, 8080, 3600).
{ok,{1234567890,8080,8080,3600}}
```

### Debugging

```bash
# View gateway logs
docker compose logs -f gateway

# Check iptables rules
docker exec nat-gateway iptables -t nat -L -n -v

# Test UPnP manually
docker exec nat-client upnpc -l
```

### Cleanup

```bash
docker compose down
```

## Test Script

The `test-nat.escript` runs through:

1. NAT gateway discovery
2. Address retrieval (external, internal, device)
3. TCP port mapping (add/verify)
4. UDP port mapping (add/verify)
5. List managed mappings
6. Event registration/unregistration
7. Port mapping deletion

## Configuration

### miniupnpd.conf

Key settings:
- `ext_ip=203.0.113.42` - Fake public IP (RFC 5737 TEST-NET-3)
- `ext_allow_private_ipv4=yes` - Allow Docker's private WAN interface
- `enable_natpmp=yes` - Enable NAT-PMP/PCP support
- `secure_mode=no` - Disabled for Docker (client IP differs from mapped IP)

### Networks

- **external_net** (172.20.0.0/24): Simulates WAN
- **internal_net** (172.21.0.0/24): Simulates LAN where client resides

## Requirements

- Docker 20.10+
- Docker Compose v2

## Troubleshooting

### Port mapping fails with error 501

Check that iptables chains exist:
```bash
docker exec nat-gateway iptables -t nat -L MINIUPNPD -n
docker exec nat-gateway iptables -t nat -L MINIUPNPD-POSTROUTING -n
```

### Discovery fails

Verify network connectivity:
```bash
docker exec nat-client ping -c 1 172.21.0.1
```

Check miniupnpd is listening:
```bash
docker exec nat-gateway netstat -ulnp | grep miniupnpd
```
