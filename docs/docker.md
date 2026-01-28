# Docker Testing Environment

The `docker/` directory contains a complete Docker-based test environment for testing NAT protocols without requiring an actual NAT router.

## Architecture

```
                    External Network (172.20.0.0/24)
                              |
                    +---------+---------+
                    |   nat-gateway     |
                    |   (miniupnpd)     |
                    |   172.20.0.2      |
                    |   ext: 203.0.113.42 (fake)
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

### Gateway Container (nat-gateway)

The gateway runs miniupnpd built from source with full protocol support:

- **UPnP IGD v1/v2**: HTTP/SOAP on port 5000
- **NAT-PMP**: UDP port 5351
- **PCP**: UDP port 5351
- **SSDP**: UDP port 1900

Configuration highlights:

- External IP: `203.0.113.42` (RFC 5737 TEST-NET-3)
- Uses iptables-legacy for miniupnpd compatibility
- Private WAN interface support enabled

### Client Container (nat-client)

The client runs Erlang/OTP 26 with erlang-nat:

- Pre-compiled erlang-nat library
- Test script included
- Debugging tools (tcpdump, upnpc, etc.)

## Quick Start

### Run Tests

```bash
cd docker/
docker compose up -d
docker exec nat-client ./docker/test-nat.escript
```

Expected output:

```
=== erlang-nat Docker Test ===

Starting applications...

[Test 1] Discovering NAT gateway...
  OK: NAT gateway discovered
  Context: {natupnp_v2,{nat_upnp,"http://172.21.0.1:5000/ctl/IPConn","172.21.0.10"}}

[Test 2] Getting addresses...
  External IP: 203.0.113.42
  Internal IP: 172.21.0.10
  Device IP:   172.21.0.1

[Test 3] Adding TCP port mapping (8080 -> 8080)...
  OK: Mapping created
  Since: 1234567890, Internal: 8080, External: 8080, Lifetime: 3600s

[Test 4] Adding UDP port mapping (9000 -> 9000)...
  OK: Mapping created

[Test 5] Listing managed mappings...
  Found 2 mapping(s)

[Test 6] Testing event registration...
  OK: Registered for events
  OK: Unregistered

[Test 7] Deleting port mappings...
  OK: TCP 8080 deleted
  OK: UDP 9000 deleted

=== All tests passed! ===
```

### Interactive Shell

```bash
docker compose up -d
docker exec -it nat-client rebar3 shell
```

```erlang
1> nat:discover().
ok
2> nat:get_external_address().
{ok,"203.0.113.42"}
3> nat:add_port_mapping(tcp, 8080, 8080).
{ok,1234567890,8080,8080,3600}
4> nat:list_mappings().
[{tcp,8080,8080,1706500000}]
```

## Debugging

### View Gateway Logs

```bash
docker compose logs -f gateway
```

### Check iptables Rules

```bash
docker exec nat-gateway iptables -t nat -L -n -v
docker exec nat-gateway iptables -t nat -L MINIUPNPD -n -v
```

### Test with upnpc

```bash
docker exec nat-client upnpc -l
docker exec nat-client upnpc -a 172.21.0.10 8080 8080 TCP 3600
```

### Network Debugging

```bash
# Capture SSDP traffic
docker exec nat-gateway tcpdump -i eth1 port 1900

# Capture NAT-PMP/PCP traffic
docker exec nat-gateway tcpdump -i eth1 port 5351

# Check connectivity
docker exec nat-client ping -c 1 172.21.0.1
```

## Configuration Files

### docker-compose.yml

```yaml
services:
  gateway:
    build:
      context: ..
      dockerfile: docker/Dockerfile.gateway
    container_name: nat-gateway
    cap_add:
      - NET_ADMIN
    sysctls:
      - net.ipv4.ip_forward=1
    networks:
      external_net:
        ipv4_address: 172.20.0.2
      internal_net:
        ipv4_address: 172.21.0.1
    healthcheck:
      test: ["CMD", "pgrep", "miniupnpd"]
      interval: 5s
      timeout: 3s
      retries: 3

  client:
    build:
      context: ..
      dockerfile: docker/Dockerfile.client
    container_name: nat-client
    depends_on:
      gateway:
        condition: service_healthy
    networks:
      internal_net:
        ipv4_address: 172.21.0.10
    dns: 8.8.8.8

networks:
  external_net:
    driver: bridge
    ipam:
      config:
        - subnet: 172.20.0.0/24
          gateway: 172.20.0.254

  internal_net:
    driver: bridge
    ipam:
      config:
        - subnet: 172.21.0.0/24
          gateway: 172.21.0.254
```

### miniupnpd.conf

Key settings:

```ini
# Fake public IP (TEST-NET-3)
ext_ip=203.0.113.42

# Allow private WAN interface (Docker)
ext_allow_private_ipv4=yes

# Enable protocols
enable_natpmp=yes
enable_upnp=yes

# Disable secure mode for Docker
secure_mode=no

# Allow all ports from any internal IP
allow 1024-65535 0.0.0.0/0 1024-65535
```

## Cleanup

```bash
docker compose down
```

## Troubleshooting

### Discovery Fails

1. Check gateway is healthy: `docker compose ps`
2. Verify network connectivity: `docker exec nat-client ping 172.21.0.1`
3. Check miniupnpd logs: `docker compose logs gateway`

### Port Mapping Fails (Error 501)

Check iptables chains exist:

```bash
docker exec nat-gateway iptables -t nat -L MINIUPNPD -n
docker exec nat-gateway iptables -t nat -L MINIUPNPD-POSTROUTING -n
```

If missing, restart the gateway:

```bash
docker compose restart gateway
```

### Container Won't Start

Check for port conflicts:

```bash
docker compose down
docker compose up -d
```

### Rebuild After Changes

```bash
docker compose down
docker compose build --no-cache
docker compose up -d
```

## Writing Custom Tests

Create a test script in `docker/`:

```erlang
#!/usr/bin/env escript
%% -*- erlang -*-
-mode(compile).

main(_) ->
    %% Add code paths
    {ok, Libs} = file:list_dir("_build/default/lib"),
    [code:add_patha(filename:join(["_build/default/lib", L, "ebin"])) 
     || L <- Libs],
    
    %% Start application
    {ok, _} = application:ensure_all_started(nat),
    
    %% Your tests here
    ok = nat:discover(),
    {ok, Ext} = nat:get_external_address(),
    io:format("External IP: ~s~n", [Ext]),
    
    halt(0).
```

Run it:

```bash
docker exec nat-client escript your_test.escript
```
