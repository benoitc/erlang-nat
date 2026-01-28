# API Reference

This section documents the public API modules of erlang-nat.

## Architecture

```mermaid
flowchart TB
    subgraph App["Your Application"]
        A[nat:discover/0]
        B[nat:add_port_mapping/3]
        C[nat:reg_pid/1]
    end

    subgraph NAT["erlang-nat"]
        NS[nat_server<br/>gen_server]

        subgraph Protocols["Protocol Modules"]
            U1[natupnp_v1]
            U2[natupnp_v2]
            NP[natpmp]
            PC[natpcp]
        end
    end

    subgraph Router["NAT Router"]
        R[Gateway Device]
    end

    A --> NS
    B --> NS
    C --> NS
    NS --> U1 & U2 & NP & PC
    U1 & U2 -->|UPnP/SOAP| R
    NP -->|NAT-PMP| R
    PC -->|PCP| R

    NS -.->|Events| App
```

## Modules

| Module | Description |
|--------|-------------|
| [nat](nat.md) | Main public API for NAT traversal operations |
| [nat_server](nat_server.md) | Gen_server managing port mappings with auto-renewal |

## Quick Reference

### Discovery

```erlang
nat:discover() -> ok | no_nat | {error, term()}.
nat:get_context() -> {ok, nat_ctx()} | {error, no_context}.
```

### Port Mapping

```erlang
nat:add_port_mapping(Protocol, InternalPort, ExternalPort) -> Result.
nat:add_port_mapping(Protocol, InternalPort, ExternalPort, Lifetime) -> Result.
nat:delete_port_mapping(Protocol, InternalPort, ExternalPort) -> ok | {error, term()}.
nat:list_mappings() -> [{Protocol, InternalPort, ExternalPort, ExpiresAt}].
```

### Addresses

```erlang
nat:get_external_address() -> {ok, string()} | {error, term()}.
nat:get_device_address() -> {ok, string()} | {error, term()}.
nat:get_internal_address() -> {ok, string()} | {error, term()}.
```

### Events

```erlang
nat:reg_pid(pid()) -> ok.
nat:unreg_pid(pid()) -> ok.
nat:reg_fun(fun()) -> {ok, reference()}.
nat:unreg_fun(reference()) -> ok.
```
