# erlang-nat

NAT traversal library for Erlang/OTP supporting UPnP IGD, NAT-PMP, and PCP protocols.

[![Hex.pm](https://img.shields.io/hexpm/v/nat.svg)](https://hex.pm/packages/nat)
[![GitHub Actions](https://github.com/benoitc/erlang-nat/actions/workflows/docs.yml/badge.svg)](https://github.com/benoitc/erlang-nat/actions)

## Documentation

**Full documentation: [benoitc.github.io/erlang-nat](https://benoitc.github.io/erlang-nat/)**

- [Getting Started](https://benoitc.github.io/erlang-nat/getting-started/)
- [API Reference](https://benoitc.github.io/erlang-nat/api/)
- [Protocols](https://benoitc.github.io/erlang-nat/protocols/)
- [Events](https://benoitc.github.io/erlang-nat/events/)

## Features

- **Multiple Protocols**: UPnP IGD (v1 & v2), NAT-PMP, PCP
- **Auto-renewal**: Port mappings are automatically renewed before expiry
- **Event System**: Get notified of mapping changes and IP changes
- **IPv6 Support**: Full IPv6 support via PCP protocol
- **OTP Application**: Runs as a supervised OTP application

## Quick Start

```erlang
%% Add to your rebar.config
{deps, [{nat, "0.5.0"}]}.
```

```erlang
%% Start the application
application:ensure_all_started(nat).

%% Discover NAT gateway
ok = nat:discover().

%% Get external IP
{ok, ExtIp} = nat:get_external_address().

%% Add port mapping (auto-renewed)
{ok, _Since, 8333, 8333, 3600} = nat:add_port_mapping(tcp, 8333, 8333).

%% Register for events
nat:reg_pid(self()).
receive
    {nat_event, {ip_changed, OldIp, NewIp}} ->
        io:format("IP changed: ~s -> ~s~n", [OldIp, NewIp])
end.

%% Delete mapping
ok = nat:delete_port_mapping(tcp, 8333, 8333).
```

## Requirements

- Erlang/OTP 26 or later
- rebar3

## License

MIT License - see [LICENSE](LICENSE) for details.

## Support

- [GitHub Issues](https://github.com/benoitc/erlang-nat/issues)
- [GitHub Sponsors](https://github.com/sponsors/benoitc)
