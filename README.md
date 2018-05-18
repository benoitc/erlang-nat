

# nat -  implements NAT handling facilities #

Copyright (c) 2016 Benoît Chesneau.

__Version:__ 0.1.0

# nat

Library that implements NAT facilities.

Currently supported protocols:
- natpmp
- UPNP v1

## Usage

### Discover the NAT gateway:

You shoiuld start by discovering the NAT gateway if it exists:

```
1> {ok, Ctx} = nat:discover().
{ok,{natupnp_v1,{nat_upnp,"http://192.168.1.254:5678/control/wan_ip_connection",
                          "192.168.1.22"}}}
```

If OK you will be able to use the context in other functions. For example to get
the external gateway:

```
2> nat:get_external_address(Ctx).
{ok,"88.163.70.217"}
```

And the local IP associated to this:

```
3> nat:get_internal_address(Ctx).
{ok,"192.168.1.22"}
```

### Map an External IP Address

```
{ok, Ctx} = nat:discover(),
Protocol = tcp,
InternalPort = 80,
ExternalPortRequest = 8080,
Lifetime = 3600,

{ok, Since, InternalPort, ExternalPortRequest, MappingLifetime} = \
    nat:add_port_mapping(Ctx, Protocol, InternalPort, ExternalPortRequest, Lifetime).
```

This map the port 80 to the port 8080.

> Note: pass 0 to the ExternalPortRequest to ask to the router to create a dynamic port.

### Remove a port mapping

```
ok = natpmp:delete_port_mapping(Ctx, Protocol, InternalPort, ExternalPort)
```

## Debug

You can scan a network and save the result to a file via:
```
./scripts/scan.sh saved_scan
```
While scanning readable logs will be created in the project's root directory under `log/`.

This file can then be re-used to simulate and debug the network with:
```
nat:debug_start(FilePath),
%Do you discover or port mapping here...
nat:debug_stop().
```




## Contribute

For issues, comments or feedback please create an [issue](https://github.com/benoitc/erlang-nat/issues).


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/benoitc/erlang-nat/blob/master/doc/nat.md" class="module">nat</a></td></tr>
<tr><td><a href="http://github.com/benoitc/erlang-nat/blob/master/doc/nat_lib.md" class="module">nat_lib</a></td></tr>
<tr><td><a href="http://github.com/benoitc/erlang-nat/blob/master/doc/natpmp.md" class="module">natpmp</a></td></tr>
<tr><td><a href="http://github.com/benoitc/erlang-nat/blob/master/doc/natupnp_v1.md" class="module">natupnp_v1</a></td></tr></table>
