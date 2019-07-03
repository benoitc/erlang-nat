

# Module natupnp_v1 #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Client for UPnP Device Control Protocol Internet Gateway Device v1.

<a name="description"></a>

## Description ##
documented in detail at: http://upnp.org/specs/gw/UPnP-gw-InternetGatewayDevice-v1-Device.pdf<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_port_mapping-4">add_port_mapping/4</a></td><td>Add a port mapping with default lifetime to 0 seconds.</td></tr><tr><td valign="top"><a href="#add_port_mapping-5">add_port_mapping/5</a></td><td>Add a port mapping and release after Timeout.</td></tr><tr><td valign="top"><a href="#delete_port_mapping-4">delete_port_mapping/4</a></td><td>Delete a port mapping from the router.</td></tr><tr><td valign="top"><a href="#discover-0">discover/0</a></td><td>discover the gateway and our IP to associate.</td></tr><tr><td valign="top"><a href="#get_device_address-1">get_device_address/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_external_address-1">get_external_address/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_internal_address-1">get_internal_address/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_port_mapping-3">get_port_mapping/3</a></td><td>get specific port mapping for a well known port and protocol.</td></tr><tr><td valign="top"><a href="#get_wan_device-2">get_wan_device/2</a></td><td></td></tr><tr><td valign="top"><a href="#status_info-1">status_info/1</a></td><td>get router status.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_port_mapping-4"></a>

### add_port_mapping/4 ###

<pre><code>
add_port_mapping(Context::<a href="nat.md#type-nat_upnp">nat:nat_upnp()</a>, Protocol::<a href="nat.md#type-nat_protocol">nat:nat_protocol()</a>, InternalPort::integer(), ExternalPort::integer()) -&gt; {ok, non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()} | {error, any()}
</code></pre>
<br />

Add a port mapping with default lifetime to 0 seconds

<a name="add_port_mapping-5"></a>

### add_port_mapping/5 ###

<pre><code>
add_port_mapping(Ctx::<a href="nat.md#type-nat_upnp">nat:nat_upnp()</a>, Protocol0::<a href="nat.md#type-nat_protocol">nat:nat_protocol()</a>, InternalPort::integer(), ExternalPort::integer(), Lifetime::integer()) -&gt; {ok, non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()} | {error, any()}
</code></pre>
<br />

Add a port mapping and release after Timeout

<a name="delete_port_mapping-4"></a>

### delete_port_mapping/4 ###

<pre><code>
delete_port_mapping(Context::<a href="nat.md#type-nat_upnp">nat:nat_upnp()</a>, Protocol::<a href="nat.md#type-nat_protocol">nat:nat_protocol()</a>, InternalPort::integer(), ExternalPort::integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete a port mapping from the router

<a name="discover-0"></a>

### discover/0 ###

<pre><code>
discover() -&gt; {ok, Context::<a href="nat.md#type-nat_upnp">nat:nat_upnp()</a>} | {error, term()}
</code></pre>
<br />

discover the gateway and our IP to associate

<a name="get_device_address-1"></a>

### get_device_address/1 ###

`get_device_address(Nat_upnp) -> any()`

<a name="get_external_address-1"></a>

### get_external_address/1 ###

`get_external_address(Nat_upnp) -> any()`

<a name="get_internal_address-1"></a>

### get_internal_address/1 ###

`get_internal_address(Nat_upnp) -> any()`

<a name="get_port_mapping-3"></a>

### get_port_mapping/3 ###

<pre><code>
get_port_mapping(Context::<a href="nat.md#type-nat_upnp">nat:nat_upnp()</a>, Protocol::<a href="nat.md#type-nat_protocol">nat:nat_protocol()</a>, ExternalPort::integer()) -&gt; {ok, InternalPort::integer(), InternalAddress::string()} | {error, any()}
</code></pre>
<br />

get specific port mapping for a well known port and protocol

<a name="get_wan_device-2"></a>

### get_wan_device/2 ###

`get_wan_device(D, RootUrl) -> any()`

<a name="status_info-1"></a>

### status_info/1 ###

<pre><code>
status_info(Context::<a href="nat.md#type-nat_upnp">nat:nat_upnp()</a>) -&gt; {Status::string(), LastConnectionError::string(), Uptime::string()} | {error, term()}
</code></pre>
<br />

get router status

