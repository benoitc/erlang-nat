

# Module natupnp_v1 #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-nat_upnp">nat_upnp()</a> ###


<pre><code>
nat_upnp() = #nat_upnp{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_port_mapping-4">add_port_mapping/4</a></td><td>Add a port mapping with default lifetime to 3600 seconds.</td></tr><tr><td valign="top"><a href="#add_port_mapping-5">add_port_mapping/5</a></td><td>Add a port mapping and release after Timeout.</td></tr><tr><td valign="top"><a href="#delete_port_mapping-4">delete_port_mapping/4</a></td><td>Delete a port mapping from the router.</td></tr><tr><td valign="top"><a href="#discover-0">discover/0</a></td><td>discover the gateway and our IP to associate.</td></tr><tr><td valign="top"><a href="#get_device_address-1">get_device_address/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_external_address-1">get_external_address/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_internal_address-1">get_internal_address/1</a></td><td></td></tr><tr><td valign="top"><a href="#status_info-1">status_info/1</a></td><td>get router status.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_port_mapping-4"></a>

### add_port_mapping/4 ###

<pre><code>
add_port_mapping(Context::<a href="#type-nat_upnp">nat_upnp()</a>, Protocol::tcp | udp, ExternalPort::integer(), InternalPort::integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

Add a port mapping with default lifetime to 3600 seconds

<a name="add_port_mapping-5"></a>

### add_port_mapping/5 ###

<pre><code>
add_port_mapping(Context::<a href="nat_upnp_proto.md#type-nat_upnp">nat_upnp_proto:nat_upnp()</a>, Protocol::tcp | udp, InternalPort::integer(), ExternalPort::integer(), Lifetime::integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

Add a port mapping and release after Timeout

<a name="delete_port_mapping-4"></a>

### delete_port_mapping/4 ###

<pre><code>
delete_port_mapping(Context::<a href="#type-nat_upnp">nat_upnp()</a>, Protocol::tcp | udp, InternalPort::integer(), ExternalPort::integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

Delete a port mapping from the router

<a name="discover-0"></a>

### discover/0 ###

<pre><code>
discover() -&gt; {ok, Context::<a href="#type-nat_upnp">nat_upnp()</a>} | {error, term()}
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

<a name="status_info-1"></a>

### status_info/1 ###

<pre><code>
status_info(Context::<a href="#type-nat_upnp">nat_upnp()</a>) -&gt; {Status::string(), LastConnectionError::string(), Uptime::string()} | {error, term()}
</code></pre>
<br />

get router status

