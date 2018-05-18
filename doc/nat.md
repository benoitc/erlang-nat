

# Module nat #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-nat_ctx">nat_ctx()</a> ###


<pre><code>
nat_ctx() = any()
</code></pre>




### <a name="type-nat_protocol">nat_protocol()</a> ###


<pre><code>
nat_protocol() = tcp | udp
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_port_mapping-4">add_port_mapping/4</a></td><td>add a port mapping with default lifetime.</td></tr><tr><td valign="top"><a href="#add_port_mapping-5">add_port_mapping/5</a></td><td>add a port mapping.</td></tr><tr><td valign="top"><a href="#debug_start-1">debug_start/1</a></td><td></td></tr><tr><td valign="top"><a href="#debug_stop-0">debug_stop/0</a></td><td></td></tr><tr><td valign="top"><a href="#delete_port_mapping-4">delete_port_mapping/4</a></td><td>delete a port mapping.</td></tr><tr><td valign="top"><a href="#discover-0">discover/0</a></td><td>discover a NAT gateway and return a context that can be used with
othe functions.</td></tr><tr><td valign="top"><a href="#get_device_address-1">get_device_address/1</a></td><td>get the IP address of the gateway.</td></tr><tr><td valign="top"><a href="#get_external_address-1">get_external_address/1</a></td><td>return the external address of the gateway device.</td></tr><tr><td valign="top"><a href="#get_internal_address-1">get_internal_address/1</a></td><td>return the address address of the local device.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_port_mapping-4"></a>

### add_port_mapping/4 ###

<pre><code>
add_port_mapping(NatCtx, Protocol, InternalPort, ExternalPortRequest) -&gt; {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>NatCtx = <a href="#type-nat_ctx">nat_ctx()</a></code></li><li><code>Protocol = <a href="#type-nat_protocol">nat_protocol()</a></code></li><li><code>InternalPort = non_neg_integer()</code></li><li><code>ExternalPortRequest = non_neg_integer()</code></li><li><code>Since = non_neg_integer()</code></li><li><code>ExternalPort = non_neg_integer()</code></li><li><code>MappingLifetime = non_neg_integer()</code></li><li><code>Reason = any() | timeout</code></li></ul>

add a port mapping with default lifetime

<a name="add_port_mapping-5"></a>

### add_port_mapping/5 ###

<pre><code>
add_port_mapping(NatCtx, Protocol, InternalPort, ExternalPortRequest, Lifetime) -&gt; {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>NatCtx = <a href="#type-nat_ctx">nat_ctx()</a></code></li><li><code>Protocol = <a href="#type-nat_protocol">nat_protocol()</a></code></li><li><code>InternalPort = non_neg_integer()</code></li><li><code>ExternalPortRequest = non_neg_integer()</code></li><li><code>Lifetime = non_neg_integer()</code></li><li><code>Since = non_neg_integer()</code></li><li><code>ExternalPort = non_neg_integer()</code></li><li><code>MappingLifetime = non_neg_integer()</code></li><li><code>Reason = any() | timeout()</code></li></ul>

add a port mapping

<a name="debug_start-1"></a>

### debug_start/1 ###

<pre><code>
debug_start(File::string()) -&gt; ok
</code></pre>
<br />

<a name="debug_stop-0"></a>

### debug_stop/0 ###

<pre><code>
debug_stop() -&gt; ok
</code></pre>
<br />

<a name="delete_port_mapping-4"></a>

### delete_port_mapping/4 ###

<pre><code>
delete_port_mapping(NatCtx, Protocol, InternalPort, ExternalPortRequest) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>NatCtx = <a href="#type-nat_ctx">nat_ctx()</a></code></li><li><code>Protocol = <a href="#type-nat_protocol">nat_protocol()</a></code></li><li><code>InternalPort = non_neg_integer()</code></li><li><code>ExternalPortRequest = non_neg_integer()</code></li><li><code>Reason = any() | timeout</code></li></ul>

delete a port mapping

<a name="discover-0"></a>

### discover/0 ###

<pre><code>
discover() -&gt; {ok, NatCtx} | no_nat
</code></pre>

<ul class="definitions"><li><code>NatCtx = <a href="#type-nat_ctx">nat_ctx()</a></code></li></ul>

discover a NAT gateway and return a context that can be used with
othe functions.

<a name="get_device_address-1"></a>

### get_device_address/1 ###

<pre><code>
get_device_address(NatCtx) -&gt; {ok, DeviceIp} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>NatCtx = <a href="#type-nat_ctx">nat_ctx()</a></code></li><li><code>DeviceIp = string()</code></li><li><code>Reason = any()</code></li></ul>

get the IP address of the gateway.

<a name="get_external_address-1"></a>

### get_external_address/1 ###

<pre><code>
get_external_address(NatCtx) -&gt; {ok, ExternalIp} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>NatCtx = <a href="#type-nat_ctx">nat_ctx()</a></code></li><li><code>ExternalIp = string()</code></li><li><code>Reason = any()</code></li></ul>

return the external address of the gateway device

<a name="get_internal_address-1"></a>

### get_internal_address/1 ###

<pre><code>
get_internal_address(NatCtx) -&gt; {ok, InternalIp} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>NatCtx = <a href="#type-nat_ctx">nat_ctx()</a></code></li><li><code>InternalIp = string()</code></li><li><code>Reason = any()</code></li></ul>

return the address address of the local device

