

# Module natpmp #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-natpmp_error">natpmp_error()</a> ###


<pre><code>
natpmp_error() = unsupported_version | not_authorized | network_failure | out_of_resource | unsupported_opcode | bad_response
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_port_mapping-4">add_port_mapping/4</a></td><td>add a port mapping with default lifetime.</td></tr><tr><td valign="top"><a href="#add_port_mapping-5">add_port_mapping/5</a></td><td>add a port mapping.</td></tr><tr><td valign="top"><a href="#delete_port_mapping-4">delete_port_mapping/4</a></td><td>delete a port mapping.</td></tr><tr><td valign="top"><a href="#discover-0">discover/0</a></td><td>discover a Nat gateway.</td></tr><tr><td valign="top"><a href="#get_device_address-1">get_device_address/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_external_address-1">get_external_address/1</a></td><td>get external ip.</td></tr><tr><td valign="top"><a href="#get_internal_address-1">get_internal_address/1</a></td><td>get internal address used for this gateway.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_port_mapping-4"></a>

### add_port_mapping/4 ###

<pre><code>
add_port_mapping(Gateway, Protocol, InternalPort, ExternalPortRequest) -&gt; {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Gateway = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a></code></li><li><code>Protocol = tcp | udp</code></li><li><code>InternalPort = non_neg_integer()</code></li><li><code>ExternalPortRequest = non_neg_integer()</code></li><li><code>Since = non_neg_integer()</code></li><li><code>ExternalPort = non_neg_integer()</code></li><li><code>MappingLifetime = non_neg_integer()</code></li><li><code>Reason = <a href="#type-natpmp_error">natpmp_error()</a></code></li></ul>

add a port mapping with default lifetime

<a name="add_port_mapping-5"></a>

### add_port_mapping/5 ###

<pre><code>
add_port_mapping(Gateway, Protocol, InternalPort, ExternalPortRequest, Lifetime) -&gt; {ok, Since, InternalPort, ExternalPort, MappingLifetime} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Gateway = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a></code></li><li><code>Protocol = tcp | udp</code></li><li><code>InternalPort = non_neg_integer()</code></li><li><code>ExternalPortRequest = non_neg_integer()</code></li><li><code>Lifetime = non_neg_integer()</code></li><li><code>Since = non_neg_integer()</code></li><li><code>ExternalPort = non_neg_integer()</code></li><li><code>MappingLifetime = non_neg_integer()</code></li><li><code>Reason = <a href="#type-natpmp_error">natpmp_error()</a></code></li></ul>

add a port mapping

<a name="delete_port_mapping-4"></a>

### delete_port_mapping/4 ###

<pre><code>
delete_port_mapping(Gateway, Protocol, InternalPort, ExternalPortRequest) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Gateway = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a></code></li><li><code>Protocol = tcp | udp</code></li><li><code>InternalPort = non_neg_integer()</code></li><li><code>ExternalPortRequest = non_neg_integer()</code></li><li><code>Reason = <a href="#type-natpmp_error">natpmp_error()</a></code></li></ul>

delete a port mapping

<a name="discover-0"></a>

### discover/0 ###

<pre><code>
discover() -&gt; {ok, Gateway} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Gateway = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Reason = any()</code></li></ul>

discover a Nat gateway

<a name="get_device_address-1"></a>

### get_device_address/1 ###

<pre><code>
get_device_address(Gateway) -&gt; {ok, Ip} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Gateway = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a></code></li><li><code>Ip = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a></code></li><li><code>Reason = <a href="#type-natpmp_error">natpmp_error()</a></code></li></ul>

<a name="get_external_address-1"></a>

### get_external_address/1 ###

<pre><code>
get_external_address(Gateway) -&gt; {ok, ExternalIp} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Gateway = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a></code></li><li><code>ExternalIp = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a></code></li><li><code>Reason = <a href="#type-natpmp_error">natpmp_error()</a></code></li></ul>

get external ip

<a name="get_internal_address-1"></a>

### get_internal_address/1 ###

<pre><code>
get_internal_address(Gateway) -&gt; {ok, InternalIp} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Gateway = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a></code></li><li><code>InternalIp = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a></code></li><li><code>Reason = any()</code></li></ul>

get internal address used for this gateway

