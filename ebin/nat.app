{application, nat, [
	{description, "implements NAT handling facilities"},
	{vsn, "0.1.°"},
	{modules, ['nat','nat_lib','natpmp','natupnp_v1']},
	{registered, []},
	{applications, [kernel,stdlib,inet_cidr,inet_ext]}
]}.