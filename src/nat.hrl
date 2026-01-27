-define(NAT_TRIES, 5).
-define(NAT_INITIAL_MS, 250).

-define(RECOMMENDED_MAPPING_LIFETIME_SECONDS, 7200).

-record(nat_upnp, {
          service_url,
          ip}).

-record(port_mapping, {
          remote_host :: string(),
          external_port :: non_neg_integer(),
          protocol :: tcp | udp,
          internal_port :: non_neg_integer(),
          internal_client :: string(),
          enabled :: boolean(),
          description :: string(),
          lease_duration :: non_neg_integer()
}).
