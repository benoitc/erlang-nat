-define(NAT_TRIES, 5).
-define(NAT_INITIAL_MS, 250).

-define(RECOMMENDED_MAPPING_LIFETIME_SECONDS, 3600).

-record(nat_upnp, {
          service_url,
          ip}).
