-define(NAT_TRIES, 5).
-define(NAT_INITIAL_MS, 250).

%% Port mapping lifetime in seconds.
%% NAT-PMP RFC (https://tools.ietf.org/html/rfc6886) recommends to set it
%% to 7200 seconds (two hours). No recommendation for UPnP found.
-define(RECOMMENDED_MAPPING_LIFETIME_SECONDS, 7200).

-record(nat_upnp, {
          service_url,
          ip}).

-record(state, {
          mappings = [],
          httpc_profile
         }).
