-define(NAT_TRIES, 5).
-define(NAT_INITIAL_MS, 250).

-define(RECOMMENDED_MAPPING_LIFETIME_SECONDS, 7200).

%% Renewal at 50% of lifetime (industry standard)
-define(RENEWAL_FACTOR, 0.5).
%% Minimum renewal interval in seconds
-define(MIN_RENEWAL_INTERVAL, 60).
%% Maximum retry count before giving up
-define(MAX_RENEWAL_RETRIES, 3).
%% Initial retry delay in milliseconds
-define(INITIAL_RETRY_DELAY_MS, 5000).

%% IP polling interval for UPnP (no native events) - 5 minutes
-define(IP_POLL_INTERVAL_MS, 300000).

%% NAT-PMP multicast address and port for IP change announcements
-define(NATPMP_MULTICAST_ADDR, {224, 0, 0, 1}).
-define(NATPMP_MULTICAST_PORT, 5350).

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

%% Managed mapping record for nat_server
-record(mapping, {
    key           :: {Protocol :: tcp | udp, InternalPort :: non_neg_integer()},
    external_port :: non_neg_integer(),
    since         :: non_neg_integer(),
    lifetime      :: non_neg_integer() | infinity,
    expires_at    :: non_neg_integer() | infinity,
    timer_ref     :: reference() | undefined,
    retry_count   :: non_neg_integer()
}).

%% NAT server state
-record(nat_state, {
    %% NAT context from discovery
    nat_ctx :: term() | undefined,

    %% Current external IP
    external_ip :: string() | undefined,

    %% Last epoch (NAT-PMP/PCP) for detecting IP changes
    last_epoch :: non_neg_integer() | undefined,

    %% Active mappings: key -> #mapping{}
    mappings :: #{term() => #mapping{}},

    %% Registered pids (monitored): pid -> monitor ref
    reg_pids :: #{pid() => reference()},

    %% Registered functions: ref -> fun
    reg_funs :: #{reference() => fun()},

    %% IP poll timer (for UPnP)
    ip_timer :: reference() | undefined,

    %% Multicast listener pid (NAT-PMP/PCP only)
    listener_pid :: pid() | undefined,

    %% Options
    opts :: map()
}).

%% Event types
-type nat_event() ::
    %% Mapping events
    {mapping_renewed, Protocol :: tcp | udp, InternalPort :: non_neg_integer(),
     ExternalPort :: non_neg_integer(), NewLifetime :: non_neg_integer()} |
    {mapping_failed, Protocol :: tcp | udp, InternalPort :: non_neg_integer(),
     ExternalPort :: non_neg_integer(), Reason :: term()} |
    {mapping_expired, Protocol :: tcp | udp, InternalPort :: non_neg_integer(),
     ExternalPort :: non_neg_integer()} |

    %% IP change events
    {ip_changed, OldIp :: string(), NewIp :: string()} |

    %% Context events
    {context_lost, Reason :: term()} |
    {context_restored, NatCtx :: term()}.

-export_type([nat_event/0]).
