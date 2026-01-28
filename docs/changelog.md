# Changelog

All notable changes to erlang-nat are documented here.

## [0.5.0] - 2026

### Added

- **Port Mapping Lifecycle Manager** (`nat_server`)
  - Automatic renewal of port mappings before expiry
  - Event system for mapping and IP changes
  - Subscriber registration (pids and callback functions)
  - External IP change detection

- **Unified API** in `nat.erl`
  - `discover/0` - Discover and store NAT context
  - `add_port_mapping/3,4` - Add auto-renewed mappings
  - `delete_port_mapping/3` - Delete mappings
  - `list_mappings/0` - List managed mappings
  - `reg_pid/1`, `unreg_pid/1` - Process event registration
  - `reg_fun/1`, `unreg_fun/1` - Callback event registration

- **OTP Application Structure**
  - `nat_app.erl` - Application behaviour
  - `nat_sup.erl` - Supervisor
  - Proper supervision tree

- **Docker Test Environment**
  - miniupnpd gateway container (built from source)
  - Erlang client container
  - Automated test script
  - Full protocol support (UPnP, NAT-PMP, PCP)

- **Documentation**
  - MkDocs-based documentation with Material theme
  - API reference for all public modules
  - Protocol guides (UPnP, NAT-PMP, PCP)
  - Event system guide
  - Advanced usage patterns
  - GitHub Pages deployment via GitHub Actions

- **Project Infrastructure**
  - GitHub Actions workflow for documentation deployment
  - GitHub Sponsors funding configuration
  - Updated hex.pm metadata with documentation links

### Changed

- PCP protocol implementation updated to RFC 6887
- Upgraded hackney dependency

### Fixed

- `natpmp:system_gateways/0` now filters empty gateway entries
- Handle interfaces without gateways properly

## [0.4.0] - Previous

### Added

- PCP (Port Control Protocol) support
- NAT-PMP protocol improvements
- UPnP IGD v2 support

### Changed

- OTP 26+ compatibility
- Updated dependencies

## [0.3.0] - Previous

### Added

- UPnP port mapping listing
- Router status information
- Improved error handling

### Fixed

- Handle `http_uri` deprecation in OTP 21+

## [0.2.0] - Previous

### Added

- NAT-PMP support
- Multiple protocol discovery
- Basic port mapping API

## [0.1.0] - Initial Release

### Added

- UPnP IGD v1 support
- Basic discovery and port mapping
- Erlang/OTP library structure
