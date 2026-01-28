#!/bin/bash
set -e

# Wait for interfaces to be ready
sleep 2

# Detect interfaces by subnet (external=172.20.x, internal=172.21.x)
EXT_IF=$(ip -4 addr | awk '/inet 172\.20\./ {print $NF}')
INT_IF=$(ip -4 addr | awk '/inet 172\.21\./ {print $NF}')

echo "=== NAT Gateway Configuration ==="
echo "External interface: $EXT_IF"
echo "Internal interface: $INT_IF"

# Get IP addresses (BusyBox-compatible)
EXT_IP=$(ip -4 addr show "$EXT_IF" | awk '/inet / {split($2, a, "/"); print a[1]}')
INT_IP=$(ip -4 addr show "$INT_IF" | awk '/inet / {split($2, a, "/"); print a[1]}')
INT_NET=$(ip -4 route | grep "$INT_IF" | grep -v default | awk '{print $1}')

echo "External IP: $EXT_IP"
echo "Internal IP: $INT_IP"
echo "Internal Network: $INT_NET"

# IP forwarding is set via docker-compose sysctls
echo "IP forwarding: $(cat /proc/sys/net/ipv4/ip_forward)"

# Flush existing rules
iptables -F
iptables -t nat -F
iptables -t mangle -F

# Default policies
iptables -P INPUT ACCEPT
iptables -P FORWARD ACCEPT
iptables -P OUTPUT ACCEPT

# NAT masquerade for outbound traffic
iptables -t nat -A POSTROUTING -o "$EXT_IF" -j MASQUERADE

# Allow forwarding between interfaces
iptables -A FORWARD -i "$INT_IF" -o "$EXT_IF" -j ACCEPT
iptables -A FORWARD -i "$EXT_IF" -o "$INT_IF" -m state --state RELATED,ESTABLISHED -j ACCEPT

# Create miniupnpd chains
iptables -t nat -N MINIUPNPD 2>/dev/null || iptables -t nat -F MINIUPNPD
iptables -t nat -N MINIUPNPD-POSTROUTING 2>/dev/null || iptables -t nat -F MINIUPNPD-POSTROUTING
iptables -t filter -N MINIUPNPD 2>/dev/null || iptables -t filter -F MINIUPNPD

# Insert jumps to miniupnpd chains
iptables -t nat -A PREROUTING -i "$EXT_IF" -j MINIUPNPD
iptables -t nat -A POSTROUTING -o "$EXT_IF" -j MINIUPNPD-POSTROUTING
iptables -t filter -A FORWARD -i "$EXT_IF" ! -o "$EXT_IF" -j MINIUPNPD

echo "iptables configured"

# Update miniupnpd config with actual interface names
# Note: ext_ip is kept as TEST-NET address (203.0.113.42) for testing
sed -i "s/^ext_ifname=.*/ext_ifname=$EXT_IF/" /etc/miniupnpd/miniupnpd.conf
sed -i "s/^listening_ip=.*/listening_ip=$INT_IF/" /etc/miniupnpd/miniupnpd.conf

# Add allow rule for internal network if not present
if ! grep -q "allow .* $INT_NET" /etc/miniupnpd/miniupnpd.conf; then
    echo "allow 1024-65535 $INT_NET 1024-65535" >> /etc/miniupnpd/miniupnpd.conf
fi

echo "=== Starting miniupnpd ==="
cat /etc/miniupnpd/miniupnpd.conf

# Start miniupnpd in foreground with debug
exec miniupnpd -d -f /etc/miniupnpd/miniupnpd.conf -i "$EXT_IF" -a "$INT_IF"
