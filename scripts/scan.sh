#!/bin/bash

echo -e "##########################"

if [ -z "$1" ]
  then
    echo "No file supplied"
fi

now="$(date '+%Y-%m-%d %H:%M:%S')"
rebar3 compile
mkdir -p log
FILE=$1 erl -noinput -pa _build/default/lib/*/ebin/ -eval 'nat_scan:start(), init:stop().' | tee -a "log/nat_scan_$now.log"

echo -e "##########################"
echo -e "Scan data in: $1"
echo -e "Logs: log/nat_scan_$now.log"
echo -e "##########################"
