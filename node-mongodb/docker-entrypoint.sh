#!/bin/bash

set -e

# Add mongod as command if needed
if [ "${1:0:1}" = '-' ]; then
    set -- mongod "$@"
fi

# Drop root privileges if we are running mongod
# allow the container to be started with `--user`
if [ "$1" = 'mongod' -a "$(id -u)" = '0' ]; then
    set -- gosu moAngodb "$@"
fi



# Execute the command
exec "$@"
