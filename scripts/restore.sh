#!/bin/bash

set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: $0 <backup.sqlite3.gz>" >&2
  exit 1
fi

db_file="${DB_FILE:-/home/stefanchurch/ferry-services-server/data/ferry-services.sqlite3}"
backup_file="$1"

if [ -f "$db_file" ]; then
  echo "Refusing to overwrite existing database: $db_file" >&2
  exit 1
fi

mkdir -p "$(dirname "$db_file")"
gunzip -c "$backup_file" >"$db_file"

echo "Restored database: $db_file"
