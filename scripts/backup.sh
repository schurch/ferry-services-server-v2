#!/bin/bash

set -euo pipefail

db_file="${DB_FILE:-/home/stefanchurch/ferry-services-server/data/ferry-services.sqlite3}"
backup_dir="${BACKUP_DIR:-/home/stefanchurch/ferry-services-server/backups}"
backup_name="$(date +%y-%m-%d-%H%M%S)-ferry-services.sqlite3"

mkdir -p "$backup_dir"

sqlite3 "$db_file" ".backup '${backup_dir}/${backup_name}'"
gzip "${backup_dir}/${backup_name}"

echo "Created backup: ${backup_dir}/${backup_name}.gz"
