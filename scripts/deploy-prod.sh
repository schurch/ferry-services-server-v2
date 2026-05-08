#!/bin/bash

set -euo pipefail

compose_file="docker-compose-prod.yml"
db_dir="./data"
db_file="${db_dir}/ferry-services.sqlite3"
maintenance_script="./scripts/sqlite-maintenance.sh"

if [ ! -f "$maintenance_script" ]; then
  maintenance_script="./sqlite-maintenance.sh"
fi

compose() {
  if command -v docker-compose >/dev/null 2>&1; then
    docker-compose "$@"
  else
    docker compose "$@"
  fi
}

mkdir -p "$db_dir" ./offline

compose -f "$compose_file" pull

DB_FILE="$db_file" sh "$maintenance_script" init
DB_FILE="$db_file" sh "$maintenance_script" migrate

compose -f "$compose_file" up -d --remove-orphans

docker image prune -f >/dev/null 2>&1 || true
