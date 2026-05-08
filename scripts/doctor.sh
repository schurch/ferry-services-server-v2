#!/bin/sh

set -eu

status=0

check_command() {
  command_name="$1"

  if command -v "$command_name" >/dev/null 2>&1; then
    echo "ok: $command_name"
  else
    echo "missing: $command_name" >&2
    status=1
  fi
}

check_file() {
  file_path="$1"

  if [ -f "$file_path" ]; then
    echo "ok: $file_path"
  else
    echo "missing: $file_path" >&2
    status=1
  fi
}

read_env_value() {
  env_file="$1"
  key="$2"
  fallback="$3"
  value=$(grep -E "^${key}=" "$env_file" 2>/dev/null | cut -d'=' -f2- || true)

  if [ -n "$value" ]; then
    echo "$value"
  else
    echo "$fallback"
  fi
}

check_sqlite_db() {
  env_file="$1"
  fallback="$2"

  if [ ! -f "$env_file" ]; then
    return
  fi

  db_file=$(read_env_value "$env_file" "DB_CONNECTION" "$fallback")

  if [ ! -f "$db_file" ]; then
    echo "missing: $db_file" >&2
    status=1
    return
  fi

  if sqlite3 "$db_file" "SELECT 1;" >/dev/null 2>&1; then
    echo "ok: $db_file opens"
  else
    echo "invalid sqlite database: $db_file" >&2
    status=1
  fi
}

check_command stack
check_command sqlite3

if command -v docker >/dev/null 2>&1; then
  echo "ok: docker"
else
  echo "optional missing: docker (required for production image build/deploy only)"
fi

if command -v docker-compose >/dev/null 2>&1 || { command -v docker >/dev/null 2>&1 && docker compose version >/dev/null 2>&1; }; then
  echo "ok: docker compose"
else
  echo "optional missing: docker compose (required for production deploy only)"
fi

check_file envfile.local
check_file envfile-test.local
check_file sqlite/schema.sql
check_file sqlite/seed.sql
check_file docker/docker-compose-prod.yml
check_file scripts/deploy-prod.sh
check_file scripts/sqlite-maintenance.sh

check_sqlite_db envfile.local var/ferry-services.sqlite3
check_sqlite_db envfile-test.local var/ferry-services-test.sqlite3

if [ "$status" -eq 0 ]; then
  echo "Doctor checks passed"
else
  echo "Doctor checks failed" >&2
fi

exit "$status"
