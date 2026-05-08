#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(CDPATH= cd -- "${SCRIPT_DIR}/.." && pwd)

DEV_DB_PATH="${DEV_DB_PATH:-var/ferry-services.sqlite3}"
TEST_DB_PATH="${TEST_DB_PATH:-var/ferry-services-test.sqlite3}"
DEV_SERVER_PORT="${DEV_SERVER_PORT:-3000}"
TEST_SERVER_PORT="${TEST_SERVER_PORT:-3001}"

require_command() {
  command_name="$1"

  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "Missing required command: $command_name" >&2
    exit 1
  fi
}

ensure_stack() {
  if command -v stack >/dev/null 2>&1; then
    return
  fi

  cat >&2 <<'EOF'
Missing required command: stack

Install ghcup, apply its environment, then rerun bootstrap:
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  . "$HOME/.ghcup/env"
EOF
  exit 1
}

ensure_machine_deps() {
  if command -v sqlite3 >/dev/null 2>&1; then
    return
  fi

  if ! command -v brew >/dev/null 2>&1 && ! command -v apt-get >/dev/null 2>&1; then
    return
  fi

  echo "Missing machine-level tool: sqlite3"
  echo "Installing machine-level dependencies"
  "${REPO_ROOT}/scripts/install-system-deps.sh"
}

replace_env_value() {
  env_file="$1"
  key="$2"
  value="$3"
  tmp_file="${env_file}.tmp"

  awk -v key="$key" -v value="$value" '
    BEGIN { replaced = 0 }
    index($0, key "=") == 1 {
      print key "=" value
      replaced = 1
      next
    }
    { print }
    END {
      if (replaced == 0) {
        print key "=" value
      }
    }
  ' "$env_file" >"$tmp_file"

  mv "$tmp_file" "$env_file"
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

upgrade_blank_log_level_if_needed() {
  env_file="$1"
  existing_log_level=$(read_env_value "$env_file" "LOG_LEVEL" "")

  if [ -z "$existing_log_level" ]; then
    replace_env_value "$env_file" "LOG_LEVEL" "Info"
    echo "Set default LOG_LEVEL=Info in $env_file"
  fi
}

upgrade_legacy_log_level_if_needed() {
  env_file="$1"
  existing_log_level=$(read_env_value "$env_file" "LOG_LEVEL" "")

  if [ "$existing_log_level" = "INFO" ]; then
    replace_env_value "$env_file" "LOG_LEVEL" "Info"
    echo "Updated LOG_LEVEL from INFO to Info in $env_file"
  fi
}

create_env_file_if_missing() {
  env_file="$1"
  db_path="$2"
  server_port="$3"
  environment_name="$4"

  if [ -f "$env_file" ]; then
    echo "Leaving existing $env_file in place"
    return
  fi

  cat >"$env_file" <<EOF
DB_CONNECTION=$db_path
SERVER_PORT=$server_port
SERVER_SENTRY_DSN=
SCRAPER_SENTRY_DSN=
TIMETABLE_DOCUMENT_SCRAPER_SENTRY_DSN=
WEATHER_FETCHER_SENTRY_DSN=
VESSEL_FETCHER_SENTRY_DSN=
TRANSXCHANGE_INGESTER_SENTRY_DSN=
RAIL_DEPARTURE_FETCHER_SENTRY_DSN=
AWS_ACCESS_KEY_ID=
AWS_SECRET_ACCESS_KEY=
AWS_APPLE_PLATFORM_ARN=
AWS_GOOGLE_PLATFORM_ARN=
ENVIRONMENT=$environment_name
DOCKER_HUB_USERNAME=
DOCKER_HUB_PASSWORD=
LOG_LEVEL=Info
OPENWEATHERMAP_APPID=
TRAVELLINE_FTP_ADDRESS=
TRAVELLINE_FTP_USERNAME=
TRAVELLINE_FTP_PASSWORD=
RAIL_DATA_API_KEY=
EOF

  echo "Created $env_file"
}

create_sqlite_database_if_missing() {
  db_path="$1"
  absolute_db_path="$REPO_ROOT/$db_path"

  mkdir -p "$(dirname "$absolute_db_path")"

  if [ -f "$absolute_db_path" ]; then
    echo "SQLite database already exists: $db_path"
    return
  fi

  sqlite3 "$absolute_db_path" ".read ${REPO_ROOT}/sqlite/schema.sql"
  sqlite3 "$absolute_db_path" ".read ${REPO_ROOT}/sqlite/seed.sql"
  echo "Created SQLite database: $db_path"
}

ensure_machine_deps

require_command sqlite3
ensure_stack

dev_env_file="${REPO_ROOT}/envfile.local"
test_env_file="${REPO_ROOT}/envfile-test.local"

create_env_file_if_missing "$dev_env_file" "$DEV_DB_PATH" "$DEV_SERVER_PORT" "development"
create_env_file_if_missing "$test_env_file" "$TEST_DB_PATH" "$TEST_SERVER_PORT" "test"

upgrade_blank_log_level_if_needed "$dev_env_file"
upgrade_blank_log_level_if_needed "$test_env_file"
upgrade_legacy_log_level_if_needed "$dev_env_file"
upgrade_legacy_log_level_if_needed "$test_env_file"

dev_db_path=$(read_env_value "$dev_env_file" "DB_CONNECTION" "$DEV_DB_PATH")
test_db_path=$(read_env_value "$test_env_file" "DB_CONNECTION" "$TEST_DB_PATH")

create_sqlite_database_if_missing "$dev_db_path"
create_sqlite_database_if_missing "$test_db_path"

echo "Bootstrap complete"
