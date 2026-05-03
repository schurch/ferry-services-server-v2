#!/bin/sh

set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(CDPATH= cd -- "${SCRIPT_DIR}/.." && pwd)

DEV_DB_NAME="${DEV_DB_NAME:-ferry-services}"
TEST_DB_NAME="${TEST_DB_NAME:-ferry-services-test}"
DB_HOST="${DB_HOST:-localhost}"
DB_PORT="${DB_PORT:-5432}"
DB_USER="${DB_USER:-$USER}"
DB_SOCKET_DIR="${DB_SOCKET_DIR:-/var/run/postgresql}"
DEV_SERVER_PORT="${DEV_SERVER_PORT:-3000}"
TEST_SERVER_PORT="${TEST_SERVER_PORT:-3001}"
DB_ADMIN_USER="${DB_ADMIN_USER:-postgres}"
DB_ADMIN_MODE=""

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
  missing_tools=""

  for command_name in createdb psql; do
    if ! command -v "$command_name" >/dev/null 2>&1; then
      missing_tools="$missing_tools $command_name"
    fi
  done

  if ! command -v brew >/dev/null 2>&1 && ! command -v apt-get >/dev/null 2>&1; then
    return
  fi

  if [ -n "$missing_tools" ]; then
    echo "Missing machine-level tools:$missing_tools"
    echo "Installing machine-level dependencies"
    "${REPO_ROOT}/scripts/install-system-deps.sh"
  fi
}

detect_db_admin_mode() {
  if psql -d postgres -tAc "SELECT rolsuper FROM pg_roles WHERE rolname = current_user" 2>/dev/null | grep -q t; then
    DB_ADMIN_MODE="direct"
    return
  fi

  if command -v sudo >/dev/null 2>&1 && sudo -u "$DB_ADMIN_USER" psql -d postgres -tAc "SELECT rolsuper FROM pg_roles WHERE rolname = current_user" 2>/dev/null | grep -q t; then
    DB_ADMIN_MODE="sudo"
    return
  fi

  cat >&2 <<EOF
Could not connect to PostgreSQL as a superuser, either as the current user or via sudo as '${DB_ADMIN_USER}'.

If your local PostgreSQL admin role is not '${DB_ADMIN_USER}', rerun bootstrap with:
  DB_ADMIN_USER=<admin-role> make bootstrap-dev

Or connect manually and ensure these roles exist:
  - admin role: ${DB_ADMIN_USER}
  - app role: ${DB_USER}
EOF
  exit 1
}

run_psql_admin() {
  sql="$1"

  case "$DB_ADMIN_MODE" in
    direct)
      psql -d postgres -v ON_ERROR_STOP=1 -c "$sql" >/dev/null
      ;;
    sudo)
      sudo -u "$DB_ADMIN_USER" psql -d postgres -v ON_ERROR_STOP=1 -c "$sql" >/dev/null
      ;;
    *)
      echo "Database admin mode is not initialized" >&2
      exit 1
      ;;
  esac
}

run_psql_admin_db() {
  db_name="$1"
  sql="$2"

  case "$DB_ADMIN_MODE" in
    direct)
      psql -d "$db_name" -v ON_ERROR_STOP=1 -c "$sql" >/dev/null
      ;;
    sudo)
      sudo -u "$DB_ADMIN_USER" psql -d "$db_name" -v ON_ERROR_STOP=1 -c "$sql" >/dev/null
      ;;
    *)
      echo "Database admin mode is not initialized" >&2
      exit 1
      ;;
  esac
}

run_createdb_admin() {
  db_name="$1"
  db_owner="$2"

  case "$DB_ADMIN_MODE" in
    direct)
      createdb -O "$db_owner" "$db_name"
      ;;
    sudo)
      sudo -u "$DB_ADMIN_USER" createdb -O "$db_owner" "$db_name"
      ;;
    *)
      echo "Database admin mode is not initialized" >&2
      exit 1
      ;;
  esac
}

role_exists() {
  role_name="$1"

  case "$DB_ADMIN_MODE" in
    direct)
      psql -d postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname = '${role_name}'" | grep -q 1
      ;;
    sudo)
      sudo -u "$DB_ADMIN_USER" psql -d postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname = '${role_name}'" | grep -q 1
      ;;
    *)
      echo "Database admin mode is not initialized" >&2
      exit 1
      ;;
  esac
}

ensure_app_role() {
  if role_exists "$DB_USER"; then
    echo "Database role already exists: $DB_USER"
    return
  fi

  run_psql_admin "CREATE ROLE \"$DB_USER\" LOGIN CREATEDB;"
  echo "Created database role: $DB_USER"
}

default_connection_string() {
  db_name="$1"

  if [ "${DB_HOST}" = "localhost" ]; then
    echo "postgres:///${db_name}?user=${DB_USER}&host=${DB_SOCKET_DIR}&sslmode=disable"
  else
    echo "postgres://${DB_USER}@${DB_HOST}:${DB_PORT}/${db_name}?sslmode=disable"
  fi
}

read_db_connection() {
  env_file="$1"
  fallback="$2"
  db_connection=$(grep -E '^DB_CONNECTION=' "$env_file" 2>/dev/null | cut -d'=' -f2- || true)

  if [ -n "$db_connection" ]; then
    echo "$db_connection"
  else
    echo "$fallback"
  fi
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

upgrade_stale_db_connection_if_needed() {
  env_file="$1"
  db_name="$2"
  current_default="$3"
  old_tcp_default="postgres://${DB_USER}@localhost:5432/${db_name}?sslmode=disable"
  old_tcp_default_with_host="postgres://${DB_USER}@${DB_HOST}:${DB_PORT}/${db_name}?sslmode=disable"
  existing_connection=$(read_db_connection "$env_file" "")

  if [ -z "$existing_connection" ]; then
    return
  fi

  if [ "$existing_connection" = "$old_tcp_default" ] || [ "$existing_connection" = "$old_tcp_default_with_host" ]; then
    replace_env_value "$env_file" "DB_CONNECTION" "$current_default"
    echo "Updated stale DB_CONNECTION in $env_file"
  fi
}

upgrade_blank_log_level_if_needed() {
  env_file="$1"
  existing_log_level=$(grep -E '^LOG_LEVEL=' "$env_file" 2>/dev/null | cut -d'=' -f2- || true)

  if [ -z "$existing_log_level" ]; then
    replace_env_value "$env_file" "LOG_LEVEL" "Info"
    echo "Set default LOG_LEVEL=Info in $env_file"
  fi
}

upgrade_legacy_log_level_if_needed() {
  env_file="$1"
  existing_log_level=$(grep -E '^LOG_LEVEL=' "$env_file" 2>/dev/null | cut -d'=' -f2- || true)

  if [ "$existing_log_level" = "INFO" ]; then
    replace_env_value "$env_file" "LOG_LEVEL" "Info"
    echo "Updated LOG_LEVEL from INFO to Info in $env_file"
  fi
}

create_env_file_if_missing() {
  env_file="$1"
  db_name="$2"
  server_port="$3"
  environment_name="$4"

  if [ -f "$env_file" ]; then
    echo "Leaving existing $env_file in place"
    return
  fi

  cat >"$env_file" <<EOF
DB_CONNECTION=$(default_connection_string "$db_name")
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
POSTGRES_USER=
POSTGRES_PASSWORD=
POSTGRES_DB=
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

database_exists() {
  db_name="$1"

  case "$DB_ADMIN_MODE" in
    direct)
      psql -d postgres -tAc "SELECT 1 FROM pg_database WHERE datname = '${db_name}'" | grep -q 1
      ;;
    sudo)
      sudo -u "$DB_ADMIN_USER" psql -d postgres -tAc "SELECT 1 FROM pg_database WHERE datname = '${db_name}'" | grep -q 1
      ;;
    *)
      echo "Database admin mode is not initialized" >&2
      exit 1
      ;;
  esac
}

create_database_if_missing() {
  db_name="$1"

  if database_exists "$db_name"; then
    echo "Database already exists: $db_name"
  else
    run_createdb_admin "$db_name" "$DB_USER"
    echo "Created database: $db_name (owner: $DB_USER)"
  fi
}

enable_postgis() {
  db_name="$1"
  run_psql_admin_db "$db_name" 'CREATE EXTENSION IF NOT EXISTS postgis;'
  echo "Ensured PostGIS extension in $db_name"
}

run_migrations() {
  db_connection="$1"
  migrate -source "file://${REPO_ROOT}/migrations" -database "$db_connection" up
}

ensure_machine_deps

require_command createdb
require_command psql
ensure_stack
detect_db_admin_mode

if ! command -v migrate >/dev/null 2>&1; then
  echo "migrate not found on PATH; installing it into \$HOME/.local/bin"
  "${REPO_ROOT}/scripts/install-migrate.sh"
  PATH="$HOME/.local/bin:$PATH"
  export PATH
fi

require_command migrate
ensure_app_role

dev_env_file="${REPO_ROOT}/envfile.local"
test_env_file="${REPO_ROOT}/envfile-test.local"

create_env_file_if_missing "$dev_env_file" "$DEV_DB_NAME" "$DEV_SERVER_PORT" "development"
create_env_file_if_missing "$test_env_file" "$TEST_DB_NAME" "$TEST_SERVER_PORT" "test"

default_dev_connection=$(default_connection_string "$DEV_DB_NAME")
default_test_connection=$(default_connection_string "$TEST_DB_NAME")

upgrade_stale_db_connection_if_needed "$dev_env_file" "$DEV_DB_NAME" "$default_dev_connection"
upgrade_stale_db_connection_if_needed "$test_env_file" "$TEST_DB_NAME" "$default_test_connection"
upgrade_blank_log_level_if_needed "$dev_env_file"
upgrade_blank_log_level_if_needed "$test_env_file"
upgrade_legacy_log_level_if_needed "$dev_env_file"
upgrade_legacy_log_level_if_needed "$test_env_file"

create_database_if_missing "$DEV_DB_NAME"
create_database_if_missing "$TEST_DB_NAME"

enable_postgis "$DEV_DB_NAME"
enable_postgis "$TEST_DB_NAME"

dev_connection=$(read_db_connection "$dev_env_file" "$default_dev_connection")
test_connection=$(read_db_connection "$test_env_file" "$default_test_connection")

run_migrations "$dev_connection"
run_migrations "$test_connection"

echo "Bootstrap complete"
