#!/bin/sh

set -eu

IMAGE="${IMAGE:-stefanchurch/ferry-services:latest}"
DB_FILE="${DB_FILE:-./data/ferry-services.sqlite3}"
BACKUP_DIR="${BACKUP_DIR:-./backups}"

usage() {
  cat >&2 <<'EOF'
Usage: scripts/sqlite-maintenance.sh init|migrate|backup|restore BACKUP_FILE

Environment:
  IMAGE       Docker image containing sqlite/schema.sql and sqlite/migrations
  DB_FILE    SQLite database file on the host
  BACKUP_DIR Directory for SQLite backup files
EOF
}

require_docker() {
  if ! command -v docker >/dev/null 2>&1; then
    echo "Missing required command: docker" >&2
    exit 1
  fi
}

prepare_paths() {
  db_dir=$(dirname "$DB_FILE")
  db_name=$(basename "$DB_FILE")

  mkdir -p "$db_dir"
  host_db_dir=$(CDPATH= cd -- "$db_dir" && pwd)
  container_db_file="/opt/ferry-services/data/${db_name}"
}

run_in_image() {
  docker run --rm \
    -u "$(id -u):$(id -g)" \
    -v "${host_db_dir}:/opt/ferry-services/data" \
    "$IMAGE" \
    "$@"
}

init_db() {
  prepare_paths

  if [ -f "$DB_FILE" ]; then
    echo "SQLite database already exists: $DB_FILE"
    return
  fi

  run_in_image /bin/sh -c '
    set -eu
    sqlite3 "$1" ".read /opt/ferry-services/sqlite/schema.sql"
    sqlite3 "$1" ".read /opt/ferry-services/sqlite/seed.sql"
  ' sh "$container_db_file"

  echo "Created SQLite database: $DB_FILE"
}

backup_db() {
  prepare_paths
  mkdir -p "$BACKUP_DIR"
  host_backup_dir=$(CDPATH= cd -- "$BACKUP_DIR" && pwd)
  backup_name="ferry-services-$(date -u +%Y%m%dT%H%M%SZ).sqlite3"

  run_in_image_with_backup /bin/sh -c '
    set -eu
    sqlite3 "$1" ".backup /opt/ferry-services/backups/$2"
    gzip "/opt/ferry-services/backups/$2"
  ' sh "$container_db_file" "$backup_name"

  echo "Created backup: ${host_backup_dir}/${backup_name}.gz"
}

run_in_image_with_backup() {
  docker run --rm \
    -u "$(id -u):$(id -g)" \
    -v "${host_db_dir}:/opt/ferry-services/data" \
    -v "${host_backup_dir}:/opt/ferry-services/backups" \
    "$IMAGE" \
    "$@"
}

migrate_db() {
  prepare_paths

  if [ ! -f "$DB_FILE" ]; then
    echo "SQLite database does not exist yet: $DB_FILE"
    echo "Run: scripts/sqlite-maintenance.sh init" >&2
    exit 1
  fi

  pending_migrations=$(run_in_image /bin/sh -c '
    set -eu

    db="$1"
    sqlite3 "$db" "CREATE TABLE IF NOT EXISTS schema_migrations (version TEXT PRIMARY KEY, applied_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP);"

    for migration in /opt/ferry-services/sqlite/migrations/*.sql; do
      [ -e "$migration" ] || continue

      version=$(basename "$migration" .sql)
      applied=$(sqlite3 "$db" "SELECT 1 FROM schema_migrations WHERE version = '\''$version'\'' LIMIT 1;")

      if [ "$applied" != "1" ]; then
        echo "$version"
      fi
    done
  ' sh "$container_db_file")

  if [ -z "$pending_migrations" ]; then
    echo "No pending SQLite migrations"
    return
  fi

  backup_db

  run_in_image /bin/sh -c '
    set -eu

    db="$1"
    sqlite3 "$db" "CREATE TABLE IF NOT EXISTS schema_migrations (version TEXT PRIMARY KEY, applied_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP);"

    for migration in /opt/ferry-services/sqlite/migrations/*.sql; do
      [ -e "$migration" ] || continue

      version=$(basename "$migration" .sql)
      applied=$(sqlite3 "$db" "SELECT 1 FROM schema_migrations WHERE version = '\''$version'\'' LIMIT 1;")

      if [ "$applied" != "1" ]; then
        command_file=$(mktemp)
        {
          echo ".bail on"
          echo "BEGIN IMMEDIATE;"
          echo ".read $migration"
          echo "INSERT INTO schema_migrations (version) VALUES ('\''$version'\'');"
          echo "COMMIT;"
        } > "$command_file"

        sqlite3 "$db" < "$command_file"
        rm -f "$command_file"
        echo "Applied SQLite migration: $version"
      fi
    done
  ' sh "$container_db_file"
}

restore_db() {
  backup_file="${1:-}"

  if [ -z "$backup_file" ]; then
    usage
    exit 1
  fi

  if [ ! -f "$backup_file" ]; then
    echo "Backup file not found: $backup_file" >&2
    exit 1
  fi

  prepare_paths

  if [ -f "$DB_FILE" ]; then
    echo "Refusing to overwrite existing database: $DB_FILE" >&2
    exit 1
  fi

  host_backup_file=$(CDPATH= cd -- "$(dirname "$backup_file")" && pwd)/$(basename "$backup_file")
  host_backup_dir=$(dirname "$host_backup_file")
  backup_name=$(basename "$host_backup_file")

  run_in_image_with_backup /bin/sh -c '
    set -eu
    case "$2" in
      *.gz) gunzip -c "/opt/ferry-services/backups/$2" > "$1" ;;
      *) cp "/opt/ferry-services/backups/$2" "$1" ;;
    esac
  ' sh "$container_db_file" "$backup_name"

  echo "Restored SQLite database: $DB_FILE"
}

main() {
  command="${1:-}"
  shift || true

  require_docker

  case "$command" in
    init)
      init_db
      ;;
    migrate)
      migrate_db
      ;;
    backup)
      backup_db
      ;;
    restore)
      restore_db "$@"
      ;;
    *)
      usage
      exit 1
      ;;
  esac
}

main "$@"
