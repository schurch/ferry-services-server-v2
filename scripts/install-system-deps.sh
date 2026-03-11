#!/bin/sh

set -eu

require_command() {
  command_name="$1"

  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "Missing required command: $command_name" >&2
    exit 1
  fi
}

install_macos() {
  require_command brew

  brew install libpq postgresql@16 postgis libffi pkg-config zlib bzip2

  if command -v brew >/dev/null 2>&1; then
    brew services start postgresql@16
  fi

  cat <<'EOF'
macOS dependencies installed.
If PostgreSQL tools are not on your PATH, add:
  export PATH="/opt/homebrew/opt/postgresql@16/bin:$PATH"
EOF
}

install_debian() {
  require_command sudo
  require_command apt-get

  sudo apt-get update
  sudo apt-get install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config zlib1g-dev libbz2-dev postgresql postgresql-contrib postgis libpq-dev

  cat <<'EOF'
Debian/Ubuntu dependencies installed.
If PostgreSQL is not running yet, start it with your service manager.
EOF
}

main() {
  os_name="$(uname -s)"

  case "$os_name" in
    Darwin)
      install_macos
      ;;
    Linux)
      if [ -f /etc/debian_version ]; then
        install_debian
      else
        echo "Unsupported Linux distribution. Add support in scripts/install-system-deps.sh." >&2
        exit 1
      fi
      ;;
    *)
      echo "Unsupported operating system: $os_name" >&2
      exit 1
      ;;
  esac
}

main "$@"
