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

  brew install sqlite libffi pkg-config zlib bzip2

  cat <<'EOF'
macOS dependencies installed.
EOF
}

install_debian() {
  require_command sudo
  require_command apt-get

  sudo apt-get update
  sudo apt-get install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config zlib1g-dev libbz2-dev sqlite3 libsqlite3-dev

  cat <<'EOF'
Debian/Ubuntu dependencies installed.
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
