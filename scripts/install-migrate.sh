#!/bin/sh

set -eu

INSTALL_DIR="${INSTALL_DIR:-$HOME/.local/bin}"

require_command() {
  command_name="$1"

  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "Missing required command: $command_name" >&2
    exit 1
  fi
}

detect_os() {
  case "$(uname -s)" in
    Darwin) echo "darwin" ;;
    Linux) echo "linux" ;;
    *)
      echo "Unsupported operating system: $(uname -s)" >&2
      exit 1
      ;;
  esac
}

detect_arch() {
  case "$(uname -m)" in
    x86_64|amd64) echo "amd64" ;;
    arm64|aarch64) echo "arm64" ;;
    *)
      echo "Unsupported architecture: $(uname -m)" >&2
      exit 1
      ;;
  esac
}

fetch_latest_tag() {
  curl -fsSL "https://api.github.com/repos/golang-migrate/migrate/releases/latest" \
    | grep '"tag_name":' \
    | head -n 1 \
    | sed -E 's/.*"([^"]+)".*/\1/'
}

require_command curl
require_command tar
require_command install

os=$(detect_os)
arch=$(detect_arch)
tag=$(fetch_latest_tag)

if [ -z "$tag" ]; then
  echo "Could not determine latest golang-migrate release tag" >&2
  exit 1
fi

archive_name="migrate.${os}-${arch}.tar.gz"
download_url="https://github.com/golang-migrate/migrate/releases/download/${tag}/${archive_name}"
tmp_dir=$(mktemp -d)

trap 'rm -rf "$tmp_dir"' EXIT INT TERM HUP

mkdir -p "$INSTALL_DIR"
curl -fsSL "$download_url" -o "${tmp_dir}/${archive_name}"
tar -xzf "${tmp_dir}/${archive_name}" -C "$tmp_dir"
install -m 0755 "${tmp_dir}/migrate" "${INSTALL_DIR}/migrate"

echo "Installed migrate ${tag} to ${INSTALL_DIR}/migrate"
