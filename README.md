# ferry-services-server

Backend services for the Ferry Services App.

## Prerequisites

- Haskell Stack via `ghcup` or the Stack installer
- PostgreSQL with PostGIS
- Native PostgreSQL client libraries for building `postgresql-simple`
- zlib development headers for the `digest` package
- bzip2 development headers for `bzlib-conduit`

This repo is currently pinned to `stack` resolver `lts-19.22` (GHC 9.0.2).

## Fresh Machine Setup

### macOS

Install the system dependencies:

```bash
make install-system-deps
```

Equivalent direct script invocation:

```bash
./scripts/install-system-deps.sh
```

If `psql` / `createdb` are not on your `PATH`, add the PostgreSQL binaries:

```bash
echo 'export PATH="/opt/homebrew/opt/postgresql@16/bin:$PATH"' >> ~/.zshrc
```

### Ubuntu / Debian

Install the system dependencies:

```bash
make install-system-deps
```

Equivalent direct script invocation:

```bash
./scripts/install-system-deps.sh
```

This includes the packages `ghcup` expects on Debian/Ubuntu, including `build-essential`, `curl`, `libffi-dev`, `libffi8`, `libgmp-dev`, `libgmp10`, `libncurses-dev`, `pkg-config`, `zlib1g-dev`, and `libbz2-dev`.

### Haskell toolchain

Install `ghcup` first if it is not already present:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Apply the `ghcup` environment changes to the current shell session:

```bash
. "$HOME/.ghcup/env"
```

After that, `stack` should be available in the current shell session:

```bash
stack --version
```

### Install `migrate`

This repo includes a helper to fetch the latest `golang-migrate/migrate` CLI release from GitHub and install it into `~/.local/bin`:

```bash
make install-migrate
```

Equivalent direct script invocation:

```bash
./scripts/install-migrate.sh
```

If `~/.local/bin` is not already on your `PATH`, add it:

```bash
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.zshrc
```

## Repeatable Bootstrap

After the machine-level packages are installed, run:

```bash
make bootstrap-dev
```

Equivalent direct script invocation:

```bash
./scripts/bootstrap-dev.sh
```

That script is safe to rerun on a fresh machine or an already-initialized machine. It will:

- install machine-level packages with `brew` or `apt-get` when required commands are missing
- install `migrate` into `~/.local/bin` if it is not already available
- create `envfile.local` and `envfile-test.local` if they do not exist
- create the `ferry-services` and `ferry-services-test` databases if missing
- enable the `postgis` extension in both databases
- run all migrations against both databases

The script assumes local PostgreSQL access via the current shell user. Override these if needed:

```bash
DB_USER=postgres DB_HOST=127.0.0.1 DB_PORT=5432 make bootstrap-dev
```

By default, bootstrap writes local `DB_CONNECTION` values that use the PostgreSQL Unix socket at `/var/run/postgresql`, which avoids password auth on a standard local install. Set `DB_HOST` to a real hostname such as `127.0.0.1` if you want TCP instead, or override `DB_SOCKET_DIR` if your local socket lives elsewhere.

If your PostgreSQL admin role is not your shell user, bootstrap will try `sudo -u postgres` automatically. You can override the admin role it uses like this:

```bash
DB_ADMIN_USER=postgres make bootstrap-dev
```

## Local Environment

This project expects environment variables from `envfile.local` for dev and `envfile-test.local` for tests (see `makefile` targets).

`make bootstrap-dev` creates both files with local defaults. Fill in the third-party credentials only for the services you want to run.

The generated env files set `LOG_LEVEL=Info` by default because an empty value causes the executables to fail at startup.

### Environment variables by executable

- Web server: `DB_CONNECTION`, `SERVER_PORT`, `SERVER_SENTRY_DSN`, `ENVIRONMENT`
- Scraper: `DB_CONNECTION`, `SCRAPER_SENTRY_DSN`, `ENVIRONMENT`
- Timetable document scraper: `DB_CONNECTION`, `TIMETABLE_DOCUMENT_SCRAPER_SENTRY_DSN`, `ENVIRONMENT`
- Weather fetcher: scraper vars plus `OPENWEATHERMAP_APPID`
- Vessel fetcher: scraper vars
- TransXChange ingester: scraper vars plus `TRAVELLINE_FTP_ADDRESS`, `TRAVELLINE_FTP_USERNAME`, `TRAVELLINE_FTP_PASSWORD`
- Rail departure fetcher: scraper vars plus `RAIL_DATA_API_KEY`
- Push notification endpoints and delivery: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_APPLE_PLATFORM_ARN`, `AWS_GOOGLE_PLATFORM_ARN`

## Build

```bash
stack build
```

`make build` now works on both macOS and Linux. On macOS it adds the `libffi` include path automatically when `xcrun` is available.

## Database Setup (dev)

If you are not using `make bootstrap-dev`, the manual setup is:

```bash
createdb ferry-services
createdb ferry-services-test
```

Install PostGIS package and extension (required by migrations using `geometry`):

```bash
psql -d ferry-services -c "CREATE EXTENSION IF NOT EXISTS postgis;"
psql -d ferry-services-test -c "CREATE EXTENSION IF NOT EXISTS postgis;"
```

## Migrations

Create a new migration:

```bash
migrate create -ext sql -dir migrations -seq name-of-migration
```

Apply migrations:

```bash
migrate -source file://migrations -database "postgres:///ferry-services?user=$USER&host=/var/run/postgresql&sslmode=disable" up
migrate -source file://migrations -database "postgres:///ferry-services-test?user=$USER&host=/var/run/postgresql&sslmode=disable" up
```

Rollback one or more migrations:

```bash
migrate -source file://migrations -database "postgres:///ferry-services?user=$USER&host=/var/run/postgresql&sslmode=disable" down
```

If migrations fail and the DB is left dirty:

```bash
migrate -source file://migrations -database "postgres:///ferry-services?user=$USER&host=/var/run/postgresql&sslmode=disable" force <last_good_version>
```

Run migrations inside the prod DB container:

```bash
docker run -v "$(pwd)"/migrations:/migrations --network container:ferry-services-db-prod migrate/migrate -path=/migrations/ -database "postgres://user:password@db-prod:5432/ferry-services-prod?sslmode=disable" up
```

## Running Services

Start web server:

```bash
make server
```

Other executables:

```bash
make scraper
make weather-fetcher
make vessel-fetcher
make transxchange-ingester
make rail-departure-fetcher
```

## Tests

Fast local check without external credentials:

```bash
make tests-json
```

Full test run:

```bash
make tests
```

`make tests` is not a pure unit-test pass. The integration suite seeds data by talking to live third-party services and expects the following to be configured:

- working local PostgreSQL / PostGIS
- `OPENWEATHERMAP_APPID`
- AWS SNS credentials and platform ARNs
- outbound network access to the ferry and vessel data providers
