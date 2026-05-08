# ferry-services-server

Backend services for the Ferry Services App.

## Prerequisites

- Haskell Stack via `ghcup` or the Stack installer
- SQLite 3
- zlib development headers
- bzip2 development headers

## Fresh Machine Setup

Install system dependencies:

```bash
make install-system-deps
```

Install `ghcup` first if it is not already present:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
. "$HOME/.ghcup/env"
stack --version
```

## Bootstrap

Create local env files and SQLite databases:

```bash
make bootstrap-dev
```

This creates:

- `envfile.local`
- `envfile-test.local`
- `var/ferry-services.sqlite3`
- `var/ferry-services-test.sqlite3`

The SQLite databases are created from `sqlite/schema.sql` and `sqlite/seed.sql`.
Existing databases are left untouched.

## Environment

This project expects environment variables from `envfile.local` for dev and
`envfile-test.local` for tests.

Key variables:

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
make build
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
make offline-snapshot-generator
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

`make tests` recreates `var/ferry-services-test.sqlite3` from the SQLite schema
and seed data before running the suite.

## Local TransXChange Ingest

After downloading/extracting a local feed to `var/transxchange-live/S`, ingest it
into the dev database with:

```bash
set -a
. ./envfile.local
set +a
DB_CONNECTION=var/ferry-services.sqlite3 SKIP_OFFLINE_SNAPSHOT_AFTER_INGEST=1 stack exec ferry-services-transxchange-ingester-v2-exe -- var/transxchange-live/S
```

Regenerate the offline snapshot with:

```bash
make offline-snapshot-generator
```

The snapshot generator writes a client-facing SQLite database at
`offline/snapshot.sqlite3`. It contains compact tables plus query-friendly views:
`client_services`, `client_service_locations`, and `client_departures`.

Clients can query departures for a service/date with:

```sql
SELECT *
FROM client_departures
WHERE service_id = ?
  AND service_date = ?
ORDER BY departure_time_utc;
```

## Docker Production

Production uses a SQLite database stored on the host machine for easy access:

```text
~/ferry-services-server/data/ferry-services.sqlite3
```

`docker/docker-compose-prod.yml` bind-mounts `./data` into each container at
`/opt/ferry-services/data` and sets:

```text
DB_CONNECTION=/opt/ferry-services/data/ferry-services.sqlite3
```

`./scripts/deploy-prod.sh` pulls the latest images, creates the host database
from `sqlite/schema.sql` and `sqlite/seed.sql` if it is missing, then starts the
containers.

Existing production databases are migrated during deploy by applying any
pending SQL files in `sqlite/migrations` from the server image. Applied files
are tracked in the database's `schema_migrations` table.

Production uses one Docker image, `stefanchurch/ferry-services:latest`. Each
compose service runs a different executable from that image.

SQLite production maintenance is available through:

```bash
./scripts/sqlite-maintenance.sh init
./scripts/sqlite-maintenance.sh migrate
./scripts/sqlite-maintenance.sh backup
./scripts/sqlite-maintenance.sh restore ./backups/example.sqlite3.gz
```

`migrate` creates a backup before applying any pending SQL migrations.

Local backups are plain SQLite backups:

```bash
./scripts/backup.sh
```

## Maintenance Helpers

Check a local development checkout:

```bash
make doctor
```

Reset or inspect the local development database:

```bash
make db-reset
make db-shell
make db-backup
```
