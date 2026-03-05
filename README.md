# ferry-services-server

Backend services for the Ferry Services App.

## Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/)
- PostgreSQL 16
- [golang-migrate](https://github.com/golang-migrate/migrate)

## Local Environment

This project expects environment variables from `envfile.local` for dev and `envfile-test.local` for tests (see `makefile` targets).

## Build

```bash
stack build
```

Note: `make build` includes:

```bash
C_INCLUDE_PATH="`xcrun --show-sdk-path`/usr/include/ffi" stack build
```

This is mainly relevant on macOS for libffi header resolution (see [GHC issue 20592](https://gitlab.haskell.org/ghc/ghc/-/issues/20592#note_391266)).

## Database Setup (dev)

Create the database:

```bash
createdb ferry-services
```

Install PostGIS package and extension (required by migrations using `geometry`):

```bash
sudo apt-get install -y postgresql-16-postgis-3 postgresql-16-postgis-3-scripts
sudo -u postgres psql -d ferry-services -c "CREATE EXTENSION IF NOT EXISTS postgis;"
```

## Migrations

Create a new migration:

```bash
migrate create -ext sql -dir migrations -seq name-of-migration
```

Apply migrations:

```bash
migrate -source file://migrations -database "postgres://stefanchurch@localhost:5432/ferry-services?sslmode=disable" up
```

Rollback one or more migrations:

```bash
migrate -source file://migrations -database "postgres://stefanchurch@localhost:5432/ferry-services?sslmode=disable" down
```

If migrations fail and the DB is left dirty:

```bash
migrate -source file://migrations -database "postgres://stefanchurch@localhost:5432/ferry-services?sslmode=disable" force <last_good_version>
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

Create a test database:

```bash
createdb ferry-services-test
```

Run tests:

```bash
make tests
```
