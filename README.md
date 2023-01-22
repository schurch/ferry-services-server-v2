# ferry-services-server

Server application for the Ferry Services App.

### Building

May need to prefix the build command with

```bash
C_INCLUDE_PATH="`xcrun --show-sdk-path`/usr/include/ffi"
```

See [https://gitlab.haskell.org/ghc/ghc/-/issues/20592#note_391266](https://gitlab.haskell.org/ghc/ghc/-/issues/20592#note_391266)

### Database migrations

These are done with the `migrate` tool [https://github.com/golang-migrate/migrate](https://github.com/golang-migrate/migrate)

Add a migration by running the following at the project root:

```bash
migrate create -ext sql -dir migrations -seq name-of-migration
```

To run the up migrations:
```bash
migrate -source file://migrations -database "postgres://stefanchurch@localhost:5432/ferry-services?sslmode=disable" up
```

To run the down migrations:
```bash
migrate -source file://migrations -database "postgres://stefanchurch@localhost:5432/ferry-services?sslmode=disable" down
```

To run the migrations in the running docker instance:
```bash
docker run -v "$(pwd)"/migrations:/migrations --network container:ferry-services-db-prod migrate/migrate -path=/migrations/ -database "postgres://user:password@db-prod:5432/ferry-services-prod?sslmode=disable" up
```