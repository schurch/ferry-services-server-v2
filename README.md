# ferry-services-server

Server application for the Ferry Services App.

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