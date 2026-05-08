# SQLite

This directory contains the schema and reference data for fresh SQLite
databases.

Create a database:

```sh
sqlite3 var/ferry-services.sqlite3 ".read sqlite/schema.sql"
sqlite3 var/ferry-services.sqlite3 ".read sqlite/seed.sql"
```

`seed.sql` is intended for fresh database generation. It is not a migration
script for patching existing databases.

Future production schema changes should be added to `migrations/` as numbered
SQL files. The production deploy script applies pending files in lexical order
and records them in `schema_migrations`.

Production migration commands are wrapped by:

```sh
./scripts/sqlite-maintenance.sh migrate
```

Production keeps the SQLite database on the host machine and bind-mounts it
into containers at:

```text
/opt/ferry-services/data/ferry-services.sqlite3
```

The production deploy script creates the host database if it is missing.
