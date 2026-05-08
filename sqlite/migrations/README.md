# SQLite Migrations

Put future production database changes in this directory as numbered SQL files,
for example:

```text
001_add_example_column.sql
```

The production deploy script applies files in lexical order and records each
applied file in `schema_migrations`.

Keep `sqlite/schema.sql` as the latest complete schema for fresh databases. A
new migration should contain only the change needed to move an existing
production database forward.
