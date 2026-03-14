# TransXChange API Fixtures

These fixtures are intended for end-to-end API tests that:

1. seed a minimal app service/location mapping
2. ingest a curated directory of real TransXChange files
3. call the dated service endpoint
4. assert the scheduled departures returned by the API

Each scenario directory should contain:

- the minimum set of real TXC files needed for the behavior
- a short README describing:
  - source/provenance
  - service code
  - stop point refs used by the API seed
  - query date
  - expected departures at a human level

Keep fixtures as small and intentional as possible. Avoid using full dumps when a curated subset is sufficient.
