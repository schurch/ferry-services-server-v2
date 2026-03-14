# 07 Multi File Same Service Real

Real CalMac fixture proving that the API selects departures from the correct imported file when multiple real TXC files exist for one service code.

Files:

- `CM21_CALM_930_CALM_CM21_20260312_20260306_183355.xml`
- `CM21_CALM_930_CALM_CM21_20260315_20260306_183535.xml`

Service details:

- service code: `CALM_CM21`
- route: `Barra - Eriskay`
- stop points:
  - `9300AHB` = `Aird Mhor Barra`
  - `9300ERI` = `Eriskay`

Query dates asserted by the API test:

- `2026-03-14`
  expects the Saturday timetable from the earlier file
- `2026-03-15`
  expects the Sunday timetable from the later file, including the `Book by 1400 on day before travel.` note on the `08:45` sailing
