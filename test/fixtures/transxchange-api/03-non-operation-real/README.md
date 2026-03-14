# 03 Non Operation Real

Real file copied from `tmp/S/LUI_ABCF_930_ABCF_LUI_20260305_20260306_225731.xml`.

- service code: `ABCF_LUI`
- seeded stop points: `9300CUN -> 9300LUI`
- excluded date: `2026-05-22`
- expected API departure counts: `30` from each terminal

This fixture exercises `SpecialDaysOperation / DaysOfNonOperation` on a real ferry service. The API test seeds the two ferry terminals and asserts the exact per-terminal departure counts returned by the API on the excluded date.
