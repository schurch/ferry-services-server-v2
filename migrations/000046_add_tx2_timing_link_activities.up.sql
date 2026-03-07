ALTER TABLE tx2_journey_pattern_timing_links
    ADD COLUMN from_activity TEXT NOT NULL DEFAULT '',
    ADD COLUMN from_timing_status TEXT NOT NULL DEFAULT '',
    ADD COLUMN to_activity TEXT NOT NULL DEFAULT '',
    ADD COLUMN to_timing_status TEXT NOT NULL DEFAULT '';
