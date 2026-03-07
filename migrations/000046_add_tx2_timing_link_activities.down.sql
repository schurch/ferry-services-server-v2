ALTER TABLE tx2_journey_pattern_timing_links
    DROP COLUMN IF EXISTS to_timing_status,
    DROP COLUMN IF EXISTS to_activity,
    DROP COLUMN IF EXISTS from_timing_status,
    DROP COLUMN IF EXISTS from_activity;
