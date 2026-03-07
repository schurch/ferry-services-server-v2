DROP INDEX IF EXISTS tx2_journey_pattern_sections_section_idx;
DROP INDEX IF EXISTS tx2_journey_pattern_sections_pattern_idx;

DROP TABLE IF EXISTS tx2_journey_pattern_sections;

ALTER TABLE tx2_journey_patterns
    ADD COLUMN section_ref TEXT NOT NULL DEFAULT '';
