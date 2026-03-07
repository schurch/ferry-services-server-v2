ALTER TABLE tx2_journey_patterns
    DROP COLUMN section_ref;

CREATE TABLE tx2_journey_pattern_sections (
    document_id BIGINT NOT NULL,
    journey_pattern_id TEXT NOT NULL,
    section_ref TEXT NOT NULL,
    section_order INT NOT NULL,
    PRIMARY KEY (document_id, journey_pattern_id, section_order),
    UNIQUE (document_id, journey_pattern_id, section_ref),
    FOREIGN KEY (document_id, journey_pattern_id)
        REFERENCES tx2_journey_patterns(document_id, journey_pattern_id)
        ON DELETE CASCADE
);

CREATE INDEX tx2_journey_pattern_sections_pattern_idx
    ON tx2_journey_pattern_sections(document_id, journey_pattern_id, section_order);

CREATE INDEX tx2_journey_pattern_sections_section_idx
    ON tx2_journey_pattern_sections(document_id, section_ref);
