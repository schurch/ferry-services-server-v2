CREATE TABLE tx2_vehicle_journey_timing_links (
    document_id BIGINT NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    sort_order INT NOT NULL,
    journey_pattern_timing_link_id TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, sort_order),
    FOREIGN KEY (document_id, vehicle_journey_code)
        REFERENCES tx2_vehicle_journeys(document_id, vehicle_journey_code)
        ON DELETE CASCADE,
    FOREIGN KEY (document_id, journey_pattern_timing_link_id)
        REFERENCES tx2_journey_pattern_timing_links(document_id, journey_pattern_timing_link_id)
        ON DELETE CASCADE
);

CREATE INDEX tx2_vehicle_journey_timing_links_lookup_idx
    ON tx2_vehicle_journey_timing_links(document_id, vehicle_journey_code, journey_pattern_timing_link_id);
