CREATE TABLE tx2_stop_points (
    document_id BIGINT NOT NULL REFERENCES tx2_documents(document_id) ON DELETE CASCADE,
    stop_point_ref TEXT NOT NULL,
    common_name TEXT NOT NULL,
    PRIMARY KEY (document_id, stop_point_ref)
);

CREATE TABLE tx2_journey_pattern_timing_links (
    document_id BIGINT NOT NULL REFERENCES tx2_documents(document_id) ON DELETE CASCADE,
    journey_pattern_timing_link_id TEXT NOT NULL,
    journey_pattern_section_ref TEXT NOT NULL,
    sort_order INT NOT NULL,
    from_stop_point_ref TEXT NOT NULL,
    to_stop_point_ref TEXT NOT NULL,
    route_link_ref TEXT NOT NULL,
    direction TEXT NOT NULL,
    run_time TEXT NOT NULL,
    from_wait_time TEXT NOT NULL,
    PRIMARY KEY (document_id, journey_pattern_timing_link_id)
);

CREATE INDEX tx2_stop_points_stop_point_ref_idx
    ON tx2_stop_points(stop_point_ref);

CREATE INDEX tx2_journey_pattern_timing_links_section_idx
    ON tx2_journey_pattern_timing_links(document_id, journey_pattern_section_ref, sort_order);

CREATE INDEX tx2_journey_pattern_timing_links_from_stop_idx
    ON tx2_journey_pattern_timing_links(document_id, from_stop_point_ref);

CREATE INDEX tx2_journey_pattern_timing_links_to_stop_idx
    ON tx2_journey_pattern_timing_links(document_id, to_stop_point_ref);
