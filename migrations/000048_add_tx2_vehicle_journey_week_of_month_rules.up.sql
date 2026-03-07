CREATE TABLE tx2_vehicle_journey_week_of_month_rules (
    document_id BIGINT NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    week_of_month_rule TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, week_of_month_rule),
    FOREIGN KEY (document_id, vehicle_journey_code)
        REFERENCES tx2_vehicle_journeys(document_id, vehicle_journey_code)
        ON DELETE CASCADE
);

CREATE INDEX tx2_vehicle_journey_week_of_month_rules_lookup_idx
    ON tx2_vehicle_journey_week_of_month_rules(document_id, vehicle_journey_code, week_of_month_rule);
