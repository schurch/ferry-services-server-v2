CREATE TABLE tx2_vehicle_journey_bank_holiday_operation_rules (
    document_id BIGINT NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    bank_holiday_rule TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, bank_holiday_rule),
    FOREIGN KEY (document_id, vehicle_journey_code)
        REFERENCES tx2_vehicle_journeys(document_id, vehicle_journey_code)
        ON DELETE CASCADE
);

CREATE TABLE tx2_vehicle_journey_bank_holiday_non_operation_rules (
    document_id BIGINT NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    bank_holiday_rule TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, bank_holiday_rule),
    FOREIGN KEY (document_id, vehicle_journey_code)
        REFERENCES tx2_vehicle_journeys(document_id, vehicle_journey_code)
        ON DELETE CASCADE
);

CREATE INDEX tx2_vehicle_journey_bank_holiday_operation_rules_rule_idx
    ON tx2_vehicle_journey_bank_holiday_operation_rules(bank_holiday_rule);

CREATE INDEX tx2_vehicle_journey_bank_holiday_non_operation_rules_rule_idx
    ON tx2_vehicle_journey_bank_holiday_non_operation_rules(bank_holiday_rule);
