CREATE TABLE tx2_vehicle_journey_days_of_operation (
    document_id BIGINT NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    start_date DATE NOT NULL,
    end_date DATE NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, start_date, end_date),
    FOREIGN KEY (document_id, vehicle_journey_code)
        REFERENCES tx2_vehicle_journeys(document_id, vehicle_journey_code)
        ON DELETE CASCADE
);

CREATE TABLE tx2_vehicle_journey_days_of_non_operation (
    document_id BIGINT NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    start_date DATE NOT NULL,
    end_date DATE NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, start_date, end_date),
    FOREIGN KEY (document_id, vehicle_journey_code)
        REFERENCES tx2_vehicle_journeys(document_id, vehicle_journey_code)
        ON DELETE CASCADE
);

CREATE INDEX tx2_vehicle_journey_days_of_operation_date_idx
    ON tx2_vehicle_journey_days_of_operation(start_date, end_date);

CREATE INDEX tx2_vehicle_journey_days_of_non_operation_date_idx
    ON tx2_vehicle_journey_days_of_non_operation(start_date, end_date);
