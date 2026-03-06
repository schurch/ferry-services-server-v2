CREATE TABLE tx2_services (
    service_code TEXT PRIMARY KEY,
    operator_ref TEXT NOT NULL,
    mode TEXT NOT NULL,
    description TEXT NOT NULL,
    origin TEXT NOT NULL,
    destination TEXT NOT NULL,
    start_date DATE NULL,
    end_date DATE NULL
);

CREATE TABLE tx2_lines (
    line_id TEXT PRIMARY KEY,
    service_code TEXT NOT NULL REFERENCES tx2_services(service_code) ON DELETE CASCADE,
    line_name TEXT NOT NULL
);

CREATE TABLE tx2_journey_patterns (
    journey_pattern_id TEXT PRIMARY KEY,
    service_code TEXT NOT NULL REFERENCES tx2_services(service_code) ON DELETE CASCADE,
    section_ref TEXT NOT NULL,
    direction TEXT NOT NULL
);

CREATE TABLE tx2_vehicle_journeys (
    vehicle_journey_code TEXT PRIMARY KEY,
    service_code TEXT NOT NULL REFERENCES tx2_services(service_code) ON DELETE CASCADE,
    line_id TEXT NOT NULL REFERENCES tx2_lines(line_id) ON DELETE CASCADE,
    journey_pattern_id TEXT NOT NULL REFERENCES tx2_journey_patterns(journey_pattern_id) ON DELETE CASCADE,
    operator_ref TEXT NOT NULL,
    departure_time TIME NOT NULL,
    note TEXT NOT NULL,
    note_code TEXT NOT NULL
);

CREATE TABLE tx2_vehicle_journey_days (
    vehicle_journey_code TEXT NOT NULL REFERENCES tx2_vehicle_journeys(vehicle_journey_code) ON DELETE CASCADE,
    day_rule TEXT NOT NULL,
    PRIMARY KEY (vehicle_journey_code, day_rule)
);

CREATE INDEX tx2_lines_service_idx ON tx2_lines(service_code);
CREATE INDEX tx2_journey_patterns_service_idx ON tx2_journey_patterns(service_code);
CREATE INDEX tx2_vehicle_journeys_service_idx ON tx2_vehicle_journeys(service_code);
CREATE INDEX tx2_vehicle_journeys_line_idx ON tx2_vehicle_journeys(line_id);
CREATE INDEX tx2_vehicle_journeys_pattern_idx ON tx2_vehicle_journeys(journey_pattern_id);
