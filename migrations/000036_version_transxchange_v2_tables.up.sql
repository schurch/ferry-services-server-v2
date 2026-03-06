DROP INDEX IF EXISTS tx2_vehicle_journeys_pattern_idx;
DROP INDEX IF EXISTS tx2_vehicle_journeys_line_idx;
DROP INDEX IF EXISTS tx2_vehicle_journeys_service_idx;
DROP INDEX IF EXISTS tx2_journey_patterns_service_idx;
DROP INDEX IF EXISTS tx2_lines_service_idx;

DROP TABLE IF EXISTS tx2_vehicle_journey_days;
DROP TABLE IF EXISTS tx2_vehicle_journeys;
DROP TABLE IF EXISTS tx2_journey_patterns;
DROP TABLE IF EXISTS tx2_lines;
DROP TABLE IF EXISTS tx2_services;

CREATE TABLE tx2_documents (
    document_id BIGSERIAL PRIMARY KEY,
    source_path TEXT NOT NULL,
    source_file_name TEXT NOT NULL,
    source_version_key TEXT NOT NULL UNIQUE,
    source_creation_datetime TIMESTAMP NULL,
    source_modification_datetime TIMESTAMP NULL
);

CREATE TABLE tx2_services (
    document_id BIGINT NOT NULL REFERENCES tx2_documents(document_id) ON DELETE CASCADE,
    service_code TEXT NOT NULL,
    operator_ref TEXT NOT NULL,
    mode TEXT NOT NULL,
    description TEXT NOT NULL,
    origin TEXT NOT NULL,
    destination TEXT NOT NULL,
    start_date DATE NULL,
    end_date DATE NULL,
    PRIMARY KEY (document_id, service_code)
);

CREATE TABLE tx2_lines (
    document_id BIGINT NOT NULL REFERENCES tx2_documents(document_id) ON DELETE CASCADE,
    line_id TEXT NOT NULL,
    service_code TEXT NOT NULL,
    line_name TEXT NOT NULL,
    PRIMARY KEY (document_id, line_id),
    FOREIGN KEY (document_id, service_code) REFERENCES tx2_services(document_id, service_code) ON DELETE CASCADE
);

CREATE TABLE tx2_journey_patterns (
    document_id BIGINT NOT NULL REFERENCES tx2_documents(document_id) ON DELETE CASCADE,
    journey_pattern_id TEXT NOT NULL,
    service_code TEXT NOT NULL,
    section_ref TEXT NOT NULL,
    direction TEXT NOT NULL,
    PRIMARY KEY (document_id, journey_pattern_id),
    FOREIGN KEY (document_id, service_code) REFERENCES tx2_services(document_id, service_code) ON DELETE CASCADE
);

CREATE TABLE tx2_vehicle_journeys (
    document_id BIGINT NOT NULL REFERENCES tx2_documents(document_id) ON DELETE CASCADE,
    vehicle_journey_code TEXT NOT NULL,
    service_code TEXT NOT NULL,
    line_id TEXT NOT NULL,
    journey_pattern_id TEXT NOT NULL,
    operator_ref TEXT NOT NULL,
    departure_time TIME NOT NULL,
    note TEXT NOT NULL,
    note_code TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code),
    FOREIGN KEY (document_id, service_code) REFERENCES tx2_services(document_id, service_code) ON DELETE CASCADE,
    FOREIGN KEY (document_id, line_id) REFERENCES tx2_lines(document_id, line_id) ON DELETE CASCADE,
    FOREIGN KEY (document_id, journey_pattern_id) REFERENCES tx2_journey_patterns(document_id, journey_pattern_id) ON DELETE CASCADE
);

CREATE TABLE tx2_vehicle_journey_days (
    document_id BIGINT NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    day_rule TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, day_rule),
    FOREIGN KEY (document_id, vehicle_journey_code) REFERENCES tx2_vehicle_journeys(document_id, vehicle_journey_code) ON DELETE CASCADE
);

CREATE INDEX tx2_documents_file_name_idx ON tx2_documents(source_file_name);
CREATE INDEX tx2_documents_modification_idx ON tx2_documents(source_modification_datetime);
CREATE INDEX tx2_services_service_code_idx ON tx2_services(service_code);
CREATE INDEX tx2_services_date_range_idx ON tx2_services(start_date, end_date);
CREATE INDEX tx2_lines_service_idx ON tx2_lines(document_id, service_code);
CREATE INDEX tx2_journey_patterns_service_idx ON tx2_journey_patterns(document_id, service_code);
CREATE INDEX tx2_vehicle_journeys_service_idx ON tx2_vehicle_journeys(document_id, service_code);
CREATE INDEX tx2_vehicle_journeys_line_idx ON tx2_vehicle_journeys(document_id, line_id);
CREATE INDEX tx2_vehicle_journeys_pattern_idx ON tx2_vehicle_journeys(document_id, journey_pattern_id);
