PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;
PRAGMA synchronous = NORMAL;
PRAGMA busy_timeout = 5000;

CREATE TABLE IF NOT EXISTS schema_migrations (
    version TEXT PRIMARY KEY,
    applied_at TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS organisations (
    organisation_id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    website TEXT NULL,
    local_phone TEXT NULL,
    international_phone TEXT NULL,
    email TEXT NULL,
    x TEXT NULL,
    facebook TEXT NULL,
    created TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS services (
    service_id INTEGER PRIMARY KEY,
    area TEXT NOT NULL,
    route TEXT NOT NULL,
    status INTEGER NOT NULL,
    additional_info TEXT NULL,
    disruption_reason TEXT NULL,
    organisation_id INTEGER NOT NULL REFERENCES organisations (organisation_id),
    last_updated_date TEXT NULL,
    updated TEXT NOT NULL,
    visible INTEGER NOT NULL DEFAULT 1,
    created TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS installations (
    installation_id TEXT PRIMARY KEY,
    device_token TEXT NOT NULL,
    device_type INTEGER NOT NULL,
    endpoint_arn TEXT NOT NULL,
    push_enabled INTEGER NOT NULL DEFAULT 1,
    updated TEXT NOT NULL,
    created TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS installation_services (
    installation_id TEXT NOT NULL REFERENCES installations (installation_id) ON DELETE CASCADE,
    service_id INTEGER NOT NULL REFERENCES services (service_id) ON DELETE CASCADE,
    PRIMARY KEY (installation_id, service_id)
);

CREATE TABLE IF NOT EXISTS locations (
    location_id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    latitude REAL NOT NULL,
    longitude REAL NOT NULL,
    stop_point_id TEXT NULL UNIQUE,
    created TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS service_locations (
    service_id INTEGER NOT NULL REFERENCES services (service_id) ON DELETE CASCADE,
    location_id INTEGER NOT NULL REFERENCES locations (location_id) ON DELETE CASCADE,
    PRIMARY KEY (service_id, location_id)
);

CREATE TABLE IF NOT EXISTS location_weather (
    location_id INTEGER PRIMARY KEY REFERENCES locations (location_id) ON DELETE CASCADE,
    description TEXT NOT NULL,
    icon TEXT NOT NULL,
    temperature REAL NOT NULL,
    wind_speed REAL NOT NULL,
    wind_direction REAL NOT NULL,
    updated TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    created TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS vessels (
    mmsi INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    speed REAL NULL,
    course REAL NULL,
    latitude REAL NOT NULL,
    longitude REAL NOT NULL,
    last_received TEXT NOT NULL,
    updated TEXT NOT NULL,
    organisation_id INTEGER NOT NULL REFERENCES organisations (organisation_id),
    created TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS rail_departures (
    departure_crs TEXT NOT NULL,
    departure_name TEXT NOT NULL,
    destination_crs TEXT NOT NULL,
    destination_name TEXT NOT NULL,
    scheduled_departure_time TEXT NOT NULL,
    estimated_departure_time TEXT NOT NULL,
    cancelled INTEGER NOT NULL,
    platform TEXT NULL,
    location_id INTEGER NOT NULL REFERENCES locations (location_id) ON DELETE CASCADE,
    created TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (departure_crs, destination_crs, scheduled_departure_time)
);

CREATE TABLE IF NOT EXISTS tx2_documents (
    document_id INTEGER PRIMARY KEY AUTOINCREMENT,
    source_path TEXT NOT NULL,
    source_file_name TEXT NOT NULL,
    source_version_key TEXT NOT NULL UNIQUE,
    source_creation_datetime TEXT NULL,
    source_modification_datetime TEXT NULL,
    created TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS tx2_services (
    document_id INTEGER NOT NULL REFERENCES tx2_documents (document_id) ON DELETE CASCADE,
    service_code TEXT NOT NULL,
    operator_ref TEXT NOT NULL,
    mode TEXT NOT NULL,
    description TEXT NOT NULL,
    origin TEXT NULL,
    destination TEXT NULL,
    start_date TEXT NULL,
    end_date TEXT NULL,
    PRIMARY KEY (document_id, service_code)
);

CREATE TABLE IF NOT EXISTS tx2_lines (
    document_id INTEGER NOT NULL REFERENCES tx2_documents (document_id) ON DELETE CASCADE,
    line_id TEXT NOT NULL,
    service_code TEXT NOT NULL,
    line_name TEXT NOT NULL,
    PRIMARY KEY (document_id, line_id),
    FOREIGN KEY (document_id, service_code) REFERENCES tx2_services (document_id, service_code) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_journey_patterns (
    document_id INTEGER NOT NULL REFERENCES tx2_documents (document_id) ON DELETE CASCADE,
    journey_pattern_id TEXT NOT NULL,
    service_code TEXT NOT NULL,
    direction TEXT NULL,
    PRIMARY KEY (document_id, journey_pattern_id),
    FOREIGN KEY (document_id, service_code) REFERENCES tx2_services (document_id, service_code) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_stop_points (
    document_id INTEGER NOT NULL REFERENCES tx2_documents (document_id) ON DELETE CASCADE,
    stop_point_ref TEXT NOT NULL,
    common_name TEXT NOT NULL,
    PRIMARY KEY (document_id, stop_point_ref)
);

CREATE TABLE IF NOT EXISTS tx2_journey_pattern_sections (
    document_id INTEGER NOT NULL,
    journey_pattern_id TEXT NOT NULL,
    section_ref TEXT NOT NULL,
    section_order INTEGER NOT NULL,
    PRIMARY KEY (document_id, journey_pattern_id, section_order),
    UNIQUE (document_id, journey_pattern_id, section_ref),
    FOREIGN KEY (document_id, journey_pattern_id) REFERENCES tx2_journey_patterns (document_id, journey_pattern_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_journey_pattern_timing_links (
    document_id INTEGER NOT NULL REFERENCES tx2_documents (document_id) ON DELETE CASCADE,
    journey_pattern_timing_link_id TEXT NOT NULL,
    journey_pattern_section_ref TEXT NOT NULL,
    sort_order INTEGER NOT NULL,
    from_stop_point_ref TEXT NOT NULL,
    from_activity TEXT NOT NULL,
    from_timing_status TEXT NOT NULL,
    to_stop_point_ref TEXT NOT NULL,
    to_activity TEXT NOT NULL,
    to_timing_status TEXT NOT NULL,
    route_link_ref TEXT NULL,
    direction TEXT NULL,
    run_time TEXT NOT NULL,
    from_wait_time TEXT NOT NULL,
    run_seconds INTEGER NOT NULL DEFAULT 0,
    from_wait_seconds INTEGER NOT NULL DEFAULT 0,
    PRIMARY KEY (document_id, journey_pattern_timing_link_id)
);

CREATE TABLE IF NOT EXISTS tx2_vehicle_journeys (
    document_id INTEGER NOT NULL REFERENCES tx2_documents (document_id) ON DELETE CASCADE,
    vehicle_journey_code TEXT NOT NULL,
    service_code TEXT NOT NULL,
    line_id TEXT NOT NULL,
    journey_pattern_id TEXT NOT NULL,
    operator_ref TEXT NOT NULL,
    departure_time TEXT NOT NULL,
    note TEXT NOT NULL,
    note_code TEXT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code),
    FOREIGN KEY (document_id, service_code) REFERENCES tx2_services (document_id, service_code) ON DELETE CASCADE,
    FOREIGN KEY (document_id, line_id) REFERENCES tx2_lines (document_id, line_id) ON DELETE CASCADE,
    FOREIGN KEY (document_id, journey_pattern_id) REFERENCES tx2_journey_patterns (document_id, journey_pattern_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_vehicle_journey_timing_links (
    document_id INTEGER NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    sort_order INTEGER NOT NULL,
    journey_pattern_timing_link_id TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, sort_order),
    FOREIGN KEY (document_id, vehicle_journey_code) REFERENCES tx2_vehicle_journeys (document_id, vehicle_journey_code) ON DELETE CASCADE,
    FOREIGN KEY (document_id, journey_pattern_timing_link_id) REFERENCES tx2_journey_pattern_timing_links (document_id, journey_pattern_timing_link_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_vehicle_journey_days (
    document_id INTEGER NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    day_rule TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, day_rule),
    FOREIGN KEY (document_id, vehicle_journey_code) REFERENCES tx2_vehicle_journeys (document_id, vehicle_journey_code) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_vehicle_journey_days_of_operation (
    document_id INTEGER NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    start_date TEXT NOT NULL,
    end_date TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, start_date, end_date),
    FOREIGN KEY (document_id, vehicle_journey_code) REFERENCES tx2_vehicle_journeys (document_id, vehicle_journey_code) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_vehicle_journey_days_of_non_operation (
    document_id INTEGER NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    start_date TEXT NOT NULL,
    end_date TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, start_date, end_date),
    FOREIGN KEY (document_id, vehicle_journey_code) REFERENCES tx2_vehicle_journeys (document_id, vehicle_journey_code) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_vehicle_journey_serviced_organisation_days_of_operation (
    document_id INTEGER NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    start_date TEXT NOT NULL,
    end_date TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, start_date, end_date),
    FOREIGN KEY (document_id, vehicle_journey_code) REFERENCES tx2_vehicle_journeys (document_id, vehicle_journey_code) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_vehicle_journey_serviced_organisation_days_of_non_operation (
    document_id INTEGER NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    start_date TEXT NOT NULL,
    end_date TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, start_date, end_date),
    FOREIGN KEY (document_id, vehicle_journey_code) REFERENCES tx2_vehicle_journeys (document_id, vehicle_journey_code) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_vehicle_journey_bank_holiday_operation_rules (
    document_id INTEGER NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    bank_holiday_rule TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, bank_holiday_rule),
    FOREIGN KEY (document_id, vehicle_journey_code) REFERENCES tx2_vehicle_journeys (document_id, vehicle_journey_code) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_vehicle_journey_bank_holiday_non_operation_rules (
    document_id INTEGER NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    bank_holiday_rule TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, bank_holiday_rule),
    FOREIGN KEY (document_id, vehicle_journey_code) REFERENCES tx2_vehicle_journeys (document_id, vehicle_journey_code) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_vehicle_journey_week_of_month_rules (
    document_id INTEGER NOT NULL,
    vehicle_journey_code TEXT NOT NULL,
    week_of_month_rule TEXT NOT NULL,
    PRIMARY KEY (document_id, vehicle_journey_code, week_of_month_rule),
    FOREIGN KEY (document_id, vehicle_journey_code) REFERENCES tx2_vehicle_journeys (document_id, vehicle_journey_code) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS tx2_service_mappings (
    service_id INTEGER NOT NULL REFERENCES services (service_id) ON DELETE CASCADE,
    service_code TEXT NOT NULL,
    PRIMARY KEY (service_id, service_code)
);

CREATE TABLE IF NOT EXISTS timetable_documents (
    timetable_document_id INTEGER PRIMARY KEY AUTOINCREMENT,
    organisation_id INTEGER NOT NULL REFERENCES organisations (organisation_id),
    title TEXT NOT NULL,
    source_url TEXT NOT NULL UNIQUE,
    content_hash TEXT NULL,
    content_type TEXT NULL,
    content_length INTEGER NULL,
    last_seen_at TEXT NOT NULL,
    updated TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
    created TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS timetable_document_services (
    timetable_document_id INTEGER NOT NULL REFERENCES timetable_documents (timetable_document_id) ON DELETE CASCADE,
    service_id INTEGER NOT NULL REFERENCES services (service_id),
    PRIMARY KEY (timetable_document_id, service_id)
);

CREATE INDEX IF NOT EXISTS services_organisation_id_idx ON services (organisation_id);
CREATE INDEX IF NOT EXISTS installation_services_service_id_idx ON installation_services (service_id);
CREATE INDEX IF NOT EXISTS service_locations_location_id_idx ON service_locations (location_id);
CREATE INDEX IF NOT EXISTS vessels_organisation_id_idx ON vessels (organisation_id);
CREATE INDEX IF NOT EXISTS tx2_documents_file_name_idx ON tx2_documents (source_file_name);
CREATE INDEX IF NOT EXISTS tx2_documents_modification_idx ON tx2_documents (source_modification_datetime);
CREATE INDEX IF NOT EXISTS tx2_services_service_code_idx ON tx2_services (service_code);
CREATE INDEX IF NOT EXISTS tx2_services_date_range_idx ON tx2_services (start_date, end_date);
CREATE INDEX IF NOT EXISTS tx2_lines_service_idx ON tx2_lines (document_id, service_code);
CREATE INDEX IF NOT EXISTS tx2_journey_patterns_service_idx ON tx2_journey_patterns (document_id, service_code);
CREATE INDEX IF NOT EXISTS tx2_stop_points_stop_point_ref_idx ON tx2_stop_points (stop_point_ref);
CREATE INDEX IF NOT EXISTS tx2_journey_pattern_sections_pattern_idx ON tx2_journey_pattern_sections (document_id, journey_pattern_id);
CREATE INDEX IF NOT EXISTS tx2_journey_pattern_sections_section_idx ON tx2_journey_pattern_sections (document_id, section_ref);
CREATE INDEX IF NOT EXISTS tx2_journey_pattern_timing_links_section_idx ON tx2_journey_pattern_timing_links (document_id, journey_pattern_section_ref);
CREATE INDEX IF NOT EXISTS tx2_journey_pattern_timing_links_from_stop_idx ON tx2_journey_pattern_timing_links (from_stop_point_ref);
CREATE INDEX IF NOT EXISTS tx2_journey_pattern_timing_links_to_stop_idx ON tx2_journey_pattern_timing_links (to_stop_point_ref);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journeys_service_idx ON tx2_vehicle_journeys (document_id, service_code);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journeys_line_idx ON tx2_vehicle_journeys (document_id, line_id);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journeys_pattern_idx ON tx2_vehicle_journeys (document_id, journey_pattern_id);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journey_timing_links_lookup_idx ON tx2_vehicle_journey_timing_links (document_id, vehicle_journey_code);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journey_days_of_operation_date_idx ON tx2_vehicle_journey_days_of_operation (start_date, end_date);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journey_days_of_non_operation_date_idx ON tx2_vehicle_journey_days_of_non_operation (start_date, end_date);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journey_serviced_org_days_op_date_idx ON tx2_vehicle_journey_serviced_organisation_days_of_operation (start_date, end_date);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journey_serviced_org_days_non_op_date_idx ON tx2_vehicle_journey_serviced_organisation_days_of_non_operation (start_date, end_date);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journey_bank_holiday_operation_rules_rule_idx ON tx2_vehicle_journey_bank_holiday_operation_rules (bank_holiday_rule);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journey_bank_holiday_non_operation_rules_rule_idx ON tx2_vehicle_journey_bank_holiday_non_operation_rules (bank_holiday_rule);
CREATE INDEX IF NOT EXISTS tx2_vehicle_journey_week_of_month_rules_lookup_idx ON tx2_vehicle_journey_week_of_month_rules (week_of_month_rule);
CREATE INDEX IF NOT EXISTS tx2_service_mappings_service_code_idx ON tx2_service_mappings (service_code);
CREATE INDEX IF NOT EXISTS timetable_documents_organisation_id_idx ON timetable_documents (organisation_id);
CREATE INDEX IF NOT EXISTS timetable_document_services_service_id_idx ON timetable_document_services (service_id);
