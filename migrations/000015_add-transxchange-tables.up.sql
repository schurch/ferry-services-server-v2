BEGIN;

CREATE TYPE day_of_week AS ENUM ('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday');

CREATE TABLE stop_points (
    stop_point_id TEXT PRIMARY KEY,
    common_name TEXT
);

CREATE TABLE route_sections (
    route_section_id TEXT PRIMARY KEY
);

CREATE TABLE route_links (
    route_link_id TEXT PRIMARY KEY,
    route_section_id TEXT NOT NULL REFERENCES route_sections (route_section_id),
    from_stop_point TEXT NOT NULL REFERENCES stop_points (stop_point_id),
    to_stop_point TEXT NOT NULL REFERENCES stop_points (stop_point_id),
    route_direction TEXT
);

CREATE TABLE routes (
    route_id TEXT PRIMARY KEY,
    route_description TEXT,
    route_section_id TEXT NOT NULL REFERENCES route_sections (route_section_id)
);

CREATE TABLE journey_pattern_sections (
    journey_pattern_section_id TEXT PRIMARY KEY
);

CREATE TABLE journey_pattern_timing_links (
    journey_pattern_timing_link_id TEXT PRIMARY KEY,
    journey_pattern_section_id TEXT NOT NULL REFERENCES journey_pattern_sections (journey_pattern_section_id),
    from_stop_point TEXT NOT NULL REFERENCES stop_points (stop_point_id),
    from_timing_status TEXT NOT NULL,
    from_wait_time TEXT NULL,
    to_stop_point TEXT NOT NULL REFERENCES stop_points (stop_point_id),
    to_timing_status TEXT NOT NULL,
    route_link_id TEXT NOT NULL REFERENCES route_links (route_link_id),
    journey_direction TEXT NOT NULL,
    run_time TEXT NOT NULL
);

CREATE TABLE operators (
    operator_id TEXT PRIMARY KEY,
    national_operator_code TEXT NOT NULL,
    operator_code TEXT NOT NULL,
    operator_short_name TEXT NOT NULL
);

CREATE TABLE transxchange_services (
    service_code TEXT PRIMARY KEY,
    operator_id TEXT NOT NULL REFERENCES operators (operator_id),
    mode TEXT NOT NULL,
    description TEXT NOT NULL,
    start_date DATE NOT NULL,
    end_date DATE NOT NULL,
    origin TEXT NOT NULL,
    destination TEXT NOT NULL
);

CREATE TABLE lines (
    line_id TEXT PRIMARY KEY,
    service_code TEXT NOT NULL REFERENCES transxchange_services (service_code),
    line_name TEXT NOT NULL
);

CREATE TABLE journey_patterns (
    journey_pattern_id TEXT PRIMARY KEY,
    service_code TEXT NOT NULL REFERENCES transxchange_services (service_code),
    journey_pattern_section_id TEXT NOT NULL REFERENCES journey_pattern_sections (journey_pattern_section_id),
    direction TEXT NOT NULL
);

CREATE TABLE vehicle_journeys (
    vehicle_journey_code TEXT PRIMARY KEY,
    service_code TEXT NOT NULL REFERENCES transxchange_services (service_code),
    line_id TEXT NOT NULL REFERENCES lines (line_id),
    journey_pattern_id TEXT NOT NULL REFERENCES journey_patterns (journey_pattern_id),
    operator_id TEXT NOT NULL REFERENCES operators (operator_id),
    days_of_week day_of_week[] NOT NULL,
    departure_time TIME NOT NULL,
    note TEXT NULL,
    note_code TEXT NULL
);

CREATE TABLE days_of_operation (
  vehicle_journey_code TEXT NOT NULL REFERENCES vehicle_journeys (vehicle_journey_code),
  start_date DATE NOT NULL,
  end_date DATE NOT NULL,
  PRIMARY KEY(vehicle_journey_code, start_date, end_date)
);

CREATE TABLE days_of_non_operation (
  vehicle_journey_code TEXT NOT NULL REFERENCES vehicle_journeys (vehicle_journey_code),
  start_date DATE NOT NULL,
  end_date DATE NOT NULL,
  PRIMARY KEY(vehicle_journey_code, start_date, end_date)
);

ALTER TABLE services ADD COLUMN transxchange_service_code TEXT;
ALTER TABLE services ADD UNIQUE (transxchange_service_code);
ALTER TABLE locations ADD COLUMN stop_point_id TEXT;
ALTER TABLE locations ADD UNIQUE (stop_point_id);

UPDATE services SET transxchange_service_code = 'FSACM05' WHERE service_id = '5';

UPDATE locations SET stop_point_id = '9300ARD' WHERE location_id = '3';
UPDATE locations SET stop_point_id = '9300BRB' WHERE location_id = '4';

CREATE TABLE serviced_organisations (
    serviced_organisation_code TEXT PRIMARY KEY,
    name TEXT
);

CREATE TABLE serviced_organisation_working_days (
  serviced_organisation_code TEXT NOT NULL REFERENCES serviced_organisations (serviced_organisation_code),
  start_date DATE NOT NULL,
  end_date DATE NOT NULL,
  PRIMARY KEY(serviced_organisation_code, start_date, end_date)
);

ALTER TABLE vehicle_journeys ADD COLUMN non_operation_serviced_organisation_code TEXT NULL;

COMMIT;
