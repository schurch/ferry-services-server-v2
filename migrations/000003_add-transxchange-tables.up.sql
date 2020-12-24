CREATE TYPE day_of_week AS ENUM ('monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday');

CREATE TABLE stop_point (
    stop_point_id TEXT PRIMARY KEY,
    common_name TEXT
);

CREATE TABLE route_section (
    route_section_id TEXT PRIMARY KEY
);

CREATE TABLE route_link (
    route_link_id TEXT PRIMARY KEY,
    route_section_id TEXT NOT NULL REFERENCES route_section (route_section_id),
    from_stop_point TEXT NOT NULL REFERENCES stop_point (stop_point_id),
    to_stop_point TEXT NOT NULL REFERENCES stop_point (stop_point_id),
    route_direction TEXT
);

CREATE TABLE route (
    route_id TEXT PRIMARY KEY,
    route_description TEXT,
    route_section_id TEXT NOT NULL REFERENCES route_section (route_section_id)
);

CREATE TABLE journey_pattern_section (
    journey_pattern_section_id TEXT PRIMARY KEY
);

CREATE TABLE journey_pattern_timing_link (
    journey_pattern_timing_link_id TEXT PRIMARY KEY,
    journey_pattern_section_id TEXT NOT NULL REFERENCES journey_pattern_section (journey_pattern_section_id),
    from_stop_point TEXT NOT NULL REFERENCES stop_point (stop_point_id),
    from_timing_status TEXT NOT NULL,
    from_wait_time TEXT NULL,
    to_stop_point TEXT NOT NULL REFERENCES stop_point (stop_point_id),
    to_timing_status TEXT NOT NULL,
    route_link_id TEXT NOT NULL REFERENCES route_link(route_link_id),
    journey_direction TEXT NOT NULL,
    run_time TEXT NOT NULL
);

CREATE TABLE operator (
    operator_id TEXT PRIMARY KEY,
    national_operator_code TEXT NOT NULL,
    operator_code TEXT NOT NULL,
    operator_short_name TEXT NOT NULL
);

CREATE TABLE transxchange_service (
    service_code TEXT PRIMARY KEY,
    operator_id TEXT NOT NULL REFERENCES operator (operator_id),
    mode TEXT NOT NULL,
    description TEXT NOT NULL,
    start_date DATE NOT NULL,
    end_date DATE NOT NULL,
    origin TEXT NOT NULL,
    destination TEXT NOT NULL
);

CREATE TABLE line (
    line_id TEXT PRIMARY KEY,
    service_code TEXT NOT NULL REFERENCES transxchange_service (service_code),
    line_name TEXT NOT NULL
);

CREATE TABLE journey_pattern (
    journey_pattern_id TEXT PRIMARY KEY,
    service_code TEXT NOT NULL REFERENCES transxchange_service (service_code),
    journey_pattern_section_id TEXT NOT NULL REFERENCES journey_pattern_section (journey_pattern_section_id),
    direction TEXT NOT NULL
);

CREATE TABLE vehicle_journey (
    vehicle_journey_code TEXT PRIMARY KEY,
    service_code TEXT NOT NULL REFERENCES transxchange_service (service_code),
    line_id TEXT NOT NULL REFERENCES line (line_id),
    journey_pattern_id TEXT NOT NULL REFERENCES journey_pattern (journey_pattern_id),
    operator_id TEXT NOT NULL REFERENCES operator (operator_id),
    days_of_week day_of_week[] NOT NULL,
    departure_time TIME NOT NULL,
    note TEXT NULL,
    note_code TEXT NULL
);

CREATE TABLE day_of_operation (
  vehicle_journey_code TEXT NOT NULL REFERENCES vehicle_journey (vehicle_journey_code),
  start_date DATE NOT NULL,
  end_date DATE NOT NULL
);

CREATE TABLE day_of_non_operation (
  vehicle_journey_code TEXT NOT NULL REFERENCES vehicle_journey (vehicle_journey_code),
  start_date DATE NOT NULL,
  end_date DATE NOT NULL
);