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

UPDATE services SET transxchange_service_code = 'FSACM01' WHERE service_id = '1';
UPDATE services SET transxchange_service_code = 'FSACM01A' WHERE service_id = '39';
UPDATE services SET transxchange_service_code = 'FSACM02' WHERE service_id = '2';
UPDATE services SET transxchange_service_code = 'FSACM03' WHERE service_id = '3';
UPDATE services SET transxchange_service_code = 'FSACM05' WHERE service_id = '5';
UPDATE services SET transxchange_service_code = 'FSACM06' WHERE service_id = '6';
UPDATE services SET transxchange_service_code = 'FSACM08' WHERE service_id = '8';
UPDATE services SET transxchange_service_code = 'FSACM09' WHERE service_id = '9';
UPDATE services SET transxchange_service_code = 'FSACM10' WHERE service_id = '10';
UPDATE services SET transxchange_service_code = 'FSACM11' WHERE service_id = '11';
UPDATE services SET transxchange_service_code = 'FSACM12' WHERE service_id = '12';
UPDATE services SET transxchange_service_code = 'FSACM13' WHERE service_id = '13';
UPDATE services SET transxchange_service_code = 'FSACM14' WHERE service_id = '14';
UPDATE services SET transxchange_service_code = 'FSACM15' WHERE service_id = '15';
UPDATE services SET transxchange_service_code = 'FSACM16' WHERE service_id = '16';
UPDATE services SET transxchange_service_code = 'FSACM17' WHERE service_id = '17';
UPDATE services SET transxchange_service_code = 'FSACM18' WHERE service_id = '18';
UPDATE services SET transxchange_service_code = 'FSACM19' WHERE service_id = '19';
UPDATE services SET transxchange_service_code = 'FSACM20' WHERE service_id = '20';
UPDATE services SET transxchange_service_code = 'FSACM21' WHERE service_id = '21';
UPDATE services SET transxchange_service_code = 'FSACM22' WHERE service_id = '22';
UPDATE services SET transxchange_service_code = 'FSACM23' WHERE service_id = '23';
UPDATE services SET transxchange_service_code = 'FSACM25' WHERE service_id = '25';
UPDATE services SET transxchange_service_code = 'FSAO015A' WHERE service_id = '38';
UPDATE services SET transxchange_service_code = 'FSAO020A' WHERE service_id = '37';
UPDATE services SET transxchange_service_code = 'FSBCM04' WHERE service_id = '4';
UPDATE services SET transxchange_service_code = 'FSBCM07' WHERE service_id = '7';

UPDATE locations SET stop_point_id = '9300ULL' WHERE location_id = '10';
UPDATE locations SET stop_point_id = '9300STO' WHERE location_id = '9';
UPDATE locations SET stop_point_id = '9300PAV' WHERE location_id = '29';
UPDATE locations SET stop_point_id = '9300DNU' WHERE location_id = '57';
UPDATE locations SET stop_point_id = '9300GAL' WHERE location_id = '46';
UPDATE locations SET stop_point_id = '9300PTM' WHERE location_id = '47';
UPDATE locations SET stop_point_id = '9300KLH' WHERE location_id = '48';
UPDATE locations SET stop_point_id = '9300CAN' WHERE location_id = '49';
UPDATE locations SET stop_point_id = '9300CBA' WHERE location_id = '20';
UPDATE locations SET stop_point_id = '9300RAA' WHERE location_id = '43';
UPDATE locations SET stop_point_id = '9300SCO' WHERE location_id = '42';
UPDATE locations SET stop_point_id = '9300CNU' WHERE location_id = '38';
UPDATE locations SET stop_point_id = '9300KEN' WHERE location_id = '28';
UPDATE locations SET stop_point_id = '9300PLN' WHERE location_id = '36';
UPDATE locations SET stop_point_id = '9300PAK' WHERE location_id = '27';
UPDATE locations SET stop_point_id = '9300CSY' WHERE location_id = '26';
UPDATE locations SET stop_point_id = '9300AMH' WHERE location_id = '31';
UPDATE locations SET stop_point_id = '9300TYL' WHERE location_id = '30';
UPDATE locations SET stop_point_id = '9300CUM' WHERE location_id = '12';
UPDATE locations SET stop_point_id = '9300LGS' WHERE location_id = '11';
UPDATE locations SET stop_point_id = '9300IOA' WHERE location_id = '35';
UPDATE locations SET stop_point_id = '9300FIO' WHERE location_id = '34';
UPDATE locations SET stop_point_id = '9300RAY' WHERE location_id = '8';
UPDATE locations SET stop_point_id = '9300WMB' WHERE location_id = '7';
UPDATE locations SET stop_point_id = '9300RHU' WHERE location_id = '23';
UPDATE locations SET stop_point_id = '9300COT' WHERE location_id = '22';
UPDATE locations SET stop_point_id = '9300ERI' WHERE location_id = '18';
UPDATE locations SET stop_point_id = '9300AHB' WHERE location_id = '17';
UPDATE locations SET stop_point_id = '9300LMR' WHERE location_id = '37';
UPDATE locations SET stop_point_id = '9300BRB' WHERE location_id = '4';
UPDATE locations SET stop_point_id = '9300ARD' WHERE location_id = '3';
UPDATE locations SET stop_point_id = '9300LBS' WHERE location_id = '21';
UPDATE locations SET stop_point_id = '9300ARM' WHERE location_id = '45';
UPDATE locations SET stop_point_id = '9300MLG' WHERE location_id = '44';
UPDATE locations SET stop_point_id = '9300KGN' WHERE location_id = '58';
UPDATE locations SET stop_point_id = '9300GUR' WHERE location_id = '56';
UPDATE locations SET stop_point_id = '9300KER' WHERE location_id = '55';
UPDATE locations SET stop_point_id = '9300BNY' WHERE location_id = '40';
UPDATE locations SET stop_point_id = '9300LEV' WHERE location_id = '41';
UPDATE locations SET stop_point_id = '9300UIG' WHERE location_id = '32';
UPDATE locations SET stop_point_id = '9300LMA' WHERE location_id = '39';
UPDATE locations SET stop_point_id = '9300TAB' WHERE location_id = '33';
UPDATE locations SET stop_point_id = '9300OBA' WHERE location_id = '19';
UPDATE locations SET stop_point_id = '9300COL' WHERE location_id = '24';
UPDATE locations SET stop_point_id = '9300TRE' WHERE location_id = '25';
UPDATE locations SET stop_point_id = '9300KLB' WHERE location_id = '14';
UPDATE locations SET stop_point_id = '9300TOB' WHERE location_id = '13';
UPDATE locations SET stop_point_id = '9300FIB' WHERE location_id = '51';
UPDATE locations SET stop_point_id = '9300LHL' WHERE location_id = '50';
UPDATE locations SET stop_point_id = '9300LCR' WHERE location_id = '16';
UPDATE locations SET stop_point_id = '9300LCT' WHERE location_id = '15';
