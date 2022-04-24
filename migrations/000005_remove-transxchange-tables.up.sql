BEGIN;

DROP TABLE IF EXISTS days_of_non_operation;
DROP TABLE IF EXISTS days_of_operation;
DROP TABLE IF EXISTS vehicle_journeys;
DROP TABLE IF EXISTS journey_patterns;
DROP TABLE IF EXISTS lines;
DROP TABLE IF EXISTS transxchange_services;
DROP TABLE IF EXISTS operators;
DROP TABLE IF EXISTS journey_pattern_timing_links;
DROP TABLE IF EXISTS journey_pattern_sections;
DROP TABLE IF EXISTS routes;
DROP TABLE IF EXISTS route_links;
DROP TABLE IF EXISTS route_sections;
DROP TABLE IF EXISTS stop_points;
DROP TABLE IF EXISTS serviced_organisation_working_days;
DROP TABLE IF EXISTS serviced_organisations;

DROP TYPE IF EXISTS day_of_week;

ALTER TABLE services DROP COLUMN transxchange_service_code;
ALTER TABLE locations DROP COLUMN stop_point_id;

COMMIT;
