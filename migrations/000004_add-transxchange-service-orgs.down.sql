DROP TABLE IF EXISTS serviced_organisation_working_days;
DROP TABLE IF EXISTS serviced_organisations;

ALTER TABLE IF EXISTS vehicle_journeys DROP COLUMN IF EXISTS non_operation_serviced_organisation_code;