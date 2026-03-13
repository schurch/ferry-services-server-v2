BEGIN;

DELETE FROM service_locations WHERE service_id = 5000;

DELETE FROM locations WHERE location_id = 98;
DELETE FROM locations WHERE location_id = 99;

DELETE FROM services WHERE service_id = 5000;

DELETE FROM organisations WHERE organisation_id = 6;

COMMIT;
