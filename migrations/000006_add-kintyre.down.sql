BEGIN;

DELETE FROM service_locations WHERE service_id = 36 AND location_id = 3;
DELETE FROM service_locations WHERE service_id = 36 AND location_id = 53;

COMMIT;
