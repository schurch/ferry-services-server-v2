BEGIN;

DELETE FROM service_locations WHERE service_id = 40 AND location_id = 39;
DELETE FROM service_locations WHERE service_id = 40 AND location_id = 33;

COMMIT;
