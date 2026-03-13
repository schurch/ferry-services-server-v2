BEGIN;

DELETE FROM tx2_service_mappings
WHERE (service_id, service_code) = (6000, 'HIGH_F9');

DELETE FROM service_locations WHERE service_id = 6000;

DELETE FROM locations WHERE location_id IN (100, 101);

DELETE FROM services WHERE service_id = 6000;

DELETE FROM organisations WHERE organisation_id = 7;

COMMIT;
