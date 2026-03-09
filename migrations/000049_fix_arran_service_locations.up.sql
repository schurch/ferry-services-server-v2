BEGIN;

DELETE FROM service_locations
WHERE service_id = 5
  AND location_id = 97;

DELETE FROM service_locations
WHERE service_id = 41
  AND location_id = 3;

COMMIT;
