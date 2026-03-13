BEGIN;

DELETE FROM tx2_service_mappings
WHERE (service_id, service_code) = (5000, 'PENT_PF1');

UPDATE locations
SET stop_point_id = NULL
WHERE location_id IN (98, 99);

COMMIT;
