BEGIN;

UPDATE locations
SET stop_point_id = '9300GIL'
WHERE location_id = 98;

UPDATE locations
SET stop_point_id = '9300SMH'
WHERE location_id = 99;

INSERT INTO tx2_service_mappings (service_id, service_code)
VALUES (5000, 'PENT_PF1')
ON CONFLICT (service_id, service_code) DO NOTHING;

COMMIT;
