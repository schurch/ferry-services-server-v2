BEGIN;

UPDATE locations SET stop_point_id = '9300TRN' WHERE location_id = 97;

INSERT INTO transxchangeservice_services (service_id, service_code)
SELECT 41, 'FSACM05' WHERE EXISTS (SELECT 1 FROM transxchangeservice_services WHERE service_id = 41);

COMMIT;
