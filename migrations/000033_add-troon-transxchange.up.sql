BEGIN;

UPDATE locations SET stop_point_id = '9300TRN' WHERE location_id = 97;

INSERT INTO transxchangeservice_services (service_id, service_code) VALUES (41, 'FSACM05');

COMMIT;
