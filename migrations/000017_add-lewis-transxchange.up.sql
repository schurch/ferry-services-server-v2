BEGIN;

UPDATE services SET transxchange_service_code = 'FSACM25' WHERE service_id = '25';

UPDATE locations SET stop_point_id = '9300STO' WHERE location_id = '9';
UPDATE locations SET stop_point_id = '9300ULL' WHERE location_id = '10';

COMMIT;
