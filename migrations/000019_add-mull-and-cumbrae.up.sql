BEGIN;

UPDATE services SET transxchange_service_code = 'FSBCM07' WHERE service_id = '7';
UPDATE services SET transxchange_service_code = 'FSACM11' WHERE service_id = '11';

UPDATE locations SET stop_point_id = '9300LGS' WHERE location_id = '11';
UPDATE locations SET stop_point_id = '9300CUM' WHERE location_id = '12';
UPDATE locations SET stop_point_id = '9300OBA' WHERE location_id = '19';
UPDATE locations SET stop_point_id = '9300CNU' WHERE location_id = '38';

COMMIT;
