BEGIN;

UPDATE services SET transxchange_service_code = NULL WHERE service_id = '7';
UPDATE services SET transxchange_service_code = NULL WHERE service_id = '11';

UPDATE locations SET stop_point_id = NULL WHERE location_id = '11';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '12';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '19';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '38';

COMMIT;
