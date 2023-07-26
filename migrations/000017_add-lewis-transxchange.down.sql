BEGIN;

UPDATE services SET transxchange_service_code = NULL WHERE service_id = '25';

UPDATE locations SET stop_point_id = NULL WHERE location_id = '9';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '10';

COMMIT;
