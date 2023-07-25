BEGIN;

UPDATE services SET transxchange_service_code = NULL WHERE service_id = '3';

UPDATE locations SET stop_point_id = NULL WHERE location_id = '7';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '8';

COMMIT;
