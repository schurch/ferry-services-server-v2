BEGIN;

UPDATE services SET transxchange_service_code = NULL WHERE service_id = '9';

UPDATE locations SET stop_point_id = NULL WHERE location_id = '26';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '27';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '28';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '36';

DELETE FROM service_locations WHERE service_id = 9 AND location_id = 19;
DELETE FROM service_locations WHERE service_id = 9 AND location_id = 26;

COMMIT;
