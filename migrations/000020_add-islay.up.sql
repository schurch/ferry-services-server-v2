BEGIN;

UPDATE services SET transxchange_service_code = 'FSACM09' WHERE service_id = '9';

UPDATE locations SET stop_point_id = '9300CSY' WHERE location_id = '26';
UPDATE locations SET stop_point_id = '9300PAK' WHERE location_id = '27';
UPDATE locations SET stop_point_id = '9300KEN' WHERE location_id = '28';
UPDATE locations SET stop_point_id = '9300PLN' WHERE location_id = '36';

INSERT INTO service_locations (service_id, location_id) VALUES (9, 19);
INSERT INTO service_locations (service_id, location_id) VALUES (9, 26);

COMMIT;
