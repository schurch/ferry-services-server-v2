BEGIN;

UPDATE transxchange_services SET service_id = NULL WHERE service_code = 'SLASF02';
UPDATE transxchange_services SET service_id = NULL WHERE service_code = 'SLASF01';
UPDATE transxchange_services SET service_id = NULL WHERE service_code = 'SLASF04';
UPDATE transxchange_services SET service_id = NULL WHERE service_code = 'SLASF05';
UPDATE transxchange_services SET service_id = NULL WHERE service_code = 'SLASF03';

UPDATE locations SET stop_point_id = NULL WHERE location_id = 66;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 67;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 68;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 69;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 70;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 71;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 72;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 73;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 74;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 75;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 76;

DELETE FROM service_locations WHERE service_id = 3000;
DELETE FROM service_locations WHERE service_id = 3001;
DELETE FROM service_locations WHERE service_id = 3002;
DELETE FROM service_locations WHERE service_id = 3003;
DELETE FROM service_locations WHERE service_id = 3004;

DELETE FROM services WHERE service_id = 3000;
DELETE FROM services WHERE service_id = 3001;
DELETE FROM services WHERE service_id = 3002;
DELETE FROM services WHERE service_id = 3003;
DELETE FROM services WHERE service_id = 3004;

DELETE FROM location_weather WHERE location_id = 66;
DELETE FROM location_weather WHERE location_id = 67;
DELETE FROM location_weather WHERE location_id = 68;
DELETE FROM location_weather WHERE location_id = 69;
DELETE FROM location_weather WHERE location_id = 70;
DELETE FROM location_weather WHERE location_id = 71;
DELETE FROM location_weather WHERE location_id = 72;
DELETE FROM location_weather WHERE location_id = 73;
DELETE FROM location_weather WHERE location_id = 74;
DELETE FROM location_weather WHERE location_id = 75;
DELETE FROM location_weather WHERE location_id = 76;

DELETE FROM locations WHERE location_id = 66;
DELETE FROM locations WHERE location_id = 67;
DELETE FROM locations WHERE location_id = 68;
DELETE FROM locations WHERE location_id = 69;
DELETE FROM locations WHERE location_id = 70;
DELETE FROM locations WHERE location_id = 71;
DELETE FROM locations WHERE location_id = 72;
DELETE FROM locations WHERE location_id = 73;
DELETE FROM locations WHERE location_id = 74;
DELETE FROM locations WHERE location_id = 75;
DELETE FROM locations WHERE location_id = 76;

DELETE FROM organisations WHERE organisation_id = 4;

COMMIT;
