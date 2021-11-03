ALTER TABLE services DROP COLUMN IF EXISTS organisation;

DELETE FROM service_locations WHERE service_id = 1000 AND location_id = 59;
DELETE FROM service_locations WHERE service_id = 1000 AND location_id = 60;
DELETE FROM service_locations WHERE service_id = 1000 AND location_id = 61;
DELETE FROM service_locations WHERE service_id = 1000 AND location_id = 62;
DELETE FROM service_locations WHERE service_id = 1000 AND location_id = 63;

DELETE FROM location_weather WHERE location_id = 59;
DELETE FROM location_weather WHERE location_id = 60;
DELETE FROM location_weather WHERE location_id = 61;
DELETE FROM location_weather WHERE location_id = 62;
DELETE FROM location_weather WHERE location_id = 63;

DELETE FROM locations WHERE location_id = 59;
DELETE FROM locations WHERE location_id = 60;
DELETE FROM locations WHERE location_id = 61;
DELETE FROM locations WHERE location_id = 62;
DELETE FROM locations WHERE location_id = 63;

DELETE FROM services WHERE service_id = 1000;
