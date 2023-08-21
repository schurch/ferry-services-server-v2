BEGIN;

DELETE FROM service_locations WHERE service_id = 4000;
DELETE FROM service_locations WHERE service_id = 4001;
DELETE FROM service_locations WHERE service_id = 4002;
DELETE FROM service_locations WHERE service_id = 4003;
DELETE FROM service_locations WHERE service_id = 4004;
DELETE FROM service_locations WHERE service_id = 4005;
DELETE FROM service_locations WHERE service_id = 4006;
DELETE FROM service_locations WHERE service_id = 4007;
DELETE FROM service_locations WHERE service_id = 4008;

DELETE FROM services WHERE service_id = 4000;
DELETE FROM services WHERE service_id = 4001;
DELETE FROM services WHERE service_id = 4002;
DELETE FROM services WHERE service_id = 4003;
DELETE FROM services WHERE service_id = 4004;
DELETE FROM services WHERE service_id = 4005;
DELETE FROM services WHERE service_id = 4006;
DELETE FROM services WHERE service_id = 4007;
DELETE FROM services WHERE service_id = 4008;
DELETE FROM services WHERE service_id = 4009;

DELETE FROM location_weather WHERE location_id = 77;
DELETE FROM location_weather WHERE location_id = 78;
DELETE FROM location_weather WHERE location_id = 79;
DELETE FROM location_weather WHERE location_id = 80;
DELETE FROM location_weather WHERE location_id = 81;
DELETE FROM location_weather WHERE location_id = 82;
DELETE FROM location_weather WHERE location_id = 83;
DELETE FROM location_weather WHERE location_id = 84;
DELETE FROM location_weather WHERE location_id = 85;
DELETE FROM location_weather WHERE location_id = 86;
DELETE FROM location_weather WHERE location_id = 87;
DELETE FROM location_weather WHERE location_id = 88;
DELETE FROM location_weather WHERE location_id = 89;
DELETE FROM location_weather WHERE location_id = 90;
DELETE FROM location_weather WHERE location_id = 91;
DELETE FROM location_weather WHERE location_id = 92;
DELETE FROM location_weather WHERE location_id = 93;
DELETE FROM location_weather WHERE location_id = 94;
DELETE FROM location_weather WHERE location_id = 95;
DELETE FROM location_weather WHERE location_id = 96;

DELETE FROM locations WHERE location_id = 77;
DELETE FROM locations WHERE location_id = 78;
DELETE FROM locations WHERE location_id = 79;
DELETE FROM locations WHERE location_id = 80;
DELETE FROM locations WHERE location_id = 81;
DELETE FROM locations WHERE location_id = 82;
DELETE FROM locations WHERE location_id = 83;
DELETE FROM locations WHERE location_id = 84;
DELETE FROM locations WHERE location_id = 85;
DELETE FROM locations WHERE location_id = 86;
DELETE FROM locations WHERE location_id = 87;
DELETE FROM locations WHERE location_id = 88;
DELETE FROM locations WHERE location_id = 89;
DELETE FROM locations WHERE location_id = 90;
DELETE FROM locations WHERE location_id = 91;
DELETE FROM locations WHERE location_id = 92;
DELETE FROM locations WHERE location_id = 93;
DELETE FROM locations WHERE location_id = 94;
DELETE FROM locations WHERE location_id = 95;
DELETE FROM locations WHERE location_id = 96;

DELETE FROM organisations WHERE organisation_id = 5;

COMMIT;
