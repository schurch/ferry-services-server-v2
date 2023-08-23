BEGIN;

UPDATE locations SET stop_point_id = NULL WHERE location_id = 77;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 78;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 79;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 80;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 81;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 82;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 83;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 85;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 86;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 87;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 88;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 89;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 90;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 91;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 92;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 93;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 94;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 95;
UPDATE locations SET stop_point_id = NULL WHERE location_id = 96;

DELETE FROM transxchangeservice_services WHERE service_id = 4000 AND service_code = 'ORAOF05';
DELETE FROM transxchangeservice_services WHERE service_id = 4001 AND service_code = 'ORAOF05';
DELETE FROM transxchangeservice_services WHERE service_id = 4002 AND service_code = 'ORAOF05';
DELETE FROM transxchangeservice_services WHERE service_id = 4003 AND service_code = 'ORAOF05';
DELETE FROM transxchangeservice_services WHERE service_id = 4004 AND service_code = 'ORAOF04';
DELETE FROM transxchangeservice_services WHERE service_id = 4005 AND service_code = 'ORAOF01';
DELETE FROM transxchangeservice_services WHERE service_id = 4006 AND service_code = 'ORAOF02';
DELETE FROM transxchangeservice_services WHERE service_id = 4007 AND service_code = 'ORAOF03';
DELETE FROM transxchangeservice_services WHERE service_id = 4008 AND service_code = 'ORAOF07';

COMMIT;