BEGIN;

CREATE TABLE transxchangeservice_services (
    service_id INTEGER NULL REFERENCES services (service_id),
    service_code TEXT NOT NULL REFERENCES transxchange_services (service_code),
    PRIMARY KEY(service_id, service_code)
);

INSERT INTO transxchangeservice_services (service_id, service_code) SELECT  service_id, service_code FROM transxchange_services WHERE service_id IS NOT NULL;

ALTER TABLE transxchange_services DROP COLUMN service_id;

UPDATE locations SET stop_point_id = '9300KWL' WHERE location_id = 77;
UPDATE locations SET stop_point_id = '9300EDY' WHERE location_id = 78;
UPDATE locations SET stop_point_id = '9300STY' WHERE location_id = 79;
UPDATE locations SET stop_point_id = '9300NDY' WHERE location_id = 80;
UPDATE locations SET stop_point_id = '9300RAP' WHERE location_id = 81;
UPDATE locations SET stop_point_id = '9300KWS' WHERE location_id = 82;
UPDATE locations SET stop_point_id = '9300SPY' WHERE location_id = 83;
UPDATE locations SET stop_point_id = '9300GAE' WHERE location_id = 85;
UPDATE locations SET stop_point_id = '9300NHY' WHERE location_id = 86;
UPDATE locations SET stop_point_id = '9300HTN' WHERE location_id = 87;
UPDATE locations SET stop_point_id = '9300FLH' WHERE location_id = 88;
UPDATE locations SET stop_point_id = '9300LYS' WHERE location_id = 89;
UPDATE locations SET stop_point_id = '9300LHP' WHERE location_id = 90;
UPDATE locations SET stop_point_id = '9300WYR' WHERE location_id = 91;
UPDATE locations SET stop_point_id = '9300ROU' WHERE location_id = 92;
UPDATE locations SET stop_point_id = '9300TWL' WHERE location_id = 93;
UPDATE locations SET stop_point_id = '9300EGI' WHERE location_id = 94;
UPDATE locations SET stop_point_id = '9300PWL' WHERE location_id = 95;
UPDATE locations SET stop_point_id = '9300PWY' WHERE location_id = 96;

UPDATE service_locations SET location_id = 60 WHERE service_id = 4005 AND location_id = 84;

DELETE FROM location_weather WHERE location_id = 84;
DELETE FROM locations WHERE location_id = 84;

INSERT INTO transxchangeservice_services (service_id, service_code) SELECT 4000, 'ORAOF05' WHERE EXISTS (SELECT service_code FROM transxchange_services tx WHERE tx.service_code = 'ORAOF05');
INSERT INTO transxchangeservice_services (service_id, service_code) SELECT 4001, 'ORAOF05' WHERE EXISTS (SELECT service_code FROM transxchange_services tx WHERE tx.service_code = 'ORAOF05');
INSERT INTO transxchangeservice_services (service_id, service_code) SELECT 4002, 'ORAOF05' WHERE EXISTS (SELECT service_code FROM transxchange_services tx WHERE tx.service_code = 'ORAOF05');
INSERT INTO transxchangeservice_services (service_id, service_code) SELECT 4003, 'ORAOF05' WHERE EXISTS (SELECT service_code FROM transxchange_services tx WHERE tx.service_code = 'ORAOF05');
INSERT INTO transxchangeservice_services (service_id, service_code) SELECT 4004, 'ORAOF04' WHERE EXISTS (SELECT service_code FROM transxchange_services tx WHERE tx.service_code = 'ORAOF04');
INSERT INTO transxchangeservice_services (service_id, service_code) SELECT 4005, 'ORAOF01' WHERE EXISTS (SELECT service_code FROM transxchange_services tx WHERE tx.service_code = 'ORAOF01');
INSERT INTO transxchangeservice_services (service_id, service_code) SELECT 4006, 'ORAOF02' WHERE EXISTS (SELECT service_code FROM transxchange_services tx WHERE tx.service_code = 'ORAOF02');
INSERT INTO transxchangeservice_services (service_id, service_code) SELECT 4007, 'ORAOF03' WHERE EXISTS (SELECT service_code FROM transxchange_services tx WHERE tx.service_code = 'ORAOF03');
INSERT INTO transxchangeservice_services (service_id, service_code) SELECT 4008, 'ORAOF07' WHERE EXISTS (SELECT service_code FROM transxchange_services tx WHERE tx.service_code = 'ORAOF07');

COMMIT;
