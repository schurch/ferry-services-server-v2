BEGIN;

UPDATE services SET transxchange_service_code = 'FSANL01' WHERE service_id = 1000;
UPDATE services SET transxchange_service_code = 'FSAWF01' WHERE service_id = 2000;

UPDATE locations SET stop_point_id = '9300SCR' WHERE location_id = 59;
UPDATE locations SET stop_point_id = '9300SNS' WHERE location_id = 60;
UPDATE locations SET stop_point_id = '9300MCP' WHERE location_id = 64;
UPDATE locations SET stop_point_id = '9300HUN' WHERE location_id = 65;


ALTER TABLE transxchange_services ADD COLUMN service_id INT NULL REFERENCES services (service_id);
UPDATE  transxchange_services ts SET service_id = (SELECT service_id FROM services s WHERE s.transxchange_service_code = ts.service_code);
ALTER TABLE services DROP COLUMN transxchange_service_code;

COMMIT;
