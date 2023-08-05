BEGIN;

UPDATE locations SET stop_point_id = NULL WHERE location_id = '59';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '60';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '64';
UPDATE locations SET stop_point_id = NULL WHERE location_id = '65';

ALTER TABLE services ADD COLUMN transxchange_service_code TEXT;
UPDATE services s SET transxchange_service_code = (SELECT service_code FROM transxchange_services ts WHERE s.service_id = ts.service_id);
ALTER TABLE transxchange_services DROP COLUMN service_id;

UPDATE services SET transxchange_service_code = NULL WHERE service_id = 1000;
UPDATE services SET transxchange_service_code = NULL WHERE service_id = 2000;

COMMIT;
