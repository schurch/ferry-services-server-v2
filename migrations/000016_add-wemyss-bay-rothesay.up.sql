BEGIN;

UPDATE services SET transxchange_service_code = 'FSACM03' WHERE service_id = '3';

UPDATE locations SET stop_point_id = '9300WMB' WHERE location_id = '7';
UPDATE locations SET stop_point_id = '9300RAY' WHERE location_id = '8';

COMMIT;
