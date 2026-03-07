INSERT INTO tx2_service_mappings (service_id, service_code)
VALUES (1000, 'NLKF_NL2')
ON CONFLICT (service_id, service_code) DO NOTHING;

UPDATE locations
SET stop_point_id = '9300ABA'
WHERE location_id = 61
  AND name = 'Aberdeen';

UPDATE locations
SET stop_point_id = '9300KWH'
WHERE location_id = 62
  AND name = 'Kirkwall';

UPDATE locations
SET stop_point_id = '9300LHM'
WHERE location_id = 63
  AND name = 'Lerwick';
