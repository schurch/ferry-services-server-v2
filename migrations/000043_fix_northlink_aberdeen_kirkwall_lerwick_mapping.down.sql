DELETE FROM tx2_service_mappings
WHERE service_id = 1000
  AND service_code = 'NLKF_NL2';

UPDATE locations
SET stop_point_id = NULL
WHERE location_id IN (61, 62, 63)
  AND name IN ('Aberdeen', 'Kirkwall', 'Lerwick');
