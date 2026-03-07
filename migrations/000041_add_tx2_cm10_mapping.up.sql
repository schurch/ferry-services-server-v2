INSERT INTO tx2_service_mappings (service_id, service_code)
VALUES (9, 'CALM_CM10')
ON CONFLICT (service_id, service_code) DO NOTHING;
