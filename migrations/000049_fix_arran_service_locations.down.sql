BEGIN;

INSERT INTO service_locations (service_id, location_id)
VALUES (5, 97)
ON CONFLICT DO NOTHING;

INSERT INTO service_locations (service_id, location_id)
VALUES (41, 3)
ON CONFLICT DO NOTHING;

COMMIT;
