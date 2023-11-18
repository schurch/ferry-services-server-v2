BEGIN;

INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (40, 'NORTH UIST & HARRIS', 'Lochmaddy (LMA) - Tarbert (TAR)', 1, -99, now()) ON CONFLICT DO NOTHING;

INSERT INTO service_locations (service_id, location_id) VALUES (40, 39);
INSERT INTO service_locations (service_id, location_id) VALUES (40, 33);

COMMIT;
