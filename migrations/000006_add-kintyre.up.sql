BEGIN;

INSERT INTO services (service_id, sort_order, area, route, status, updated) VALUES (36, '30', 'KINTYRE', 'Ardrossan (ARD) - Campbeltown (CAM)', -99, now()) ON CONFLICT DO NOTHING;

INSERT INTO service_locations (service_id, location_id) VALUES (36, 3);
INSERT INTO service_locations (service_id, location_id) VALUES (36, 53);

COMMIT;
