BEGIN;

INSERT INTO services (service_id, area, route, status, organisation_id, updated) VALUES (41, 'ARRAN', 'Troon (TRO) - Brodick (BRO)', -99, 1, now());

INSERT INTO locations (location_id, name, coordinate) VALUES (97, 'Troon', ST_SetSRID(ST_Point(55.549294792361195, -4.682825196631705), 4326));

INSERT INTO service_locations (service_id, location_id) VALUES (5, 97);

INSERT INTO service_locations (service_id, location_id) VALUES (41, 3);
INSERT INTO service_locations (service_id, location_id) VALUES (41, 4);
INSERT INTO service_locations (service_id, location_id) VALUES (41, 97);

COMMIT;
