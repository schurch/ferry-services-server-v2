BEGIN;

CREATE TABLE organisations (
    organisation_id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
);

INSERT INTO organisations (organisation_id, name) VALUES (1, 'Caledonian MacBrayne');
INSERT INTO organisations (organisation_id, name) VALUES (2, 'NorthLink Ferries');
INSERT INTO organisations (organisation_id, name) VALUES (3, 'Western Ferries');

ALTER TABLE services DROP COLUMN IF EXISTS organisation;
ALTER TABLE services ADD COLUMN organisation_id INT NOT NULL DEFAULT 1 REFERENCES organisations (organisation_id);
UPDATE services SET organisation_id = 2 WHERE service_id = 1000;
ALTER TABLE services ALTER COLUMN organisation_id DROP DEFAULT;

ALTER TABLE vessels ADD COLUMN organisation_id INT NOT NULL DEFAULT 1 REFERENCES organisations (organisation_id);
UPDATE vessels SET organisation_id = 2 WHERE mmsi IN (235449000, 235450000, 235448000);

INSERT INTO services (service_id, area, route, organisation_id, status, updated) VALUES (2000, 'COWAL & DUNOON (Western Ferries)', 'McInroy''s Point (Gourock) - Hunters Quay (Dunoon)', 3, -99, now());

INSERT INTO locations (location_id, name, coordinate) VALUES (64, 'McInroy''s Point', ST_SetSRID(ST_Point(55.95160698614026, -4.85339972935733), 4326));
INSERT INTO locations (location_id, name, coordinate) VALUES (65, 'Hunters Quay', ST_SetSRID(ST_Point(55.970673603254305, -4.909260840891939), 4326));

INSERT INTO service_locations (service_id, location_id) VALUES (2000, 64);
INSERT INTO service_locations (service_id, location_id) VALUES (2000, 65);

COMMIT;
