BEGIN;

INSERT INTO organisations (organisation_id, name, website, local_phone, international_phone, email, x, facebook)
VALUES (
  7,
  'Highland Council',
  'https://www.highland.gov.uk/corran-ferry',
  '01855 841243',
  '+44 1855 841243',
  NULL,
  'https://twitter.com/CorranFerry',
  'https://www.facebook.com/CorranFerryService/'
);

INSERT INTO services (service_id, area, route, organisation_id, status, updated)
VALUES (6000, 'CORRAN', 'Nether Lochaber Ferry Terminal - Ardgour Ferry Terminal', 7, -99, now());

INSERT INTO locations (location_id, name, coordinate)
VALUES (100, 'Nether Lochaber Ferry Terminal', ST_SetSRID(ST_Point(56.72145, -5.23548), 4326));

INSERT INTO locations (location_id, name, coordinate)
VALUES (101, 'Ardgour Ferry Terminal', ST_SetSRID(ST_Point(56.72270, -5.24278), 4326));

UPDATE locations
SET stop_point_id = '9300CRR'
WHERE location_id = 100;

UPDATE locations
SET stop_point_id = '9300ARG'
WHERE location_id = 101;

INSERT INTO service_locations (service_id, location_id) VALUES (6000, 100);
INSERT INTO service_locations (service_id, location_id) VALUES (6000, 101);

INSERT INTO tx2_service_mappings (service_id, service_code)
VALUES (6000, 'HIGH_F9')
ON CONFLICT (service_id, service_code) DO NOTHING;

COMMIT;
