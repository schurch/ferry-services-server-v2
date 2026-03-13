BEGIN;

INSERT INTO organisations (organisation_id, name, website, local_phone, international_phone, email, x, facebook)
VALUES (
  6,
  'Pentland Ferries',
  'https://pentlandferries.co.uk/',
  '01856 831226',
  '+44 1856 831226',
  'sales@pentlandferries.co.uk',
  NULL,
  NULL
);

INSERT INTO services (service_id, area, route, organisation_id, status, updated)
VALUES (5000, 'PENTLAND FIRTH', 'Gills Bay - St Margaret''s Hope', 6, -99, now());

INSERT INTO locations (location_id, name, coordinate)
VALUES (98, 'Gills Bay', ST_SetSRID(ST_Point(58.641464, -3.228096), 4326));

INSERT INTO locations (location_id, name, coordinate)
VALUES (99, 'St Margaret''s Hope', ST_SetSRID(ST_Point(58.824266, -2.958273), 4326));

INSERT INTO service_locations (service_id, location_id) VALUES (5000, 98);
INSERT INTO service_locations (service_id, location_id) VALUES (5000, 99);

COMMIT;
